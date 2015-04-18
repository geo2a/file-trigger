{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.RWS
import Control.Concurrent
import System.Environment
import System.Process
import System.Directory
import System.FilePath
import Data.Time.Clock
import Data.List
import Text.Regex.Posix

isSpecialFile :: FilePath -> Bool
isSpecialFile "."  = True
isSpecialFile ".." = True
isSpecialFile _    = False

type ShellScript = String

data AppConfig = AppConfig {
      baseDir           :: FilePath,
      logFile           :: FilePath,
      refreshInterval   :: Int,
      ignore            :: [FilePath],
      onCreate          :: ShellScript
    } deriving (Show)

data FileInfo = FileInfo {
  path         :: FilePath,
  lastModified :: UTCTime
} deriving (Show)

instance Eq FileInfo where
  x == y = path x == path y

data AppState = AppState {
      filesInfo  :: [FileInfo],
      checkPoint :: UTCTime
    } deriving (Show)

data LogEntry = LogEntry {
      file  :: FilePath,
      event :: FileSystemEvent,
      time  :: UTCTime
} deriving (Show)

newtype MyApp a = MyApp {
      runA :: RWST AppConfig [LogEntry] AppState IO a
    } deriving (Functor, Applicative, 
                Monad, MonadIO, 
                MonadReader AppConfig,
                MonadWriter [LogEntry], 
                MonadState AppState)

data FileSystemEvent = Created 
                     | Modified
                     | Deleted
  deriving (Show, Eq)

runMyApp :: MyApp a -> AppConfig -> AppState -> IO (a, AppState, [LogEntry])
runMyApp k config initState =
  runRWST (runA k) config initState

insertLogEntry :: LogEntry -> MyApp ()
insertLogEntry entry = do
  log <- logFile `fmap` ask
  liftIO $ appendFile log (show entry ++ "\n")

makeLogEntry :: FileSystemEvent -> FileInfo -> LogEntry 
makeLogEntry event (FileInfo file time) = LogEntry file event time 

watchCreatedFiles :: [FileInfo] -> MyApp [LogEntry]
watchCreatedFiles currentFilesInfo = do
  ignoredFiles <- ignore `fmap` ask 
  script <- onCreate `fmap` ask
  (AppState prevFilesInfo lastTime) <- get
  let createdFilesInfo = 
        filter (\x -> not $ path x `elem` ignoredFiles) $ 
          currentFilesInfo \\ prevFilesInfo 
      entries = map (makeLogEntry Created) createdFilesInfo
  when (not . null $ entries) 
    (liftIO $ system script >> return ())
  return entries

watchDeletedFiles :: [FileInfo] -> MyApp [LogEntry]
watchDeletedFiles currentFilesInfo = do
  ignoredFiles <- ignore `fmap` ask 
  (AppState prevFilesInfo lastTime) <- get
  let deletedFilesInfo = 
        filter (\x -> not $ path x `elem` ignoredFiles) $ 
          prevFilesInfo \\ currentFilesInfo 
      entries = map (makeLogEntry Deleted) deletedFilesInfo
  return entries

watchModifiedFiles :: [FileInfo] -> MyApp [LogEntry]
watchModifiedFiles currentFilesInfo = do
  ignoredFiles <- ignore `fmap` ask 
  (AppState prevFilesInfo lastTime) <- get
  let modifiedFilesInfo = filter (`elem` prevFilesInfo) .
        filter (\x -> not $ path x `elem` ignoredFiles) . map snd .
        filter (uncurry older) $ zip prevFilesInfo currentFilesInfo 
      entries = map (makeLogEntry Modified) modifiedFilesInfo
  return entries
  where older x y = lastModified y > lastModified x

getFilesInfo :: FilePath -> IO [FileInfo]
getFilesInfo dir = do
  ls <- liftIO $ getDirectoryContents dir
  modifTimes <- liftIO $ mapM getModificationTime ls 
  return $ map (uncurry FileInfo) (zip ls modifTimes)

loop :: MyApp ()
loop = do
  (AppConfig dir log refreshInterval _ _) <- ask
  (AppState  prevFilesList lastTime) <- get  
  currentFilesList <- liftIO $ getFilesInfo dir
  created  <- (watchCreatedFiles currentFilesList)
  deleted  <- (watchDeletedFiles currentFilesList)
  modified <- (watchModifiedFiles currentFilesList)
  mapM insertLogEntry (created ++ deleted ++ modified)
  liftIO $ threadDelay refreshInterval
  curTime <- liftIO getCurrentTime
  put $ (AppState currentFilesList curTime)
  loop

--------------------------
----Configuration Info----
--------------------------

defaultConfig :: AppConfig
defaultConfig = AppConfig "." "app.log" 1000000 
                          [".","..","app.log"] onCreateScript

parseCfg :: String -> AppConfig
parseCfg cfgStr =
  let [workDir,delay,log, scriptOnCreate] = lines cfgStr
      ignoreByDefault = [".","..",log]
  in AppConfig workDir log (read delay) ignoreByDefault scriptOnCreate

onCreateScript :: String
onCreateScript = "echo \"hello\""

main = do
  args <- getArgs
  cfg  <- if null args
          then do
            putStrLn "Running with default config"
            return defaultConfig 
          else 
            parseCfg `fmap` (readFile $ head args)
  putStrLn "Directory Keeper v0.0.1"
  setCurrentDirectory $ baseDir cfg
  curDir <- getCurrentDirectory
  putStrLn $ "Running in directory: " ++ curDir
  startTime <- getCurrentTime
  putStrLn $ "Start Time: " ++ show startTime
  putStrLn $ "Using config: " ++ (show cfg)
  filesInfo <- getFilesInfo curDir
  runMyApp loop cfg (AppState filesInfo startTime)

