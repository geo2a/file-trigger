{-# LANGUAGE GeneralizedNewtypeDeriving,
             DeriveDataTypeable,
             FlexibleContexts,
             OverlappingInstances#-}

module Main where

import Control.Applicative
import Control.Monad
import Control.Eff
import Control.Eff.Lift
import Control.Eff.Reader.Lazy
import Control.Eff.State.Lazy
import Control.Concurrent
import System.Environment
import System.Process
import System.Directory
import System.FilePath
import Data.Time.Clock
import Data.List
import Data.Typeable
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
    } deriving (Typeable, Show)

data FileInfo = FileInfo {
  path         :: FilePath,
  lastModified :: UTCTime
} deriving (Show)

instance Eq FileInfo where
  x == y = path x == path y

data FileSystemEvent = Created 
                     | Modified
                     | Deleted
  deriving (Show, Eq)

data AppState = AppState {
      filesInfo  :: [FileInfo],
      checkPoint :: UTCTime
    } deriving (Typeable, Show)

data LogEntry = LogEntry {
      file  :: FilePath,
      event :: FileSystemEvent,
      time  :: UTCTime
} deriving (Show)

f :: (Member (Reader AppConfig) r, 
      Member (State AppState)   r, 
      SetMember Lift (Lift IO)  r) => Eff r ()
f = do
  cfg   <- ask
  state <- get
  lift $ putStrLn "Hi, bro!"
  lift $ putStrLn $ "Running with configuration: " ++ 
    show (cfg :: AppConfig)
  lift $ putStrLn $ "Initial state: " ++ 
    show (state :: AppState)

runApp action cfg initState = 
  runLift $ runState initState (runReader action cfg)

loop ::(Member (Reader AppConfig) r, 
        Member (State AppState)   r, 
        SetMember Lift (Lift IO)  r) => Eff r ()
loop = do
  (AppConfig dir log refreshInterval _ _) <- ask
  (AppState  prevFilesList lastTime) <- get  
  currentFilesList <- lift $ getFilesInfo dir
  --created  <- (watchCreatedFiles currentFilesList)
  --deleted  <- (watchDeletedFiles currentFilesList)
  --modified <- (watchModifiedFiles currentFilesList)
  --mapM insertLogEntry (created ++ deleted ++ modified)
  lift $ threadDelay refreshInterval
  curTime <- lift getCurrentTime
  put $ (AppState currentFilesList curTime)
  loop

getFilesInfo :: FilePath -> IO [FileInfo]
getFilesInfo dir = do
  ls <- getDirectoryContents dir
  modifTimes <- mapM getModificationTime ls 
  return $ map (uncurry FileInfo) (zip ls modifTimes)

main = do
  startTime <- getCurrentTime
  runApp f defaultCfg (AppState [] startTime)

defaultCfg :: AppConfig
defaultCfg = 
  AppConfig "." "app.log" 1000000 [".","..","app.log"] ""

insertLogEntry :: 
      (Member (Reader AppConfig) r, 
       Member (State AppState)   r, 
       SetMember Lift (Lift IO)  r) => 
         LogEntry -> Eff r ()
insertLogEntry entry = do
  log <- logFile `fmap` ask
  lift $ appendFile log (show entry ++ "\n")

makeLogEntry :: FileSystemEvent -> FileInfo -> LogEntry 
makeLogEntry event (FileInfo file time) = LogEntry file event time 

--watchCreatedFiles :: [FileInfo] -> MyApp [LogEntry]
--watchCreatedFiles currentFilesInfo = do
--  ignoredFiles <- ignore `fmap` ask 
--  script <- onCreate `fmap` ask
--  (AppState prevFilesInfo lastTime) <- get
--  let createdFilesInfo = 
--        filter (\x -> not $ path x `elem` ignoredFiles) $ 
--          currentFilesInfo \\ prevFilesInfo 
--      entries = map (makeLogEntry Created) createdFilesInfo
--  when (not . null $ entries) 
--    (liftIO $ system script >> return ())
--  return entries

--watchDeletedFiles :: [FileInfo] -> MyApp [LogEntry]
--watchDeletedFiles currentFilesInfo = do
--  ignoredFiles <- ignore `fmap` ask 
--  (AppState prevFilesInfo lastTime) <- get
--  let deletedFilesInfo = 
--        filter (\x -> not $ path x `elem` ignoredFiles) $ 
--          prevFilesInfo \\ currentFilesInfo 
--      entries = map (makeLogEntry Deleted) deletedFilesInfo
--  return entries

--watchModifiedFiles :: [FileInfo] -> MyApp [LogEntry]
--watchModifiedFiles currentFilesInfo = do
--  ignoredFiles <- ignore `fmap` ask 
--  (AppState prevFilesInfo lastTime) <- get
--  let modifiedFilesInfo = filter (`elem` prevFilesInfo) .
--        filter (\x -> not $ path x `elem` ignoredFiles) . map snd .
--        filter (uncurry older) $ zip prevFilesInfo currentFilesInfo 
--      entries = map (makeLogEntry Modified) modifiedFilesInfo
--  return entries
--  where older x y = lastModified y > lastModified x

----------------------------
------Configuration Info----
----------------------------

--defaultConfig :: AppConfig
--defaultConfig = AppConfig "." "app.log" 1000000 
--                          [".","..","app.log"] onCreateScript

--parseCfg :: String -> AppConfig
--parseCfg cfgStr =
--  let [workDir,delay,log, scriptOnCreate] = lines cfgStr
--      ignoreByDefault = [".","..",log]
--  in AppConfig workDir log (read delay) ignoreByDefault scriptOnCreate

--onCreateScript :: String
--onCreateScript = "echo \"hello\""

--main = do
--  args <- getArgs
--  cfg  <- if null args
--          then do
--            putStrLn "Running with default config"
--            return defaultConfig 
--          else 
--            parseCfg `fmap` (readFile $ head args)
--  putStrLn "Directory Keeper v0.0.1"
--  setCurrentDirectory $ baseDir cfg
--  curDir <- getCurrentDirectory
--  putStrLn $ "Running in directory: " ++ curDir
--  startTime <- getCurrentTime
--  putStrLn $ "Start Time: " ++ show startTime
--  putStrLn $ "Using config: " ++ (show cfg)
--  filesInfo <- getFilesInfo curDir
--  runMyApp loop cfg (AppState filesInfo startTime)

