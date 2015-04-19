{-# LANGUAGE GeneralizedNewtypeDeriving,
             DeriveDataTypeable,
             FlexibleContexts,
             OverloadedStrings #-}

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
import Data.Yaml


isSpecialFile :: FilePath -> Bool
isSpecialFile "."  = True
isSpecialFile ".." = True
isSpecialFile _    = False

----------------------
---- Domain Types ----
----------------------

type ShellScript = String

type RefreshInterval = Int

data AppConfig = AppConfig {
      directory         :: FilePath,
      logFileName       :: FilePath,
      refreshRate       :: RefreshInterval,
      onCreateScript    :: ShellScript
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

--------------------------
---- Helper Functions ----
--------------------------

-- | Sort of zipping file paths with last 
-- | modification times
getFilesInfo :: FilePath -> IO [FileInfo]
getFilesInfo dir = do
  ls <- getDirectoryContents dir
  modifTimes <- mapM getModificationTime ls 
  return $ map (uncurry FileInfo) (zip ls modifTimes)

-- | Inserting entry in app external log, IO 
insertLogEntry :: (Member (Reader AppConfig) r, 
                   Member (State AppState)   r, 
                   SetMember Lift (Lift IO)  r) => 
                     LogEntry -> Eff r ()
insertLogEntry entry = do
  log <- logFileName `fmap` ask
  lift $ appendFile log (show entry ++ "\n")

makeLogEntry :: FileSystemEvent -> FileInfo -> LogEntry 
makeLogEntry event (FileInfo file time) = LogEntry file event time 

--------------------------------
---- Parsing Configurations ----
--------------------------------

instance FromJSON AppConfig where
    parseJSON (Object m) = AppConfig <$>
        m .: "directory"   <*>
        m .: "logFileName" <*>
        m .: "refreshRate" <*>
        m .: "onCreateScript"
    parseJSON x = fail ("not an object: " ++ show x)

readConfig :: FilePath ->  IO AppConfig
readConfig fname =
    either (error . show) id <$>
    decodeFileEither fname  

------------------------
---- Business Logic ----
------------------------

-- | Main loop
loop :: (Member (Reader AppConfig) r, 
         Member (State AppState)   r, 
         SetMember Lift (Lift IO)  r) => Eff r ()
loop = do
  (AppConfig dir log refreshInterval _) <- ask
  (AppState  prevFilesList lastTime) <- get  
  currentFilesList <- lift $ getFilesInfo dir
  handleCreate currentFilesList
  lift $ threadDelay refreshInterval
  curTime <- lift getCurrentTime
  put $ (AppState currentFilesList curTime)
  loop

-----------------------------
---- Loop Event handlers ----
-----------------------------
invokeScript :: ShellScript -> FileInfo -> IO ()
invokeScript script fileInfo =
  system (script ++ ' ':(path fileInfo)) >> return ()

handleCreate :: (Member (Reader AppConfig) r, 
                 Member (State AppState)   r, 
                 SetMember Lift (Lift IO)  r) => 
  [FileInfo] -> Eff r ()
handleCreate currentFilesInfo = do
  cfg <- ask
  (AppState prevFilesInfo lastTime) <- get
  let createdFilesInfo = 
          currentFilesInfo \\ prevFilesInfo
  lift $ mapM_ (invokeScript (onCreateScript cfg)) createdFilesInfo 


--watchCreatedFiles :: [FileInfo] -> MyApp [LogEntry]
--watchCreatedFiles currentFilesInfo = do
--  ignoredFiles <- ignore `fmap` ask 
--  (AppState prevFilesInfo lastTime) <- get
--  let createdFilesInfo = 
--        filter (\x -> not $ path x `elem` ignoredFiles) $ 
--          currentFilesInfo \\ prevFilesInfo 
--      entries = map (makeLogEntry Created) createdFilesInfo
--  return entries

-- | Handles all effects produced
-- | by application. 
runApp action cfg initState = 
  runLift . runState initState . runReader action $ cfg

main = do
  greetings
  cfg <- readConfig =<< head `fmap` getArgs
  setCurrentDirectory $ directory cfg 
  startTime <- getCurrentTime
  startDirectoryContents <- getDirectoryContents $ directory cfg
  let startFilesInfo = map (uncurry FileInfo) $ 
        zip startDirectoryContents (repeat startTime)
  runApp loop cfg (AppState startFilesInfo startTime)
    where
      greetings =
        putStrLn "File Trigger, v0.1"
