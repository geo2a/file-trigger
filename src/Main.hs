{-# LANGUAGE GeneralizedNewtypeDeriving,
             DeriveDataTypeable,
             DeriveGeneric,
             FlexibleContexts,
             OverloadedStrings,
             ConstraintKinds #-}

module Main where

import Control.Applicative
import Control.Monad
import Control.Eff
import Control.Eff.Lift
import Control.Eff.Reader.Lazy
import Control.Eff.State.Lazy
import Control.Concurrent
import qualified GHC.Generics as GHC 
import System.Environment
import System.Process
import System.Directory
import System.FilePath
import Data.Time.Clock
import Data.List
import Data.Typeable
import Data.Aeson
import Data.Yaml

----------------------
---- Domain Types ----
----------------------

type RefreshInterval = Int

data AppConfig = AppConfig 
  { directory         :: FilePath -- ^ path directory to watch 
  , logFileName       :: FilePath -- ^ filename of log file
  , refreshRate       :: RefreshInterval -- ^ refresh rate in seconds 
  , onCreateScript    :: FilePath -- ^ action to execute on file creation
  , onRemoveScript    :: FilePath -- ^ action to execute on file modification
  , onModifyScript    :: FilePath -- ^ action to execute on file deletion
  , ignore            :: [FilePath] -- ^ list of ignored files
  } deriving (Typeable, Show, GHC.Generic)

instance FromJSON AppConfig
instance ToJSON AppConfig where
  toJSON = genericToJSON defaultOptions

data FileInfo = FileInfo 
  { path         :: FilePath
  , lastModified :: UTCTime
  } deriving (Show)

instance Eq FileInfo where
  x == y = path x == path y

data FileSystemEvent = Created 
                     | Modified
                     | Deleted
  deriving (Show, Eq)

data AppState = AppState 
  { filesInfo  :: [FileInfo]
  , checkPoint :: UTCTime
  } deriving (Typeable, Show)

data LogEntry = LogEntry 
  { file  :: FilePath
  , event :: FileSystemEvent
  , time  :: UTCTime
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

readConfig :: FilePath -> IO AppConfig
readConfig fname =
    either (error . show) id <$>
    decodeFileEither fname  

------------------------
---- Business Logic ----
------------------------

-- | Type alias for effects set, demanded by application
type DemandedEffects r = 
  ( Member (Reader AppConfig) r
  , Member (State AppState)   r 
  , SetMember Lift (Lift IO)  r
  )

-- | Main loop
loop :: DemandedEffects r => Eff r ()
loop = do
  cfg <- ask
  (AppState  prevFilesList lastTime) <- get  
  currentFilesList <- lift $ getFilesInfo $ directory cfg 
  handleCreate currentFilesList
  handleRemove currentFilesList
  handleModify currentFilesList
  lift $ threadDelay $ refreshRate cfg
  curTime <- lift getCurrentTime
  put $ (AppState currentFilesList curTime)
  loop

-----------------------------
---- Loop Event handlers ----
-----------------------------

-- | Helper function for script invokation
invokeScript :: FilePath -> FileInfo -> IO ()
invokeScript script fileInfo = do
  callProcess script [path fileInfo]

handleCreate :: DemandedEffects r => [FileInfo] -> Eff r ()
handleCreate currentFilesInfo = do
  cfg <- ask
  (AppState prevFilesInfo lastTime) <- get
  let createdFilesInfo = 
          currentFilesInfo \\ prevFilesInfo
  lift $ mapM_ (invokeScript (onCreateScript cfg)) createdFilesInfo 

handleRemove :: DemandedEffects r => [FileInfo] -> Eff r ()
handleRemove currentFilesInfo = do
  cfg <- ask
  (AppState prevFilesInfo lastTime) <- get
  let removedFilesInfo = 
          prevFilesInfo \\ currentFilesInfo 
  lift $ mapM_ (invokeScript (onRemoveScript cfg)) removedFilesInfo

handleModify :: DemandedEffects r => [FileInfo] -> Eff r ()
handleModify currentFilesInfo = do
  cfg <- ask
  (AppState prevFilesInfo lastTime) <- get
  let modifiedFilesInfo = filter (`elem` prevFilesInfo) .
        filter (\x -> not $ path x `elem` (ignore cfg)) . map snd .
        filter (uncurry older) $ zip prevFilesInfo currentFilesInfo 
      entries = map (makeLogEntry Modified) modifiedFilesInfo
  lift $ mapM_ (invokeScript (onModifyScript cfg)) modifiedFilesInfo
  where older x y = lastModified y > lastModified x

-- | Handles all effects produced by application. 
runApp action cfg initState = 
  runLift . runState initState . runReader action $ cfg

main = do
  greetings
  args <- getArgs
  if null args
  then do
    error "Error: please specify config file name"
  else do
    cfg <- readConfig . head $ args
     
    setCurrentDirectory $ directory cfg 
    startTime <- getCurrentTime
    startDirectoryContents <- getDirectoryContents $ directory cfg
    let startFilesInfo = map (uncurry FileInfo) $ 
          zip startDirectoryContents (repeat startTime)
    runApp loop cfg (AppState startFilesInfo startTime)
      where
        greetings =
          putStrLn "File Trigger, v0.1"
