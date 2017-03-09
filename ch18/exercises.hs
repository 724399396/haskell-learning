{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts #-}

import System.Directory
import System.FilePath
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import CountEntries

data AppConfig = AppConfig {
  cfgMaxDepth :: Int
  } deriving (Show)

data AppState = AppState {
  stDeepestReached :: Int
  } deriving (Show)

type App = WriterT [(FilePath,Int)] (ReaderT AppConfig (StateT AppState IO))

runApp :: App a -> Int -> IO ((a, [(FilePath,Int)]), AppState)
runApp k maxDepth =
  let config = AppConfig maxDepth
      state = AppState 0
  in runStateT (runReaderT (runWriterT k) config) state

constrainedCount :: Int -> FilePath -> App ()
constrainedCount curDepth path = do
  contents <- liftIO . listDirectory $ path
  cfg <- ask
  tell [(path, length contents)]
  forM_ contents $ \name -> do
             let newPath = path </> name
             isDir <- liftIO $ doesDirectoryExist newPath
             when (isDir && curDepth < cfgMaxDepth cfg) $ do
                 let newDepth = curDepth + 1
                 st <- get
                 when (stDeepestReached st < newDepth) $
                   put st {stDeepestReached = newDepth }
                 constrainedCount newDepth newPath
