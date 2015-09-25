{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}

import MonadHandle
import qualified System.IO
import System.IO (IOMode(..))
import Control.Monad.Trans (MonadIO(..), MonadTrans(..))
import System.Directory (removeFile)

import SafeHello

instance MonadHandle System.IO.Handle IO where
  openFile = System.IO.openFile
  hPutStr = System.IO.hPutStr
  hClose = System.IO.hClose
  hGetContents = System.IO.hGetContents
  hPutStrLn = System.IO.hPutStrLn

class (MonadHandle h m, MonadIo m) => MonadHandleIO h m | m -> h


instance MonadHandleIO System.IO.Handle IO

tidierHello :: (MonadHandleIO h m) => FilePath -> m ()
tidierHello path = do
	SafeHello path
	liftIO (removeFile path)

tidyHello :: (MonadIO m, MonadHandle h m) => FilePaht -> m ()
	safeHello path
	liftIO (removeFile path)