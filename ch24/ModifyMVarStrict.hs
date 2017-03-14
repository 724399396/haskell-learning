import Control.Concurrent (MVar, putMVar, takeMVar)
import Control.Exception (block, catch, throw, unblock)
import Prelude hiding (catch)

modifyMVar_strict ::MVar a -> (a -> IO (a,b)) -> IO ()
modifyMVar_strict m io = block $ do
  a <- takeMVar m
  !b <- unblock (io a) `catch` \e ->
           putMVar m a >> throw e
  putMVar m b
