import Control.Concurrent

data BoundedChan a = BC {
  n :: Int, xs :: MVar [a]
  }

newBoundedChan :: Int -> IO (BoundedChan a)
newBoundedChan n = do
  xsm <- newMVar []
  return $ BC n xsm

writeBoundedChan :: BoundedChan a -> a -> IO ()
writeBoundedChan (BC n xsm) x =
  modifyMVar xsm $ \xs -> do                   
                     let l = length xs
                     if (l < n - 1)
                     then (x:xs, ())

                        
