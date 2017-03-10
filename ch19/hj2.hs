import Control.Exception

handler :: ArithException -> IO ()
handler e = putStrLn $ "Caught error: divede by zero" ++ show e

safePrint :: Integer -> IO ()
safePrint x = handleJust arithExceptions handler (print x)
