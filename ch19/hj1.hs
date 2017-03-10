import Control.Exception

catchIt :: Exception -> Maybe ()
catchIt (ArithException DivideByZero) = Just ()
catchIt _ = Nothing

handler :: () -> IO ()
handler _ = putStrLn "Caught error: divede by zero"

safePrint :: Integer -> IO ()
safePrint x = handleJust catchIt handler (print x)
