import WriterIO
import Control.Monad.State
import Control.Arrow (second)
import System.IO (IOMode(..))
import MonadHandle
import Control.Monad (when)
import Test.QuickCheck

uncons :: [a] -> Maybe (a, [a])
uncons (x:xs) = Just (x,xs)
uncons _ = Nothing

isCloseE :: Event -> Bool
isCloseE (Close _) = True
isCloseE _ = False

isPutE :: Event -> Bool
isPutE (Put _ _) = True
isPutE _ = False

isCloseOf :: String -> Event -> Bool
isCloseOf h (Close hstr) | h == hstr = True
isCloseOf _ _ = False

isOpenOf :: String -> Event -> Bool
isOpenOf h (Open hstr _) | h == hstr = True
isOpenOf _ _ = False

hasPutAfterClose :: [Event] -> Bool
hasPutAfterClose xs =
  let (acc, ys) = break isPutE xs
      hasPuts ls rs =
        case uncons rs of
          Nothing -> [False]
          Just (Put h s, rest) ->
            let (rls, rs') =break isPutE rest
                ls' = filter (\x -> isOpenOf h x || isCloseOf h x) ls
            in  any (isCloseOf h) ls : hasPuts (ls' ++ rls) rs'
  in or $ hasPuts acc ys

prop_monadBehave :: (FilePath -> WriterIO ()) -> FilePath -> Bool
prop_monadBehave mf s = not . hasPutAfterClose . snd . runWriterIO $ mf s

safeHello :: MonadHandle h m => FilePath -> m ()
safeHello path = do
    h <- openFile path WriteMode
    hPutStrLn h "hello world"
    hClose h

unsafeHello :: MonadHandle h m => FilePath -> m ()
unsafeHello path = do
    h <- openFile path WriteMode
    hPutStrLn h "hello world"
    hClose h
    when (path == "") $ hPutStrLn h "malicious attempt"

main = do
    putStrLn "Testing safeHello for arbitrary file name inputs: "
    quickCheck $ prop_monadBehave safeHello
    putStrLn "Testing unsafeHello for arbitrary file name inputs: "
    quickCheck $ prop_monadBehave unsafeHello
    
