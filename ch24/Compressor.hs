import Control.Concurrent (forkIO)
import Control.Exception (handle, SomeException)
import Control.Monad (forever)
import qualified Data.ByteString.Lazy as L

-- Provided by the 'readline' package
import System.Console.Readline (readline)

-- Provided by the 'zlib' package
import Codec.Compression.GZip (compress)

main = do
  maybeLine <- readline "Enter a file to compress> "
  case maybeLine of
    Nothing -> return () -- user entered EOF
    Just "" -> return () -- threat no name as "want to quit"
    Just name -> do
      handleSome print $ do
        content <- L.readFile name
        forkIO (compressFile name content)
        return ()
      main
  where compressFile path = L.writeFile (path ++ ".gz") . compress

handleSome :: (SomeException -> IO a) -> IO a -> IO a
handleSome = handle
