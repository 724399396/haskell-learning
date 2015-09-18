import Control.Monad (filterM)
import System.Directory (Permissions(..), getModificationTime, getPermissions)
import Data.Time.Clock (UTCTime)
import System.FilePath (takeExtension)
import Control.Exception (bracket,handle, SomeException)
import System.IO (IOMode(..), hClose, hFileSize, openFile)

-- the fucntion we wrote earlier
import RecursiveContents (getRecursiveContents)

type Predicate = FilePath -- path to directory entry
               -> Permissions --permissions
               -> Maybe Integer -- file size(Nothing if not file)
               -> UTCTime -- last modified
               -> Bool

handleAny :: (SomeException -> IO a) -> IO a -> IO a
handleAny = handle

-- soon to be defined
getFileSize :: FilePath -> IO (Maybe Integer)

getFileSize path = handleAny (\_ -> return Nothing) $
  bracket (openFile path ReadMode) hClose $ \h -> do
    size <- hFileSize h
    return (Just size)

betterFind :: Predicate -> FilePath -> IO [FilePath]

betterFind p path = getRecursiveContents path >>= filterM check
    where check name = do
            perms <- getPermissions name
            size <- getFileSize name
            modified <- getModificationTime name
            return (p name perms size modified)

simpleFileSize :: FilePath -> IO Integer
simpleFileSize path = do
    h <- openFile path ReadMode
    size <- hFileSize h
    hClose h
    return size

saferFileSize :: FilePath -> IO (Maybe Integer)
saferFileSize path = do    
        h <- openFile path ReadMode
        size <- hFileSize h
        hClose h
        return (Just size)

myTest path _ (Just size) _ =
    takeExtension path == ".cpp" && size > 131072
myTest _ _ _ _ = False

