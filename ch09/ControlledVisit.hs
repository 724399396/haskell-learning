-- file: ch09/ControlledVisit.hs
module ControlledVisit where

import System.Directory (Permissions(..), getModificationTime, getPermissions,getDirectoryContents)
import Data.Time.Clock (UTCTime)
import System.FilePath (takeExtension,splitDirectories,(</>))
import Control.Exception (bracket,handle, SomeException)
import System.IO (IOMode(..), hClose, hFileSize, openFile)
import Control.Monad (liftM,forM)
import Data.List (sortBy)
import Prelude hiding (traverse)

data Info = Info {
    infoPath :: FilePath,
    infoPerms :: Maybe Permissions,
    infoSize :: Maybe Integer,
    infoModTime :: Maybe UTCTime
} deriving (Eq, Ord, Show)

getInfo :: FilePath -> IO Info

traverse :: ([Info] -> [Info]) -> FilePath -> IO [Info]
traverse order path = do
  names <- getUsefulContents path
  contents <- mapM getInfo (path : map (path </>) names)
  liftM concat $ forM (order contents) $ \info -> do
      if isDirectory info && infoPath info /= path
        then traverse order (infoPath info)
        else return [info]

getUsefulContents :: FilePath -> IO [String]
getUsefulContents path = do
  names <- getDirectoryContents path
  return (filter (`notElem` [".", ".."]) names)

isDirectory :: Info -> Bool
isDirectory = maybe False searchable . infoPerms

maybeIO :: IO a -> IO (Maybe a)
maybeIO act = handleAny (\_ -> return Nothing) (Just `liftM` act)

handleAny :: (SomeException -> IO a) -> IO a -> IO a
handleAny = handle

getInfo path = do
  perms <- maybeIO (getPermissions path)
  size <- maybeIO (bracket (openFile path ReadMode) hClose hFileSize)
  modified <- maybeIO (getModificationTime path)
  return (Info path perms size modified)

traverseVerbose order path = do
  names <- getDirectoryContents path
  let usefulNames = filter (`notElem` [".",".."]) names
  contents <- mapM getEntryName ("" : usefulNames)
  recursiveContents <- mapM recurse (order contents)
  return (concat recursiveContents)
    where getEntryName name = getInfo (path </> name)
          isDirectory info = case infoPerms info of
                               Nothing -> False
                               Just perms -> searchable perms
          recurse info = do
            if isDirectory info && infoPath info /= path
              then traverseVerbose order (infoPath info)
              else return [info]

liftCompare :: (a -> a ->  Ordering) -> (Info -> a) -> Info -> Info -> Ordering
liftCompare cmp ext ia ib = (ext ia) `cmp` (ext ib)

traverseReverse ::  FilePath -> IO [Info]
traverseReverse = traverse (sortBy $ liftCompare compare infoPath)

traversePostOrder :: FilePath -> IO [Info]
traversePostOrder = traverse (sortBy $ flip $ liftCompare compare (length . splitDirectories . infoPath))
