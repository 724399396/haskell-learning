module Glob (namesMatching) where

import System.Directory (doesDirectoryExist, doesFileExist,
                         getCurrentDirectory, getDirectoryContents)

import System.FilePath (dropTrailingPathSeparator, splitFileName, (</>),
                       isSearchPathSeparator)

import Control.Exception (handle)
import Control.Monad (forM)
import GlobRegex (matchesGlob, matchesGlobI)
import Data.List (isSuffixOf)

isPattern :: String -> Bool
isPattern = any (`elem` "[*?")

namesMatching pat
    | not (isPattern pat) = do
      exists <- doesNameExist pat
      return (if exists then [pat] else [])

    | otherwise = do
        case splitFileName pat of
            ("", baseName) -> do
                curDir <- getCurrentDirectory
                listMatches curDir baseName
            (dirName, baseName) -> do
                dirs <- if isPattern dirName
                        then namesMatching (dropTrailingPathSeparator dirName)
                        else return [dirName]
                let listDir = if isPattern baseName
                              then listMatches
                              else listPlain
                pathNames <- forM dirs $ \dir -> do
                                baseNames <- listDir dir baseName
                                return (map (dir </>) baseNames)
                return (concat pathNames)

doesNameExist :: FilePath -> IO Bool
doesNameExist name = do
    fileExists <- doesFileExist name
    if fileExists
        then return True
        else doesDirectoryExist name

listMatches :: FilePath -> String -> IO [String]
listMatches dirName pat = do
    dirName' <- if null dirName
                then getCurrentDirectory
                else return dirName
    do
        names <- if ("**" `isSuffixOf` pat)
                 then subFiles dirName'
                 else getDirectoryContents dirName'
        let names' = if isHidden pat
                     then filter isHidden names
                     else filter (not . isHidden) names
        return (filter (`systemDetermin` pat) names')

isHidden ('.':_) = True
isHidden _ = False

listPlain :: FilePath -> String -> IO [String]
listPlain dirName baseName = do
    exists <- if null baseName
              then doesDirectoryExist dirName
              else doesNameExist (dirName </> baseName)
    return (if exists then [baseName] else [])

systemDetermin :: FilePath -> String -> Bool
systemDetermin = if (isSearchPathSeparator ':')
                      then matchesGlobI
                      else matchesGlob

subFiles :: FilePath -> IO [FilePath]
subFiles path = do
  con <- getDirectoryContents path
  let fullPath = map (path </>) $ filter (\x -> x /= "." && x /= "..") con 
  dirs <- filterM doesDirectoryExist fullPath
  subFiles <- mapM subFiles dirs
  files <- filterM doesFileExist fullPath
  return (files ++ concat subFiles)

filterM :: (FilePath -> IO Bool) -> [FilePath] -> IO [FilePath]
filterM pre fs = foldr (\x accIO -> do
                                      acc <- accIO
                                      m <- pre x
                                      return $ if (m)
                                        then  x:acc
                                        else acc) (return []) fs
