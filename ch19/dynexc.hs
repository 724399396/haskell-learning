{-# LANGUAGE DeriveDataTypeable #-}

import Data.Dynamic
import Control.Exception

data SqlError = SqlError {seState :: String,
                          seNativeError :: Int,
                          seErrorMsg :: String}
                deriving (Eq, Show, Read, Typeable)

{- | Execute the given IO aciton.

If it raise a 'SeqError', then execute the spplied
handler and return its return value. Otherwise, proceed
as normal. -}
catchSql :: IO a -> (SqlError -> IO a) -> IO a
catchSql = catchDyn

{- | Like 'catchSql', with the order of arguments reversed. -}
handleSql :: (SqlError -> IO a) -> IO a -> IO a
handleSql = flip catchSql

{- | Catches 'SqlError's, and re-raises them as IO errors with fail.
Useful if you don't care to catch SQL errors, but want to see a sane
error message if one happends. One would often use this as a
hight-level wrapper around SQL calls. -}
handleSqlError :: IO a -> IO a
handleSqlError action =
  catchSql action handler
  where handler e = fail ("SQL error: " ++ show e)

throwSqlError :: String -> Int -> String -> a
throwSqlError state nativeerror errormsg =
  throwDyn (SqlError state nativerror errormsg)
