module PodDB where

import Database.HDBC
import Database.HDBC.Sqlite3
import PodTypes
import Control.Monad(when)
import Data.List(sort)

-- | Initialize DB and return database Connection
connect :: FilePath -> IO Connection
connect fp =
  do dbh <- connectSqlite3 fp
     prepDB dbh
     return dbh

{- | Prepare the database for our data.

We create two tables and ask the database engine to verify some pieces
of data consistency for us:

* castid and epid both are unique primary keys and must never be duplicated
* castURL also is unique
* In the episodes table, for a giben podcat (epcast), there must be only on instance of each gibven URL or rpisode ID
-}
prepDB :: IConnection conn => conn -> IO ()
prepDB dbh =
  do tables <- getTables dbh
     when (not ("podcasts" `elem` tables)) $
       do run dbh "CREATE TABLE podcasts (\
                  \castid INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
                  \castURL TEXT NOT NULL UNIQUE)" []
          return ()
     when (not ("episodes" `elem` tables)) $
       do run dbh "CREATE TABLE episodes (\
                  \epid INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
                  \epcastid INTEGER NOT NULL,\
                  \epurl TEXT NOT NULL,\
                  \epdone INTEGER NOT NULL,\
                  \UNIQUE(epcastid, epurl),\
                  \UNIQUE(epcastid, epid))" []
          return ()
     commit dbh
          
{- | Adds a new podcast to the database. Ignores the castid on the incoming podcast, and returns a new object whe the castid populated.
An attempt to add a podcast that alredy exists is an error. -}
addPodcast :: IConnection conn => conn -> Podcast -> IO Podcast
addPodcast dbh podcast =
  handleSql errorHandler $
    do -- Insert the castURL into the table. The database
       -- will automatically assign a cast ID.
      run dbh "INSERT INTO podcasts (castURL) VALUES (?)"
        [toSql (castURL podcast)]
      -- Find out the castID for the URL we just added.
      r <- quickQuery' dbh "SELECT castid FROM podcasts WHERE castURL = ?"
           [toSql (castURL podcast)]
      case r of
        [[x]] -> return $ podcast {castId = fromSql x}
        y -> fail $ "addPodcast: unexpected result: " ++ show y
  where errorHandler e =
          do fail $ "Error adding podcast; does this URL already exist?\n"
               ++ show e

{- | Adds a new episode to the database.

Since this is done by automation instead of by user request, we will
simply ignore requests to add duplicate episodes. This way, when we are
processing a feed, each URL encontered can be fed to this function,
without having to first look it up in the DB.

Also, we generally won't care about the new ID hrere, so don't bother
fetching it. -}
addEpisode :: IConnection conn => conn -> Episode -> IO ()
addEpisode dbh ep =
  run dbh "INSERT OR IGNORE INTO episodes (epCastID, epURL, epDone) \
            \VALUES (?,?,?)"
          [toSql (castId . epCast $ ep), toSql (epURL ep),
           toSql (epDone ep)]
  >> return ()

{- | Modifies an existing podcast. Looks up the given podcast by
ID and modifies the database record to match the passed Podcat. -}
updatePodcast :: IConnection conn => conn -> Podcast -> IO ()
updatePodcast dbh podcast =
  run dbh "UPDATE podcats set castURL = ? where castId = ?"
          [toSql (castURL podcast), toSql (castId podcast)]
  >> return ()


{- | Modifies an existing episode. Looks it up by ID and modifies the
database record to match the given episode. -}
updateEpisode :: IConnection conn => conn -> Episode -> IO ()
updateEpisode dbh episode =
  run dbh "UPDATE episodes SET epCastId = ?, epURL = ?, epDone = ? \
           \WHERE epId = ?"
          [toSql (castId . epCast $ episode),
           toSql (epURL episode),
           toSql (epDone episode),
           toSql (epId episode)]
  >> return ()

{- | Remove a pod cast. First removes any episodes that may exist
for this podcast. -}
removePodcast :: IConnection conn => conn -> Podcast -> IO ()
removePodcast dbh podcast =
  do run dbh "DELETE FROM episodes WHERE epcastid = ?"
       [toSql (castId podcast)]
     run dbh "DELETE FROM podcasts WHERE castid = ?"
       [toSql (castId podcast)]
     return ()

{- | Gets a list of all podcasts. -}
getPodcasts :: IConnection conn => conn -> IO [Podcast]
getPodcasts dbh =
  do res <- quickQuery' dbh
            "SELECT castid, casturl FROM podcasts ORDER BY castid" []
     return (map convPodcastRow res)

{- | Get particular podcast. Nothing if the ID doesn't match, or
Just podcast if it does. -}
getPodcast :: IConnection conn => conn -> Integer -> IO (Maybe Podcast)
getPodcast dbh wantedId =
  do res <- quickQuery' dbh
            "SELECT castid, casturl FROM podcasts WHERE castid = ?"
            [toSql wantedId]
     case res of
       [x] -> return (Just (convPodcastRow x))
       [] -> return Nothing
       x -> fail $ "Really bad erro; more than one podcast with ID"

{- | Convert the result of a SELECT into a Podcast record -}
convPodcastRow :: [SqlValue] -> Podcast
convPodcastRow [svId, svURL] =
  Podcast {castId = fromSql svId,
           castURL = fromSql svURL}
convPodcastRow x = error $ "Cna't convert podcat row " ++ show x

{- | Get all episodes fro a particular podcast. -}
getPodcastEpisodes :: IConnection conn => conn -> Podcast -> IO [Episode]
getPodcastEpisodes dbh pc =
  do r <- quickQuery' dbh
          "SELECT epId, epURL, epDone FROM episodes WHERE epCastId = ?"
          [toSql (castId pc)]
     return (map convEpisodeRow r)
  where convEpisodeRow [svId, svURL, svDone] =
          Episode {epId = fromSql svId, epURL = fromSql svURL,
                   epDone = fromSql svDone, epCast = pc}
  
