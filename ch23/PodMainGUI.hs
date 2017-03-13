module PodMainGUI where

import PodDownload
import PodDB
import PodTypes
import System.Environment
import Database.HDBC
import Network.Socket(withSocketsDo)

-- GUI libraries
import Graphics.UI.Gtk hiding (disconnect)
import Graphics.UI.Gtk.Glade

-- Threading
import Control.Concurrent

-- | Our main GUI type
data GUI = GUI {
  mainWin :: Window,
  mwAddBt :: Button,
  mwUpdateBt :: Button,
  mwDownloadBt :: Button,
  mwFetchBt :: Button,
  mwExitBt :: Button,
  statusWin :: Dialog,
  swOkBt :: Button,
  swCancelBt :: Button,
  swLabel :: Label,
  addWin :: Dialog,
  awOKBt :: Button,
  awCancelbt :: Button,
  awEntry :: Entry}
  
  
main :: FilePath -> IO ()
main gladepath = withSocketsDo $ handleSqlError $
  do initGUI  -- Initialize GTK+ engine

     -- Every so often, we try to run other threads.
     timeoutAddFull (yield >> return True)
                    priorityDefaultIdle 100

     -- Load the GUI from the Glade file
     gui <- loadGlade gladepath

     -- Connect to the database
     dbh <- connect "pod.db"

     -- Set up our events
     connectGui gui dbh

     -- Run the GTK+ main loop; exists after GUI is done
     mainGUI

     -- Disconnect from the database at the end
     disconnect dbh

loadGlade gladepath =
  do -- Load XML from glade path.
     -- Note: crashes with a runtime error on console if fails!
    Just xml <- xmlNew gladepath

    --  Load main window
    mw <- xmlGetWidget xml castToWindow "mainWindow"

    -- Load all buttons
    [mwAdd, mwUpdate, mwDownload, mwFetch, mwExit, swOK, swCancel,
     auOk, auCancel] <-
      mapM (xmlGetWidget xml castToButton)
      ["addButton", "updateButton", "downloadButton",
"fetchButton", "exitButton", "okButton", "cancelButton",
"auOK", "auCancel"]

    sw <- xmlGetWidget xml castToDialog "statusDialog"
    swl <- xmlGetWidget xml castToLabel "statusLabel"

    au <- xmlGetWidget xml castToDialog "addDialog"
    aue <- xmlGetWidget xml castToEntry "auEntry"

    return $ GUI mw mwAdd mwUpdate mwDownload mwFetch mwExit sw swOK swCancel swl au auOK auCancel aue

connectGui gui dbh =
  do -- When the close button is clicked, terminate the GUI loop
     -- by calling TEK mainQuit function
    onDesctory (mainWin gui) mainQuit

    -- Main window buttons
    onClicked (mwAddBt gui) (guiAdd gui dbh)
    onClicked (mwUpdateBt gui) (guiUpdate gui dbh)
    onClicked (mwDownloadBt gui) (guiDownload gui dbh)
    onClicked (mwFetchBt gui) (guiFetch gui dbh)
    onClicked (mwExitBt gui) mainQuit

guiAdd gui dbh =
  do -- Initialize the add URL window
    entrySetText (awEntry gui) ""
    onClicked (awCancelBt gui) (widgetHide (addWin gui))
    onClicked (awOkBt gui) procOK

    -- Show the add URL window
    windowPresent (addWin gui)
  where procOk =
          do url <- entryGetText (awEntry gui)
             widgetHide (addWin gui) -- Remove the dialog
               add dbh url

add dbh url =
  do addPodcast dbh pc
     commit dbh
  where pc = Podcast {castId = 0, castURL = url}

statusWindow :: IConnection conn =>
                GUI
             -> conn
             -> String
             -> ((String -> IO()) -> IO ())
             -> IO ()
statusWindow gui dbh title func =
  do -- Clear the status text
    labelSetText (swLabel gui) ""

    -- Diable the OK button, enable Cancel button
    widgetSetSensitivity (swOkBt gui) False
    widgetSetSensitivity (swCancel gui) True

    -- Set the title
    windowSetTitle (statusWin gui) title

    -- Start the operation
    childThread <- forkIO childTasks

    -- Define what happens when clicking on Cancel
    onClicked (swCancleBt gui) (cancelChild childThread)

    -- Show the window
    windowPresent (statusWin gui)

update dbh =
  do pclist <- getPodcasts dbh
     mapM_ procPodcast pclist
  where procPodcast pc =
          do putStrLn $ "Updating from " ++ (castURL pc)
             updatePodcastFromFeed dbh pc

download dbh =
  do pclist <- getPodcasts dbh
     mapM_ procPodcast pclist
  where procPodcast pc =
          do putStrLn $ "Considering " ++ (castURL pc)
             episodelist <- getPodcastEpisodes dbh pc
             let dleps = filter (\ep -> epDone ep == False)
                         episodelist
             mapM_ procEpisode dleps
             
        procEpisode ep =
          do putStrLn $ "Downloading " ++ (epURL ep)
             getEpisode dbh ep

syntaxError = putStrLn
  "Usage: pod command [args]\n\
  \\n\
  \pod add url     Adds a new podcast with the given URL\n\
  \pod download    Downloads all pending episodes\n\
  \pod fetch       Updates, then downloads\n\
  \pod update      Downloads podcast feeds, looks for new episodes\n"
    
