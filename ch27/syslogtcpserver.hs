import Data.Bits
import Network.Socket
import Network.BSD
import Data.List
import Control.Concurrent
import Control.Concurrent.MVar
import System.IO

type HandlerFunc = SockAddr -> String -> IO ()

serveLog :: String        -- ^ Port number or name; 514 is default
         -> HandlerFunc   -- ^ Function to handle incoming messages
         -> IO ()
serveLog port handlerfunc = withSocketsDo $
  do -- Look up the port. Either raises an exception or returns
     -- a nonempty list.
    addrinfos <- getAddrInfo
                 (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                 Nothing (Just port)
    let serveraddr = head addrinfos

    -- Create a socket
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol

    -- Bind it to the address we're listening to
    bindSocket sock (addrAddress serveraddr)

    -- Start listening for connection requests. Maximum queue size
    -- of 5 connection request waiting to be accepted.
    listen sock 5

    -- Created a lock to use for syschoninzing access to the handler
    lock <- newMVar ()

    -- Loop forever wating for connections. Ctrl-C to abort.
    procRequest lock sock

  where
    -- | Process incoming connectio nrequest
    procRequest :: MVar () -> Socket -> IO ()
    procRequest lock mastersock =
      do (connsock, clientaddr) <- accept mastersock
         handle lock clientaddr
           "syslogtcpserver.hs: client connected"
         forkIO $ procMessages lock connsock clientaddr
         procRequest lock mastersock

    -- | Process incoming messages
    procMessages :: MVar () -> Socket -> SockAddr -> IO ()
    procMessages lock connsock clientaddr =
      do connhd1 <- socketToHandle connsock ReadMode
         hSetBuffering connhd1 LineBuffering
         messages <- hGetContents connhd1
         mapM_ (handle lock clientaddr) (lines messages)
         hClose connhd1
         handle lock clientaddr
           "syslogtcpserver.hs: clinet disconnected"

    -- Lock the handler before passing data to it.
    handle:: MVar () -> HandlerFunc
    -- This type is the same as
    -- handle :: MVar () -> SockAddr -> String -> IO ()
    handle lock clientaddr msg =
      withMVar lock
        (\a -> handlerfunc clientaddr msg >> return a)

-- A simple handler that prints incoming packets
plainHandler :: HandlerFunc
plainHandler addr msg =
  putStrLn $ "From " ++ show addr ++ ": " ++ msg        

 
