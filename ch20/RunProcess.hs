{-# OPTIONS_GHC -fglasgow-exts #-}
{-# LANGUAGE DatatypeContexts #-}
module RunProcess where

import System.Process hiding (createPipe)
import Control.Concurrent
import Control.Concurrent.MVar
import System.IO
import System.Exit
import Text.Regex
import System.Posix.Process
import System.Posix.IO
import System.Posix.Types
import System.Directory(setCurrentDirectory,createDirectory)
import Control.Exception
import Data.List
import System.Posix.Env(getEnv)

{- | The type for running external commands. The first part
of the tuple is the program name. the list represents the
command-line parameters to pass to the command. -}
type SysComand = (String, [String])

{- | The result of running any command -}
data CommandResult = CommandResult {
  cmdOutput :: IO String,
  getExitStatus :: IO ProcessStatus
  }

{- | The type for handling global lists of FDs to always close in the clients -}
type CloseFDs = MVar [Fd]

{- | Class representing anything that is a runnable command -}
class CommandLike a where
  {- | Given the command and a String representing input,
       invokes the command. Returns a String
       representing the output of the command. -}
  invoke :: a -> CloseFDs -> String -> IO CommandResult

instance CommandLike SysComand where
  invoke (cmd, args) closefds input =
    do -- Create two pipes: one to handle stdin and the other
       -- to handle stdout. We do not redirect stderr in this program.
      (stdinread, stdinwrite) <- createPipe
      (stdoutread, stdoutwrite) <- createPipe

      -- We add the parent FDs to this list because we always need
      -- to close them in the clients.
      addCloseFDs closefds [stdinwrite, stdoutread]

      -- Now, grab the closed FDs list and fork the child
      childPID <- withMVar closefds (\fds ->
                                        forkProcess (child fds stdinread stdoutwrite))

      -- Now, on the parent, close the client-side FDs.
      closeFd stdinread
      closeFd stdoutwrite

      -- Write the input to the command
      stdinhd1 <- fdToHandle stdinwrite
      forkIO $ do hPutStr stdinhd1 input
                  hClose stdinhd1

      -- Prepare to receive output from the command
      stdouthd1 <- fdToHandle stdoutread

      -- Set up the function to call when ready to wait for the
      -- child to exit
      let waitfunc =
            do status <- getProcessStatus True False childPID
               case status of
                 Nothing -> fail $ "Error: Nothing from getProcessStatus"
                 Just ps -> do removeCloseFDs closefds
                                 [stdinwrite, stdoutread]
                               return ps

      return $ CommandResult {cmdOutput = hGetContents stdouthd1,
                              getExitStatus = waitfunc}
    -- Define what happens in the child process
    where child closefds stdinread stdoutwrite =
            do -- Copy our pipes over the regular stdin/stdout FDS
              dupTo stdinread stdInput
              dupTo stdoutwrite stdOutput
              
              -- Now close the original pipe FDs
              closeFd stdinread
              closeFd stdoutwrite

              -- Close all the open fDS we inherited from the parent
              mapM_ (\fd -> catchSome (closeFd fd) (\_ -> return ())) closefds

              -- start the program
              executeFile cmd True args Nothing

{- | An isntance of 'CommandLike' for an external command. The String is
passed to a shell for evaluation and invotation. -}
instance CommandLike String where
  invoke cmd closefds input =
    do -- Use the shell given by the environment variable SHELL,
       -- if any. Otherwise, use /bin/sh
      esh <- getEnv "SHELL"
      let sh = case esh of
            Nothing -> "/bin/sh"
            Just x -> x
      invoke (sh, ["-c", cmd]) closefds input

addCloseFDs :: CloseFDs -> [Fd] -> IO ()
addCloseFDs closefds newfds =
  modifyMVar_ closefds (\oldfds -> return $ oldfds ++ newfds)

-- Remove FDs from the list
removeCloseFDs :: CloseFDs -> [Fd] -> IO ()
removeCloseFDs closefds removethem =
  modifyMVar_ closefds (\fdlist -> return $ procfdlist fdlist removethem)
  where
    procfdlist fdlist [] = fdlist
    procfdlist fdlist (x:xs) = procfdlist (removefd fdlist x) xs

    -- We want to remove only the first occurance ot any given fd
    removefd [] _ = []
    removefd (x:xs) fd
      | fd == x = xs
      | otherwise = x : removefd xs fd

-- Support for running Haskell commmands
instance CommandLike (String -> IO String) where
  invoke func _ input =
    return $ CommandResult (func input) (return (Exited ExitSuccess))

-- Support pure Haskell Functions by wrapping them in IO
instance CommandLike (String -> String) where
  invoke func = invoke iofunc
    where iofunc :: String -> IO String
          iofunc = return . func

-- It's also useful to operate on lines. Define support for line-based
-- functions both within and without the IO monad.

instance CommandLike ([String] -> IO [String]) where
  invoke func _ input =
    return $ CommandResult linedfunc (return (Exited ExitSuccess))
    where linedfunc = func (lines input) >>= (return . unlines)

instance CommandLike ([String] -> [String]) where
  invoke func = invoke (unlines . func . lines)  

{- | Type representing a pipe. A 'PipeCOmmand' consists of a source
'CommandLike'. -}
data (CommandLike src, CommandLike dest) =>
  PipeCommand src dest = PipeCommand src dest

{- | A convenient function for createing a 'PipeCommand'. -}
(-|-) :: (CommandLike a, CommandLike b) => a -> b -> PipeCommand a b
(-|-) = PipeCommand

{- | Make 'PipeCommand' runnable as a command -}
instance (CommandLike a, CommandLike b) =>
         CommandLike (PipeCommand a b) where
  invoke (PipeCommand src dest) closefds input =
    do res1 <- invoke src closefds input
       output1 <- cmdOutput res1
       res2 <- invoke dest closefds output1
       return $ CommandResult (cmdOutput res2) (getEC res1 res2)

{- | Giben two 'CommandResult' items, evaluate the exit codes for
both and then return a "combined" exit code. This will be ExitSuccess
if both exited successfully. Otherwise, it will reflect the first
error encontered. -}
getEC :: CommandResult -> CommandResult -> IO ProcessStatus
getEC src dest =
  do sec <- getExitStatus src
     dec <- getExitStatus dest
     case sec of
       Exited ExitSuccess -> return dec
       x -> return x

{- | Different ways to get data from 'run'.

 * IO () runs, throws an exception on error, and sends stdout to stdout

 * IO String runs, throws an exception on error, reads stdout into
   a buffer, and returns it as a string.

 * IO [String] is same as IO String, but returns the result as lines.

 * IO ProcessStatus runs and returns a ProcessStatus with the exit
   infomation. stdout is send to stdout. Exceptions are not thrown.

 * IO (String, ProcessStatus) is like IO ProcessStatus, but also
   includes a description of the last command in the pipe to have
   an error (or the last comman, if there was no error).

 * IO Int returns the exit code from a program directly. If a signal
   caused the command to be reaped, return 128 + SIGNUM.

 * IO Bool returns True if the program exited normally (exit code 0,
   not stopped by a signal) and False otherwise.
-}

class RunResult a where
  {- | Runs a command (or pipe of commands), with results presented
     in any number of different ways. -}
  run :: (CommandLike b) => b -> a

-- | Utility function for use by 'RunResult' instances
setUpCommand :: CommandLike a => a -> IO CommandResult
setUpCommand cmd =
  do -- Initialize our closefds list
    closefds <- newMVar []

    -- Invoke the command
    invoke cmd closefds []

instance RunResult (IO ()) where
  run cmd = run cmd >>= checkResult

instance RunResult (IO ProcessStatus) where
  run cmd =
    do res <- setUpCommand cmd

       -- Process its output
       output <- cmdOutput res
       putStr output
       getExitStatus res

instance RunResult (IO Int) where
  run cmd = do rc <- run cmd
               case rc of
                 Exited (ExitSuccess) -> return 0
                 Exited (ExitFailure x) -> return x
                 Terminated x _ -> return (128 + (fromIntegral x))
                 Stopped x -> return (128 + (fromIntegral x))

instance RunResult (IO Bool) where
  run cmd = do rc <- run cmd
               return ((rc::Int) == 0)

instance RunResult (IO [String]) where
  run cmd = do r <- run cmd
               return (lines r)

instance RunResult (IO String) where
  run cmd =
    do res <- setUpCommand cmd
       output <- cmdOutput res
       -- Force output to be buffered
       evaluate (length output)

       ec <- getExitStatus res
       checkResult ec
       return output

checkResult :: ProcessStatus -> IO ()
checkResult ps =
  case ps of
    Exited (ExitSuccess) -> return ()
    x -> fail (show x)

runIO :: CommandLike a => a -> IO ()
runIO = run

--------------------
-- Utility Functions
--------------------
cd :: FilePath -> IO ()
cd = setCurrentDirectory

{- | Takes a string and sends it on as standard output.
The input to this function is never read. -}
echo :: String -> String -> String
echo inp _ = inp

-- | Search for the regexp in the lines. Return those that match.
grep :: String -> [String] -> [String]
grep pat = filter (ismatch regex)
  where regex = mkRegex pat
        ismatch r inp = case matchRegex r inp of
                          Nothing -> False
                          Just _ -> True

{- | Creates the given directory. A value of 00755 for mode woule be typical.
An alias for System.Posix.Directory.crateDirectory. -}
mkdir :: FilePath -> FileMode -> IO ()
mkdir f _ = createDirectory f

{- | Remove duplicate lines from a file (like Unix uniq).
Takes a String representing a file or output and plugs it through
lines and then nub to uniqify on a line basis. -}
uniq :: String -> String
uniq = unlines . nub . lines

-- | Count number of lines. wc -l
wcL, wcW :: [String] -> [String]
wcL inp = [show (genericLength inp :: Integer)]

-- | Count number of words in a file (like wc -w)
wcW inp = [show ((genericLength $ words $ unlines inp) :: Integer)]

sortLines :: [String] -> [String]
sortLines = sort

countLines :: String -> IO String
countLines = return . (++) "\n" . show . length . lines

catchSome :: IO a -> (SomeException -> IO a) -> IO a
catchSome = catch
