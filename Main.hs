module Main where

import System.IO
import System.Environment
import System.Process.Internals
import System.Directory

import Control.Monad
import Control.Exception
import Text.Read
import Data.List

import qualified System.Process as P
import qualified Data.ByteString.Lazy as B
import qualified System.Posix.Types as T
import qualified Data.Map as Map

-- type declarations ----------------------
type Options = Map.Map String String
type Config = Map.Map String String

empty :: Options
empty = Map.fromList []

defcon :: Config
defcon = Map.fromList []

-------------------------------------------

main = do
  putStrLn "spawn v0.5.1"
  args <- getArgs
  case args of
    (cmd:rest) -> runCommand cmd $ consumeOpts rest
    otherwise  -> cError

-- function definitions -------------------
runCommand :: String -> Options -> IO ()
runCommand "init"   opts = cInit opts
runCommand "start"  opts = cStart opts 
runCommand "stop"   opts = cStop
runCommand "reload" opts = cReload opts
runCommand "status" opts = cStatus
runCommand "clean"  opts = cClean
runCommand c        o    = cError

consumeOpts' :: Options -> [String] -> Options
consumeOpts' opt []                 = opt
consumeOpts' opt [_]                = opt
consumeOpts' opt ("-f":v:os)        = consumeOpts' (Map.insert "proc" v opt) os
consumeOpts' opt ("-p":v:os)        = consumeOpts' (Map.insert "port" v opt) os
consumeOpts' opt ("-d":v:os)        = consumeOpts' (Map.insert "dir" v opt) os
consumeOpts' opt ("--onstart":v:os) = consumeOpts' (Map.insert "start" v opt) os
consumeOpts' opt ("--onstop":v:os)  = consumeOpts' (Map.insert "stop" v opt) os
consumeOpts' opt (_:_:os)           = consumeOpts' opt os

consumeOpts :: [String] -> Options
consumeOpts = consumeOpts' empty

readConfig  :: IO (Config)
readConfig = do
  contents <- fmap lines $ readFile ".spawn"
  let keys = ["proc","port","start","stop"]
  return (Map.fromList $ zip keys contents)

(#) :: Map.Map String String -> String -> Maybe String
k # m = if value=="$invalid" then Nothing
        else Just value
  where value = extract $ Map.lookup m k

extract :: Maybe String -> String
extract Nothing  = "$invalid"
extract (Just x) = x

countProcess :: String -> IO (Int) 
countProcess pname = fmap read $ P.readCreateProcess (P.shell $ "pgrep -f \"" ++ cb pname ++ "\" | wc -l") ""
  where cb (x:xs) = '[':x:']':xs

cInit :: Options -> IO ()
cInit opts = do
  putStr "creating spawn config: "
  let proc  = opts # "proc"
  let port  = opts # "port"
  if proc == Nothing || port == Nothing then
    putStrLn "Invalid options: \"-p <port>\" and \"-f <process>\" are both required."
  else do
    let start = opts # "start"
    let stop  = opts # "stop"
    let config = extract proc ++ "\n" ++ extract port ++ "\n" ++ extract start ++ "\n" ++ extract stop
    writeFile ".spawn" config
    putStrLn "ok."

getPid ph = withProcessHandle ph go
  where go ph_ = case ph_ of
                   OpenHandle   x -> return x
                   ClosedHandle _ -> return (-1)

cStart :: Options -> IO ()
cStart opts = do
  putStr "spawning process: "

  config <- readConfig
  let exec = "./" ++ (extract $ config # "proc")
  let start = config # "start"

  pcount <- countProcess exec
  if pcount > 0 then 
    putStrLn "already running!"
  else do
    ph     <- P.spawnCommand $ intercalate " " ["spawn-fcgi -f", exec, "-p", (extract $ config # "port"), ">/dev/null", "2>&1"]
    newpid <- getPid ph
    putStrLn $ show newpid
    if start /= Nothing then do
      P.spawnCommand $ extract start
      return ()
    else return ()
  
cStop :: IO ()
cStop = do
  config <- readConfig
  putStr "terminating process: "
  let exec = "\"[.]/" ++ (extract $ config # "proc") ++ "\""
  P.callCommand $ intercalate " " ["pgrep", "-f", exec, "|", "xargs", "kill"]
  let stop = config # "stop"
  if stop /= Nothing then do
    P.spawnCommand $ extract stop
    return ()
  else return ()
  putStrLn "ok."

cReload :: Options -> IO ()
cReload opts = do 
  cStop
  (cStart opts)

cStatus :: IO ()
cStatus = do
  config <- readConfig
  let onstart = config # "start"
  let onstop  = config # "stop"
  let exec = "./" ++ (extract $ config # "proc")
  let port = (extract $ config # "port")
  pcount <- countProcess exec

  putStrLn "configuration:"
  putStrLn $ "status:   " ++ if pcount>0 then "running" else "stopped"
  putStrLn $ "process:  " ++ exec
  putStrLn $ "port:     " ++ port
  if onstart/=Nothing then putStrLn $ "onstart:  " ++ extract onstart
  else return ()
  if onstop/=Nothing then putStrLn $ "onstop:   " ++ extract onstop
  else return ()

cClean :: IO ()
cClean = do
  spconf <- doesFileExist ".spawn"
  if spconf then do
    config <- readConfig
    let exec = "./" ++ (extract $ config # "proc")
    pcount <- fmap read $ P.readCreateProcess (P.shell $ "pgrep -f \"" ++ exec ++ "\" | wc -l") ""

    if pcount > 0 then cStop else return ()

    putStr "removing configuration: "
    removeFile ".spawn"
    putStrLn "ok."
  else
    putStrLn "error: spawn template not found."

cError :: IO ()
cError = do
  putStrLn "unknown command!"
  putStrLn "usage: spawn [init|start|stop|reload|status|clean] [options]"
-------------------------------------------
