module Main where

import System.IO
import System.Environment
import System.Process.Internals
import System.Directory

import Control.Monad
import Control.Exception
import Text.Read
import Text.Regex
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

defconf :: Config
defconf = Map.fromList []

-------------------------------------------

main = do
  putStrLn "spawn v0.9.4"
  args <- getArgs
  case args of
    ["--help"] -> showHelp
    (cmd:rest) -> runCommand cmd $ consumeOpts rest
    otherwise  -> cError

-- function definitions -------------------
runCommand :: String -> Options -> IO ()
runCommand "init"   opts = cInit opts
runCommand "start"  opts = checkConfig opts cStart
runCommand "stop"   opts = checkConfig opts cStop 
runCommand "reload" opts = checkConfig opts cReload 
runCommand "status" opts = checkConfig opts cStatus 
runCommand "clean"  opts = checkConfig opts cClean 
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

checkConfig :: Options -> (Options -> IO ()) -> IO ()
checkConfig opts continue = do
  let dir = getDir $ opts # "dir"
  let confPath = (dir ++ "/" ++ ".spawn")
  confExists <- doesFileExist confPath  
  if confExists
    then continue opts
    else putStrLn $ "not a spawn config" ++ "\nrun spawn init first"

readConfig :: String -> IO (Config)
readConfig dir = do
  contents <- fmap lines $ readFile (dir ++ "/" ++ ".spawn")
  let keys = ["proc","port","start","stop"]
  return (Map.fromList $ zip keys contents)

(#) :: Map.Map String String -> String -> Maybe String
k # m = if value=="$invalid" then Nothing
        else Just value
  where value = extract $ Map.lookup m k

extract :: Maybe String -> String
extract Nothing  = "$invalid"
extract (Just x) = x

-- |fallback to current directory if no -d flag passed
getDir :: Maybe String -> String
getDir Nothing  = "."
getDir (Just x) = x

-- |read process id from spawn-fcgi output
getPid :: String -> Maybe String
getPid output = fmap (!! 0) $ matchRegex (mkRegex "spawn-fcgi: child spawned successfully: PID: ([0-9]+)") output

-- |retrieve process id of application
processState :: String -> IO (Maybe String)
processState dir = do
  let pidPath = dir ++ "/.pid"
  pidfExists <- doesFileExist pidPath
  if pidfExists
    then do
      oPid <- readFile pidPath 
      procExists <- doesDirectoryExist ("/proc/" ++ oPid)
      if procExists 
        then return (Just oPid)
        else do
            removeFile pidPath
            return Nothing
    else return Nothing

-- |initialize a spawn template 
cInit :: Options -> IO ()
cInit opts = do
  putStr "creating spawn config: "
  let proc  = opts # "proc"
  let port  = opts # "port"
  if proc == Nothing || port == Nothing 
    then putStrLn $ "invalid options\n" ++ "usage: spawn init -p <port> -f <process> [--onstart <command>] [--onstop <command>]"
    else do
      let start = opts # "start"
      let stop  = opts # "stop"
      let config = extract proc ++ "\n" ++ extract port ++ "\n" ++ extract start ++ "\n" ++ extract stop
      writeFile ".spawn" config
      putStrLn "ok."

cStart :: Options -> IO ()
cStart opts = do
  putStr "spawning process: "

  let dir = getDir $ opts # "dir"
  config <- readConfig dir
  let exec = "./" ++ (extract $ config # "proc")
  let start = config # "start"
  mPid <- processState dir 

  case mPid of
    Just _ -> putStrLn "already running!"
    Nothing -> do
      ph <- if dir /= "." 
              then P.readCreateProcess (P.shell $ intercalate " " ["spawn-fcgi -d" , dir, " -f", exec, "-p", (extract $ config # "port")]) ""
              else P.readCreateProcess (P.shell $ intercalate " " ["spawn-fcgi -f", exec, "-p", (extract $ config # "port")]) ""
      let mPid = getPid ph
      case mPid of
        Nothing -> putStrLn "error" 
        Just p -> do 
          let pidPath = dir ++ "/.pid"
          writeFile pidPath p
          putStrLn p
      if start /= Nothing 
        then do
          P.spawnCommand $ extract start
          return ()
        else return ()
  
cStop :: Options -> IO ()
cStop opts = do
  let dir = getDir $ opts # "dir"
  config <- readConfig dir
  putStr "terminating process: "
  case processState dir of
    Just pid -> do 
      handle <- mkProcessHandle (CPid $ read pid) False
      waitForProcess handle
      putStrLn "ok."
    Nothing -> putStrLn "cannot attach to process for termination."

  let stop = config # "stop"
  if stop /= Nothing 
    then do
      let pidPath = dir ++ "/.pid"
      P.spawnCommand $ extract stop
      pidExists <- doesFileExist pidPath 
      if pidExists
        then removeFile pidPath
        else return ()
    else return ()

cReload :: Options -> IO ()
cReload opts = do 
  (cStop opts)
  P.callCommand $ "sleep 1"
  (cStart opts)

cStatus :: Options -> IO ()
cStatus opts = do
  let dir = getDir $ opts # "dir"
  config <- readConfig dir

  let onstart = config # "start"
  let onstop  = config # "stop"
  let exec = (extract $ config # "proc")
  let port = (extract $ config # "port")

  mPid <- processState dir

  putStrLn "configuration:"
  putStrLn $ "status:   " ++ case mPid of Just p -> "running (" ++ p ++ ")"  
                                          Nothing -> "stopped"
  putStrLn $ "process:  " ++ exec
  putStrLn $ "port:     " ++ port
  if onstart/=Nothing 
    then putStrLn $ "onstart:  " ++ extract onstart
    else return ()
  if onstop/=Nothing 
    then putStrLn $ "onstop:   " ++ extract onstop
    else return ()

cClean :: Options -> IO ()
cClean opts = do
  spconf <- doesFileExist ".spawn"
  if spconf 
    then do
      cStop opts
      putStr "removing configuration: "
      removeFile ".spawn"
      putStrLn "ok."
    else putStrLn "error: spawn template not found."

cError :: IO ()
cError = do
  putStrLn "unknown command!"
  putStrLn "usage: spawn [init|start|stop|reload|status|clean] [options]"

showHelp :: IO ()
showHelp = do
  putStrLn "usage: spawn [init|start|stop|reload|status|clean] [options]"
  putStrLn $ "init: initialize new spawn template\n" ++
    "  -f: (required) fcgi application file\n" ++
    "  -p: (required) port number\n" ++
    "  --onstart: command to run after application start\n" ++
    "  --onstop: command to run after application termination"
  putStrLn $ "start: start the spawn process\n" ++
    "  -d: path to spawn directory"
  putStrLn $ "stop: terminate the spawn process\n" ++
    "  -d: path to spawn directory"
  putStrLn $ "reload: restart the spawn process\n" ++
    "  -d: path to spawn directory"
  putStrLn $ "status: print process status and configuration"
  putStrLn $ "clean: stop the process and remove configuration file"
