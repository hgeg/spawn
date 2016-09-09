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

-- type declarations ----------------------
data Options = O { oproc  :: String 
                 , oport  :: String 
                 , owd    :: String 
                 , ostart :: String 
                 , ostop  :: String
                 } deriving (Show)

empty :: Options
empty = O {oproc = "", oport = "", owd = "", ostart = "", ostop = ""}

data Config = C { cproc  :: String
                , cport  :: String
                , cstart :: String
                , cstop  :: String
                } deriving (Show)
-------------------------------------------

main = do
  putStrLn "spawn v0.3.0"
  args <- getArgs
  let (cmd:rest) = args
  runCommand cmd $ consumeOpts rest

-- function declarations ------------------
-- helpers --
runCommand  :: String -> Options -> IO ()
consumeOpts :: [String] -> Options
readConfig  :: IO (Config)

toLines   :: Config -> [String]
fromLines :: [String] -> Config
-- commands --
cInit   :: Options -> IO ()
cStart  :: Options -> IO ()
cStop   :: IO ()
cReload :: Options -> IO ()
cStatus :: IO ()
cClean  :: IO ()
cError  :: IO ()
-------------------------------------------

-- function definitions -------------------
runCommand "init"   opts = cInit opts
runCommand "start"  opts = cStart opts 
runCommand "stop"   opts = cStop
runCommand "reload" opts = cReload opts
runCommand "status" opts = cStatus
runCommand "clean"  opts = cClean
runCommand c        o    = putStrLn $ "err: " ++ c ++ " opts: " ++ (show o)

consumeOpts' opt []                 = opt
consumeOpts' opt [_]                = opt
consumeOpts' opt ("-f":v:os)        = consumeOpts' O {oproc = v        , oport = oport opt, owd = owd opt, ostart = ostart opt, ostop = ostop opt} os
consumeOpts' opt ("-p":v:os)        = consumeOpts' O {oproc = oproc opt, oport = v        , owd = owd opt, ostart = ostart opt, ostop = ostop opt} os
consumeOpts' opt ("-d":v:os)        = consumeOpts' O {oproc = oproc opt, oport = oport opt, owd =      v , ostart = ostart opt, ostop = ostop opt} os
consumeOpts' opt ("--onstart":v:os) = consumeOpts' O {oproc = oproc opt, oport = oport opt, owd = owd opt, ostart = v         , ostop = ostop opt} os
consumeOpts' opt ("--onstop":v:os)  = consumeOpts' O {oproc = oproc opt, oport = oport opt, owd = owd opt, ostart = ostart opt, ostop = v        } os
consumeOpts' opt (_:_:os)           = consumeOpts' opt os

consumeOpts = consumeOpts' empty

readConfig = do
  contents <- fmap lines $ readFile ".spawn"
  return C {cproc = contents !! 0, cport = contents !! 1, cstart = contents !! 2, cstop = contents !! 3}

toLines cfg = [cproc cfg, cport cfg, cstart cfg, cstop cfg]
fromLines lines = C {cproc= lines !! 0, cport = lines !! 1, cstart = lines !! 2, cstop = lines !! 3}

mint Nothing = -999
mint (Just n)  = n

cInit opts = do
  putStr "creating spawn config: "
  let proc  = oproc opts
  let port  = readMaybe (oport opts) :: Maybe Int
  let start = ostart opts
  let stop  = ostop opts
  let config = proc ++ "\n" ++ show (mint port) ++ "\n" ++ start ++ "\n" ++ stop
  writeFile ".spawn" config
  putStrLn "ok."

getPid ph = withProcessHandle ph go
  where go ph_ = case ph_ of
                   OpenHandle   x -> return x
                   ClosedHandle _ -> return (-1)

isRunning pid = do
  handle <- (ph pid)
  (ex handle)
  where ph p  = mkProcessHandle (T.CPid p) False
        ex ph = P.getProcessExitCode ph

cStart opts = do
  putStr "starting process: "

  config <- readConfig
  let exec = "./" ++ (cproc config)
  pcount <- fmap read $ P.readCreateProcess (P.shell $ "pgrep -f \"" ++ exec ++ "\" | wc -l") ""

  if pcount > 0 then 
    putStrLn "already running!"
  else do
    ph     <- P.spawnCommand $ intercalate " " [exec, "-p", (cport config), ">/dev/null", "2>&1"]
    newpid <- getPid ph
    putStrLn $ show newpid
  
cStop = do
  config <- readConfig
  putStr "terminating process: "
  let exec = "\"./" ++ (cproc config) ++ "\""
  P.spawnCommand $ intercalate " " ["pgrep","-f",exec,"|","xargs","kill"]
  putStrLn "ok."

cReload opts = cStop >> (cStart opts)

cStatus = do
  config <- readConfig
  let onstart = cstart config
  let onstop  = cstop config
  let exec = "./" ++ (cproc config)
  pcount <- fmap read $ P.readCreateProcess (P.shell $ "pgrep -f \"" ++ exec ++ "\" | wc -l") ""

  putStrLn "configuration:"
  putStrLn $ "status:   " ++ if pcount>0 then "running" else "stopped"
  putStrLn $ "process:  " ++ cproc config
  putStrLn $ "port:     " ++ cport config
  --if onstart=="" then putStrLn $ "on start: " ++ onstart
  --else return ()
  --if onstop=="" then putStrLn $ "on stop:  " ++ onstop
  --else return ()

cClean = do
  putStr "removing configuration: "
  spconf <- doesFileExist ".spawn"
  if spconf then
    removeFile ".spawn"
  else return ()
  cStop
  putStrLn "ok."

cError = do
  putStrLn "unknown command: "
  putStrLn "usage: spawn [init|start|stop|reload|status|clean] [options]"
-------------------------------------------
