import System.Process
import Control.Monad.Maybe

type PID = Int

main = do
  putStrLn "spawn v0.0.1"
  args <- getArgs
  consumeArgs args

consumeArgs :: [String] -> IO ()
init   :: String -> Int -> Maybe String -> Maybe String -> IO ()
start  :: Maybe String -> Maybe PID
stop   :: PID -> IO ()
status :: IO ()
clean  :: IO ()


