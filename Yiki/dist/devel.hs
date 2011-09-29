
{-# LANGUAGE PackageImports #-}
import "Yiki" Application (withDevelAppPort)
import Data.Dynamic (fromDynamic)
import Network.Wai.Handler.Warp (run)
import Data.Maybe (fromJust)
import Control.Concurrent (forkIO)
import System.Directory (doesFileExist, removeFile)
import System.Exit (exitSuccess)
import Control.Concurrent (threadDelay)

main :: IO ()
main = do
  putStrLn "Starting devel application"
  wdap <- (return . fromJust . fromDynamic) withDevelAppPort
  forkIO . wdap $ \(port, app) -> run port app
  loop

loop :: IO ()
loop = do
  threadDelay 100000
  e <- doesFileExist "dist/devel-terminate"
  if e then terminateDevel else loop

terminateDevel :: IO ()
terminateDevel = exitSuccess
