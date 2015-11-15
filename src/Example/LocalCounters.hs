module Example.LocalCounters where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.Crdt
import Control.Monad.Reader
import Data.Crdt.Counter
import Data.Crdt.Transport.Local

action :: Crdt Counter ()
action = update $ CounterUpdate 1

thread :: CrdtVar Counter -> Int -> Chan String -> IO ()
thread originalVar delay log = do
  me <- myThreadId
  var <- dupCrdtVar originalVar
  writeChan log $ show me
  forkUpdater var
  forever $ do
    snap <- readCrdtVar var
    writeChan log $ show (me, snap)
    runCrdtOnVar action var
    threadDelay $ delay * 1000

main = do
  v <- newCrdtVar $ Counter 0
  log <- newChan
  forkIO $ thread v 1000 log
  forkIO $ thread v 2000 log
  forkIO $ thread v 3000 log
  forkIO $ forever $ do
    threadDelay 1000000
    writeChan log ""
  forever $ do
    line <- readChan log
    putStrLn line

