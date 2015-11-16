module Example.PostgresCounters where

import Control.Concurrent
import Control.Monad
import Control.Monad.Crdt
import Control.Monad.Reader
import Data.Crdt.Counter
import Data.Crdt.Transport.Postgresql
import System.Environment (getArgs)

run n d = do
  var <- newCrdtVar "" $ Counter 0
  forkUpdater var
  forever $ do
    snap <- readCrdtVar var
    print snap
    runCrdtOnVar (update $ CounterUpdate n) var
    threadDelay $ d * 1000 * 1000

main = do
  [n, d] <- getArgs
  run (read n) (read d)

