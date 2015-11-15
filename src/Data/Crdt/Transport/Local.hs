module Data.Crdt.Transport.Local where

import Data.Crdt
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Crdt
import Control.Concurrent (forkIO, ThreadId)
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.Trans

data Stream a = Stream a (MVar (Stream a))


data CrdtVar s = CrdtVar { state :: MVar (s, MVar (Stream (Update s)))
                         , sink :: MVar (MVar (Stream (Update s)))
                         }

newCrdtVar :: s -> IO (CrdtVar s)
newCrdtVar s = do
  updates <- newEmptyMVar
  sink <- newMVar updates
  state <- newMVar (s, updates)
  return $ CrdtVar state sink

dupCrdtVar :: CrdtVar s -> IO (CrdtVar s)
dupCrdtVar (CrdtVar state sink) = do
  snap <- readMVar state
  state' <- newMVar snap
  return $ CrdtVar state' sink

readCrdtVar :: CrdtVar s -> IO s
readCrdtVar (CrdtVar {state=state}) = fst <$> readMVar state

enqueueUpdate :: Update s -> CrdtVar s -> IO ()
enqueueUpdate u (CrdtVar {sink=sink}) =
  modifyMVar_ sink $ \tail -> do
    tail' <- newEmptyMVar
    putMVar tail $ Stream u tail'
    return tail'

runCrdtTOnVar :: (CanUpdate s, MonadIO m) => CrdtT s m a -> CrdtVar s -> m a
runCrdtTOnVar c var = do
  s <- liftIO $ readCrdtVar var
  (_, u, a) <- runCrdtT c s
  liftIO $ enqueueUpdate u var
  return a

runCrdtOnVar :: (CanUpdate s) => Crdt s a -> CrdtVar s -> IO a
runCrdtOnVar c var = do
  s <- readCrdtVar var
  let (_, u, a) = runCrdt c s
  enqueueUpdate u var
  return a


updateCrdtVar :: (CanUpdate s) => CrdtVar s -> IO ()
updateCrdtVar (CrdtVar {state=state}) = do
  (s, updates) <- readMVar state
  Stream u us <- readMVar updates
  let s' = updateWith u s
  modifyMVar_ state $ \_ -> return (s', us)

forkUpdater :: (CanUpdate s) => CrdtVar s -> IO ThreadId
forkUpdater c = forkIO $ forever $ updateCrdtVar c
