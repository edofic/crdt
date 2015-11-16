module Data.Crdt.Transport.Postgresql where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Crdt
import Control.Monad.Trans
import Data.Aeson.Types
import Data.ByteString (ByteString)
import Data.Crdt
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

data UpdateEntry = UpdateEntry { id :: !Integer
                               , action :: !Value
                               } deriving (Eq, Show)

instance FromRow UpdateEntry where
  fromRow = UpdateEntry <$> field <*> field


load :: Connection -> Integer -> IO [UpdateEntry]
load conn id = query conn "SELECT id, action FROM updates WHERE id > ?" (Only id)

store :: Connection -> Value -> IO ()
store conn value = do
  execute conn "INSERT INTO updates (action) VALUES (?)" (Only value)
  return ()


data CrdtVar s = CrdtVar { state :: MVar (s, Integer)
                         , connStr :: ByteString
                         , conn :: Connection
                         }

newCrdtVar :: ByteString -> s -> IO (CrdtVar s)
newCrdtVar connectionString s = do
  conn <- connectPostgreSQL connectionString
  state <- newMVar (s, 0)
  return $ CrdtVar state connectionString conn

dupCrdtVar :: CrdtVar s -> IO (CrdtVar s)
dupCrdtVar (CrdtVar state connStr _) = do
  conn <- connectPostgreSQL connStr
  snap <- readMVar state
  state <- newMVar snap
  return $ CrdtVar state connStr conn

readCrdtVar :: CrdtVar s -> IO s
readCrdtVar (CrdtVar {state=state}) = fst <$> readMVar state

enqueueUpdate :: (ToJSON (Update s)) => Update s -> CrdtVar s -> IO ()
enqueueUpdate u (CrdtVar {conn}) = store conn $ toJSON u


runCrdtTOnVar :: (CanUpdate s, MonadIO m, ToJSON (Update s)) => CrdtT s m a -> CrdtVar s -> m a
runCrdtTOnVar c var = do
  s <- liftIO $ readCrdtVar var
  (_, u, a) <- runCrdtT c s
  liftIO $ enqueueUpdate u var
  return a

runCrdtOnVar :: (CanUpdate s, ToJSON (Update s)) => Crdt s a -> CrdtVar s -> IO a
runCrdtOnVar c var = do
  s <- readCrdtVar var
  let (_, u, a) = runCrdt c s
  enqueueUpdate u var
  return a

updateCrdtVar :: (CanUpdate s, FromJSON (Update s)) => CrdtVar s -> IO ()
updateCrdtVar (CrdtVar {state, conn}) = do
  (s, i) <- readMVar state
  updates <- load conn i
  let f (UpdateEntry {action}) = case fromJSON action of Success a -> a
      s' = updateWith (foldMap f updates) s
      i' = maximum $ i : [i | UpdateEntry i _ <- updates]
  modifyMVar_ state $ \_ -> return (s', i')

forkUpdater :: (CanUpdate s, FromJSON (Update s)) => CrdtVar s -> IO ThreadId
forkUpdater c = forkIO $ forever $ do
  updateCrdtVar c
  threadDelay $ 1000 * 1000
  -- todo wait for NOTIFY
