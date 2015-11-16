module Data.Crdt.Counter where

import Data.Aeson
import Data.Crdt
import GHC.Generics

newtype Counter = Counter { readCounter :: Integer } deriving (Eq, Show, Generic)

newtype CounterUpdate = CounterUpdate { readCounterUpdate :: Integer } deriving (Eq, Show, Generic)

instance Monoid CounterUpdate where
  mempty = CounterUpdate 0
  CounterUpdate u1 `mappend` CounterUpdate u2 = CounterUpdate (u1 + u2)

instance CanUpdate Counter where
  type Update Counter = CounterUpdate
  updateWith (CounterUpdate u) (Counter c) = Counter (c + u)

instance ToJSON Counter
instance ToJSON CounterUpdate
instance FromJSON Counter
instance FromJSON CounterUpdate
