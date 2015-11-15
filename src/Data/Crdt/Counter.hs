module Data.Crdt.Counter where

import Data.Crdt

newtype Counter = Counter { readCounter :: Integer } deriving (Eq, Show)

newtype CounterUpdate = CounterUpdate { readCounterUpdate :: Integer } deriving (Eq, Show)

instance Monoid CounterUpdate where
  mempty = CounterUpdate 0
  CounterUpdate u1 `mappend` CounterUpdate u2 = CounterUpdate (u1 + u2)

instance CanUpdate Counter where
  type Update Counter = CounterUpdate
  updateWith (CounterUpdate u) (Counter c) = Counter (c + u)
