module Data.Crdt where

class (Monoid (Update s)) => CanUpdate s where
  type Update s
  updateWith :: Update s -> s -> s
  -- updateWith (u1 <> u2) == updateWith u1 . updateWith u2
