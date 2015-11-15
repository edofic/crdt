module Example.SingleCounter where

import Control.Monad
import Control.Monad.Crdt
import Control.Monad.Reader
import Data.Crdt.Counter

action :: Crdt Counter ()
action = do
  Counter c <- ask
  when (c < 10) $ do
    update $ CounterUpdate 1
    action

main = putStrLn $ unlines $ fmap (show . runCrdt action . Counter) [0..12]
