module Helpers where

import Debug.Trace

oTrace :: (Show a) => a -> a
oTrace foo = trace (show foo) foo

sTrace :: (Show a) => a -> b -> b
sTrace foo bar = trace (show foo) bar
