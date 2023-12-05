module Helpers where

import Debug.Trace

oTrace :: (Show a) => a -> a
oTrace foo = trace (show foo) foo

sTrace :: (Show a) => a -> b -> b
sTrace foo bar = trace (show foo) bar

-- | Split is like break, but the matching element is dropped.
split :: (a -> Bool) -> [a] -> ([a], [a])
split f s = (left,right)
        where
        (left,right')=break f s
        right = if null right' then [] else tail right'

-- | Repeadly splits a list by the provided separator and collects the results
splitList :: Eq a => a -> [a] -> [[a]]
splitList _   [] = []
splitList sep list = h:splitList sep t
        where (h,t)=split (==sep) list
