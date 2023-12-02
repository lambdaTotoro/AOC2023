module Day02 where

import Data.Char
import Text.Regex.TDFA

--import Debug.Trace

findColour :: String -> String -> [Int]
findColour line colour = fmap (read . (takeWhile isDigit)) matches
  where
    matches = getAllTextMatches (line =~ regex)
    regex = "[[:digit:]]+ " ++ colour

rgbs :: String -> ([Int], [Int], [Int])
rgbs line = (findColour line "red", findColour line "green", findColour line "blue")

valid :: (Int, Int, Int) -> String -> Bool
valid (r,g,b) line = (all (<=r) reds) && (all (<=g) greens) && (all (<=b) blues)
  where
    (reds, greens, blues) = rgbs line

gameNumber :: String -> Int
gameNumber line = read $ filter isDigit ((line =~ "Game [[:digit:]]+:") :: String)

minSet :: String -> (Int, Int, Int)
minSet line = (maximum reds, maximum greens, maximum blues)
  where
    (reds, greens, blues) = rgbs line

power :: String -> Int
power line = r * g * b
  where
    (r,g,b) = minSet line

day02_main :: IO ()
day02_main = do
  t_contents <- readFile "data/day02_test.txt"
  q_contents <- readFile "data/day02_01.txt"
  let compute1 = \c -> sum $ (gameNumber <$> (((filter (valid (12,13,14))) . lines) c))
  let compute2 = \c -> sum $ power <$> (lines c)
  putStrLn $ "Test 1: " ++ (show (compute1 t_contents))
  putStrLn $ "Solution Part 1: " ++ (show (compute1 q_contents))
  putStrLn $ "Test 2: " ++ (show (compute2 t_contents))
  putStrLn $ "Solution Part 2: " ++ (show (compute2 q_contents))
