module Day01 where

import Text.Regex.TDFA
import Data.List

frontReplace :: String -> String
frontReplace "" = ""
frontReplace str | "one"   `isPrefixOf` str = '1':(frontReplace (tail str))
                 | "two"   `isPrefixOf` str = '2':(frontReplace (tail str))
                 | "three" `isPrefixOf` str = '3':(frontReplace (tail str))
                 | "four"  `isPrefixOf` str = '4':(frontReplace (tail str))
                 | "five"  `isPrefixOf` str = '5':(frontReplace (tail str))
                 | "six"   `isPrefixOf` str = '6':(frontReplace (tail str))
                 | "seven" `isPrefixOf` str = '7':(frontReplace (tail str))
                 | "eight" `isPrefixOf` str = '8':(frontReplace (tail str))
                 | "nine"  `isPrefixOf` str = '9':(frontReplace (tail str))
                 | otherwise = (head str):(frontReplace (tail str))

calibration :: String -> Integer
calibration inp = read $ (head matches) ++ (last matches)
  where
    matches = getAllTextMatches (inp =~ "[[:digit:]]") :: [String]

day01_main :: IO ()
day01_main = do
  contents <- readFile "data/day01_01.txt"
  putStrLn $ "PART 1: " ++ show ((sum . (fmap calibration) . lines) contents)
  putStrLn $ "PART 2: " ++ show ((sum . (fmap (calibration . frontReplace)) . lines) contents)
