module Day03 where

import Data.Array
import Data.Char
import Data.List

--import Helpers

type Position  = (Int, Int)
type Schematic = Array Position Char

-- Transforms a String for a schematic into an array
-- Probably not necessary in hindsight, but fun and good practice
schematic2array :: String -> Schematic
schematic2array contents = array ((0,0), (xlen-1,ylen-1)) vals
  where
    allLines :: [String]
    allLines = lines contents

    xlen, ylen :: Int
    xlen = length (allLines !! 0)
    ylen = length allLines

    vals :: [(Position, Char)]
    vals = [((x,y), ((allLines !! y) !! x)) | x <- [0..(xlen-1)], y <- [0..(ylen-1)]]

-- Finds position (start of number)
positions :: [String] -> [Position]
positions flines = [(x,y) | y <- [0..(length flines)-1], x <- (noConsec (findIndices isDigit (flines !! y)) Nothing)]
  where
    -- Filters out consecutive indices
    noConsec :: [Int] -> Maybe Int -> [Int]
    noConsec []     _        = []
    noConsec (x:xs) Nothing  = noConsec (x:xs) (Just x)
    noConsec (x:xs) (Just n) | x == (n+1) = noConsec xs (Just x) 
                             | otherwise  = x : (noConsec xs (Just x))

asterisks :: [String] -> [Position]
asterisks flines = [(x,y) | y <- [0..(length flines)-1], x <- (findIndices (== '*') (flines !! y))]

-- Gives the gear power of an asterisk (*). Returns 0 if it's not actually a gear.
gearPower :: Schematic -> [Position] -> Position -> Int 
gearPower arr nums ast = let genv = filter isInEnv nums :: [Position] 
                         in if length genv == 2
                            then product $ map (number arr) genv
                            else 0
  where
    isInEnv :: Position -> Bool
    isInEnv n = ast `elem` (environment arr n)
    

-- Gives the complete environment of a number starting at a position
environment :: Schematic -> Position -> [Position]
environment arr pos@(x,y) | not (inbounds pos)  = []
                          | isDigit (arr ! pos) = let env = filter inbounds [(x',y') | x' <-[x-1,x,x+1], y'<-[y-1,y,y+1]]
                                                  in nub $ env ++ (environment arr (x+1,y))
                          | otherwise           = []
  where
    inbounds :: Position -> Bool
    inbounds (xp,yp) = let ((lx,ly),(ux,uy)) = bounds arr
                        in (xp >= lx) && (xp <= ux) && (yp >= ly) && (yp <= uy) 

-- Gives the number starting at a position
number :: Schematic -> Position -> Int
number arr position = read $ collect "" position
  where
    collect :: String -> Position -> String
    collect str pos@(x,y) | isDigit (arr ! pos) = if   x == (snd . snd . bounds) arr
                                                  then str ++ [arr ! pos]
                                                  else collect (str ++ [arr ! pos]) (x+1,y)
                          | otherwise           = str

valid :: Schematic -> Position -> Bool
valid arr pos = any (\p -> isSchematicSymbol (arr ! p)) (environment arr pos)
  where
    -- Anything that isn't a digit or a period is a symbol
    isSchematicSymbol :: Char -> Bool
    isSchematicSymbol c = ((not . isDigit) c) && ((/= '.') c)

day03_main :: IO ()
day03_main = do
  t_contents <- readFile "data/day03_test.txt"
  q_contents <- readFile "data/day03_01.txt"

  let t_array = schematic2array t_contents
  let q_array = schematic2array q_contents

  let t_positions = positions (lines t_contents)
  let t_results = sum $ (number t_array) <$> (filter (valid t_array) t_positions)
  
  let q_positions = positions (lines q_contents)
  let q_results = sum $ (number q_array) <$> (filter (valid q_array) q_positions)

  let t_asts = asterisks (lines t_contents)
  let q_asts = asterisks (lines q_contents)

  let t_power = sum $ (gearPower t_array t_positions) <$> t_asts
  let q_power = sum $ (gearPower q_array q_positions) <$> q_asts

  --print $ environment q_array (137,2)
  --print $ q_positions
  --print $ bounds q_array
  --print $ (number q_array) <$> q_positions

  putStrLn $ "Test 1: " ++ show t_results
  putStrLn $ "Part 1: " ++ show q_results
  putStrLn $ "Test 2: " ++ show t_power
  putStrLn $ "Part 2: " ++ show q_power
  
