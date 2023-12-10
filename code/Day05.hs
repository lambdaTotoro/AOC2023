module Day05 where

import Control.Monad (void, msum)
import Control.Monad.Combinators

import Data.List (intersperse, foldl')

import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

import Helpers

type Seed    = Integer
type Mapping = (Integer, Integer, Integer) 
data Map     = Map String String [Mapping]
             deriving (Show, Eq)

-- # Parsing Nonsense # --

parseSeeds :: Parser [Seed]
parseSeeds = do
  void $ string "seeds:"
  seeds <- some (hspace1 *> decimal)
  return seeds

parseMapping :: Parser Mapping
parseMapping = do
  d1 <- decimal ; void $ space
  d2 <- decimal ; void $ space
  d3 <- decimal ; void $ optional $ string "#"
  return (d1, d2, d3)

parseMap :: Parser Map
parseMap = do
  source <- some $ letterChar
  void $ string "-to-"
  destination <- some $ letterChar
  void $ string " map:#"
  mappings <- some parseMapping
  return $ Map source destination mappings

-- # Functions (Part 1) # --

tryMapping :: Integer -> Mapping -> Maybe Integer
tryMapping n (dest, src, len)
  | n < src         = Nothing
  | n > (src + len) = Nothing
  | otherwise       = Just $ dest + n - src

applyMap :: Integer -> Map -> Integer
applyMap n (Map _ _ ms) = case msum ((tryMapping n) <$> ms) of
  Nothing  -> n
  (Just k) -> k

applyMaps :: Seed -> [Map] -> Integer
applyMaps = foldl' applyMap -- 8)

-- # Functions (Part 2) # --

inSeedRanges :: Seed -> [Seed] -> Bool
inSeedRanges n (x:y:zs)
  | (x <= n) && (n <= x+y) = True
  | otherwise              = inSeedRanges n zs
inSeedRanges _ _ = False

flipMap :: Map -> Map
flipMap (Map s d ms) = Map d s ((\(x,y,z) -> (y,x,z)) <$> ms) 

-- # Main # --

day05_main :: IO ()
day05_main = do
  t_contents <- readFile "data/day05_test.txt"
  q_contents <- readFile "data/day05_01.txt"

  let t_sections = splitList "" $ head $ splitList "\n" (lines t_contents)
  let q_sections = splitList "" $ head $ splitList "\n" (lines q_contents)

  let t_seeds = makeParse parseSeeds ((head . head) t_sections)
  let q_seeds = makeParse parseSeeds ((head . head) q_sections)

  let t_maps = (makeParse parseMap) <$> ((concat . (intersperse "#")) <$> (tail t_sections))
  let q_maps = (makeParse parseMap) <$> ((concat . (intersperse "#")) <$> (tail q_sections))

  let t_results = ((flip applyMaps) t_maps) <$> t_seeds
  let q_results = ((flip applyMaps) q_maps) <$> q_seeds
  
  putStrLn "PART 1:\n"
  putStrLn $ "  Closest test seed: " ++ (show (minimum t_results))
  putStrLn $ "  Closest real seed: " ++ (show (minimum q_results))
  
  putStrLn "\nPART 2:\n"

  let t_fmaps = reverse $ flipMap <$> t_maps 
  let t_valid = filter (\k -> inSeedRanges (applyMaps k t_fmaps) t_seeds) [0..]

  let q_fmaps = reverse $ flipMap <$> q_maps 
  let q_valid = filter (\k -> (inSeedRanges (applyMaps k q_fmaps) q_seeds)) [0..]

  putStrLn $ "  Closest test seed: " ++ (show (head t_valid))
  putStrLn $ "  Closest real seed: " ++ (show (head q_valid))
