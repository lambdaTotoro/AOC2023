module Day08 where

import Control.Monad (void)

import Data.Functor (($>))
import Data.Map.Lazy

import Text.Megaparsec
import Text.Megaparsec.Char

import Helpers

type Node = String
type NodeMap = Map Node (Node, Node)

data Direction = LEFT | RIGHT deriving (Eq, Show)

-- | Parsing | --

parseDir :: Parser Direction
parseDir = choice [ (char 'L') $> LEFT
                  , (char 'R') $> RIGHT ]

parseNodes :: Parser (Node, (Node, Node))
parseNodes = do
  node  <- count 3 alphaNumChar ; void (string " = (")
  left  <- count 3 alphaNumChar ; void (string ", ")
  right <- count 3 alphaNumChar ; void (char ')')
  void (optional space)
  return (node, (left, right))

parseMap :: Parser NodeMap
parseMap = do
  nl <- some parseNodes
  return (fromList nl)

parseAll :: Parser ([Direction], NodeMap)
parseAll = do
  path <- (some parseDir)
  void (string "\n\n")
  nmap <- parseMap
  return (path, nmap)

-- | Part 1 | --

walk :: NodeMap -> [Direction] -> Node -> [Node] -> [Node]
walk m ds goal accum
  | goal == head accum = reverse accum
  | (head ds) == LEFT  = walk m (tail ds) goal ((fst (m ! (head accum))):accum)
  | (head ds) == RIGHT = walk m (tail ds) goal ((snd (m ! (head accum))):accum)
  | otherwise  = error "WALK ERROR" 

-- | Part 2 | --

ghost_walk :: NodeMap -> [Direction] -> Char -> [Node] -> Integer -> Integer
ghost_walk m ds goal accums steps
  | all (\n -> (last n) == goal) accums = steps
  | otherwise = ghost_walk m (tail ds) goal ((next (head ds)) <$> accums) (steps + 1)
  where
    next :: Direction -> Node -> Node
    next LEFT  node = fst $ (m ! node)
    next RIGHT node = snd $ (m ! node)

-- | Execution | --

day08_main :: IO ()
day08_main = do
  t1_contents <- readFile "data/day08_test1.txt"
  t2_contents <- readFile "data/day08_test2.txt"
  q1_contents <- readFile "data/day08_01.txt"

  let (t1_path, t1_map) = makeParse parseAll t1_contents
  let (t2_path, t2_map) = makeParse parseAll t2_contents
  let (q1_path, q1_map) = makeParse parseAll q1_contents

  let t1_walk = walk t1_map (cycle t1_path) "ZZZ" ["AAA"]
  let t2_walk = walk t2_map (cycle t2_path) "ZZZ" ["AAA"]
  let q1_walk = walk q1_map (cycle q1_path) "ZZZ" ["AAA"]

  putStrLn "PART 1:\n"
  putStrLn $ "  First test walk length:  " ++ ((show . length . tail) t1_walk)
  putStrLn $ "  Second test walk length: " ++ ((show . length . tail) t2_walk)
  putStrLn $ "  Real walk length:        " ++ ((show . length . tail) q1_walk)
  
  t3_contents <- readFile "data/day08_test3.txt"
  let (t3_path, t3_map) = makeParse parseAll t3_contents
  let t3_starts = Prelude.filter (\n -> last n == 'A') (keys t3_map)

  let q1_starts = Prelude.filter (\n -> last n == 'A') (keys q1_map)

  putStrLn "\nPART 2:\n"
  putStrLn $ "  Ghost test walk length: " ++ (show (ghost_walk t3_map (cycle t3_path) 'Z' t3_starts 0))
  putStrLn $ "  Real ghost walk length: " ++ (show (ghost_walk q1_map (cycle q1_path) 'Z' q1_starts 0))
