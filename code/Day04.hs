module Day04 where

import Control.Monad (void)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

data ScratchCard = SC Int [Int] [Int] deriving (Show, Eq)

type Parser = Parsec Void String
type Pile = [Int]

parseSC :: Parser ScratchCard
parseSC = do
  void (string "Card"); void hspace1 
  cnumber <- decimal  ; void (char ':')
  void hspace1
  wins <- some (decimal <* hspace1)
  void (char '|')
  haves <- some (hspace1 *> decimal)
  return $ SC cnumber wins haves

winners :: ScratchCard -> Int
winners (SC _ w h) = length $ filter (`elem` w) h

points :: ScratchCard -> Int
points sc = let ws = winners sc
             in if ws == 0 then 0 else 2^(ws-1)

makeSC :: String -> ScratchCard
makeSC input = case parseMaybe parseSC input of
  (Just sc) -> sc
  Nothing   -> error $ "PARSE ERROR ON " ++ input

winSCs :: ScratchCard -> Pile
winSCs sc@(SC cn _ _) = p_pref ++ p_inf ++ p_suff
  where
    p_pref = replicate cn 0
    p_inf  = replicate (winners sc) 1
    p_suff = replicate (198 - (length p_pref) - (length p_inf)) 0

winAllSCs :: Pile -> ScratchCard -> Pile
winAllSCs ini sc@(SC n _ _) = zipWith (+) ini allwns
  where
    factor = ini !! (n-1)
    allwns = (* factor) <$> (winSCs sc)

day04_main :: IO ()
day04_main = do
  t_contents <- readFile "data/day04_test.txt"
  q_contents <- readFile "data/day04_01.txt"

  let t_cards = makeSC <$> (lines t_contents)
  let q_cards = makeSC <$> (lines q_contents)

  let t1_results = sum $ points <$> t_cards
  let q1_results = sum $ points <$> q_cards

  putStrLn "### PART 1:\n"
  putStrLn $ "  Test:      " ++ (show t1_results)
  putStrLn $ "  Real data: " ++ (show q1_results)
  putStrLn ""

  let t2_results = sum $ foldl winAllSCs ((replicate 6 1) ++ (replicate 192 0)) t_cards
  let q2_results = sum $ foldl winAllSCs (replicate 198 1) q_cards

  putStrLn "### PART 2:\n"
  putStrLn $ "  Test:      " ++ (show t2_results)
  putStrLn $ "  Real data: " ++ (show q2_results)
  putStrLn ""
  
