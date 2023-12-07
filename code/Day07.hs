module Day07 where

import Data.Functor (($>))
import Data.List
import Data.Void

--import Control.Applicative.Combinators (choice)
import Control.Monad (void)

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void String

data Card = C2 | C3 | C4 | C5 | C6 | C7 | C8 | C9 | CT | CJ | CQ | CK | CA
          deriving (Eq, Ord, Enum, Show)

data Hand = HC [Card] | OP [Card] | TP [Card] | THREE [Card] | FH [Card] | FOUR [Card] | FIVE [Card]
          deriving (Eq, Ord, Show)

newtype JCard = JCard Card
              deriving (Eq, Show)

instance Ord JCard where
  (JCard CJ) `compare` (JCard CJ) = EQ
  (JCard CJ) `compare` _          = LT
  _          `compare` (JCard CJ) = GT
  (JCard a)  `compare` (JCard b)  = a `compare` b

-- Original and best replacement of jokers
data JHand = JHand [JCard] Int
            deriving (Eq, Show)

instance Ord JHand where
  (JHand jcs1 v1) `compare` (JHand jcs2 v2)
    | v1 /= v2  = v1 `compare` v2
    | otherwise = jcs1 `compare`jcs2

combo :: [Card] -> Hand
combo cards = case sort (length <$> (group . sort) cards) of
  [5]         -> FIVE cards
  [1,4]       -> FOUR cards
  [2,3]       -> FH cards
  [1,1,3]     -> THREE cards
  [1,2,2]     -> TP cards
  [1,1,1,2]   -> OP cards
  _           -> HC cards

replace :: Eq a => a -> a -> [a] -> [a]
replace x y xs = (\z -> if z == x then y else z) <$> xs

valuation :: Hand -> Int
valuation (HC _)    = 0
valuation (OP _)    = 1
valuation (TP _)    = 2
valuation (THREE _) = 3
valuation (FH _)    = 4
valuation (FOUR _)  = 5
valuation (FIVE _)  = 6

jcombo :: [Card] -> JHand
jcombo cards = (last . sort) allJhands
  where
    allJhands :: [JHand]
    allJhands = [ JHand (JCard <$> cards) ((valuation . combo) (replace CJ c cards)) | c <- poss_reps]

    poss_reps :: [Card]
    poss_reps = filter (/= CJ) [toEnum 0 .. toEnum 12]

parseCard :: Parser Card
parseCard = choice
  [ char 'A' $> CA 
  , char 'K' $> CK
  , char 'Q' $> CQ
  , char 'J' $> CJ
  , char 'T' $> CT
  , char '9' $> C9
  , char '8' $> C8
  , char '7' $> C7
  , char '6' $> C6
  , char '5' $> C5
  , char '4' $> C4
  , char '3' $> C3
  , char '2' $> C2 ]

parseEntry :: Parser (Hand, Int)
parseEntry = do 
  cards <- count 5 parseCard
  (void hspace)
  bet <- decimal
  return (combo cards, bet)

parseJEntry :: Parser (JHand, Int)
parseJEntry = do 
  cards <- count 5 parseCard
  (void hspace)
  bet <- decimal
  return (jcombo cards, bet)

makeParse :: Parser a -> String -> a
makeParse parser text = case parseMaybe parser text of
  (Just n) -> n
  Nothing  -> error $ "PARSE ERROR!\n" ++ (show text)

day07_main :: IO ()
day07_main = do
  t_contents <- readFile "data/day07_test.txt"
  q_contents <- readFile "data/day07_01.txt"

  let t_entries = (makeParse parseEntry) <$> (lines t_contents)
  let t_result  = sum $ (\(r,(_,b)) -> r*b) <$> (zip [1..] (sort t_entries))

  let q_entries = (makeParse parseEntry) <$> (lines q_contents)
  let q_result  = sum $ (\(r,(_,b)) -> r*b) <$> (zip [1..] (sort q_entries))
  
  putStrLn "PART 1:\n"
  putStrLn $ "  Test sum: " ++ (show t_result)
  putStrLn $ "  Real sum: " ++ (show q_result)
  
  let t_jentries = (makeParse parseJEntry) <$> (lines t_contents)
  let t_jresult  = sum $ (\(r,(_,b)) -> r*b) <$> (zip [1..] (sort t_jentries))

  let q_jentries = (makeParse parseJEntry) <$> (lines q_contents)
  let q_jresult  = sum $ (\(r,(_,b)) -> r*b) <$> (zip [1..] (sort q_jentries))
 
  putStrLn "\nPART 2:\n"
  putStrLn $ "  Test jsum: " ++ (show t_jresult)
  putStrLn $ "  Real jsum: " ++ (show q_jresult)
