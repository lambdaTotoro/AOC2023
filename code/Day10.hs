module Day10 where

import Data.Map.Lazy

type PipeMap   = Map (Int, Int) Char
type DistMap   = Map (Int, Int) Int
data Direction = North | East | South | West deriving (Eq, Show)

-- | "Parsing" | --

mkPMap :: String -> PipeMap
mkPMap str = fromList massocs
  where
    nlines :: [(Int, String)]
    nlines = zip [0..] (lines str)

    massocs :: [((Int, Int), Char)]
    massocs = concat $ (\(l,s) -> [((c,l),k) | (c,k) <- zip [0..] s]) <$> nlines

-- | Part 1 | --

walk :: PipeMap -> (Int, Int) -> Direction -> DistMap -> DistMap
walk pm c@(x,y) dir dm
  | (pm ! c) == 'S' = dm
  | (pm ! c) == 'J' && dir == East  = let nc = (x,y-1) in walk pm nc North (insert nc ((dm ! c) + 1) dm)
  | (pm ! c) == 'J' && dir == South = let nc = (x-1,y) in walk pm nc West  (insert nc ((dm ! c) + 1) dm)
  | (pm ! c) == 'F' && dir == West  = let nc = (x,y+1) in walk pm nc South (insert nc ((dm ! c) + 1) dm)
  | (pm ! c) == 'F' && dir == North = let nc = (x+1,y) in walk pm nc East  (insert nc ((dm ! c) + 1) dm)
  | (pm ! c) == 'L' && dir == West  = let nc = (x,y-1) in walk pm nc North (insert nc ((dm ! c) + 1) dm)
  | (pm ! c) == 'L' && dir == South = let nc = (x+1,y) in walk pm nc East  (insert nc ((dm ! c) + 1) dm)
  | (pm ! c) == '7' && dir == East  = let nc = (x,y+1) in walk pm nc South (insert nc ((dm ! c) + 1) dm)
  | (pm ! c) == '7' && dir == North = let nc = (x-1,y) in walk pm nc West  (insert nc ((dm ! c) + 1) dm)
  | (pm ! c) == '|' && dir == North = let nc = (x,y-1) in walk pm nc North (insert nc ((dm ! c) + 1) dm)
  | (pm ! c) == '|' && dir == South = let nc = (x,y+1) in walk pm nc South (insert nc ((dm ! c) + 1) dm)
  | (pm ! c) == '-' && dir == East  = let nc = (x+1,y) in walk pm nc East  (insert nc ((dm ! c) + 1) dm)
  | (pm ! c) == '-' && dir == West  = let nc = (x-1,y) in walk pm nc West  (insert nc ((dm ! c) + 1) dm)
  | otherwise = error $ "ERROR: Misstep @ " ++ (show c)

-- | Part 2 | --

enclosed :: PipeMap -> (Int, Int) -> Maybe Bool -> Int
enclosed = undefined

-- | Presentation | --

day10_main :: IO ()
day10_main = do
  t1_contents <- readFile "data/day10_test1.txt"
  t2_contents <- readFile "data/day10_test2.txt"
  qz_contents <- readFile "data/day10_01.txt"

  let t1_pmap = mkPMap t1_contents
  let t2_pmap = mkPMap t2_contents
  let qz_pmap = mkPMap qz_contents

  let t1_dmap1 = insert (1,1) 0 $ walk t1_pmap (2,1) East  (singleton (2,1) 1)
  let t1_dmap2 = insert (1,1) 0 $ walk t1_pmap (1,2) South (singleton (1,2) 1)
  let t1_union = unionWith min t1_dmap1 t1_dmap2

  let t2_dmap1 = insert (0,2) 0 $ walk t2_pmap (0,3) South (singleton (0,3) 1)
  let t2_dmap2 = insert (0,2) 0 $ walk t2_pmap (1,2) East  (singleton (1,2) 1)
  let t2_union = unionWith min t2_dmap1 t2_dmap2  

  let qz_dmap1 = insert (15,54) 0 $ walk qz_pmap (14,54) West  (singleton (14,54) 1)
  let qz_dmap2 = insert (15,54) 0 $ walk qz_pmap (15,53) North (singleton (15,53) 1)
  let qz_union = unionWith min qz_dmap1 qz_dmap2

  putStrLn "PART 1:\n"
  putStrLn $ "  Farthest point (simple test loop):  " ++ (show ((maximum . elems) t1_union))
  putStrLn $ "  Farthest point (complex test loop): " ++ (show ((maximum . elems) t2_union))
  putStrLn $ "  Farthest point (real loop):         " ++ (show ((maximum . elems) qz_union))
  
  putStrLn "\nPART 2:\n"
  putStrLn $ "  nothing here yet..."
