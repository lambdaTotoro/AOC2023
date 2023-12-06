module Day06 where

-- Not even worth importing the parser for.
test_races :: [(Int,Int)]
test_races = [(7,9),(15,40),(30,200)]

real_races :: [(Int,Int)]
real_races = [(44,202),(82,1076),(69,1138),(81,1458)]

test_race :: (Int, Int)
test_race = (71530,940200)

real_race :: (Int, Int)
real_race = (44826981, 202107611381458)

wins_race :: (Int,Int) -> Int -> Bool
wins_race (time, record) duration = (time - duration) * duration > record

day06_main :: IO ()
day06_main = do
  putStrLn "PART 1:\n"

  let t1_result = product $ (\r -> (length . (filter id)) ((wins_race r) <$> [0..fst r])) <$> test_races
  let q1_result = product $ (\r -> (length . (filter id)) ((wins_race r) <$> [0..fst r])) <$> real_races

  putStrLn $ "  Test races: " ++ (show t1_result)
  putStrLn $ "  Real races: " ++ (show q1_result)
  
  putStrLn "\nPART 2:\n"
 
  let t2_result = length . (filter id) $ (wins_race test_race) <$> [0..(fst test_race)] 
  let q2_result = length . (filter id) $ (wins_race real_race) <$> [0..(fst real_race)] 
  
  putStrLn $ "  Test race: " ++ (show t2_result)
  putStrLn $ "  Real race: " ++ (show q2_result)
