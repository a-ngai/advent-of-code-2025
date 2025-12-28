module Day1 ( runDay1, getPassword, getClicks) where

runDay1 :: IO ()
runDay1 = do
  text <- readFile  "./inputs/day1.txt"
  let password = getPassword $ lines text
  putStrLn $ "Day 1 part 1: " ++ show password
  let num_clicks = getClicks $ lines text
  putStrLn $ "Day 1 part 2: " ++ show num_clicks

rotationFromStr :: String -> Integer
rotationFromStr ('L':num) = - (read num :: Integer)
rotationFromStr ('R':num) = (read num :: Integer)
rotationFromStr other = error $ "neither L nor R!" ++ other

_getHistory :: Integer -> [Integer] -> [Integer] -> [Integer]
_getHistory _ ints [] = ints
_getHistory int ints (num: list) = _getHistory next (ints ++ [next]) list
  where next = mod (toInteger int + num) 100

getHistory :: [Integer] -> [Integer]
getHistory = _getHistory 50 [50]

getPassword :: [String] -> Int
getPassword = length . filter (==0) . getHistory . map rotationFromStr

clicks :: (Integer, Integer) -> Integer
clicks (pos, rot) =  div (pos + rot) 100

getClicks :: [String] -> Integer
getClicks strings = sum $ map (abs . clicks) $ zip history rotations
  where
    rotations = map rotationFromStr strings
    history =  getHistory rotations
