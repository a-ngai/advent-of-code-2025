module Day2 ( runDay2, getInvalidIds, getMultiInvalidIds ) where

import Data.List (nub)

runDay2 :: IO ()
runDay2 = do
  text <- readFile  "./inputs/day2.txt"
  let answer = sum $ getInvalidIds text
  putStrLn $ "Day 2 part 1: " ++ show answer
  let multi_answer = sum $ getMultiInvalidIds text
  putStrLn $ "Day 2 part 2: " ++ show multi_answer

getInvalidIds :: String -> [Integer]
getInvalidIds string = concat $ map (invalidInRange 2) $ map getIdRange $ splitWith ',' string

splitWith :: Char -> String -> [String]
splitWith delim string = case break (==delim) string of
  (a, _:b) -> a:(splitWith delim b)
  (a, "") -> [a]

getIdRange :: String -> (Integer, Integer)
getIdRange str = (read a :: Integer, read b :: Integer)
  where
    (a, b) = case break (=='-') str of
      (c, _:d) -> (c, d)
      (_, []) -> error "should not be here"

invalidInRange :: Int -> (Integer, Integer) -> [Integer]
invalidInRange mul (lo, hi) = case odd_digits of
  True -> invalidInRange mul (replace_odd_digit_number, hi)
  False -> case ((left_repeat >= lo), (left_repeat <= hi)) of
    (_, False) -> []
    (False, True) -> invalidInRange mul (next_lo, hi)
    (True, True) -> left_repeat : (invalidInRange mul (next_lo, hi))
  where
    replace_odd_digit_number = read (concat $ "1" : (take lo_len $ repeat "0")) :: Integer
    odd_digits = mod lo_len mul /= 0
    left_lo = take (div lo_len mul) $ show lo
    lo_len = length $ show lo
    left_repeat = read (concat $ take mul $ repeat left_lo) :: Integer
    left_lo_int = read left_lo :: Integer
    next_lo = read (concat $ take mul $ repeat (show (left_lo_int +1))) :: Integer

multiInvalidInRange :: (Integer, Integer) -> [Integer]
multiInvalidInRange (lo, hi) = nub $ concat $ map (flip invalidInRange (lo, hi)) range
  where range = [2..(length $ show hi)]

getMultiInvalidIds :: String -> [Integer]
getMultiInvalidIds string = concat $ map multiInvalidInRange $ map getIdRange $ splitWith ',' string
