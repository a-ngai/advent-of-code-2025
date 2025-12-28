module Day3 ( runDay3, makeCells, getMaxPower ) where

runDay3 :: IO ()
runDay3 = do
    text <- readFile "inputs/day3.txt"
    let cells = map makeCells $ lines text
    putStrLn $ "Day 3 Part 1: " ++ (show . sum . map (getMaxPower 2)) cells
    putStrLn $ "Day 3 Part 2: " ++ (show . sum . map (getMaxPower 12)) cells

makeCells :: String -> [Int]
makeCells str = map (read . (:"")) str :: [Int]

_getMaxPower :: Int -> [Int] -> [Int] -> Int
_getMaxPower num_ints curr list = case num_ints of
    0 -> read (reverse $ concat $ map show curr ) :: Int
    _ -> _getMaxPower (num_ints - 1) (select:curr) remaining
    where
      allowed = reverse $ drop (num_ints - 1) $ reverse list 
      select = maximum allowed
      remaining = drop 1 $ dropWhile (/=select) list

getMaxPower :: Int -> [Int] -> Int
getMaxPower num cell = _getMaxPower num [] cell
