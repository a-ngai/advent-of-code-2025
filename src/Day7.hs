module Day7 ( runDay7, Path (Path), findStartingSplit, evolveBeam, findSplits, drawChart ) where

import Data.List ( nub )

data Path = Path Int (Int, Int)  deriving (Eq, Show)

runDay7 :: IO ()
runDay7 = do
    text <- readFile "inputs/day7.txt"
    let chart = lines text
    let start = findStartingSplit $ chart  :: [Path]
    let beam = evolveBeam chart start  :: [[Path]]
    -- putStrLn $ concat $ intersperse "\n" $ drawChart chart (concat beam)
    putStrLn $ "Day 7 Part 1: " ++ (show $ length $ findSplits chart (concat beam))
    putStrLn $ "Day 7 Part 2: " ++ (show $ sum $ map (\(Path num _) -> num) $ concat $ take 1 $ reverse beam)

findStartingSplit :: [String] -> [Path]
findStartingSplit chart = map (\coor -> Path 1 coor) found_coor
    where
        combined_chart = (zipWith . zipWith) (, ) (makeCoorChart chart) chart
        found_coor = concat $ (map . map)  fst $ (map . filter) ( (=='S') . snd ) combined_chart

tryFind :: [[a]] -> (Int, Int) -> [a]
tryFind chart (x, y) = take 1 $ drop y $ concat $ take 1 $ drop x chart

beamNext :: [String] -> Path -> [Path]
beamNext chart (Path num (x, y)) = case tryFind chart (x+1, y) of
    "" -> []
    "^" -> [Path num (x+1, y-1), Path num (x+1, y+1)]
    "." -> [Path num (x+1, y)]
    "S" -> [Path num (x+1, y)]
    other -> error $ "should not be here! "  ++ other

evolveBeam :: [String] -> [Path] -> [[Path]]
evolveBeam _ [] = []
evolveBeam chart list = case next_list of
    [] -> []
    a -> a : (evolveBeam chart a)
    where next_list = nubPath $ concat $ map (beamNext chart) list

numIfSamePath :: (Int, Int) -> Path -> Int
numIfSamePath coor (Path num path_coor) = if path_coor == coor then num else 0

nubPath :: [Path] -> [Path]
nubPath paths = map (\coor -> Path (sum $ map (numIfSamePath coor) paths) coor ) unique_coor
    where unique_coor = nub $ map (\(Path _ coor) -> coor) paths  :: [(Int, Int)]

findSplits :: [String] -> [Path] -> [Path]
findSplits _ [] = []
findSplits chart ((Path num (x, y)):next) = case tryFind chart (x+1, y) == "^" of
    True -> [Path num (x, y)] ++ findSplits chart next
    False -> findSplits chart next

drawChart :: [String] -> [Path] -> [String]
drawChart chart list = (map . map) (chooseElement list chart) coor_chart
    where coor_chart = makeCoorChart chart

makeCoorChart :: [[a]] -> [[(Int, Int)]]
makeCoorChart chart = map (\(x, num) -> zip (repeat x) [0..(num-1)]) $ zip [0..] $ map length chart

chooseElement :: [Path] -> [String] -> (Int, Int) -> Char
chooseElement coor_list char_chart (x,y)  = case elem (x,y) $ map (\(Path _ coor) -> coor) coor_list of
    True -> '|'
    False -> case look_up of
        char:[] -> char
        _ -> error $ "cannot find it! " ++ show (x, y)
    where look_up = take 1 $ drop y $ concat $ take 1 $ drop x char_chart
