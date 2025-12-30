module Day8 ( runDay8, makeCoor, findNClosest, generatePairs, continueCircuits, makeCircuits, Coor (Coor), Pair (Pair) ) where

import Data.List ( sortBy, nub )

runDay8 :: IO ()
runDay8 = do
    text <- readFile "inputs/day8.txt"
    let coors = map makeCoor $ lines text
    let connections = findNClosest 1000 $ generatePairs coors

    let circuits = makeCircuits connections
    let sorted = sortBy (\a -> \b -> compare (length a) (length b)) circuits
    putStrLn $ show $ "Day 8 Part 1: " ++ show (product $ take 3 $ map length $ reverse sorted)

    let final_connection = continueCircuits coors :: Maybe Pair
    let answer = case final_connection of
            Just (Pair (Coor x1 _ _) (Coor x2 _ _)) -> (show $ x1 * x2)
            Nothing -> "error!"
    putStrLn $ "Day 8 Part 2: " ++ answer

data Coor = Coor Integer Integer Integer deriving (Eq, Show)
data Pair = Pair Coor Coor deriving (Eq, Show)

makeCoor :: String -> Coor
makeCoor str = case splitDelim ',' str of
    (a:b:c:[]) -> Coor (read a) (read b) (read c)
    other -> error $ "cannot make coor: " ++ show other

splitDelim :: (Eq a, Show a) => a -> [a] -> [[a]]
splitDelim _ [] = []
splitDelim delim str = left:(splitDelim delim right)
    where
        (left, rest) = break (==delim) str
        right = drop 1 rest

comparePairs :: Pair -> Pair -> Ordering
comparePairs = (\pair1 -> \pair2 -> compare (pairDistance pair1) (pairDistance pair2))

generatePairs :: [Coor] -> [Pair]
generatePairs coors = map (uncurry Pair) $ concat $ map rows $ zip [1..] coors
    where rows = (\(num, coor1) -> zip (drop num $ coors) $ repeat coor1)

pairDistance :: Pair -> Float
pairDistance (Pair (Coor x1 y1 z1) (Coor x2 y2 z2)) = sqrt $ fromInteger squared
    where squared = ( (x1 - x2)^(2::Integer) + (y1 - y2)^(2::Integer) + (z1 - z2)^(2::Integer) )

findNClosest :: Int -> [Pair] -> [Pair]
findNClosest 0 _ = []
findNClosest remain pairs = take remain $ sortBy comparePairs pairs

makeCircuits :: [Pair] -> [[Coor]]
makeCircuits = _makeCircuits []

_makeCircuits :: [[Coor]] -> [Pair] -> [[Coor]]
_makeCircuits circuits [] = circuits
_makeCircuits circuits (pair:rest) = _makeCircuits next_circuits rest
    where next_circuits = addToCircuits circuits pair

addToCircuits :: [[Coor]] -> Pair -> [[Coor]]
addToCircuits circuits (Pair coor1 coor2) = case containing of 
    [] -> circuits ++ [[coor1, coor2]]
    a -> (nub $ coor1:coor2:concat a) : excised
    where
        contains = map (\circuit -> elem coor1 circuit || elem coor2 circuit) circuits
        containing = map snd $ filter fst $ zip contains circuits
        excised = map snd $ filter (not . fst) $ zip contains circuits

continueCircuits :: [Coor] -> Maybe Pair
continueCircuits coors = _continueCircuits coors []sorted_pairs
    where
        pairs = generatePairs coors
        sorted_pairs = findNClosest ((length coors)^(2::Integer)) pairs

_continueCircuits :: [Coor] -> [[Coor]] -> [Pair] -> Maybe Pair
_continueCircuits _ _ [] = Nothing
_continueCircuits coors_left circuits (pair:rest) = case (length next_coors_left, length circuits) of
    (0, 1) -> Just pair
    _ -> _continueCircuits next_coors_left next_circuits rest
    where
        next_circuits = addToCircuits circuits $ pair
        Pair coor1 coor2 = pair
        next_coors_left = filter (\coor -> coor /= coor1 && coor /= coor2) coors_left  ::[Coor]
