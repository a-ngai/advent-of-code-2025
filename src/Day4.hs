module Day4 ( runDay4, isAvailable, recurseAvailable ) where

runDay4 :: IO ()
runDay4 = do
    text <- readFile "inputs/day4.txt"
    let map_text = lines text
    putStrLn $ "Day 4 Part 1: " ++ (show $ length $ filter id $ concat $ isAvailable map_text)
    putStrLn $ "Day 4 Part 2: " ++ (show $ recurseAvailable map_text)

expandMap :: [String] -> [String]
expandMap [] = error "empty map!"
expandMap (hi:rest) = buffer ++ middle ++ buffer
  where
    buffer = [take (2 + length hi)  $ repeat '.']
    text = hi:rest
    middle = map (\str -> ['.'] ++ str ++ ['.']) text

makeSurround :: [String] -> [[String]]
makeSurround [] =  error "empty map!"
makeSurround (hi:rest) = (map . map) getSurround coor
  where
    ny = length hi
    text = hi:rest
    nx = length text
    coor :: [[(Int, Int)]]
    coor = map (\x -> map (\y -> (x, y)) [1..(ny-2)]) [1..(nx-2)]
    getSurroundCoor :: (Int, Int) -> [(Int, Int)]
    getSurroundCoor (x,y) = filter (\tup -> tup /= (x, y)) $ surroundingTiles (x,y)
    surroundingTiles (x,y) = concat $ map (\_x -> zip (repeat _x) [(y-1)..(y+1)]) [(x-1)..(x+1)]
    select :: [[a]] -> (Int, Int) -> [a]
    select array (x, y) = (take 1 . drop y . concat . take 1 . drop x) array
    getSurround :: (Int, Int) -> String
    getSurround (x, y) = concat $ map (select text) (getSurroundCoor (x,y))

hasSpace :: String -> Bool
hasSpace = (<4) . length . filter (=='@')

isAvailable :: [String] -> [[Bool]]
isAvailable [] = error "empty map!"
isAvailable (hi:rest) = (map . map) (\(a,b) -> a && b) $ (zipWith zip) isRoll surroundOpen
  where
    text = hi:rest
    larger = expandMap text
    isRoll = (map . map) (\x -> x == '@') text
    surroundOpen = (map . map) hasSpace $ (makeSurround larger)

_recurseAvailable :: Int -> [String] -> Int
_recurseAvailable _ [] = error "empty map!"
_recurseAvailable curr text = case removed of
    0 -> curr
    _ -> _recurseAvailable (curr + removed) new_text
    where
        remove_loc = isAvailable text
        removed = length $ filter id $ concat $ remove_loc
        not_roll = (map . map) (\x -> x /= '@') text
        zipped = (zipWith zip) not_roll remove_loc
        new_text = (map . map) (emptyChar . (\(a,b) -> a || b) ) zipped  :: [String]
        emptyChar bool = if bool then '.' else '@'

recurseAvailable :: [String] -> Int
recurseAvailable text = _recurseAvailable 0 text
