-- Quick but ugly solution with many error paths. Some error paths are due to
-- the data structure for the boundary (i.e. [Line]); a invalid non-contiguous
-- boundary can be represented this way, hence the error paths. Other error
-- paths are from the assumption that the line segments denoting the boundary
-- do not double-back on themselves.
--
-- A better data structure for the boundary would be (Coor, [Displacement])
-- denoting a starting position and a list of displacements. The only
-- non-trivial check is that the the list of displacements sums to zero, which
-- is simple; this is still however a flaw of the data structure.

module Day9 ( runDay9 ) where

import qualified Data.Map as Map

data Coor = Coor Int Int deriving (Show, Eq, Ord)
data Line = LineX Coor Int 
          | LineY Coor Int deriving Show
data Rect = Rect [Line]

runTestDay9 :: IO ()
runTestDay9 = do
    text <- readFile "test_inputs/day9.txt"
    let barrier_coors = map stringToCoor $ lines text :: [Coor]
        barrier_rects = map (uncurry makeRect) $ concat $ map (\rect -> zip barrier_coors (repeat rect)) barrier_coors
        rect_areas = map rectArea barrier_rects

    let plot = take 15 $ repeat ( take 15 $ repeat '.')
        barrier = case linesFromList barrier_coors of
                Just a -> a
                Nothing -> error "cannot parse lines!"
        barrier_plot = updatePlot '#' plot $ concat $ map lineToCoor barrier
    putStrLn $ unlines barrier_plot

    putStrLn $ "test: maximum rectangle area of test input: " ++ (show $ maximum rect_areas)

    let exterior = makeExterior barrier
        exterior_plot = updatePlot 'o' barrier_plot $ concat $ map lineToCoor exterior
    putStrLn $ unlines exterior_plot

    let intersectsExterior :: Rect -> Bool
        intersectsExterior = rectIntersectsLines exterior
        interior_rects = filter intersectsExterior barrier_rects
        interior_areas = map rectArea interior_rects
    putStrLn $ "test: filter out rectangles intersecting with exterior; maximum area: " ++ (show $ maximum interior_areas)

runDay9 :: IO ()
runDay9 = do
    _ <- runTestDay9
    text <- readFile "inputs/day9.txt"
    let barrier_coors = map stringToCoor $ lines text :: [Coor]
        barrier_rects = map (uncurry makeRect) $ concat $ map (\rect -> zip barrier_coors (repeat rect)) barrier_coors
        rect_areas = map rectArea barrier_rects
    putStrLn $ "Day 9 part 2: " ++ (show $ maximum rect_areas)

    let barrier = case linesFromList barrier_coors of
                Just a -> a
                Nothing -> error "cannot parse lines!"
    let exterior = makeExterior barrier

    let intersectsExterior :: Rect -> Bool
        intersectsExterior = rectIntersectsLines exterior
        interior_rects = filter intersectsExterior barrier_rects
        interior_areas = map rectArea interior_rects
    putStrLn $ "Day 9 part 2: " ++ (show $ maximum interior_areas)

rectIntersectsLines :: [Line] -> Rect -> Bool
rectIntersectsLines ls (Rect rect_ls) = not $ True `elem` map (uncurry intersects) elements
    where elements = [(x, y) | x <- ls, y <- rect_ls]

intersects :: Line -> Line -> Bool
intersects (LineX (Coor x1 y1) x1len) (LineX (Coor x2 y2) x2len) = (y1==y2) && intersect1D x1 x1len x2 x2len
intersects (LineY (Coor x1 y1) y1len) (LineY (Coor x2 y2) y2len) = (x1==x2) && intersect1D y1 y1len y2 y2len
intersects (LineX (Coor x1 y1) x1len) (LineY (Coor x2 y2) y2len) = x_intersect && y_intersect
    where
        x_intersect = intersect1D x1 x1len x2 0
        y_intersect = intersect1D y1 0 y2 y2len
intersects (LineY coor2 len2) (LineX coor1 len1) = intersects (LineX coor1 len1) (LineY coor2 len2)

intersect1D :: Int -> Int -> Int -> Int -> Bool
intersect1D x1 len1 x2 len2 = a2 >= b1
    where
        (_a1, a2, b1, _b2) = case x1 <= x2 of
            True -> (x1, x1+len1, x2, x2+len2)
            False -> (x2, x2+len2, x1, x1+len1)

makeExterior :: [Line] -> [Line]
makeExterior (l1:l2:rest) = interior
    where
        ls = l1:l2:rest ++ [l1, l2]
        l0 = case reverse rest of
            a:_ -> a
            [] -> error "list has too few elements!"
        (point1, point2) = findCornerPoints l0 l1
        interior1 = (makeBoundary point1 ls)
        interior2 = (makeBoundary point2 ls)
        interior = case (sum $ map getLen interior1) < (sum $ map getLen interior2) of
            True -> interior2
            False -> interior1
makeExterior _ = error "too small of a list!"

findCornerPoints :: Line -> Line -> (Coor, Coor)
findCornerPoints line1@(LineX _ _) line2@(LineX _ _) = error $ "findCornerPoints is doubling back!" ++ show (line1, line2)
findCornerPoints line1@(LineY _ _) line2@(LineY _ _) = error $ "findCornerPoints is doubling back!" ++ show (line1, line2)
findCornerPoints (LineX (Coor x1 y1) x1len) (LineY (Coor x2 y2) _y2len) 
        | x1+x1len==x2 && y1==y2 = (Coor (x1+x1len-1) (y1+1), Coor (x1+x1len+1) (y1-1))   -- _|
        | x1+x1len==x2 && y1>y2  = (Coor (x1+x1len-1) (y1-1), Coor (x1+x1len+1) (y1+1))   -- -|
        | x1==x2       && y1==y2 = (Coor (x1-1) (y1-1), Coor (x1+1) (y1+1))               -- |_
        | x1==x2       && y1>y2  = (Coor (x1-1) (y1+1), Coor (x1+1) (y1-1))               -- |-
        | otherwise = error "is not a corner!"
findCornerPoints line_y@(LineY _ _) line_x@(LineX _ _) = findCornerPoints line_x line_y

makeBoundary :: Coor -> [Line] -> [Line]
makeBoundary coor (l1:l2:rest) = new_line : makeBoundary new_coor (l2:rest)
    where (new_coor, new_line) = followLine coor l1 l2
makeBoundary _ _ = []

followLine :: Coor -> Line -> Line -> (Coor, Line)
followLine coor line1 line2  = case (line1, line2) of
    ((LineX _ _), LineX _ _) -> error "followLine is doubling back!"
    ((LineY _ _), LineY _ _) -> error "followLine is doubling back!"
    ((LineX _ _), LineY _ _) -> (new_coor, new_line)
        where
            new_coor 
                | y==y1' = coor1'
                | y==y2' = coor2'
                | otherwise = error $ "no match for followLine; LineX + LineY"
            new_line = case makeLine (coor, new_coor) of
                    Just a -> a
                    Nothing -> error "cannot make line!"
    ((LineY _ _), LineX _ _) -> (new_coor, new_line)
        where
            new_coor 
                | x==x1' = coor1'
                | x==x2' = coor2'
                | otherwise = error $ "no match for followLine!; LineY + LineX"
            new_line = case makeLine (coor, new_coor) of
                    Just a -> a
                    Nothing -> error "cannot make line!"
    where
        Coor x y = coor
        (coor1'@(Coor x1' y1'), coor2'@(Coor x2' y2')) = findCornerPoints line1 line2

stringToCoor :: String -> Coor
stringToCoor string = Coor (read front) (read back)
    where
        (front, back') = break (==',') string
        back = drop 1 back'

makeRect :: Coor -> Coor -> Rect
makeRect (Coor x1 y1) (Coor x2 y2) = Rect [line1, line2, line3, line4]
    where
        (xlo, xhi) = (min x1 x2, max x1 x2)
        (ylo, yhi) = (min y1 y2, max y1 y2)
        line1 = LineX (Coor xlo ylo) (xhi - xlo)
        line2 = LineY (Coor xhi ylo) (yhi - ylo)
        line3 = LineX (Coor xlo yhi) (xhi - xlo)
        line4 = LineY (Coor xlo ylo) (yhi - ylo)

getLen :: Line -> Int
getLen (LineX (Coor _ _) len) = len
getLen (LineY (Coor _ _) len) = len

getX :: Line -> Int
getX (LineX (Coor x _) _) = x
getX (LineY (Coor x _) _) = x

getY :: Line -> Int
getY (LineX (Coor _ y) _) = y
getY (LineY (Coor _ y) _) = y

rectArea :: Rect -> Int
rectArea (Rect coors) = (1 + (xhi - xlo)) * (1 + (yhi - ylo))
    where
        xhi = maximum $ map getX coors
        xlo = minimum $ map getX coors
        yhi = maximum $ map getY coors
        ylo = minimum $ map getY coors

linesFromList :: [Coor] -> Maybe [Line]
linesFromList coors = sequence $ map makeLine $ zip coors (drop 1 coors ++ (take 1 coors))

makeLine :: (Coor, Coor) -> Maybe Line
makeLine ((Coor x1 y1), (Coor x2 y2)) = case (x1==x2, y1==y2) of
    (False, False) -> Nothing
    (True, _) -> Just $ LineY (Coor xlo ylo) (yhi-ylo)
    (_, True) -> Just $ LineX (Coor xlo ylo) (xhi-xlo)
    where 
        (xlo, xhi) = (min x1 x2, max x1 x2)
        (ylo, yhi) = (min y1 y2, max y1 y2)

-- Plotting functions

lineToCoor :: Line -> [Coor]
lineToCoor (LineX (Coor x y) len) = map (\i -> Coor (x+i) y) [0..len]
lineToCoor (LineY (Coor x y) len) = map (\i -> Coor x (y+i)) [0..len]

updatePlot :: Char -> [String] -> [Coor] -> [String]
updatePlot char plot coors = (map.map) newChar plot_coors
    where
        map_coor = Map.fromList $ zip coors $ repeat char
        plot_coors = (zipWith zip) plot _coors :: [[(Char, Coor)]]
        _coors = map (\(x, line) -> map (Coor x) $ [0..(length line)]) $ zip [0..] plot
        newChar (_char, coor) = maybe _char id $ (Map.lookup) coor map_coor
