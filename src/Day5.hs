module Day5 ( runDay5, parseRangesIngred, fulfill, Range (Range), removeOverlaps, lengthOverlaps ) where

newtype Range = Range (Int, Int) deriving (Eq, Show)
newtype Ingred = Ingred Int

runDay5 :: IO ()
runDay5 = do
    text <- readFile "inputs/day5.txt"
    let (ranges, ingred) = parseRangesIngred text
    let fulfilled = map (fulfill ranges) ingred  :: [[Bool]]
    let answer = length $ filter id $ map (elem True) fulfilled
    putStrLn $ "Day 5 part 1: " ++ show answer

    let lengths = lengthOverlaps ranges
    putStrLn $ "Day 5 part 2: " ++ show lengths

parseRangesIngred :: String -> ([Range], [Ingred])
parseRangesIngred text = (ranges, ingred)
  where
    split = lines text
    (range_lines, second_part) = break (not . elem '-') split
    ingred_lines = drop 1 second_part
    ingred = map ( Ingred . read ) ingred_lines
    ranges = map ( Range . (\(a, b) -> (read a, read $ drop 1 b )) . break (=='-')) range_lines

fulfill :: [Range] -> Ingred -> [Bool]
fulfill ranges (Ingred a) = map (\(Range (lo, hi)) -> (lo<=a) && (a<=hi)) ranges

removeOverlaps :: [Range] -> [Range]
removeOverlaps [] = []
removeOverlaps (a:b) = chiselOut a b ++ removeOverlaps b

chiselOut :: Range -> [Range] -> [Range]
chiselOut a [] = [a]
chiselOut a (b:c) = case overlap a b of
    Nothing -> chiselOut a c
    Just _ -> concat $ map (\piece -> chiselOut piece c) $ getFragments a b

overlap :: Range -> Range -> Maybe Range
overlap (Range (a1, a2)) (Range (b1, b2)) = case (b1>a2 || a1>b2) of
    True -> Nothing
    False -> Just (Range (max a1 b1, min a2 b2))

getFragments :: Range -> Range -> [Range]
getFragments range1 range2 = case overlap range1 range2 of
    Nothing -> []
    Just (Range (b1, b2)) -> case (a1<b1, a2<=b2) of
        (True, True) -> [Range (a1, b1 - 1)]
        (True, False) -> [Range (a1, b1-1), Range (b2+1, a2)]
        (False, True) -> []
        (False, False) -> [Range (b2+1, a2)]
    where Range (a1, a2) = range1

lengthOverlaps :: [Range] -> Int
lengthOverlaps = sum . map (\(Range (a, b)) -> b-a+1) . removeOverlaps
