module Day10 ( runDay10 ) where

import Day8 ( splitDelim )
import Data.Ratio ( (%), denominator )
import Data.List ( transpose, intercalate, isPrefixOf )
import Debug.Trace ( trace, traceWith )

data Problem = Problem Indicator [Button] Joltage deriving (Show, Eq)

type Button = [Integer]
type Press = Int
type Indicator = [Bool]
type Joltage = [Integer]

runDay10 :: IO ()
runDay10 = do
    -- text <- readFile "test_inputs/day10.txt"
    text <- readFile "inputs/day10.txt"

    let problems = map makeProblem $ lines text
        result_part1 = (Just . sum ) =<< (sequence $ map fewestPressIndicator $ problems) :: Maybe Int
    putStrLn $ "Day 10 Part 1: " ++ show result_part1

    -- -- testing area
    -- let first_problem = problems !! 11
    let first_problem = problems !! 16
        first_matrix = (map.map) (\x -> x % 1) $ makeJoltageProblem first_problem
        reduced_rational_form = gaussElim first_matrix
        reduced_integer_form = makeIntegerAugment reduced_rational_form
        collapsed_form = foldl' (\b -> \a -> map (uncurry (+)) $ zip a b) (repeat 0) reduced_integer_form
        free_indices = getFreeIndicesAugment reduced_integer_form

    putStrLn $ "Very slow part 2!"
    putStrLn $ show $ first_problem
    putStrLn "\n"
    putStrLn "Problem statement:"
    putStrLn $ prettyMatrixAugment $ ((map.map) floor $ first_matrix :: [[Int]])
    putStrLn "\n"
    putStrLn "Reduced integer form:"
    putStrLn $ prettyMatrixAugment $ reduced_integer_form
    putStrLn "\n"
    putStrLn "Collapsed form:"
    putStrLn $ prettyMatrixAugment $ [collapsed_form]
    putStrLn "\n"
    putStrLn "Free indices:"
    putStrLn $ show $ free_indices
    let result = depthSearchJoltage first_problem reduced_integer_form free_indices
    putStrLn $ "depth search result: " ++ (show result)

    let result_part2 = (Just . sum ) =<< (sequence $ map (traceWith (show) . fewestPressJoltage) $ problems) :: Maybe Int
    -- let result_part2 = (Just . sum ) =<< (sequence $ map fewestPressJoltage $ problems) :: Maybe Int
    putStrLn $ "Day 10 Part 2: " ++ show result_part2


--- part 1

evalIndicator :: Indicator -> Joltage -> Bool
-- evalIndicator indicator = all id . map (\(a, b) -> if a then (odd b) else (even b)) . zip indicator
evalIndicator indicator = all id . zipWith (\a -> \b -> a == odd b) indicator

updateJoltage :: Joltage -> Button -> Joltage
updateJoltage joltage list = foldl' func joltage list
    where func joltage' loc = zipWith (\num -> \index -> if loc==index then num+1 else num) joltage' [0..]

-- depth-first pruning algorithm
fewestPressIndicator :: Problem -> Maybe Record
fewestPressIndicator (Problem target buttons _) = fewestPressIndicator' (target, buttons) [initial_node] Nothing
    where initial_node = (take (length target) $ repeat 0, buttons, 0) :: Node

type Node = (Joltage, [Button], Press) -- parent node; child nodes made with [Button]
type Record = Int
type IndicatorGlobal = (Indicator, [Button])
fewestPressIndicator' :: IndicatorGlobal -> [Node] -> Maybe Record -> Maybe Record
fewestPressIndicator' _ [] record = record
fewestPressIndicator' (target, all_buttons) _stack@(node:rest) maybe_record = case pruneIndicatorNode (target, maybe_record, all_buttons) node of
    True -> fewestPressIndicator' (target, all_buttons) rest maybe_record
    False -> fewestPressIndicator' (target, all_buttons) (new_nodes ++ rest) new_record
    where
        (joltage, buttons, presses) = node 
        (new_nodes, new_record) = case evalIndicator target joltage of
            True -> ([], maybe (Just presses) (Just . min presses) maybe_record)
            False -> (child_nodes, maybe_record)
        makeChildNodes :: (Button, [Button]) -> Node
        makeChildNodes (button, cut_buttons) = (updateJoltage joltage button, cut_buttons, presses+1)
        child_nodes = map makeChildNodes $ zip buttons $ scanr (:) [] buttons :: [Node]

pruneIndicatorNode :: (Indicator, Maybe Record, [Button]) -> Node -> Bool
pruneIndicatorNode (_, maybe_record, all_buttons) _node@(_, _, presses') =
    presses_exceed_record
    || presses_exceed_limit
    where
        presses_exceed_record = case maybe_record of
            Nothing -> False
            Just record -> presses' > record
        presses_exceed_limit = presses' > (length all_buttons)

--- part 2

giveIntegerMatrix :: (Eq a, RealFrac a) => [[a]] -> Maybe [[Integer]]
giveIntegerMatrix matrix = sequence $ map sequence $ (map.map) (\x -> if fromIntegral (truncate x) == x then Just (floor x) else Nothing) matrix


getFreeIndicesAugment :: (Num a, Eq a) => [[a]] -> [Int]
getFreeIndicesAugment [] = []
getFreeIndicesAugment matrix@(first:_) = concat $ zipWith (\lo -> \hi -> [(lo+1)..(hi-1)]) first_non_zero (drop 1 first_non_zero)
    where
        first_non_zero = (filter (< len) $ map (length . takeWhile (==0)) matrix) ++ [len-1]
        len = length first

prettyMatrix :: (Eq a, Show a) => [[a]] -> String
prettyMatrix matrix = intercalate "\n" $ map (intercalate ", " . uncurry adaptPadding) $ zip (repeat lengths) strings
    where
        strings = (map.map) show matrix
        lengths = getMaxColLengths strings

prettyMatrixAugment :: (Eq a, Show a) => [[a]] -> String
prettyMatrixAugment = intercalate "\n" . map replaceWithPipe . splitStr "\n" . prettyMatrix
    where replaceWithPipe = (\(back_r, front_r) -> (reverse (drop 1 front_r)) ++ "|" ++ (reverse back_r)) . break (==',') . reverse

splitStr :: String -> String -> [String]
splitStr _ "" = []
splitStr delim str = substr : (splitStr delim next_str)
    where
        len = length $ takeWhile (not . isPrefixOf delim . flip drop str) [0..(length str - 1)]
        (substr, rest) = splitAt len str
        (_, next_str) = splitAt (length delim) rest

getMaxColLengths :: [[String]] -> [Int]
getMaxColLengths = foldl' (\b -> \a -> map (uncurry max) $ zip b (map length a)) (repeat 0)

adaptPadding :: [Int] -> [String] -> [String]
adaptPadding lens strs = zipWith (\num -> \str -> (take (num - length str) $ repeat ' ') ++ str) lens strs

makeProblem :: String -> Problem
makeProblem string = Problem indicator buttons joltage
    where
        indicator = case words string of
            [] -> error "nothing in string!"
            cs:_ -> map (=='#') $ take_middle cs
        joltage = case reverse $ words string of
            [] -> error "nothing in string!"
            cs:_ -> make_tuple cs
        buttons = map make_tuple $ take_middle $ words string
        make_tuple :: String -> [Integer]
        make_tuple = map read . splitDelim ',' . take_middle
        take_middle :: [a] -> [a]
        take_middle = drop 1 . reverse . drop 1 . reverse

makeJoltageProblem :: Problem -> [[Integer]]
makeJoltageProblem (Problem _ buttons joltages) = zipWith (++) main_matrix augment_matrix
    where
         main_matrix = transpose $ map buttonToVec buttons :: [[Integer]]
         augment_matrix = map ( :[] ) joltages :: [[Integer]]
         len = toInteger (length joltages - 1) :: Integer
         buttonToVec :: [Integer] -> [Integer]
         buttonToVec button = map (\index -> if elem index button then 1 else 0) [0..len]

fewestPressJoltage :: Problem -> Maybe Int
fewestPressJoltage problem = case all (==1) $ drop 1 $ reverse collapsed_form of
    True -> Just $ fromInteger $ sum $ take 1 $ reverse collapsed_form
    False -> depthSearchJoltage problem reduced_integer_form free_indices
    where
        reduced_integer_form = makeIntegerAugment $ gaussElim $ (map.map) (\x -> x % 1) $ makeJoltageProblem problem
        collapsed_form = foldl' (zipWith (+)) (repeat 0) reduced_integer_form
        free_indices = getFreeIndicesAugment reduced_integer_form

depthSearchJoltage :: Problem -> [[Integer]] -> [Int] -> Maybe Int
depthSearchJoltage problem augmatrix free_indices = depthSearchJoltage' (augmatrix, free_indices) stack Nothing
    where
        (Problem _ buttons joltages) = problem
        max_values = traceWith (show) $ map (fromInteger . minimum) $ map (\index -> takeList (map fromInteger $ concat $ take 1 $ drop index buttons) joltages) free_indices :: [Int]
        values = map (\bound -> [0..bound]) max_values :: [[Int]]
        stack = traceWith (show . length) $ map (zip free_indices) $ combinations values

        -- makeStack :: [Int] -> [Int] -> [[Int]]
        -- makeStack free_indices current = case valid of
        --     True -> concat $ map (\(list, indices) -> makeStack indices $ zipWith (+) list current) $ zip next_indices next_lists
        --     False -> []
        --     where
        --         valid = zip current $ concat $ map (\index -> take 1 $ drop index buttons) free_indices
        --         next_indices = scanl (\acc -> \new -> acc++[new] ) [] $ reverse free_indices  :: [[Int]]
        --         next_lists = map (\index' -> zipWith (\(loc', index') -> if loc'==index' then 1 else 0) [0..] $ take (length free_indices) $ repeat 0) [0..(length free_indices-1)]

depthSearchJoltage' :: ([[Integer]], [Int]) -> [[(Int, Int)]] -> Maybe Int -> Maybe Int
depthSearchJoltage' _ [] record = record
depthSearchJoltage' state@(augmatrix, _free_indices) (pop:stack) record = depthSearchJoltage' state stack new_record
    where
        new_record = case (record, result) of
            (Nothing, val) -> val
            (val, Nothing) -> val
            (Just val1, Just val2) -> Just $ min val1 val2
        result = fmap fromInteger $ checkSolution augmatrix pop

takeList :: [Int] -> [a] -> [a]
takeList indices list = foldl' (\acc -> \index -> acc ++ (take 1 $ drop index list)) [] indices

combinations :: [[Int]] -> [[Int]]
combinations [] = []
combinations (a:[]) = map (:[]) a
combinations (a:b:rest) = concat $ map (\x -> map (x:) next_combinations) a
    where next_combinations = combinations (b:rest) :: [[Int]]

checkSolution :: [[Integer]] -> [(Int, Int)] -> Maybe Integer
checkSolution matrix free_var = case is_diag of
    True -> sum_result
    -- False -> error $ "somethin wrong!" ++ "\n" ++ (prettyMatrixAugment $ reduced_integer_form)
    False -> Nothing
    where
        row_len = sum $ take 1 $ map length matrix
        expandMatrix :: [[Integer]] -> [(Int, Int)] -> [[Integer]]
        expandMatrix matrix' [] = matrix'
        expandMatrix matrix' ((loc, val):rest) = expandMatrix (below ++ [new_row] ++ above) new_rest
            where
                newNum :: Int -> Integer
                newNum index
                    | index==loc = 1
                    | index==row_len-1 = toInteger val
                    | otherwise = 0
                new_row = map newNum $ [0..(row_len-1)] :: [Integer]
                (below, above) = splitAt loc matrix'
                -- new_rest = map (\(a, b) -> (a+1, b)) rest
                new_rest = rest
        expanded = (map.map) toRational $ expandMatrix matrix free_var

        reduced_rational_form = upRightAugmentElim 0 expanded
        reduced_integer_form = makeIntegerAugment reduced_rational_form
        solution = concat $ map (take 1 . reverse) reduced_integer_form
        sum_result = case all (>=0) solution of
            True -> Just $ sum solution
            False -> Nothing
        is_diag = all (==1) $ foldl' (\b -> \a -> zipWith (+) a b) (repeat 0) (map (take (row_len - 1)) reduced_integer_form)

upRightAugmentElim :: (Fractional a, Eq a, Show a) => Int -> [[a]] -> [[a]]
upRightAugmentElim index matrix
    | index >= (sum $ take 1 $ map ((\x -> x - 1) . length) matrix) = matrix
    | all (==0) $ concat $ take 1 $ drop index matrix = matrix
    | otherwise = upRightAugmentElim (index + 1) next
    where
        select_row = concat $ take 1 $ drop index matrix
        select_lead = sum $ take 1 $ dropWhile (==0) select_row
        select_norm_row = map ( / select_lead) select_row
        col_index = sum $ map snd $ take 1 $ dropWhile ((==0) . fst) $ zip select_row [(0::Int)..]
        subtracted = map subtractRow matrix
            where
                subtractRow vec = zipWith (+) vec $ map (*((rowLead vec)*(-1))) select_norm_row
                rowLead = sum . take 1 . drop col_index
        next = take index subtracted ++ [select_norm_row] ++ drop (index+1) subtracted

botLeftElim :: (Fractional a, Eq a) => [[a]] -> [[a]]
botLeftElim [] = []
botLeftElim matrix@([]:_) = matrix
botLeftElim matrix@(x@(lead:_):xs)
    | all (==[0]) $ map (take 1) matrix = map ( 0: ) $ botLeftElim $ map (drop 1) matrix
    | lead == 0 = botLeftElim $ take (length matrix) $ drop 1 $ cycle matrix
    | otherwise = (take 1 eliminated) ++ (zipWith (++) heads $ botLeftElim next)
    where
        eliminated = x_norm : map norm xs
        x_norm = (map ( / lead) x)
        norm array = zipWith (-) (map ( (sum $ take 1 array)* ) x_norm ) array
        heads = map (take 1) $ drop 1 eliminated
        next = map (drop 1) $ drop 1 eliminated

gaussElim :: (Fractional a, Eq a, Show a) => [[a]] -> [[a]]
gaussElim = upRightAugmentElim 0 . botLeftElim

makeIntegerAugment :: [[Rational]] -> [[Integer]]
makeIntegerAugment [] = []
makeIntegerAugment (x:rest) = new_x : (makeIntegerAugment rest)
    where
        new_x = map (floor . (*lcm_val)) x
        lcmList :: [Integer] -> Integer
        lcmList [] = 1
        lcmList (a:[]) = a
        lcmList (a:b:c) = lcmList $ (lcm a b):c
        lcm_val = fromInteger $ lcmList $ map denominator x
