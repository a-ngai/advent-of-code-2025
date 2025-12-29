module Day6 ( runDay6, makeTable, makeQuestion, evaluateQuestion, makeCephTable, Question (Add, Mul) ) where

data Question = Add [Int]
              | Mul [Int] deriving (Eq, Show)

runDay6 :: IO ()
runDay6 = do
    text <- readFile "inputs/day6.txt"
    let table = makeTable text
    let questions = map makeQuestion table
    putStrLn $ "Day 6 Part 1: " ++ (show $ sum $ map evaluateQuestion questions)
    let ceph_questions = makeCephTable text
    putStrLn $ "Day 6 Part 2: " ++ (show $ sum $ map evaluateQuestion ceph_questions)

makeTable :: String -> [[String]]
makeTable list = map reverse $ _makeTable $ map words $ lines list

_makeTable :: [[a]] -> [[a]]
_makeTable ([]:_) = []
_makeTable list = items:(_makeTable next_list)
    where
        next_list = map (drop 1) list
        items = concat $ map (take 1) list

makeQuestion :: [String] -> Question
makeQuestion [] = error "cannot be empty!"
makeQuestion (symbol:b) = case symbol of
    "*" -> Mul array
    "+" -> Add array
    _ -> error $ "symbol not recognized: " ++ symbol
    where
        array = map read b

evaluateQuestion :: Question -> Int
evaluateQuestion (Mul list) = product list
evaluateQuestion (Add list) = sum list

charPos :: String -> [Int]
charPos text = map (\(num, _) -> num) $ filter (\(_, char) -> char /= ' ') $ zip [0..] text

getColumns :: [Int] -> [String] -> [[String]]
getColumns [] _ = []
getColumns (a:b) text = (map (take (a-1)) text) : (getColumns b (map (drop a) text) )

makeCephTable :: String -> [Question]
makeCephTable text = map columnToQuestion $ zip operators columns
  where
    list = lines text
    operator_row = case (take 1 . reverse) list of
        [] -> error "no last row!"
        a:_ -> a
    operators = words operator_row
    text_rows = reverse $ drop 1 $ reverse list
    column_lengths = map (\(a, b) -> b-a) $ zip (charPos operator_row) ((++[1 + length operator_row]) $ drop 1 $ charPos operator_row)
    columns = getColumns column_lengths text_rows

columnToQuestion :: (String, [String]) -> Question
columnToQuestion (symbol, col_text) = case symbol of
    "*" -> Mul array
    "+" -> Add array
    _ -> error $ "symbol not recognized: " ++ symbol
    where
        array = map (read) $ _makeTable col_text
