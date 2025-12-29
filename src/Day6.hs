module Day6 ( runDay6, makeTable, evalQuestion, makeQuestions, makeCeph, Question (Add, Mul) ) where

import Data.List ( transpose )
import Data.Bifunctor ( second )

data Question = Add [Int]
              | Mul [Int] deriving (Eq, Show)

runDay6 :: IO ()
runDay6 = do
    text <- readFile "inputs/day6.txt"
    let table = makeTable text
    let questions = makeQuestions table
    putStrLn $ "Day 6 Part 1: " ++ (show $ sum $ map evalQuestion questions)
    let ceph_questions = makeCeph table
    putStrLn $ "Day 6 Part 2: " ++ (show $ sum $ map evalQuestion ceph_questions)

makeTable :: String -> [(String, [String])]
makeTable text = zip operators columns
  where
    list = lines text
    op_row = case (take 1 . reverse) list of
        [] -> error "no last row!"
        a:_ -> a
    text_rows = (reverse . drop 1 . reverse) list
    op_pos = charPos op_row ++ [1 + length op_row]
    col_lengths = map (uncurry (-)) $ zip (drop 1 op_pos) op_pos 
    columns = getColumns col_lengths text_rows
    operators = words op_row

makeQuestions :: [(String, [String])] -> [Question]
makeQuestions = map (uncurry columnToQuestion)

makeCeph :: [(String, [String])] -> [Question]
makeCeph = makeQuestions . map (second transpose)

evalQuestion :: Question -> Int
evalQuestion (Mul list) = product list
evalQuestion (Add list) = sum list

charPos :: String -> [Int]
charPos = map fst . filter ((/=' ') . snd) . zip [0..]

getColumns :: [Int] -> [String] -> [[String]]
getColumns [] _ = []
getColumns (a:b) text = (map (take (a-1)) text) : (getColumns b (map (drop a) text) )

columnToQuestion :: String -> [String] -> Question
columnToQuestion "*" = Mul . map read
columnToQuestion "+" = Add . map read
columnToQuestion rest = error $ "symbol not recognized: " ++ rest
