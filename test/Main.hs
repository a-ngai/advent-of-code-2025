module Main (main) where

import Day1 ( getPassword, getClicks,)
import Day2 ( getInvalidIds, getMultiInvalidIds )
import Day3 ( makeCells, getMaxPower )
import Day4 ( isAvailable, recurseAvailable )
import Day5 ( parseRangesIngred, fulfill, Range (Range), removeOverlaps, lengthOverlaps )
import Day6 ( makeTable, makeQuestion, evaluateQuestion, makeCephTable, Question (Add, Mul) )

import Test.HUnit (assertEqual, runTestTT, Test (TestList), Test (TestLabel), Test (TestCase) )
import Test.HUnit (Counts)

test_day1_part1 :: IO ()
test_day1_part1 = do
    text <- readFile  "./test_inputs/day1.txt"
    let password = getPassword $ lines text
    assertEqual "should be 3" password 3 

test_day1_part2 :: IO ()
test_day1_part2 = do
    text <- readFile  "./test_inputs/day1.txt"
    let clicks = getClicks $ lines text
    assertEqual "should be 6" clicks 6

test_day2_part1 :: IO ()
test_day2_part1 = do
    text <- readFile  "./test_inputs/day2.txt"
    let invalid_ids = getInvalidIds text
    let answer = sum invalid_ids
    assertEqual "should be 1227775554" 1227775554 answer 

test_day2_part2 :: IO ()
test_day2_part2 = do
    text <- readFile  "./test_inputs/day2.txt"
    let invalid_ids = getMultiInvalidIds text
    let answer = sum invalid_ids
    assertEqual "should be 4174379265" 4174379265 answer 

test_day3_part1 :: IO ()
test_day3_part1 = do
    text <- readFile  "./test_inputs/day3.txt"
    let cells = map makeCells $ lines text
    let max_power = map (getMaxPower 2) cells
    assertEqual "" [98, 89, 78, 92] max_power 

test_day3_part2 :: IO ()
test_day3_part2 = do
    text <- readFile  "./test_inputs/day3.txt"
    let cells = map makeCells $ lines text
    let max_power = map (getMaxPower 12) cells
    assertEqual "" [987654321111, 811111111119, 434234234278, 888911112111] max_power 

test_day4_part1 :: IO ()
test_day4_part1 = do
    text <- readFile "test_inputs/day4.txt"
    let map_text = lines text
    let answer = length $ filter id $ (concat $ isAvailable map_text)
    assertEqual "" 13 answer 

test_day4_part2 :: IO ()
test_day4_part2 = do
    text <- readFile "test_inputs/day4.txt"
    let map_text = lines text
    let answer = recurseAvailable map_text
    assertEqual "" 43 answer 

test_day5_part1 :: IO ()
test_day5_part1 = do
    text <- readFile "test_inputs/day5.txt"
    let (ranges, ingred) = parseRangesIngred text
    let fulfilled = map (fulfill ranges) ingred  :: [[Bool]]
    let answer = length $ filter id $ map (elem True) fulfilled
    assertEqual "" 3 answer 

test_day5_part2 :: IO ()
test_day5_part2 = do
    text <- readFile "test_inputs/day5.txt"
    let (ranges, _) = parseRangesIngred text
    let combined = removeOverlaps ranges :: [Range]
    assertEqual "" [Range (3, 5), Range (10, 11), Range (19, 20), Range (12, 18)] combined 
    let lengths = lengthOverlaps ranges
    assertEqual "" 14 lengths 

test_day6_part1 :: IO ()
test_day6_part1 = do
    text <- readFile "test_inputs/day6.txt"
    let table = makeTable text
    let questions = map makeQuestion table
    let answer = sum $ map evaluateQuestion questions
    assertEqual "" 4277556 answer 

test_day6_part2 :: IO ()
test_day6_part2 = do
    text <- readFile "test_inputs/day6.txt"
    let questions = makeCephTable text
    assertEqual "" [Mul [1, 24, 356], Add [369, 248, 8], Mul [32, 581, 175], Add [623, 431, 4]]  questions 
    let ceph_answer = sum $ map evaluateQuestion questions
    assertEqual "" 3263827 ceph_answer 

main :: IO Counts
main = do
  runTestTT tests
  where
    tests = TestList [
      TestLabel "test day1 part1" (TestCase test_day1_part1),
      TestLabel "test day1 part2" (TestCase test_day1_part2),

      TestLabel "test day2 part1" (TestCase test_day2_part1),
      TestLabel "test day2 part2" (TestCase test_day2_part2),

      TestLabel "test day3 part1" (TestCase test_day3_part1),
      TestLabel "test day3 part2" (TestCase test_day3_part2),

      TestLabel "test day4 part1" (TestCase test_day4_part1),
      TestLabel "test day4 part2" (TestCase test_day4_part2),

      TestLabel "test day5 part1" (TestCase test_day5_part1),
      TestLabel "test day5 part2" (TestCase test_day5_part2),

      TestLabel "test day6 part1" (TestCase test_day6_part1),
      TestLabel "test day6 part2" (TestCase test_day6_part2)
      ]
