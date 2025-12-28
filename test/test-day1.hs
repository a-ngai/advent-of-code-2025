module test_day1 (main) where

import Day1 (
  getPassword, getClicks,
  )
import Data.List (intercalate)
import Test.HUnit (assertEqual, runTestTT, Test (TestList), Test (TestLabel), Test (TestCase))
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


main :: IO Counts
main = do
  runTestTT tests
  where
    tests = TestList [
      TestLabel "test day1 part1" (TestCase test_day1_part1), TestLabel "test day1 part2" (TestCase test_day1_part2),
      ]

