module Day11 ( runDay11 ) where

import qualified Data.Map as Map

data PreNode = PreNode { getInput::String, getOutput::[String] } deriving Show

runDay11 :: IO ()
runDay11 = do

    -- text1 <- readFile "test_inputs/day11_part1.txt"
    -- text2 <- readFile "test_inputs/day11_part2.txt"

    text <- readFile "inputs/day11.txt"
    let text1 = text
        text2 = text

    let prenodes1 = makePreNodes text1
        num_paths = memoPathNum prenodes1 "you" "out"

    putStrLn $ "Day 11 Part 1: " ++ (show num_paths)

    let prenodes2 = makePreNodes text2
        memoPath2 = memoPathNum prenodes2
        num_svr_to_fft = memoPath2 "svr" "fft"
        num_fft_to_dac = memoPath2 "fft" "dac"
        num_dac_to_out = memoPath2 "dac" "out"
        num_svr_to_dac = memoPath2 "svr" "dac"
        num_dac_to_fft = memoPath2 "dac" "fft"  -- is going to be zero
        num_fft_to_out = memoPath2 "fft" "out"
        num_passthrough = (
            (num_svr_to_fft * num_fft_to_dac * num_dac_to_out)
            + (num_svr_to_dac * num_dac_to_fft * num_fft_to_out)
            )

    putStrLn $ "Day 11 Part 2: " ++ (show num_passthrough)

makePreNodes :: String -> [PreNode]
makePreNodes text = (PreNode "out" []) : (map parseLines $ lines text)

parseLines :: String -> PreNode
parseLines str = PreNode input outputs
    where
        (input, output_str) = break (==':') str
        outputs = words $ drop 1 output_str

memoPathNum :: [PreNode] -> String -> String -> Int
memoPathNum prenodes start end = case Map.lookup (start, end) memoi_pathnum of
    Just val -> val
    Nothing -> 0
    where
        prenode_names = map getInput prenodes
        pair_keys = concat $ map (zipWith (,) prenode_names . repeat) prenode_names :: [(String, String)]

        memoi_pathnum :: Map.Map (String, String) Int
        memoi_pathnum = Map.fromList $ map (\key -> (key, makeMemoiEntry memoi_pathnum key)) pair_keys

        child_map :: Map.Map String [String]
        child_map = Map.fromList $ map (\(PreNode parent child) -> (parent, child)) prenodes

        makeMemoiEntry :: Map.Map (String, String) Int -> (String, String) -> Int
        makeMemoiEntry dict (start', end')
            | start' == end' = 1
            | otherwise = case fmap sum $ sequence $ map (flip Map.lookup dict) pairs of
                Just val -> val
                Nothing -> error "makeMemoiEntry does not work!"
            where
                next = case Map.lookup start' child_map of
                    Just list -> list
                    Nothing -> error "cannot find single entry!"
                pairs = map (, end') next :: [(String, String)]
