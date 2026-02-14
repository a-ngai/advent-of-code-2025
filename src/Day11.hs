module Day11 ( runDay11 ) where

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
memoPathNum prenodes start end = sum $ map snd $ filter ((==(start, end)) . fst) memoi
    where
        prenode_names = map getInput prenodes
        keys = concat $ map (zipWith (,) prenode_names . repeat) prenode_names :: [(String, String)]

        memoi :: [((String, String), Int)]
        memoi = map (\key -> (key, makeMemoiEntry key)) keys

        makeMemoiEntry :: (String, String) -> Int
        makeMemoiEntry (start', end')
            | start' == end' = 1
            | otherwise = sum $ map (\str -> sum $ map snd $ filter ((==(str, end')) . fst) memoi) next
            where
                next = concat $ map getOutput $ filter ((==start') . getInput) prenodes
