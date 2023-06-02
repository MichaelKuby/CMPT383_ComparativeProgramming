import System.Random

-- generate n random integers
randInts :: Int -> Int -> Int -> IO [Int]
randInts n minval maxval = do
    gen <- newStdGen
    return $ take n $ randomRs (minval, maxval) gen


-- convert a list of values into a histogram
histogram :: (Enum a, Eq a, Ord a) => [a] -> [String]
histogram vals = bars
    where
    counts = [length $ filter (==i) vals
             | i <- [(minimum vals)..(maximum vals)]]
    bars = [take n $ repeat 'X' | n <- counts]


-- print histogram of randomly-generated values
printHisto :: IO ()
printHisto = do
    vals <- randInts 1000 1 20
    let bars = histogram vals
    mapM_ putStrLn bars
