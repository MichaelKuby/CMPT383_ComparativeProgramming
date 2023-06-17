import Data.Ratio

myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate' f x
    where
        myIterate' :: (a -> a) -> a -> [a]
        myIterate' f y = 
            let next = f y 
            in next : myIterate' f next

mySplitAt :: Int -> [a] -> ([a], [a])
mySplitAt 0 xs = ([], xs)  -- Base case: no elems to take
mySplitAt _ [] = ([], [])  -- Base case: no elems in the original list
mySplitAt n (x:xs) =
  let (taken, remaining) = mySplitAt (n - 1) xs
  in (x : taken, remaining)

rationalSum :: Int -> [Ratio Int]
rationalSum n = [x % (n - x) | x <- [1..n-1]]

rationalSumLowest :: Int -> [Ratio Int]
rationalSumLowest n = [x % (n - x) | x <- [1..n-1], gcd x (n-x) == 1]

rationals :: [Ratio Int]
rationals = concat [ rationalSumLowest x | x <- [0..]] -- creates a list of lists -> concat them together

sumFile :: IO ()
sumFile = do
    contents <- readFileToList "input.txt"
    let ints = map readInt contents
        summation = sum ints
    print summation
    where 
        readFileToList :: FilePath -> IO [String]
        readFileToList filePath = do
            contents <- readFile filePath
            return (lines contents)

        readInt :: String -> Int
        readInt = read