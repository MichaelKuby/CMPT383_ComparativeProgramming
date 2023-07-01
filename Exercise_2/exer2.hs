divisors :: Int -> [Int]
divisors n = [i | i <- [2..(n `div` 2)], n `mod` i == 0]

primes :: Int -> [Int]
primes n = [i | i <- [2..n], divisors i == []]

pythagorean' :: (Int, Int, Int) -> Bool
pythagorean' (a, b, c) = (a^2 + b^2 == c^2) == True

pythagorean :: Int -> [(Int, Int, Int)]
pythagorean n = [ (a, b, c) | c <- [1..n], b <- [1..c-1], a <- [1..b-1], pythagorean' (a, b, c)]

join :: String -> [String] -> String
join _ [] = []
join _ [x] = x
join sep (x:xs) = x ++ sep ++ join sep xs

reversejoin :: String -> [String] -> String
--reversejoin s = (join s) . reverse
reversejoin s string = join s (reverse string)

fact' :: (Integral a) => a -> a
fact' n = foldl (*) 1 [1..n]

hailstone :: Integral a => a -> a
hailstone n
    | even n = div n 2
    | odd n = 3 * n + 1

hailLen :: (Integral t1, Num t2) => t1 -> t2
hailLen n = hailTail 0 n
  where
    hailTail count 1 = count
    hailTail count n = hailTail (count + 1) (hailstone n)