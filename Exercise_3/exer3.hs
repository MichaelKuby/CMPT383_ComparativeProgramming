import Data.Time.Calendar ( Day, fromGregorian, toGregorian )
import Data.Time.Calendar.OrdinalDate ( Day, mondayStartWeek )

merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge xs [] = xs
merge [] ys = ys
merge (x:xs)(y:ys)
    | x <= y = x : merge xs (y:ys)
    | x >  y = y : merge (x:xs) ys

splitInTwo :: [a] -> ([a], [a])
splitInTwo xs = splitAt (length xs `div` 2) xs

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs =
    let (leftSide, rightSide) = splitInTwo xs
    in merge (mergeSort leftSide) (mergeSort rightSide)

daysInYear :: Integer -> [Day]
daysInYear y = [jan1..dec31]
    where   jan1    = fromGregorian y 1 1
            dec31   = fromGregorian y 12 31

isFriday :: Day -> Bool
isFriday day
    | snd (mondayStartWeek day) == 5    = True
    | otherwise                         = False

divisors :: Int -> [Int]
divisors n = [i | i <- [2..(n `div` 2)], n `mod` i == 0]

isPrime :: Int -> Bool
isPrime n
    | n == 1            = False
    | null (divisors n)  = True
    | otherwise         = False

isPrimeDay :: Day -> Bool
isPrimeDay day = isPrime d
    where (_, _, d) = toGregorian day

primeFridays :: Integer -> [Day]
primeFridays year = filter primeFridays' (daysInYear year)
    where
        primeFridays' :: Day -> Bool
        primeFridays' day = isFriday day && isPrimeDay day