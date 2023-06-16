pascal :: Int -> [Int]
pascal 0 = [1]
pascal 1 = [1, 1]
pascal n = 
    let prev = pascal (n - 1)
    in [1] ++ map (\(x,y) -> x+y) (zip prev (tail prev)) ++[1]

addPair :: Integral a => (a, a) -> a
addPair = (uncurry (+))

withoutZeros :: (Eq a, Num a) => [a] -> [a]
withoutZeros = filter removeZero
    where   removeZero :: (Eq a, Num a) => a -> Bool
            removeZero n = case n of
                0 -> False
                otherwise -> True

findElt :: (Ord a) => a -> [a] -> Maybe Int
findElt _ [] = Nothing -- Base case: element not found
findElt elem (x:xs)
    | elem == x = Just 0 -- Found the element at the first index
    | otherwise = case (findElt elem xs) of -- recurse on the tail
        Nothing -> Nothing -- If we get to the base case.
        Just n -> Just (n + 1) -- If found, increment as we unwind