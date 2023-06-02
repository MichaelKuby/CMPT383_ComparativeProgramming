lst :: [Int]
lst = [1,2,3]

val :: Int
val = 6

flip_args :: (a -> b -> c) -> b -> a -> c
flip_args f x y = f y x

areInOrder :: (Ord a) => a -> a -> a -> Bool
areInOrder x y z = (x <= y) && (y <= z)

divide :: Int -> Int -> Int
divide x y = div y x
divide2 = divide 2

join :: String -> [String] -> String
join _ [ ] = [ ]
join _ [x] = x
join sep (x:xs) = x ++ sep ++ join sep xs

commajoin :: [String] -> String
commajoin = join ", "

half_of' :: Float -> Float
half_of' = \x -> x/2 -- using a lambda expression to define a function on the fly

addToEach :: Num a => a -> [a] -> [a]
addToEach n lst = map (\x -> x + n) lst

succInput :: IO ()
succInput = do
    text <- getLine
    let succtext = map succ text
    putStrLn succtext