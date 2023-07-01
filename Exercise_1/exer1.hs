det :: Num a => a -> a -> a -> a
det a b c = b^2 - 4*a*c

quadsol1 :: Floating a => a -> a -> a -> a
quadsol1 a b c = (-b - sqrt (det a b c)) / 2*a

quadsol2 :: Floating a => a -> a -> a -> a
quadsol2 a b c = (-b + sqrt (det a b c)) / 2*a

third_a :: [a] -> a
third_a xs = xs !! 2

third_b :: [a] -> a
third_b (_:_:a:xs) = a

fact :: (Eq t, Num t) => t -> t
fact 0 = 1
fact n = n * fact (n-1)

hailstone :: Integral a => a -> a
hailstone n
    | even n = div n 2
    | odd n = 3 * n + 1

hailLen :: (Num a, Integral t) => t -> a
hailLen 1 = 0
hailLen n = 1 + hailLen (hailstone n)