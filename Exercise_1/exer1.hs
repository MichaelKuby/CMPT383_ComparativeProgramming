det a b c = b^2 - 4*a*c
quadsol1 a b c = (-b - sqrt (det a b c)) / 2*a
quadsol2 a b c = (-b + sqrt (det a b c)) / 2*a

third_a xs = xs !! 2

third_b (_:_:a:xs) = a

fact 0 = 1
fact n = n * fact (n-1)

hailstone n
    | even n = div n 2
    | odd n = 3 * n + 1

hailLen 1 = 0
hailLen n = 1 + hailLen (hailstone n)