-- ghc -O2 -with-rtsopts="-N32" -threaded concurrent1.hs
-- time ./concurrent1

import Control.Parallel
import Control.Parallel.Strategies

n = 10000000000

myPowerSeq :: Int -> Int -> Int -> Int
myPowerSeq a _ 0 = a
myPowerSeq a x y = seq newacc (myPowerSeq newacc x (y-1))
    where newacc = a*x

-- some function to take a reasonable amount of time in examples...
calculation i = myPowerSeq i 1 n
slowCalc i = myPowerSeq i 1 (n `div` 50)
fastCalc i = myPowerSeq i 1 (n `div` 500000)

calcA = a + b
    where a = calculation 1
          b = calculation 2


calcB = (a `par` b) `pseq` (a + b)
    where a = calculation 1
          b = calculation 2


calcC = map slowCalc [0..100]

calcD = parMap rseq slowCalc [0..100]


calcE = map fastCalc [0..1000000]
calcF = parMap rseq fastCalc [0..1000000]





main :: IO ()
main = print $ calcE

-- ./concurrent1  real	18.30s user	18.06s sys	0.17s for calcA
-- ./concurrent1  real	9.40s user	18.27s sys	0.14s for calcB

-- ./concurrent1  real	18.62s user	18.34s sys	0.09s for calcC
-- ./concurrent1  real	5.65s user	19.16s sys	0.12s for calcD

-- ./concurrent1  real	19.09s user	18.46s sys	0.19s for calcE
-- ./concurrent1  real	19.70s user	19.06s sys	0.31s for calcF
