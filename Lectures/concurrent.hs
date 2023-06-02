import Control.Parallel

fibSer, fibPar1, fibPar2, fibPar3 :: Int -> Integer

fibSer 0 = 0
fibSer 1 = 1
fibSer n = fibSer (n-1) + fibSer (n-2)

--real	28.28s
--user	27.79s
--sys	0.57s


fibPar1 0 = 0
fibPar1 1 = 1
fibPar1 n = (term1 `par` term2) `pseq` (term1 + term2)
    where
      term1 = fibPar1 (n-1)
      term2 = fibPar1 (n-2)

--real	35.51s
--user	113.15s
--sys	1.82s

fibPar2 n = (term1 `par` term2) `pseq` (term1 + term2)
    where
      term1 = fibSer (n-1)
      term2 = fibSer (n-2)

--real	17.61s
--user	27.81s
--sys	0.38s

cutoff :: Int
cutoff = 20
fibPar3 n 
  | n<cutoff     = fibSer (n-1) + fibSer (n-2)
  | n>=cutoff    = (term1 `par` term2) `pseq` (term1 + term2)
    where
      term1 = fibPar3 (n-1)
      term2 = fibPar3 (n-2)

--real	8.37s
--user	29.74s
--sys	0.28s

main = do
  print (fibPar2 46)




