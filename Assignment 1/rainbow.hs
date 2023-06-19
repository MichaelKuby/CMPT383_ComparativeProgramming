{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Replace case with maybe" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Eta reduce" #-}
import RainbowAssign
    ( Passwd, Hash, pwHash, buildTable, writeTable, readTable, randomPasswords )
import qualified Data.Map as Map
import Data.Maybe ( catMaybes, isJust, isNothing )
import qualified Data.Maybe as Maybe

-- Parameters
pwLength, nLetters, width, height :: Int
filename :: FilePath
pwLength = 8            -- length of each password
nLetters = 5            -- nuber of letters to use in passwords: 5 -> a-e
width = 40              -- length of each chain in the table
height = 1000           -- number of "rows" in the table
filename = "table.txt"  -- filename to store the table

pwReduce :: Hash -> [Char]
pwReduce hash = reverse . digitsToLetters $ take pwLength $ convertToBase hash nLetters
  where
    convertToBase :: Hash -> Int -> [Int]
    convertToBase num base = fromIntegral (num `mod` fromIntegral base) : convertToBase (num `div` fromIntegral base) base

    intToLetter :: Int -> Char
    intToLetter n = ['a'..'z'] !! n

    digitsToLetters :: [Int] -> String
    digitsToLetters = map intToLetter

rainbowTable :: Int -> [Passwd] -> Map.Map Hash Passwd
rainbowTable numChains pwords = Map.fromList $ zip (mapChainHash numChains pwords) pwords
    where
        mapChainReduce :: Int -> [String] -> [String]
        mapChainReduce 0 passwords = passwords
        mapChainReduce n passwords = mapChainReduce (n-1) (map (pwReduce . pwHash) passwords)

        mapChainHash :: Int -> [String] -> [Hash]
        mapChainHash 0 passwords = map pwHash $ mapChainReduce 0 passwords
        mapChainHash n passwords = map pwHash $ mapChainReduce n passwords

generateTable :: IO ()
generateTable = do
    table <- buildTable rainbowTable nLetters pwLength width height
    writeTable table filename

reduceAndHash :: Hash -> Hash
reduceAndHash = pwHash . pwReduce

hashAndReduce :: Passwd -> Passwd
hashAndReduce = pwReduce . pwHash

-- From [Maybe Passwd], drop all 'Nothing' values
validPasswords :: [Maybe Passwd] -> [Passwd]
validPasswords = catMaybes

-- From [Passwd], if empty, return nothing, else, return first elem as Maybe.
maybeOrNothing :: [Passwd] -> Maybe Passwd
maybeOrNothing [] = Nothing
maybeOrNothing [x] = Just x
maybeOrNothing (_:x:_) = Just x

extractValuesForKey :: Map.Map Hash Passwd -> [Maybe Passwd]
extractValuesForKey table = [Map.lookup h table | h <- Map.keys table]

findPassword :: Map.Map Hash Passwd -> Int -> Hash -> Maybe Passwd
findPassword table chainLen hash = case extractValuesForKey table of
    []      |   chainLen > 0 -> findPassword table (chainLen - 1) (reduceAndHash hash)
            |   otherwise -> Nothing
    pwvals  |   all isNothing pwvals && chainLen > 0 -> findPassword table (chainLen - 1) (reduceAndHash hash)
            |   all isNothing pwvals && chainLen <= 0 -> Nothing
            |   any isJust pwvals -> maybeOrNothing $ validPasswords $ map (passwordChainLookup width hash) pwvals

passwordChainLookup :: Int -> Hash -> Maybe Passwd -> Maybe Passwd
passwordChainLookup (-1) _ _ = Nothing
passwordChainLookup n hash pw = passwordChainLookup' n pw hash
    where
        passwordChainLookup' :: Int -> Maybe Passwd -> Hash -> Maybe Passwd
        passwordChainLookup' num pword h
            | (pwHash <$> pword) == Just h = pword
            | otherwise = passwordChainLookup (num-1) h (hashAndReduce <$> pword)

test2 :: Int -> IO ([Passwd], Int)
test2 n = do
  table <- readTable filename
  pws <- randomPasswords nLetters pwLength n
  let hs = map pwHash pws
  let result = Maybe.mapMaybe (findPassword table width) hs
  return (result, length result)