import RainbowAssign
import Data.Maybe (listToMaybe)
import qualified Data.Map as Map
import Data.Int (Int32)

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

-- Code for findPassword begins

-- Lookup if a hash value is in the table
lookup :: Hash -> IO (Maybe Passwd)
lookup hash = do
    table <- readTable filename
    return (Map.lookup hash table)

-- Take a hash, reduce, and rehash.
reduceAndHash :: Hash -> Hash
reduceAndHash = pwHash . pwReduce



--findPassword :: Map.Map Hash Passwd -> Int -> Hash -> Maybe Passwd

