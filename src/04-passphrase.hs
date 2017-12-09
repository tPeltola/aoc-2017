module Four where

import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T

readInput = do 
    content <- readFile "input/04.txt"
    pure $ lines content

test = 
    [ "aa bb cc dd ee"
    , "aa bb cc dd aa"
    , "aa bb cc dd aaa"
    ]

test2 = 
    [ "abcde fghij"
    , "abcde fghij"
    , "abcde fghij"
    , "iiii oiii ooii oooi oooo"
    , "oiii ioii iioi iiio"
    ]

main :: IO ()
main = do
    input <- readInput
    putStrLn $ show $ solve2 $ parse input

parse :: [String] -> [[String]]
parse rows = map parseRow rows
    where parseRow row = map T.unpack $ T.split isSpace $ T.pack row 

solve1 passphrases = length $ filter isValid passphrases
    where isValid passphrase = (length passphrase) == (S.size $ S.fromList passphrase)

solve2 passphrases = length $ filter isValid passphrases
    where isValid passphrase = (length passphrase) == (S.size $ S.fromList $ map S.fromList passphrase)