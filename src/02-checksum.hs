module Two where

import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Text as T

readInput = do 
    content <- readFile "../input/02.txt"
    pure $ lines content

test1 = 
    [ "5 1 9 5"
    , "7 5 3"
    , "2 4 6 8"
    ]

test2 = 
    [ "5 9 2 8"
    , "9 4 7 3"
    , "3 8 6 5"
    ]

main :: IO ()
main = do
    input <- readInput
    putStrLn $ show $ solve2 $ parse input

parse :: [String] -> [[Int]]
parse rows = map parseRow rows
    where parseRow row = map (read . T.unpack) $ T.split isSpace $ T.pack row 

solve1 spreadsheet = sum $ map checksum spreadsheet
    where checksum row = (maximum row) - (minimum row)

solve2 spreadsheet = sum $ map evenDivision spreadsheet

evenDivision (x:rest) 
    | isJust maybeEven =  (max x other) `div` (min x other) 
    | otherwise = evenDivision rest
    where 
        maybeEven = find isEvenDiv rest
        isEvenDiv y = ((max x y) `mod` (min x y)) == 0
        other = fromJust maybeEven