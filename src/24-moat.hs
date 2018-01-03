module Twentyfour where

import Data.Char
import Data.Ord
import Data.List
import qualified Data.Set as S

toComponents :: String -> S.Set (Int, Int)
toComponents input = S.fromList $ map parse $ lines input
    where parse line = 
            let
                (first, rest) = span isDigit line
                second = tail rest
            in (read first, read second)

main :: IO ()
main = do
    input <- readFile "../input/24.txt"
    let components = toComponents input
    putStrLn $ show $ buildStrongest components 0
    putStrLn $ show $ buildLongest components 0

buildStrongest :: S.Set (Int, Int) -> Int -> Int
buildStrongest components connection
    | S.null matches    = 0
    | otherwise         = maximum $ S.map subBridge matches
    where 
        matches = choose components connection
        subBridge c@(x, y) = 
            let 
                other = if x == connection then y else x 
            in (x + y) + (buildStrongest (S.delete c components) other)

buildLongest :: S.Set (Int, Int) -> Int -> (Int, Int)
buildLongest components connection
    | S.null matches    = (0, 0)
    | otherwise         = maximumBy bridgeLength $ S.map subBridge matches
    where 
        bridgeLength (l1, s1) (l2, s2) 
            | l1 > l2               = GT
            | l1 < l2               = LT
            | s1 > s2               = GT
            | s1 < s2               = LT
            | otherwise             = EQ
        matches = choose components connection
        subBridge c@(x, y) = 
            let 
                other = if x == connection then y else x
                (l, s) = buildLongest (S.delete c components) other
            in (l + 1, s + x + y)

choose components connection = S.filter matching components
    where matching (x, y) = x == connection || y == connection 