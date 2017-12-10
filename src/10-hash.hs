module Ten where

import qualified Data.Vector as V
import Data.Bits
import Data.Char
import Numeric


test = [3, 4, 1, 5]
input1 = [106,16,254,226,55,2,1,166,177,247,93,0,255,228,60,36]
input2 = "106,16,254,226,55,2,1,166,177,247,93,0,255,228,60,36"

hashTest = knotHash [0..4] test
hashInput = knotHash [0..255] input1

solve1 = product $ take 2 hashInput
solve2 = fullHash $ map ord input2

fullHash lengths = concatMap toHex xorred
    where
        start = V.fromList [0..255]
        finalLengths = lengths ++ [17, 31, 73, 47, 23]
        knotted = go 64 0 0 start
        xorred = xorGroups knotted
        toHex x = pad 2 '0' $ showHex x ""
        go 0 _ _ string = string
        go i current skip string = go (i - 1) next nextSkip hashed
            where (next, nextSkip, hashed) = hash current skip string finalLengths

pad l c str
    | length str < l = (take (l - (length str)) $ repeat c) ++ str
    | otherwise = str

xorGroups string
    | V.null string = []
    | otherwise = xorred : (xorGroups rest) 
    where 
        xorred = foldl1 xor toXor
        (toXor, rest) = V.splitAt 16 string

knotHash :: [Int] -> [Int] -> [Int]
knotHash string lengths = V.toList hashed
    where (_, _, hashed) = hash 0 0 (V.fromList string) lengths

hash :: Int -> Int -> V.Vector Int -> [Int] -> (Int, Int, V.Vector Int)
hash current skip string [] = (current, skip, string)
hash current skip string (l:rest) = hash next (skip + 1) twisted rest
    where
        fromCurrent = V.slice current fromCurrentLength string
        fromStart = V.slice 0 fromStartLength string
        fromCurrentLength = min (strLength - current) l
        fromStartLength
            | (current + l) <= strLength = 0
            | otherwise = (current + l) `mod` strLength 
        strLength = V.length string
        reversed = V.reverse $ (V.++) fromCurrent fromStart
        updatedIdx = V.fromList $ map (`mod` strLength) $ take l [current..]
        twisted = V.update string (V.zip updatedIdx reversed)
        next = (current + skip + l) `mod` strLength
