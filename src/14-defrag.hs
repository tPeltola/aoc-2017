module Fourteen where

import qualified Data.Vector as V
import qualified Data.Set as S
import Data.Bits
import Data.Char
import Numeric

testKey = "flqrgnkx"
inputKey = "ffayrhll"

grouped inUse visited groups
    | (S.size inUse) == (S.size visited) = groups
    | otherwise                          = grouped inUse (S.union visited newGroup) (groups + 1)
    where 
        newGroup = findGroup inUse S.empty $ S.findMin $ S.filter notVisited inUse
        notVisited p = S.notMember p visited
        
findGroup inUse group (x, y) = foldl (findGroup inUse) (S.insert (x, y) group) neighbors
    where 
        neighbors = filter toGroup [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]
        toGroup p = (S.member p inUse) && (S.notMember p group)

inUseIdx key = S.fromList $ V.toList $ V.concatMap id $ V.imap coordinates $ V.map (V.findIndices id) $ grid key
    where coordinates y xs = V.map (\x -> (x, y)) xs

inUse = S.size . inUseIdx

grid key = V.fromList $ map use $ gridHash key
    where 
        use row = V.fromList $ concatMap squares row
        squares i = map (testBit i) [7, 6..0]
gridHash key = map fullHash $ map ((key ++ "-") ++) $ map show [0..127]

-- 10

fullHash str = xorred
    where
        lengths = map ord str
        start = V.fromList [0..255]
        finalLengths = lengths ++ [17, 31, 73, 47, 23]
        knotted = go 64 0 0 start
        xorred = xorGroups knotted
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
