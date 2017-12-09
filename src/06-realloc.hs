{-# LANGUAGE BangPatterns #-}
module Six where

import Data.Vector ((!))
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Set as S
import qualified Data.Map.Strict as M

test =  [0, 2, 7, 0]
input = [14,0,15,12,11,11,3,5,1,6,8,4,9,1,8,4]

main :: IO ()
main = putStrLn $ show $ solve1 $ V.fromList input

solve1 banks = realloc 0 M.empty banks

realloc :: Int -> M.Map (V.Vector Int) Int -> V.Vector Int -> Int 
realloc !steps visited banks
    | isJust alreadyVisited     = steps - (fromJust alreadyVisited)
    | otherwise                 = realloc (steps + 1) (M.insert banks steps visited) reallocated 
    where
        alreadyVisited = M.lookup banks visited
        l = V.length banks
        reallocated = V.imap distribute banks
        reallocateIdx = V.maxIndex banks
        toReallocate = banks ! reallocateIdx
        (evenly, extraFor) = toReallocate `divMod` l
        maxExtra = reallocateIdx + extraFor
        wrappedAroundExtra
            | maxExtra >= l = (maxExtra `mod` l)
            | otherwise = -1 
        distribute idx mem 
            | idx == reallocateIdx = evenly
            | (idx > reallocateIdx && idx <= maxExtra) || idx <= wrappedAroundExtra = mem + evenly + 1
            | otherwise = mem + evenly