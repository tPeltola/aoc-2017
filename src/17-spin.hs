{-# LANGUAGE BangPatterns #-}
module Seventeen where

import qualified Data.Sequence as Seq
import Data.Maybe

test :: Int
test = 3
input :: Int
input = 303

solve1 = spinLock input 2018 2017
solve2 = spinLock input 50000001 0

spinLock step length findAfter = buffer `Seq.index` (idx + 1)
    where
        idx = fromJust $ Seq.elemIndexL findAfter buffer
        buffer = go 1 0 $ Seq.singleton 0
        go s pos !buffer 
            | s == length = buffer
            | otherwise = 
                let idx = ((pos + step) `mod` (Seq.length buffer)) + 1
                    buffer' = Seq.insertAt idx s buffer 
                in  go (s + 1) idx  buffer'