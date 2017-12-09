{-# LANGUAGE BangPatterns #-}
module Five where

import Data.Char
import Data.List
import Data.Maybe
import Data.Vector ((!))
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T

readInput = do 
    content <- readFile "../input/05.txt"
    pure $ lines content

test = 
    [ "0"
    , "3"
    , "0"
    , "1"
    , "-3"
    ]

main :: IO ()
main = do
    input <- readInput
    putStrLn $ show $ solve1 $ V.fromList $ parse input

parse :: [String] -> [Int]
parse rows = map read rows

solve1 instructions = jump 0 0 instructions M.empty

jump :: Int -> Int -> V.Vector Int -> M.Map Int Int -> Int
jump counter !steps instructions visits
    | isOutside counter = steps
    | otherwise = jump next (steps + 1) instructions (M.insertWith (+) counter increment2 visits)
    where 
        current = (instructions ! counter) + (M.findWithDefault 0 counter visits)
        next = counter + current
        isOutside c = c < 0 || c >= (V.length instructions)
        increment1 = 1
        increment2 = if current >= 3 then -1 else 1
