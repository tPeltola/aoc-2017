{-# LANGUAGE BangPatterns #-}

module Twentytwo where

import qualified Data.Map.Strict as Map
import Data.List
import Data.Maybe

main :: IO ()
main = do
    input <- readFile "../input/22.txt"
    let 
        rows = lines input
        inputMap = toMap rows
    putStrLn $ show $ burst basic 10000 0 (1,1) N testMap
    putStrLn $ show $ burst basic 10000 0 (0, 0) N inputMap
    putStrLn $ show $ burst evolved 10000000 0 (1, 1) N testMap
    putStrLn $ show $ burst evolved 10000000 0 (0, 0) N inputMap

toMap rows = Map.fromList $ concatMap withCoordinates $ map onlyInfected $ zip [startIdx, (startIdx - 1)..] $ map (zip [-startIdx..]) rows
    where 
        startIdx = ((length rows) `div` 2)
        onlyInfected (i, row) = (i, filter ((== '#') . snd) row)
        withCoordinates (y, row) = map (\(x, c) -> ((x, y), Infected)) row 

testMap = Map.fromList [((2, 2), Infected), ((0, 1), Infected)]

data State = Clean | Weakened | Infected | Flagged

data Direction = N | E | S | W deriving (Eq, Show)
data Turn = L | R | AHD | REV

directions = [N, E, S, W]

turn t d = directions !! ((l + idx + offset) `mod` l)
    where
        idx = fromJust $ elemIndex d directions
        l = length directions
        offset = case t of
            L   -> (-1)
            R   -> 1
            AHD -> 0
            REV -> 2

move (x, y) d = (x + i, y + j)
    where 
        (i, j) = case d of
            N -> (0, 1)
            E -> (1, 0)
            S -> (0, -1)
            W -> (-1, 0)

burst act 0 !infections _ _ _ = infections
burst act n !infections pos direction infected = burst act (n - 1) (infections + offset)  pos' direction' infected'
    where
        direction' = turn t direction
        (t, offset, infected') = act pos infected
        pos' = move pos direction'

basic pos infected
    | isInfected  = (R, 0, Map.delete pos infected)
    | otherwise   = (L, 1, Map.insert pos Infected infected)
    where
        isInfected = Map.member pos infected
evolved pos infected = case state of
    Clean       -> (L, 0, set Weakened)
    Weakened    -> (AHD, 1, set Infected)
    Infected    -> (R, 0, set Flagged)
    Flagged     -> (REV, 0, Map.delete pos infected)
    where
        state = Map.findWithDefault Clean pos infected
        set state' = Map.insert pos state' infected

