module Nineteen where

import qualified Data.Vector as V
import Data.Vector ((!))
import Data.Maybe
import Data.Char
import Data.List

main = do
    input <- readFile "../input/19.txt"
    let
        m = toMap input
        s = start m
        (steps, letters) = findEnd s D m "" 1
    putStrLn letters
    putStrLn $ show $ steps

start m = (fromJust $ V.findIndex (not . isSpace) (m ! 0), 0)

toMap input = V.fromList $ map V.fromList $ lines input

data Direction = U | R | D | L deriving Eq

directions = [U, R, D, L]

findEnd pos direction m letters steps
    | pos == pos'   = (steps, reverse letters)
    | otherwise     = findEnd pos' direction' m letters' (steps + 1)
    where 
        (direction', pos') 
            | isValid m $ move pos direction    = (direction, move pos direction)
            | otherwise                         = fromMaybe (direction, pos) $ find (isValid m . snd) $ zip ds $ map (move pos) ds 
        letters'
            | isLetter $ onMap m pos'   = (onMap m pos'):letters
            | otherwise                 = letters
        ds = validDirections direction

validDirections d = map (directions !!) $ map (\o ->((l + idx + o) `mod` l)) [-1, 1] 
    where
        idx = fromJust $ elemIndex d directions
        l = length directions

move (x, y) d = (x + i, y + j)
    where (i, j) = case d of
            U -> (0, -1)
            R -> (1, 0)
            D -> (0, 1)
            L -> (-1, 0)
isValid m p@(x, y)
    | y >= 0 && y < (V.length m) && x >= 0 && x < (V.length (m ! y))    = not $ isSpace (onMap m p)
    | otherwise                                                         = False
onMap m (x, y) = (m ! y ! x)