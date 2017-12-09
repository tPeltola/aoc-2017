module Three where

import qualified Data.Set as S

test1 = 1
test2 = 12
test3 = 23
test4 = 1024
input = 312051

main :: IO ()
main = putStrLn $ show $ head $ dropWhile (< input) fibSpiral

squares = [1, 3..]
squared = zip [0..] $ zip squares $ map (^2) squares

distance index = spiralIdx + (abs (((index - previousSquares - 1) `mod` spiralLength) - center))
    where 
        square = head $ dropWhile ((< index) . snd . snd) squared
        spiralIdx = fst square
        sideLength = fst $ snd square
        spiralLength = sideLength - 1
        previousSquares = (sideLength - 2)^2
        center = spiralIdx - 1

fibSpiral = map (neighborSum fibSpiral) [1..] 

neighborSum _ 1 = 1
neighborSum spiral idx = sum $ map (spiral !!) $ neighbors
    where
        neighbors = [ p - 1 | i <- [1, 0, -1], j <- [-1, 0, 1], let p = fromXy (x + i, y + j), p < idx ]
        (x, y) = toXy idx

toXy idx 
    | quadrant == 0 = (level, position - level + 1)
    | quadrant == 1 = (level - 1 - position, level)
    | quadrant == 2 = (-level, level - position - 1)
    | quadrant == 3 = (position - level + 1, -level)
    | otherwise = error $ show size
    where
        (level, (size, _)) = head $ dropWhile ((< idx) . snd . snd) squared
        edge = 2 * level
        idxOnLevel = idx - ((size - 2) ^ 2) - 1
        quadrant
            | edge > 0 = idxOnLevel `div` edge
            | otherwise = 0
        position 
            | edge > 0 = idxOnLevel `mod` edge
            | otherwise = -1

fromXy :: (Int, Int) -> Int
fromXy (x, y) = (previous ^ 2) + quadrant * edge + position + 1
    where 
        level = max (abs x) (abs y)
        edge = 2 * level
        size = edge + 1
        previous = size - 2
        quadrant = fst qPos
        position = snd qPos 
        qPos 
            | x == level && y > -level  = (0, y + level - 1)
            | y == level                = (1, level - 1 - x)
            | x == -level               = (2, level - 1 - y)
            | y == -level               = (3, x + level - 1)
        