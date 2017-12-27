module Twentyone where

import Text.Parsec
import Data.Either
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!))
import qualified Data.BitVector as BV

ruleParser = rule `sepBy` endOfLine
    where 
        rule = pair <$> grid <*> (sep *> grid)
        pair x y = (x, y)
        grid = row `sepBy` (char '/')
        sep = string " => "
        row = many1 $ pixel <$> oneOf ".#"
        pixel '.' = False
        pixel '#' = True

main :: IO ()
main = do
    input <- readFile "../input/21.txt"
    let rules = runParser ruleParser () "" input
    putStrLn $ show $ lefts [rules]
    putStrLn $ show $ countPixels $ artify 18 grid $ Map.fromList $ concatMap expand $ fromRight [] rules

grid = 
    [ [False, True, False] -- ".#."
    , [False, False, True] -- "..#"
    , [True, True, True] -- "###"
    ]

countPixels = length . concatMap (filter id)

artify :: Int -> [[Bool]] -> Map.Map [[Bool]] [[Bool]] -> [[Bool]]
artify steps grid rules = foldl go grid [1..steps]
    where 
        go g _ = combine $ map enhance $ divide g
        enhance g = rules ! g

divide grid = concatMap zipList $ split edge $ map (split edge) grid
    where 
        edge 
            | even $ length grid    = 2
            | otherwise             = 3
        zipList ([]:_) = []
        zipList l = (map head l) : (zipList $ map tail l)

split :: Int -> [a] -> [[a]]
split _ [] = []
split l row = (take l row) : (split l $ drop l row)

combine :: [[[a]]] -> [[a]]
combine grids = concatMap toRows $ split edge grids
    where 
        edge = isqrt $ length grids

toRows :: [[[a]]] -> [[a]]
toRows ([]:_) = []
toRows grids = let (grids', row) = toRow grids [] [] in row:(toRows grids')
toRow :: [[[a]]] -> [[[a]]] -> [a] -> ([[[a]]], [a])
toRow [] rest acc = (reverse rest, acc)
toRow ((x:xs):ys) rest acc = toRow ys (xs:rest) (acc ++ x)

isqrt = round . sqrt . fromIntegral

expand (pattern, result) = map (\f -> (f pattern, result)) [id, flipx, flipy, rotatel, rotater, rotatel . rotatel, flipx . rotatel, flipy . rotatel, flipx . rotatel, flipy . rotater, flipx . rotater]
    where 
        flipx p = map reverse p
        flipy p = reverse p
        rotatel p = let (start, end) = splitAt (shifted p) (ring p)  in rows p (end ++ start)
        rotater p = let (start, end) = splitAt ((length (ring p)) - (shifted p)) (ring p) in rows p (end ++ start)
        shifted p = (edge p) - 1
        edge p = length pattern
        ring p = case p of
            [x, y] -> x ++ (reverse y)
            [x, [y1, y2, y3], z] -> x ++ [y3] ++ (reverse z) ++ [y1]
        rows p r = case r of 
            [x1, x2, y2, y1] -> [[x1, x2], [y1, y2]]
            [x1, x2, x3, y3, z3, z2, z1, y1] -> [[x1, x2, x3], [y1, p !! 1 !! 1, y3], [z1, z2, z3]]