{-# LANGUAGE BangPatterns #-}
module Sixteen where
    
import Prelude hiding ((++))
import Text.Parsec
import Data.Either
import qualified Data.Vector as V
import Data.Vector ((!), (++), (//))

data Move = Spin Int 
    | Exchange Int Int
    | Partner Char Char 

danceParser = move `sepBy` comma
    where
        move = spin <|> exchange <|> partner
        comma = char ','
        spin = Spin <$> ((char 's') *> num)
        exchange = Exchange <$> ((char 'x') *> num) <*> ((char '/') *> num)
        partner = Partner <$> ((char 'p') *> anyChar) <*> ((char '/') *> anyChar)
        num :: Parsec String st Int 
        num = read <$> (many digit)

parseDance content = runParser danceParser () "" content

test = [Spin 1, Exchange 3 4, Partner 'e' 'b']

main :: IO ()
main = do
    input <- readFile "../input/16.txt"
    putStrLn $ show $ dance test $ V.fromList "abcde"
    let inputMoves = parseDance input
    putStrLn $ show $ lefts [inputMoves]
    putStrLn $ show $ dance (fromRight [] inputMoves) $ V.fromList ['a'..'p']
    putStrLn $ show $ justDance (fromRight [] inputMoves) $ V.fromList ['a'..'p']

justDance moves programs = go (1000000000 `mod` 48) programs
    where
        go 0 programs = programs
        go n !programs = go (n - 1) $ dance moves programs

dance [] programs = programs
dance (move:rest) !programs = dance rest programs'
    where 
        programs' = case move of
            Spin x -> spin x
            Exchange a b -> exchange a b
            Partner a b -> partner a b
        l = V.length programs
        spin x = let (start, end) = V.splitAt (l - x) programs in end ++ start
        exchange a b = programs // [(a, programs ! b), (b, programs ! a)]
        partner a b = V.map (swap a b) programs
        swap a b x
            | a == x    = b
            | b == x    = a
            | otherwise = x
