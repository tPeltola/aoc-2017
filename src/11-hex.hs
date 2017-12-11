module Eleven where 

import Data.Char
import Data.Either
import Text.Parsec

data Direction = N | NE | SE | S | SW | NW deriving (Show, Read)
type Hex = (Int, Int)

parser = direction `sepBy` comma
    where 
        direction = readDirection <$> (many1 $ oneOf "nesw")
        comma = char ','
        readDirection :: String -> Direction
        readDirection = read . map toUpper

parseFile file = do 
    content <- readFile file
    return (runParser parser () file content)

main :: IO ()
main = do
    input <- parseFile "../input/11.txt"
    putStrLn $ show $ lefts [input]
    putStrLn $ show $ distance $ last $ follow $ fromRight [] input
    putStrLn $ show $ maximum $ map distance $ follow $ fromRight [] input

follow route = scanl go (0, 0) route
    where 
        go (x, y) direction = case direction of
            N           -> (x, y + 1)
            S           -> (x, y - 1)
            NE | even x -> (x + 1, y)
            NE          -> (x + 1, y + 1)
            SE | even x -> (x + 1, y - 1)
            SE          -> (x + 1, y)
            SW | even x -> (x - 1, y - 1)
            SW          -> (x - 1, y)
            NW | even x -> (x - 1, y)
            NW          -> (x - 1, y + 1)

distance (x, y) = (abs x) + (max 0 ((abs y) - (((abs x) + 1) `div` 2)))