module Thirteen where

import Text.Parsec
import Data.Either

data Scanner = Scanner 
    { depth     :: Int
    , range     :: Int
    } deriving Show

firewall = scanner `sepBy` endOfLine
    where 
        scanner = Scanner <$> (num <* colon <* space) <*> num
        num :: Parsec String st Int 
        num = read <$> (many digit)
        colon = char ':'

parseFile file = do 
    content <- readFile file
    return (runParser firewall () file content)

main :: IO ()
main = do
    input <- parseFile "../input/13.txt"
    putStrLn $ show $ lefts [input]
    putStrLn $ show $ severity $ caught 0 $ fromRight [] input
    putStrLn $ show $ delayed $ fromRight [] input

delayed scanners = fst $ head $ filter (null . snd) $ map withDelay [0..]
    where withDelay delay =  (delay, caught delay scanners)

severity caught = sum $ map scannerSeverity caught
    where scannerSeverity (Scanner d r) = d * r 

caught delay scanners = filter wasCaught scanners
    where 
        wasCaught (Scanner d r) = ((d + delay) `mod` (2 * r - 2)) == 0

