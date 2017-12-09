module Seven where

import Data.Char
import Data.List
import Data.Maybe
import Data.Either
import Text.Parsec
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Set as S

data Program = Program 
    { name :: String
    , weight :: Int
    , discs :: [String]
    }
    deriving Show

parseFile file = do 
    content <- readFile file
    return (runParser parser () file content)

main :: IO ()
main = do
    input <- parseFile "../input/07.txt"
    putStrLn $ show $ solve2 $ fromRight [] input

parser = do 
    result <- many (programParser <* ((skipMany1 endOfLine) <|> eof)) 
    return result
    where 
        programParser = do 
            name <- many1 alphaNum
            skipMany space
            skipMany (char '(')
            weight <- num 
            skipMany (char ')')
            discs <- option [] discParser
            return $ Program name weight discs
        num :: Parsec String st Int 
        num = read <$> (many digit)
        discParser = do 
            skipMany $ oneOf " ->"
            sepBy1 (many1 alphaNum) (many1 $ oneOf ", ")

solve1 programs = find notOnDisc all
    where 
        all = map name programs
        notOnDisc p = notElem p onDiscs
        onDiscs = concatMap discs programs

solve2 programs = unbalanced $ fromJust $ M.lookup root byName
    where 
        byName = M.fromList $ zip (map name programs) programs
        root = fromJust $ solve1 programs
        unbalanced p
            | isJust subUn = fromJust subUn
            | (S.size $ S.fromList subWeights) > 1 = Left $ zip subprograms subWeights
            | otherwise = Right $ sum ((weight p):subWeights)
            where 
                subBalances = map unbalanced subprograms
                subUn = find isLeft subBalances
                subWeights = (map (fromRight 0) subBalances)
                subprograms = map subp $ discs p
                subp n = fromJust $ M.lookup n byName
