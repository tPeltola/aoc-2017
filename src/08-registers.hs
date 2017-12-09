module Eight where

import Data.List
import Data.Either
import Text.Parsec
import qualified Data.Map.Strict as M

type Operator = Int -> Int -> Int
type Comparator = Int -> Int -> Bool

data Instruction = Instruction 
    { register :: String
    , operator :: Operator
    , operand :: Int
    , comparedRegister :: String
    , comparison :: Comparator
    , comparedValue :: Int
    }

parseFile file = do 
    content <- readFile file
    return (runParser parser () file content)

main :: IO ()
main = do
    input <- parseFile "../input/08.txt"
    putStrLn $ show $ lefts [input]
    putStrLn $ show $ solve1 $ fromRight [] input
    putStrLn $ show $ solve2 $ fromRight [] input

parser = many (instruction <* ((skipMany1 endOfLine) <|> eof)) 
    where 
        instruction = do 
            name <- reg <* space
            operator <- op <* space
            operad <- num <* space 
            (char 'i') <* (char 'f') <* space
            comparedRegister <- reg <* space
            comparison <- cmp <* space
            comparedValue <- num
            return $ Instruction name operator operad comparedRegister comparison comparedValue
        reg = many1 alphaNum
        num :: Parsec String st Int 
        num = read <$> (many (digit <|> char '-'))
        op :: Parsec String st Operator
        op = readOp <$> many1 alphaNum
        cmp :: Parsec String st Comparator
        cmp = readCmp <$> (many1 $ oneOf "=><!")
        readOp "inc" = (+)
        readOp "dec" = (-)
        readCmp ">" = (>)
        readCmp ">=" = (>=)
        readCmp "==" = (==)
        readCmp "!=" = (/=)
        readCmp "<" = (<)
        readCmp "<=" = (<=)

solve1 instructions = maximum $ map snd $ M.assocs $ foldl run M.empty instructions
solve2 instructions = maximum $ map snd $ concatMap M.assocs $ scanl run M.empty instructions
run :: M.Map String Int -> Instruction -> M.Map String Int
run registers (Instruction reg op oper cmpReg cmp cmpVal)
    | cmp registerValue cmpVal = M.insert reg (op (M.findWithDefault 0 reg registers) oper) registers
    | otherwise = registers 
    where 
        registerValue = M.findWithDefault 0 cmpReg registers

