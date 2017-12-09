module Nine where

import Data.List
import Data.Either
import Text.Parsec

tests = 
    [ "{}"
    , "{{{}}}"
    , "{{},{}}"
    , "{{{},{},{{}}}}"
    , "{<a>,<a>,<a>,<a>}"
    , "{{<ab>},{<ab>},{<ab>},{<ab>}}"
    , "{{<!!>},{<!!>},{<!!>},{<!!>}}"
    , "{{<a!>},{<a!>},{<a!>},{<ab>}}"
    ]

data GarbageStream = Garbage String | Group [GarbageStream]
    deriving Show

parseFile file = do 
    content <- readFile file
    return (runParser parser () file content)

main :: IO ()
main = do
    input <- parseFile "../input/09.txt"
    putStrLn $ show $ lefts [input]
    putStrLn $ show $ solve1 0 $ fromRight (Garbage "") input
    putStrLn $ show $ solve2 $ fromRight (Garbage "") input

parser = group
    where
        group = Group <$> (open *> ((group <|> garbage) `sepBy` comma) <* close)
        open = char '{'
        close = char '}'
        comma = char ','
        garbage = Garbage <$> (openGarbage *> (concat <$> (many ((many1 garbageContent) <|> garbageEscape))) <* closeGarbage)
        openGarbage = char '<'
        closeGarbage = char '>'
        garbageContent = noneOf "!>"
        garbageEscape = cancel <$> ((char '!') *> anyChar)
        cancel _ = "" 

runTests = map (solve1 0 . fromRight (Garbage "") . runParser parser () "test") tests
runTests2 = map (solve2 . fromRight (Garbage "") . runParser parser () "test") tests

solve1 _ (Garbage _) = 0
solve1 score (Group subStreams) = (score + 1) + (sum $ map (solve1 (score + 1)) subStreams)

solve2 (Garbage s) = length s
solve2 (Group subStreams) = sum $ map solve2 subStreams 

