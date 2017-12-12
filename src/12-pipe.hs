module Twelve where

import Text.Parsec
import Data.Either
import qualified Data.Vector as V
import Data.Vector ((!))
import qualified Data.Set as S

data Program = Program Int [Int] 

parser = program `sepBy` endOfLine
    where 
        program = Program <$> (id <* space <* pipe <* space) <*> (id `sepBy` pipeSep)
        id :: Parsec String st Int 
        id = read <$> (many digit)
        comma = char ','
        pipe = string "<->"
        pipeSep = comma <* space

parseFile file = do 
    content <- readFile file
    return (runParser parser () file content)

main :: IO ()
main = do
    input <- parseFile "../input/12.txt"
    putStrLn $ show $ lefts [input]
    putStrLn $ show $ S.size $ connected 0 $ V.fromList $ fromRight [] input
    putStrLn $ show $ S.size $ groups $ fromRight [] input

groups programs = go programs S.empty S.empty
    where 
        programsVector = V.fromList programs
        go [] groups _ = groups
        go ((Program id _):rest) groups inGroups
            | S.member id inGroups = go rest groups inGroups
            | otherwise            = go rest newGroups newInGroups
            where
                newGroups = S.insert group groups
                newInGroups = S.union group inGroups
                group = connected id programsVector

connected id programs = dfs S.empty id programs

dfs visited id programs 
    | null unvisited    = nowVisited
    | otherwise         = foldl visitConnected nowVisited unvisited
    where 
        visitConnected alreadyVisited connected = dfs alreadyVisited connected programs
        (Program _ connections) = programs ! id
        unvisited = filter (not . isVisited) connections
        toVisit = head unvisited
        isVisited p = S.member p visited
        nowVisited = S.insert id visited