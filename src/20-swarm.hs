module Twenty where

import Text.Parsec
import Data.Either
import qualified Data.Vector as V
import qualified Data.Sequence as Seq
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!))
import qualified Data.Set as Set
import Control.Applicative (liftA3)

data Xyz = Xyz Int Int Int deriving (Eq, Ord) 

instance Num Xyz where
    (Xyz x1 y1 z1) + (Xyz x2 y2 z2) = Xyz (x1 + x2) (y1 + y2) (z1 + z2)
    (Xyz x1 y1 z1) * (Xyz x2 y2 z2) = Xyz (x1 * x2) (y1 * y2) (z1 * z2)
    abs (Xyz x y z) = Xyz (abs x) (abs y) (abs z)
    signum (Xyz x y z) = Xyz (signum x) (signum y) (signum z)
    fromInteger i = Xyz (fromInteger i) (fromInteger i) (fromInteger i)
    negate (Xyz x y z) = Xyz (-x) (-y) (-z)
    

data Particle = Particle 
    { p :: Xyz 
    , v :: Xyz 
    , a ::Xyz
    }

particleParser = particle `sepBy` endOfLine
    where
        particle = liftA3 Particle (position <* sep) (velocity <* sep) acceleration
        sep = comma *> space
        position = (string "p=") *> xyz
        velocity = (string "v=") *> xyz
        acceleration = (string "a=") *> xyz
        comma = char ','
        xyz = liftA3 Xyz ((char '<') *> num <* comma) (num <* comma) (num <* (char '>'))
        num = read <$> (many1 (digit <|> (char '-')))

main :: IO ()
main = do
    input <- readFile "../input/20.txt"
    let particles = runParser particleParser () "" input
    putStrLn $ show $ lefts [particles]
    putStrLn $ show $ V.minIndexBy avp $ V.fromList $ fromRight [] particles
    putStrLn $ show $ Seq.length $ simulate $ fromRight [] particles

avp p1 p2 = case compare (distance $ a p1) (distance $ a p2) of
    EQ -> case compare (distance $ v p1) (distance $ v p2) of
        EQ -> compare (distance $ p p1) (distance $ p p2)
        ord -> ord
    ord -> ord
distance (Xyz x y z) = (abs x) + (abs y) + (abs z) 

simulate particles = go 10000 $ Seq.fromList particles
    where 
        go 0 particles = particles
        go n particles = go (n - 1) $ fmap move $ removeCollisions particles

removeCollisions particles = foldr Seq.deleteAt particles collisions
    where 
        (_, collisions) = Seq.foldlWithIndex collide (Map.empty, Set.empty) particles
        collide (positions, collisions) idx particle 
            | Map.member (p particle) positions     = (positions, Set.union collisions $ Set.fromList [idx, positions ! (p particle)]) 
            | otherwise                             = (Map.insert (p particle) idx positions, collisions)

move (Particle p v a) = Particle (p + v') v' a
    where v' = v + a