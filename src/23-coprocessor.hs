{-# LANGUAGE BangPatterns #-}

module Twentythree where

import Text.Parsec
import Data.Either
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import Data.Vector ((!))
import Debug.Trace

type Register = Char

data Operand = Val Int | Reg Register deriving Show

data Move = Set Register Operand
    | Sub Register Operand
    | Mul Register Operand
    | Mod Register Operand
    | Jnz Operand Operand
    | Jlz Operand Operand
    deriving Show

codeParser = instruction `sepBy` endOfLine
    where
        instruction = choice [set, sub, mul, mod, jnz, jlz]
        set = instruction2 Set "set"
        sub = instruction2 Sub "sub"
        mul = instruction2 Mul "mul"
        mod = instruction2 Mod "mod"
        jnz = instruction2op Jnz "jnz"
        jlz = instruction2op Jlz "jlz"
        instruction2op f name = try $ f <$> ((string name) *> space *> valOrReg) <*> (space *> valOrReg)
        instruction2 f name = try $ f <$> ((string name) *> space *> register) <*> (space *> valOrReg)
        
        valOrReg = val <|> reg
        register = oneOf "abcdefgh"
        val = try $ Val <$> read <$> (many1 (digit <|> (char '-')))
        reg = try $ Reg <$> register

main :: IO ()
main = do
    input <- readFile "../input/23-optimized.txt"
    let instructions = runParser codeParser () "" input
    putStrLn $ show $ lefts [instructions]
    putStrLn $ show $ interpret M.empty $ fromRight [] instructions
    putStrLn $ show $ interpret (M.singleton 'a' 1) $ fromRight [] instructions

interpret registers instructions = run (V.fromList instructions) 0 registers 0

run instructions pc registers !mulCounter
    | (pc < 0) || (pc >= (V.length instructions)) = (mulCounter, registers)
    | otherwise = run instructions pc' registers' mulCounter'
    where
        instruction = instructions ! pc
        pc' = pc + pcOffset
        (pcOffset, mulCounter', registers') = case instruction of
            (Set reg op)   -> withRegisterValue mulCounter reg (const (val op))
            (Sub reg op)   -> withRegisterValue mulCounter reg (+ (-(val op)))
            (Mul reg op)   -> withRegisterValue (mulCounter + 1) reg (* (val op))
            (Mod reg op)   -> withRegisterValue mulCounter reg (`mod` (val op))
            (Jnz cmp op)   -> jump cmp (/= 0) op
            (Jlz cmp op)   -> jump cmp (<= 0) op

        jump cmp p op
            | p $ val cmp   = ((val op), mulCounter, registers)
            | otherwise     = (1, mulCounter, registers)


        withRegisterValue c reg f = (1, c, M.insert reg (f (value reg)) registers)

        val (Val x) = x
        val (Reg x) = value x
        value reg = M.findWithDefault 0 reg registers