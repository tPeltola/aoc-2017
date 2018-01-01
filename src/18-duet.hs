module Eighteen where

import Text.Parsec
import Data.Either
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import Data.Vector ((!))

type Register = Char

data Operand = Val Int | Reg Register deriving Show

data Move = Snd Register 
    | Set Register Operand
    | Add Register Operand
    | Mul Register Operand
    | Mod Register Operand
    | Rcv Register
    | Jgz Operand Operand
    deriving Show

codeParser = instruction `sepBy` endOfLine
    where
        instruction = choice [snd, set, add, mul, mod, rcv, jgz]
        snd = instruction1 Snd "snd" 
        set = instruction2 Set "set"
        add = instruction2 Add "add"
        mul = instruction2 Mul "mul"
        mod = instruction2 Mod "mod"
        rcv = instruction1 Rcv "rcv"
        jgz = try $ Jgz <$> ((string "jgz") *> space *> (val <|> reg)) <*> (space *> (val <|> reg))
        instruction1 f name = try $ f <$> ((string name) *> space *> register)
        instruction2 f name = try $ f <$> ((string name) *> space *> register) <*> (space *> (val <|> reg))
        register = letter 
        val = try $ Val <$> read <$> (many1 (digit <|> (char '-')))
        reg = try $ Reg <$> register

main :: IO ()
main = do
    input <- readFile "../input/18.txt"
    let instructions = runParser codeParser () "" input
    putStrLn $ show $ lefts [instructions]
    putStrLn $ show $ interpret $ fromRight [] instructions
    putStrLn $ show $ interpretParallel $ fromRight [] instructions

interpret instructions = run (V.fromList instructions) 0 M.empty [] []

run _ _ _ _ [recovered] = [recovered]
run instructions pc registers played recovered
    | (pc < 0) || (pc >= (V.length instructions)) = recovered
    | otherwise = run instructions pc' registers' played' recovered'
    where
        instruction = instructions ! pc
        pc' = pc + pcOffset
        registers' = M.union (M.fromList registerValue) registers
        played' = justPlayed ++ played
        recovered' = justRecovered ++ recovered 
        (pcOffset, justRecovered, justPlayed, registerValue) = case instruction of
            (Snd reg)      -> (1, [], [value reg], [])
            (Set reg op)   -> withRegisterValue reg (const (val op))
            (Add reg op)   -> withRegisterValue reg (+ (val op))
            (Mul reg op)   -> withRegisterValue reg (* (val op))
            (Mod reg op)   -> withRegisterValue reg (`mod` (val op))
            (Rcv reg)
                | (value reg) /= 0  -> (1, take 1 played, [], []) 
                | otherwise         -> nop
            (Jgz cmp op)
                | (val cmp) > 0  -> ((val op), [], [], [])
                | otherwise         -> nop

        nop = (1, [], [], [])
        withRegisterValue reg f = (1, [], [], [(reg, f (value reg))])

        val (Val x) = x
        val (Reg x) = value x
        value reg = M.findWithDefault 0 reg registers

interpretParallel instructionList = go 0 (M.singleton 'p' 0) [] 0 0 (M.singleton 'p' 1) [] 0
    where 
        instructions = V.fromList instructionList
        go pc1 registers1 tx1 c1 pc2 registers2 tx2 c2
            | (null tx1'') && (null tx2'') = (c1, c2)
            | otherwise = go pc1' registers1' tx1' (c1 + (length tx1'')) pc2' registers2' tx2' (c2 + (length tx2''))
            where 
                (pc1', registers1', rx1, tx1'') = runParallel instructions pc1 registers1 tx2 []
                (pc2', registers2', rx2, tx2'') = runParallel instructions pc2 registers2 tx1 []
                tx1' = rx2 ++ tx1''
                tx2' = rx1 ++ tx2''

runParallel instructions pc registers rx tx
    | runnable  = runParallel instructions pc' registers' rx' tx'
    | otherwise = (pc, registers, rx, tx) 
    where
        runnable = (pc >= 0) && (pc < (V.length instructions)) && (pc' /= pc)
        instruction = instructions ! pc
        pc' = pc + pcOffset
        registers' = M.union (M.fromList registerValue) registers
        tx' = tx ++ justTx
        rx'
            | null justRx = rx
            | otherwise = tail rx 
        (pcOffset, justRx, justTx, registerValue) = case instruction of
            (Snd reg)      -> (1, [], [value reg], [])
            (Set reg op)   -> withRegisterValue reg (const (val op))
            (Add reg op)   -> withRegisterValue reg (+ (val op))
            (Mul reg op)   -> withRegisterValue reg (* (val op))
            (Mod reg op)   -> withRegisterValue reg (`mod` (val op))
            (Rcv reg)
                | null rx   -> wait 
                | otherwise -> (1, [head rx], [], [(reg, head rx)])
            (Jgz cmp op)
                | (val cmp) > 0  -> ((val op), [], [], [])
                | otherwise         -> nop

        nop = (1, [], [], [])
        wait = (0, [], [], [])
        withRegisterValue reg f = (1, [], [], [(reg, f (value reg))])

        val (Val x) = x
        val (Reg x) = value x
        value reg = M.findWithDefault 0 reg registers

