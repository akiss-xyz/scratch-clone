{-# LANGUAGE TupleSections #-}
--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Coursework 2: Scratch clone                                                --
--------------------------------------------------------------------------------

module Interpreter where

--------------------------------------------------------------------------------

import Language
import Prelude hiding (lookup)
import Control.Monad (foldM)

--------------------------------------------------------------------------------

-- | In our memory, named locations map to values.
type Memory = [(String, Int)]
type Definitions = [(String, Program)]

-- | Enumerates reasons for errors.
data Err
    = DivByZeroError                    -- ^ Division by zero was attempted.
    | NegativeExponentError             -- ^ Raising a number to a negative
                                        -- exponent was attempted.
    | UninitialisedMemory String        -- ^ Tried to read from a variable
                                        -- that does not exist.
    deriving (Eq, Show)

--------------------------------------------------------------------------------

-- region Utils
-- lookup :: Eq k => k -> [(k, a)] -> Either Err a
lookup :: Eq k => Show k => k -> [(k, a)] -> Either Err a
lookup id ((s, v):rest) | id == s   = Right v
                        | otherwise = lookup id rest
-- TODO: overlapping patterns?
lookup id  [(s, v)]     | id == s   = Right v
                        | otherwise = Left (UninitialisedMemory (show id))
lookup id  []                       = Left (UninitialisedMemory (show id))

modify :: Eq k => k -> [(k, a)] -> a -> [(k, a)]
modify id ((s, w):rest) v  | id == s   = (id, v):rest
                           | otherwise = (s, w):(modify id rest v)
modify id _             v              = [(id, v)]

safestDiv :: Int -> Int -> Either Err Int
safestDiv l 0 = Left DivByZeroError
safestDiv l r = Right (l `div` r)

safestPow :: Int -> Int -> Either Err Int
safestPow l r | r < 0     = Left NegativeExponentError
              | otherwise = Right (l ^ r)

bool :: Bool -> Int
bool True = 1
bool _    = 0
-- endregion

eval :: Expr -> (Memory, Definitions) -> Either Err Int
eval (ValE i) _                      = Right i
eval (VarE s) (mem, def)             = lookup s mem
eval (BinOpE Add l r) mem            = (+) <$> eval l mem <*> eval r mem
eval (BinOpE Sub l r) mem            = (-) <$> eval l mem <*> eval r mem
eval (BinOpE Mul l r) mem            = (*) <$> eval l mem <*> eval r mem
eval (BinOpE Div l r) mem            = do l <- eval l mem
                                          r <- eval r mem
                                          safestDiv l r
eval (BinOpE Pow l r) mem            = do l <- eval l mem
                                          r <- eval r mem
                                          safestPow l r
eval (BinOpE Equal l r) mem          = do l <- eval l mem
                                          r <- eval r mem
                                          return (bool $ l == r)
eval (BinOpE Neq l r) mem            = do l <- eval l mem
                                          r <- eval r mem
                                          return (bool $ l /= r)
eval (BinOpE LessThan l r) mem       = do l <- eval l mem
                                          r <- eval r mem
                                          return (bool $ l < r)
eval (BinOpE LessOrEqual l r) mem    = do l <- eval l mem
                                          r <- eval r mem
                                          return (bool $ l <= r)
eval (BinOpE GreaterThan l r) mem    = do l <- eval l mem
                                          r <- eval r mem
                                          return (bool $ l > r)
eval (BinOpE GreaterOrEqual l r) mem = do l <- eval l mem
                                          r <- eval r mem
                                          return (bool $ l >= r)
-- eval (BinOpE Equal l r) mem          = (bool $ (==)) <$> eval l mem <*> eval r mem
-- eval (BinOpE Neq l r) mem            = (-) <$> eval l mem <*> eval r mem
-- eval (BinOpE LessThan l r) mem       = (<) <$> eval l mem <*> eval r mem
-- eval (BinOpE LessOrEqual l r) mem    = (<=) <$> eval l mem <*> eval r mem
-- eval (BinOpE GreaterThan l r) mem    = (>) <$> eval l mem <*> eval r mem
-- eval (BinOpE GreaterOrEqual l r) mem = (>=) <$> eval l mem <*> eval r mem
-- -- eval (BinOpE Add l r) mem            = eval l mem >>= eval r mem >>= pure . (+)
-- eval (BinOpE Sub l r) mem            = eval l mem >>= eval r mem >>= Right . (-)
-- eval (BinOpE Mul l r) mem            = eval l mem >>= eval r mem >>= (*)
-- eval (BinOpE Div l r) mem            = eval l mem >>= eval r mem >>= safestDiv
-- eval (BinOpE Pow l r) mem            = eval l mem >>= eval r mem >>= (^)
-- eval (BinOpE Equal l r) mem          = eval l mem >>= eval r mem >>= (==)
-- eval (BinOpE Neq l r) mem            = eval l mem >>= eval r mem >>= (/=)
-- eval (BinOpE LessThan l r) mem       = eval l mem >>= eval r mem >>= (<)
-- eval (BinOpE LessOrEqual l r) mem    = eval l mem >>= eval r mem >>= (<=)
-- eval (BinOpE GreaterThan l r) mem    = eval l mem >>= eval r mem >>= (>)
-- eval (BinOpE GreaterOrEqual l r) mem = eval l mem >>= eval r mem >>= (>=)
--eval _                           = UninitialisedMemory "Eval doesn't support this yet"


-- modify :: String -> Memory -> Int -> Memory
execute :: Stmt -> (Memory, Definitions) -> Either Err (Memory, Definitions)
execute (AssignStmt v e) (m, d) = eval e (m, d) >>= Right . (,d) . modify v m
execute (IfStmt cond body elifs el) mem = do
    v <- eval cond mem
    case v of 
        0 -> case elifs of
                ((c, ss):r) -> execute (IfStmt c ss r el) mem
                _           -> innerpret el mem
        _ -> innerpret body mem
-- This is technically a one-liner if you have your column width at 100!
execute (RepeatStmt t w) m = eval t m >>= \c -> foldM (flip innerpret) m (take c (repeat w))
execute (FnStmt n p) (mem, def) = Right (mem, modify n def p)
execute (CallStmt n) (mem, def) = lookup n def >>= flip innerpret (mem, def)
-- TODO: I might just flip innerpret at this point.

execute (BuiltinStmt Write) (mem, def) = do
    putStrLn "Hi"
    Right (mem, def)
-- | Given a program and the initial memory contents, determines
-- what evaluating the program does to the memory.

standardLibrary :: Definitions
standardLibrary = [
                      ("s.write", [ BuiltinStmt Write ]),
                      ("s.read",  [ BuiltinStmt Read ])
                  ];

innerpret :: Program -> (Memory, Definitions) -> Either Err (Memory, Definitions)
innerpret []     (mem, def) = Right (mem, def)
innerpret (s:ss) (mem, def) = execute s (mem, def) >>= innerpret ss

interpret :: Program -> Memory -> Either Err Memory
interpret ss mem = innerpret ss (mem, []) >>= Right . fst

juicer :: [(String, Int)]
juicer = [("x",0),("y",0),("z",1)]

-- juicer = fib 3
-- (first:rest) = juicer
--------------------------------------------------------------------------------
