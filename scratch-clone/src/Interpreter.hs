{-# LANGUAGE TupleSections #-}
--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Coursework 2: Scratch clone                                                --
--------------------------------------------------------------------------------
-- Extensions in this branch:                                                 --
-- * Functions                                                                --
--   * Functions are created using a block I created. You give them a string  --
--     name, and they simply encapsulate an arbitrary program.                --
--     They have no named parameters or return values, but instead have       --
--     access to the entire global memory of the program. Hence, named        --
--     parameters and return values can be emulated however you like.         --
--                                                                            --
--     Since functions have access to the global store of definitions and     --
--     data, they can do crazy things such as:                                --
--      * Modifying global variables.                                         --
--      * Varying state between function calls.                               --
--      * Declare functions.                                                  --
--      * Re-declare/change functions.                                        --
--      * Re-declare themselves.                                              --
--      * Recursion!                                                          --
--   * Functions are called using another statement block. You simply type in --
--     the name of the function, and the corresponding program is visited in  --
--     memory, and interpreted.                                               --
-- * Variables and definitions are stored in a Map data structure (courtesy   --
--   of Data.Map) for faster lookup.                                          --
--   Therefore, you'll see that the implementation uses the type Data instead --
--   of Memory, as was originally done.                                       --
--------------------------------------------------------------------------------

module Interpreter where

--------------------------------------------------------------------------------

import Language

import Prelude hiding (lookup)
import Control.Monad (foldM)

import Data.Map (Map, fromList, toList)
import qualified Data.Map as Map

--------------------------------------------------------------------------------

-- | In our memory, named locations map to values.
type Memory      = [(String, Int)]
-- | Data is the internal represenation of `Memory` for faster lookup.
type Data        = Map String Int
-- | Definitions contain the functions used by the program. The reason this is a
-- seperate store is so that we can easily seperate out the function definitions
-- from the actual memory output.
type Definitions = Map String Program

-- | Enumerates reasons for errors.
data Err = DivByZeroError | NegativeExponentError | UninitialisedMemory String
           deriving (Eq, Show)

--------------------------------------------------------------------------------

-- region Utils
-- | add is just a wrapper around the Map.insert function for elegance's sake.
-- Helps express some things more succinctly if the value is the last argument,
-- and I also feel it is more natural to have the arguments in this order.
add :: Ord k => k -> Map k v -> v -> Map k v
add key map val = Map.insert key val map

-- | `lookup`, too, is just a wrapper, however it's use is a bit more concrete -
-- it's used to embellish the values returned by `Map.lookup` so that we get the
-- error reporting we want.
-- Yes, lookup is more general than this. But if I make this more general, I'd
-- need a Show constraint on the key, but then the call to show puts extra
-- quotes around the variable name, which fails the tests.
lookup :: String -> Map String a -> Either Err a
lookup key map = case Map.lookup key map of
                     Nothing  -> Left $ UninitialisedMemory $ key
                     Just val -> Right val

-- | `safestDiv` and `safestPow` wrap around their respective operations, again
-- to give us the error messages we want.
safestDiv :: Int -> Int -> Either Err Int
safestDiv l 0 = Left DivByZeroError
safestDiv l r = Right $ l `div` r

safestPow :: Int -> Int -> Either Err Int
safestPow l r | r < 0     = Left NegativeExponentError
              | otherwise = Right $ l ^ r

-- | `bool` is a short utility function so that we can use Haskell's operators
-- to implement the operators used by the language, just by wrapping them in a
-- call to `bool`. This way, we can store these in `Data` as integers.
bool :: Bool -> Int
bool True = 1
bool _    = 0
-- endregion

--------------------------------------------------------------------------------

-- region eval
-- | `eval` deals with expressions - given an expression and some memory, `eval`
-- will try to find the value of that expression recursively.
-- Most of these definitions proceed by evaluating the left and right
-- subexpressions, and then combining the results to produce an answer.
eval :: Expr -> (Data, Definitions) -> Either Err Int
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
-- endregion

--------------------------------------------------------------------------------

-- region execute
-- | Given a program and the initial memory contents, determines
-- what evaluating the program does to the memory.
execute :: Stmt -> (Data, Definitions) -> Either Err (Data, Definitions)
-- We simply evaluate the e(xpression), and then modify the m(emory)
-- accordingly. The section (,d) is used to build up the required (Memory,
-- Definitions) pair.
execute (AssignStmt v e) (m, d) = eval e (m, d) >>= Right . (,d) . add v m
-- For an if statement, we just evaluate the cond(ition). If it's true, we
-- evaluate the body. Otherwise, we recurse - go one level deeper, depending on
-- whether or not we have more else-ifs to look at.
execute (IfStmt cond body elifs el) mem = do
    v <- eval cond mem
    case v of 
        0 -> case elifs of
                ((c, ss):r) -> execute (IfStmt c ss r el) mem
                _           -> innerpret el mem
        _ -> innerpret body mem
-- To repeat a statement, we evaluate the n(umber) of times requested, and use
-- foldM to call innerpret c(ount) times.
-- foldM automatically sequences `flip innerpret`, so that the resulting memory
-- from the previous `innerpret` is passed to the current one, while still using
-- our monad bind so that errors can still be propagated.
-- You can see that the list we're folding over is `take c (repeat p)` - this
-- makes sense since we want to repeat the same p(rogram) c(ount) times. In this
-- way, we can succinctly express repeated sequential composition over the same
-- input.
execute (RepeatStmt n p) m =
    eval n m >>= \c -> foldM (flip innerpret) m (take c (repeat p))
-- A FnStmt declares a function, using a n(ame) and a p(rogram) - the body of
-- the function.
-- Therefore, to 'execute' a function declaration, we just add the program to
-- our definitions.
execute (FnStmt n p) (mem, def) = Right (mem, add n def p)
-- A CallStmt evaluates a function using the current memory contents.
-- So, we just find the stored p(rogram)/function body (see above) in our
-- definitions, and `innerpret` that program with the current memory state.
-- Interestingly, in this way, functions can declare and redeclare themselves -
-- or call themselves - or change/read 'static' variables that'll have an effect
-- on how they behave later.
execute (CallStmt n) (mem, def) = lookup n def >>= flip innerpret (mem, def)
-- endregion

--------------------------------------------------------------------------------

-- | `interpret` is just a wrapper around the `innerpret` function so that we can
-- deal with the `Data` type instead of the usual `Memory` - in this way, the
-- conversion to and from the `Data.Map` type is only done once.
-- We simply evaluate the program a statement at a time, continuing recursively.
innerpret :: Program -> (Data, Definitions) -> Either Err (Data, Definitions)
innerpret (s:ss) (mem, def) = execute s (mem, def) >>= innerpret ss
innerpret _      (mem, def) = Right (mem, def)

-- | `interpret` is just a wrapper around `innerpret` for converting between
-- Data.Map and a regular list.
interpret :: Program -> Memory -> Either Err Memory
interpret ss mem = toList . fst <$> innerpret ss (fromList mem, fromList [])

--------------------------------------------------------------------------------
