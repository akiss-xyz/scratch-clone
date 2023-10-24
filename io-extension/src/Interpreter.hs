{-# LANGUAGE TupleSections, OverloadedLists #-}
--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Coursework 2: Scratch clone                                                --
--------------------------------------------------------------------------------
-- Note:                                                                      --
-- I had to change some of the surrounding code to work with this branch.     --
-- This means you might run into some issues with the server.                 --
-- * If the server doesn't respond, run `$ stack clean; stack run;`.          --
-- * If the server still doesn't respond, try uncommenting `void getChar` in  --
--   `exe/Main.hs`. This means testing the IO will be a bit harder, but hey.  --
--                                                                            --
-- Extensions in this branch:                                                 --
-- * IO functions.                                                            --
--   * Building off of the functions introduced in the previous extension, I  --
--     thought adding IO to the language would make it a bit more interesting --
--     and open up new possibilities.                                         --
--     However, in order to implement this, much of the code had to be        --
--     changed, so that functions such as `execute` and `interpret` now work  --
--     in the IO monad.                                                       --
--     Consequently, the return types of such functions now reflect this -    --
--     but furthermore, some of the surrounding code had to be updated to     --
--     work with the updated return type.                                     --
--     Hence, some things may not work as expected - please excuse this, for  --
--     example I didn't really have the time to make the benchmarking suite   --
--     work for this branch.                                                  --
--                                                                            --
--     IO is implemented by via a new kind of statement, the BuiltinStmt,     --
--     which corresponds to action that the interpreter should carry out on   --
--     behalf of the code. Since BuiltinStmts cannot be accessed by the code  --
--     itself, we instead provide a 'standard library' of functions that do   --
--     have access to these statements.                                       --
--                                                                            --
--     Currently, there are two functions implemented:                        --
--       * `s.write`, which takes the value of the variable `char` and prints --
--         it to stdout.                                                      --
--       * `s.read`, which takes a character from stdin and sets the variable --
--         `char` to the `ord` value of that character.                       --
--                                                                            --
--     With this, we can now do basically arbitrary IO, by redirecting input  --
--     and output from the server!                                            --
--     A short (and barely functional) program is included for demonstration  --
--     - it is a simple interactive calculator that works with single-digit   --
--     numbers. You simply type 32+, and the calculator will print out 5.     --
--     You can find the loadable XML in the file `calc.xml`.                  --
--                                                                            --
-- * The Morenad.                                                             --
--   * Not really an extension, just an interesting way to make what used to  --
--     be not very elegant at all into something that's a lot more readable.  --
--     You can find more details in `Morenad.hs`.                             --
--                                                                            --
-- You can find some simple example scratch programs in the root of this      --
-- branch. `example-single-digit-calc.xml` is a simple calculator that        --
-- evaluates expressions such as `23+` -> 5, or `31*` -> 3.                   --
--------------------------------------------------------------------------------

module Interpreter where

--------------------------------------------------------------------------------

import Language
import Morenad

import Prelude hiding (lookup)
import Control.Monad (foldM)
import Control.Monad.Trans.Class

import Data.Char
import Data.Map (Map, fromList, toList)
import qualified Data.Map as Map

--------------------------------------------------------------------------------

-- | In our memory, named locations map to values.
type Memory = [(String, Int)]
-- | Data is the internal represenation of `Memory` for faster lookup.
type Data = Map String Int
-- | Definitions contain the functions used by the program. The reason this is a
-- seperate store is so that we can easily seperate out the function definitions
-- from the actual memory output.
type Definitions = Map String Program
-- 
type Storage = (Data, Definitions)

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
-- | `execute` takes a statement, and some initial memory & definitions, and does
-- what that statement asks, returning an IO (Either Err Storage) in a Morenad.
execute :: Stmt -> Storage -> Morenad IO (Either Err) Storage
-- We simply evaluate e, (lift it and) bind the result, and just add that result
-- to the memory.
-- The (,d) section is used to put the changed memory back into Storage.
execute (AssignStmt v e) (m, d) = lift $ eval e (m, d) >>= pure . (,d) . add v m
-- This fn is the same (more or less) as the one in the original. We evaluate
-- the condition:
--  * If it's true, we innerpret the body.
--  * If it's false, we just go to the next part of the if statement.
execute (IfStmt cond body elifs els) mem =
    do v <- lift $ eval cond mem
       case v of
           0 -> case elifs of
                   ((cond', body'):r) -> execute (IfStmt cond' body' r els) mem
                   _                  -> innerpret els mem
           _ -> innerpret body mem
-- To repeat a statement, we first just evaluate the n(umber) of times desired,
-- and then use foldM to innerpret c(ount) times.
-- With foldM, the resulting memory from the previous call is automatically
-- given to the current call to innerpret.
-- The list we fold over is simply `take c (repeat w)`, because we just want to
-- repeat w(hat) was specified c(ount) times.
execute (RepeatStmt n w) m = 
    lift (eval n m) >>= \c -> foldM (flip innerpret) m $ take c (repeat w)
-- We simply add the function body - which is just a program - to Storage.
execute (FnStmt n p) (mem, def) = return (mem, add n def p)
-- To call a function, we lookup the program in the definitions and innerpret it
execute (CallStmt n) (m, d) = lift (lookup n d) >>= flip innerpret (m, d)
-- To write a character, we look up the value of the variable `char`. We take
-- that integer, and output it's character representation, returning the
-- unchanged storage afterwards.
execute (BuiltinStmt Write) (mem, def) = 
    lift (lookup "char" mem) >>= wrap . putChar . chr >>= pure . pure (mem, def)
-- To read a character, we use getChar, and put the recieved value `c` into
-- memory. 
execute (BuiltinStmt Read) (mem, def) = 
    wrap $ getChar >>= \c -> return (add "char" mem (ord c), def)
-- endregion

--------------------------------------------------------------------------------

-- region stdLib
-- The standard library contains the two functions for read and write - the user
-- needs some way to be able to interface with the `BuiltingStmt`s above, and I
-- didn't want to make these feel like something 'fake' by having a special
-- block for them. Hence, they're just functions that are defined by default!
stdLib :: Definitions
stdLib = [
             ("s.write", [ BuiltinStmt Write ])
         ,   ("s.read",  [ BuiltinStmt Read ])
         ];
-- By the by, I've always wondered why Haskellers write the comma at the start
-- in cases like these - any particular reason?
-- endregion

--------------------------------------------------------------------------------

-- | Given a program, and some Storage (a.k.a Memory and Definitions),
-- recursively performs the program statement by statement.
--
-- Morenad is used here to allow us to 'reach into' IO (Either Err Storage) in a
-- neat way without having annoying case statements everywhere!
innerpret :: Program -> Storage -> Morenad IO (Either Err) Storage
innerpret (s:ss) st = execute s st >>= innerpret ss
innerpret _      st = return st

-- | Given a program and the initial memory contents, determines
-- what evaluating the program does to the memory.
--
-- This function really only exists to wrap (with fromList and stdLib) and
-- unwrap (take the memory out of the Storage type, unwrap the Morenad with
-- lessnad) the innerpret function.
interpret :: Program -> [(String, Int)] -> IO (Either Err [(String, Int)])
interpret ss m = lessnad $ toList <$> fst <$> innerpret ss (fromList m, stdLib)

--------------------------------------------------------------------------------
