module Main where

import Eval
import Parser
import Expr
import Control.Monad
import System.IO

-- REPL loop
repl :: Env -> IO ()
repl env = do
  putStr "λ> "
  hFlush stdout
  input <- getLine
  unless (input == "exit") $ do
    evaled <- runIOThrows $ liftM show (readExpr input >>= eval env)
    putStrLn evaled
    repl env



main :: IO ()
main = do
    env <- primitiveEnv  -- Create a new environment
    putStrLn "Welcome to Mini Lisp (Haskell)"
    repl env
