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
    case readExpr input >>= eval env of
      Left err -> print err
      Right val -> print val
    repl env

main :: IO ()
main = do
  putStrLn "Welcome to Mini Lisp (Haskell)"
  repl primitiveEnv
