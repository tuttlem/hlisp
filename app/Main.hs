module Main where

import Eval
import Parser
import Expr
import Control.Monad
import System.Environment (getArgs)
import System.IO

-- | Read and evaluate a Lisp file
runFile :: Env -> String -> IO ()
runFile env filename = do
    content <- readFile filename
    case readExprList content of
        Left err -> putStrLn $ "Parse error: " ++ show err
        Right exprs -> mapM_ (runAndPrint env) exprs
  where
    runAndPrint :: Env -> LispVal -> IO ()
    runAndPrint env expr = do
        result <- runIOThrows (fmap show (eval env expr))
        return ()


-- | Read and evaluate input from STDIN (for pipes)
runStdin :: Env -> IO ()
runStdin env = do
    content <- getContents  -- Reads all input from stdin
    result <- runIOThrows (liftM show (readExpr content >>= eval env))
    putStrLn result

-- | Interactive REPL loop
repl :: Env -> IO ()
repl env = do
  putStr "λ> "
  hFlush stdout
  input <- getLine
  unless (input == "exit") $ do
    result <- runIOThrows (liftM show (readExpr input >>= eval env))
    putStrLn result
    repl env

main :: IO ()
main = do
    args <- getArgs
    env <- primitiveEnv
    case args of
        []      -> do
            putStrLn "Welcome to Mini Lisp (Haskell)"
            repl env
        ["-"]   -> runStdin env  -- Read from stdin when "-" is provided
        [file]  -> runFile env file  -- Run a Lisp file
        _       -> putStrLn "Usage: hlisp [file.lisp | -]"
