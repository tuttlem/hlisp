module Eval where

import Expr
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, readIORef, writeIORef, newIORef)
import qualified Data.Map as Map

-- Look up variable in environment
lookupVar :: Env -> String -> IOThrowsError LispVal
lookupVar envRef var = do
    env <- liftIO $ readIORef envRef
    case Map.lookup var env of
        Just val -> return val
        Nothing  -> throwError $ UnboundVar ("Undefined variable: " ++ var)

-- Define a new variable in the environment
defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var val = do
    env <- liftIO $ readIORef envRef
    liftIO $ writeIORef envRef (Map.insert var val env)
    return val

-- Update an existing variable
setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var val = do
    env <- liftIO $ readIORef envRef
    case Map.lookup var env of
        Just _  -> liftIO $ writeIORef envRef (Map.insert var val env)
        Nothing -> throwError $ UnboundVar ("Setting undefined variable: " ++ var)
    return val

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
    env <- liftIO $ readIORef envRef
    case Map.lookup var env of
        Just val -> return val
        Nothing  -> throwError $ UnboundVar ("Undefined variable: " ++ var)

-- Apply a function (either built-in or user-defined)
apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (BuiltinFunc f) args = liftThrows $ f args
apply (Lambda params body closure) args = do
    env <- liftIO $ readIORef closure
    if length params == length args
        then eval closure body
        else throwError $ NumArgs (length params) args
apply notFunc _ = throwError $ NotAFunction (show notFunc)

-- Evaluator function
eval :: Env -> LispVal -> IOThrowsError LispVal
eval env (Atom var) = getVar env var
eval _ val@(Number _) = return val
eval _ val@(Bool _) = return val
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "define", Atom var, expr]) = do
    val <- eval env expr
    defineVar env var val
eval _ val@(String _) = return val
eval env (List [Atom "set!", Atom var, expr]) = do
    val <- eval env expr
    setVar env var val
eval env (List [Atom "if", condition, thenExpr, elseExpr]) = do
    result <- eval env condition
    case result of
        Bool True  -> return thenExpr
        Bool False -> return elseExpr
        _          -> throwError $ TypeMismatch "Expected boolean in if condition" result
eval env (List (Atom func : args)) = do
    func' <- getVar env func
    args' <- mapM (eval env) args
    apply func' args'
eval _ badForm = throwError $ BadSpecialForm "Unrecognized form" badForm


-- | Built-in function table
primitives :: [(String, LispVal)]
primitives =
  [ ("+", BuiltinFunc numericAdd),
    ("-", BuiltinFunc numericSub),
    ("*", BuiltinFunc numericMul),
    ("/", BuiltinFunc numericDiv),
    ("<", BuiltinFunc compareLessThan),
    (">", BuiltinFunc compareGreaterThan),
    ("=", BuiltinFunc compareEquals),
    ("<=", BuiltinFunc compareLessThanEq),
    (">=", BuiltinFunc compareGreaterThanEq),
    ("not", BuiltinFunc logicalNot),
    ("and", BuiltinFunc logicalAnd),
    ("or", BuiltinFunc logicalOr),
    ("xor", BuiltinFunc logicalXor)
  ]


-- | Basic integer math
numericAdd, numericSub, numericMul, numericDiv :: [LispVal] -> ThrowsError LispVal
numericAdd [Number a, Number b] = Right $ Number (a + b)
numericAdd args = Left $ TypeMismatch "Expected numbers" (List args)

numericSub [Number a, Number b] = Right $ Number (a - b)
numericSub args = Left $ TypeMismatch "Expected numbers" (List args)

numericMul [Number a, Number b] = Right $ Number (a * b)
numericMul args = Left $ TypeMismatch "Expected numbers" (List args)

numericDiv [Number a, Number b] =
  if b == 0 then Left $ TypeMismatch "Division by zero" (Number b)
  else Right $ Number (a `div` b)
numericDiv args = Left $ TypeMismatch "Expected numbers" (List args)

-- | General Comparison Functions
compareLessThan, compareGreaterThan, compareEquals, compareLessThanEq, compareGreaterThanEq :: [LispVal] -> ThrowsError LispVal

compareLessThan [Number a, Number b] = return $ Bool (a < b)
compareLessThan [String a, String b] = return $ Bool (a < b)
compareLessThan args = throwError $ TypeMismatch "Expected numbers or strings" (List args)

compareGreaterThan [Number a, Number b] = return $ Bool (a > b)
compareGreaterThan [String a, String b] = return $ Bool (a > b)
compareGreaterThan args = throwError $ TypeMismatch "Expected numbers or strings" (List args)

compareEquals [Number a, Number b] = return $ Bool (a == b)
compareEquals [String a, String b] = return $ Bool (a == b)
compareEquals args = throwError $ TypeMismatch "Expected numbers or strings" (List args)

compareLessThanEq [Number a, Number b] = return $ Bool (a <= b)
compareLessThanEq [String a, String b] = return $ Bool (a <= b)
compareLessThanEq args = throwError $ TypeMismatch "Expected numbers or strings" (List args)

compareGreaterThanEq [Number a, Number b] = return $ Bool (a >= b)
compareGreaterThanEq [String a, String b] = return $ Bool (a >= b)
compareGreaterThanEq args = throwError $ TypeMismatch "Expected numbers or strings" (List args)

-- | Boolean Logic Functions
logicalNot :: [LispVal] -> ThrowsError LispVal
logicalNot [Bool b] = return $ Bool (not b)
logicalNot [val] = throwError $ TypeMismatch "Expected boolean" val
logicalNot args = throwError $ NumArgs 1 args

logicalAnd, logicalOr, logicalXor :: [LispVal] -> ThrowsError LispVal
logicalAnd args = return $ Bool (all isTruthy args)
logicalOr args = return $ Bool (any isTruthy args)
logicalXor args =
    let countTrue = length (filter isTruthy args)
    in return $ Bool (countTrue == 1)

-- Helper function to determine truthiness
isTruthy :: LispVal -> Bool
isTruthy (Bool False) = False
isTruthy (Number 0) = False
isTruthy (String "") = False
isTruthy _ = True

-- Initialize environment
primitiveEnv :: IO Env
primitiveEnv = newIORef (Map.fromList primitives)
