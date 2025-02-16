module Eval where

import Expr
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, readIORef, writeIORef, newIORef)
import qualified Data.Map as Map

-- Look up variable in environment
lookupVar :: Env -> String -> IOThrowsError LispVal
lookupVar envRef var = do
    env <- liftIO $ readIORef envRef  -- Read the env (which is in IO)
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
eval env (List [Atom "quote", val]) = return val  -- Fixed
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
        Bool True  -> return thenExpr  -- ✅ Return without evaluating again
        Bool False -> return elseExpr  -- ✅ Return without evaluating again
        _          -> throwError $ TypeMismatch "Expected boolean in if condition" result
eval env (List (Atom func : args)) = do
    func' <- getVar env func  -- Look up function name in the environment
    args' <- mapM (eval env) args  -- Evaluate arguments
    apply func' args'
eval _ badForm = throwError $ BadSpecialForm "Unrecognized form" badForm


-- Sample built-in functions
primitives :: [(String, LispVal)]
primitives =
  [ ("+", BuiltinFunc numericAdd),
    ("-", BuiltinFunc numericSub),
    ("*", BuiltinFunc numericMul),
    ("/", BuiltinFunc numericDiv),
    ("<", BuiltinFunc numericLessThan),
    (">", BuiltinFunc numericGreaterThan),
    ("=", BuiltinFunc numericEquals),
    ("<=", BuiltinFunc numericLessThanEq),
    (">=", BuiltinFunc numericGreaterThanEq),
    ("not", BuiltinFunc notFunc),
    ("and", BuiltinFunc booleanAnd),
    ("or", BuiltinFunc booleanOr),
    ("xor", BuiltinFunc booleanXor)
  ]

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

numericLessThan, numericGreaterThan, numericEquals, numericLessThanEq, numericGreaterThanEq :: [LispVal] -> ThrowsError LispVal

numericLessThan [Number a, Number b] = return $ Bool (a < b)
numericLessThan args = throwError $ TypeMismatch "Expected numbers" (List args)

numericGreaterThan [Number a, Number b] = return $ Bool (a > b)
numericGreaterThan args = throwError $ TypeMismatch "Expected numbers" (List args)

numericEquals [Number a, Number b] = return $ Bool (a == b)
numericEquals [String a, String b] = return $ Bool (a == b)
numericEquals args = throwError $ TypeMismatch "Expected numbers" (List args)

numericLessThanEq [Number a, Number b] = return $ Bool (a <= b)
numericLessThanEq args = throwError $ TypeMismatch "Expected numbers" (List args)

numericGreaterThanEq [Number a, Number b] = return $ Bool (a >= b)
numericGreaterThanEq args = throwError $ TypeMismatch "Expected numbers" (List args)

notFunc :: [LispVal] -> ThrowsError LispVal
notFunc [Bool b] = return $ Bool (not b)
notFunc [val] = throwError $ TypeMismatch "Expected boolean" val
notFunc args = throwError $ NumArgs 1 args

booleanAnd, booleanOr, booleanXor :: [LispVal] -> ThrowsError LispVal
booleanAnd args = return $ Bool (all isTruthy args)
booleanOr args = return $ Bool (any isTruthy args)
booleanXor args =
    let countTrue = length (filter isTruthy args)
    in return $ Bool (countTrue == 1)

-- Helper function to determine truthiness
isTruthy :: LispVal -> Bool
isTruthy (Bool False) = False
isTruthy _ = True

-- Initialize environment
primitiveEnv :: IO Env
primitiveEnv = newIORef (Map.fromList primitives)
