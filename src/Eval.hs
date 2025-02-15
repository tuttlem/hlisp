module Eval where

import Expr
import Control.Monad.Except
import qualified Data.Map as Map

-- Look up variable in environment
lookupVar :: Env -> String -> ThrowsError LispVal
lookupVar env var = case Map.lookup var env of
  Just val -> Right val
  Nothing -> Left $ UnboundVar var

-- Apply a function (either built-in or user-defined)
apply :: LispVal -> [LispVal] -> ThrowsError LispVal
apply (BuiltinFunc f) args = f args
apply (Lambda params body closure) args =
  if length params == length args
    then eval (Map.union (Map.fromList (zip params args)) closure) body
    else Left $ NumArgs (length params) args
apply notFunc _ = Left $ NotAFunction (show notFunc)

-- Evaluator function
eval :: Env -> LispVal -> ThrowsError LispVal
eval env (Atom var) = lookupVar env var
eval _ val@(Number _) = Right val
eval _ val@(Bool _) = Right val
eval env (List [Atom "quote", val]) = Right val
eval env (List (Atom func : args)) = do
  func' <- eval env (Atom func)
  args' <- mapM (eval env) args
  apply func' args'
eval _ badForm = Left $ BadSpecialForm "Unrecognized form" badForm

-- Sample built-in functions
primitives :: [(String, LispVal)]
primitives =
  [ ("+", BuiltinFunc numericAdd),
    ("-", BuiltinFunc numericSub),
    ("*", BuiltinFunc numericMul),
    ("/", BuiltinFunc numericDiv)
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

-- Initialize environment
primitiveEnv :: Env
primitiveEnv = Map.fromList primitives
