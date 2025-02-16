module Expr where

import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Except

-- Lisp expression representation
data LispVal
    = Atom String
    | Number Integer
    | Bool Bool
    | String String
    | List [LispVal]
    | Lambda [String] LispVal Env -- user-defined function
    | BuiltinFunc ([LispVal] -> ThrowsError LispVal) -- built-in functions

instance Show LispVal where
  show (Atom name) = name
  show (Number n) = show n
  show (Bool True) = "#t"
  show (Bool False) = "#f"
  show (String s) = "\"" ++ s ++ "\""
  show (List xs) = "(" ++ unwords (map show xs) ++ ")"
  show (Lambda params body _) =
    "(lambda (" ++ unwords params ++ ") " ++ show body ++ ")"
  show (BuiltinFunc _) = "<builtin function>"

instance Eq LispVal where
  (Atom a) == (Atom b) = a == b
  (Number a) == (Number b) = a == b
  (Bool a) == (Bool b) = a == b
  (List a) == (List b) = a == b
  _ == _ = False  -- Functions and different types are not comparable

-- Environment for variable storage
type Env = IORef (Map String LispVal)

-- Create a new environment
nullEnv :: IO Env
nullEnv = newIORef Map.empty

-- Error handling
data LispError
    = UnboundVar String
    | TypeMismatch String LispVal
    | BadSpecialForm String LispVal
    | NotAFunction String
    | NumArgs Int [LispVal]
    | ParserError String
    deriving (Show)

type ThrowsError = Either LispError
type IOThrowsError = ExceptT LispError IO

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT action >>= return . extract
  where
    extract (Left err)  = "Error: " ++ show err
    extract (Right val) = val

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err)  = throwError err
liftThrows (Right val) = return val
