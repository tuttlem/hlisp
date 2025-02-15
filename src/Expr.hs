module Expr where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Except

-- Lisp expression representation
data LispVal
    = Atom String
    | Number Integer
    | Bool Bool
    | List [LispVal]
    | Lambda [String] LispVal Env -- user-defined function
    | BuiltinFunc ([LispVal] -> ThrowsError LispVal) -- built-in functions

instance Show LispVal where
  show (Atom name) = name
  show (Number n) = show n
  show (Bool True) = "#t"
  show (Bool False) = "#f"
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
type Env = Map String LispVal

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

