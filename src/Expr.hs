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
    | Pair LispVal LispVal
    | Lambda [String] LispVal Env
    | BuiltinFunc ([LispVal] -> ThrowsError LispVal)

instance Show LispVal where
  show (Atom name) = name
  show (Number n) = show n
  show (Bool True) = "#t"
  show (Bool False) = "#f"
  show (String s) = "\"" ++ s ++ "\""
  show (List xs) = "(" ++ unwords (map show xs) ++ ")"
  show (Pair x y) = "(" ++ show x ++ " . " ++ show y ++ ")"
  show (Lambda params body _) =
    "(lambda (" ++ unwords params ++ ") " ++ show body ++ ")"
  show (BuiltinFunc _) = "<builtin function>"

instance Eq LispVal where
  (Atom a) == (Atom b) = a == b
  (Number a) == (Number b) = a == b
  (Bool a) == (Bool b) = a == b
  (List a) == (List b) = a == b
  _ == _ = False

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

-- | Runs an `IOThrowsError` action and converts the result into an `IO String`.
-- This is useful for handling errors gracefully in the REPL, ensuring that
-- errors are displayed as strings rather than causing program crashes.
--
-- If the action succeeds, the result is returned as a string.
-- If the action fails, an error message is returned instead.
--
-- Example:
--
-- >>> runIOThrows (return "Success")
-- "Success"
--
-- >>> runIOThrows (throwError $ UnboundVar "Undefined variable: x")
-- "Error: UnboundVar \"Undefined variable: x\""
--
runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT action >>= return . extract
  where
    extract (Left err)  = "Error: " ++ show err
    extract (Right val) = val

-- | Converts a `ThrowsError` (a pure Either-based error) into an `IOThrowsError`.
-- This allows pure error-handling computations to be lifted into the IO-based
-- error monad (`IOThrowsError`).
--
-- This is especially useful when transitioning between pure computations
-- (like parsing or evaluation) and those that require IO (like variable storage).
--
-- Example:
--
-- >>> liftThrows (Right 42)
-- Returns `IOThrowsError 42`
--
-- >>> liftThrows (Left $ TypeMismatch "Expected number" (String "hello"))
-- Returns `IOThrowsError (TypeMismatch "Expected number" (String "hello"))`
--
liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err)  = throwError err
liftThrows (Right val) = return val
