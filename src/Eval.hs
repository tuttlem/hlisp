module Eval where

import Debug.Trace
import Expr
import Control.Monad (filterM, foldM)
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Data.Typeable (typeOf)
import Data.IORef (IORef, readIORef, writeIORef, newIORef, modifyIORef)
import Data.List (sortBy)
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
apply (BuiltinFuncIO f) args = f args
apply (Lambda params body closure) args = do
    env <- liftIO $ readIORef closure
    if length params == length args
        then do
            let localEnv = Map.union (Map.fromList (zip params args)) env
            newEnvRef <- liftIO $ newIORef localEnv
            eval newEnvRef body
        else throwError $ NumArgs (length params) args
apply notFunc _ = throwError $ NotAFunction (show notFunc)

-- Evaluator function
eval :: Env -> LispVal -> IOThrowsError LispVal
eval env (Atom var) = getVar env var
eval _ val@(Number _) = return val
eval _ val@(Float _) = return val
eval _ val@(Bool _) = return val
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "define", Atom var, expr]) = do
    val <- eval env expr
    defineVar env var val
eval env (List [Atom "define", List (Atom funcName : params), body]) = do
    let paramNames = map extractParam params
    defineVar env funcName (Lambda paramNames body env)
  where
    extractParam (Atom name) = name
    extractParam nonAtom = error $ "Expected parameter name, got: " ++ show nonAtom
eval _ val@(String _) = return val
eval env (List (Atom "let" : List bindings : body)) = do
    newEnv <- extendEnv env bindings
    last <$> mapM (eval newEnv) body
  where
    extendEnv :: Env -> [LispVal] -> IOThrowsError Env
    extendEnv envRef [] = return envRef
    extendEnv envRef (List [Atom var, expr] : rest) = do
        val <- eval envRef expr
        env <- liftIO $ readIORef envRef
        newEnvRef <- liftIO $ newIORef (Map.insert var val env)
        extendEnv newEnvRef rest
    extendEnv _ badForm = throwError $ BadSpecialForm "Invalid let binding" (List badForm)
eval env (List [Atom "set!", Atom var, expr]) = do
    val <- eval env expr
    setVar env var val
eval env (List [Atom "if", condition, thenExpr, elseExpr]) = do
    result <- eval env condition
    case result of
        Bool True  -> eval env thenExpr
        Bool False -> eval env elseExpr
        _          -> throwError $ TypeMismatch "Expected boolean in if condition" result
eval env (List [Atom "lambda", List params, body]) =
    case mapM getParamName params of
        Right paramNames -> return $ Lambda paramNames body env
        Left err -> throwError err
  where
    getParamName (Atom name) = Right name
    getParamName badArg = Left $ TypeMismatch "Expected parameter name" badArg
eval env (List (Atom func : args)) = do
    func' <- getVar env func
    args' <- mapM (eval env) args
    apply func' args'
eval _ badForm = do
    let typeName = case badForm of
            Atom name  -> "Atom: " ++ name
            Number n   -> "Number: " ++ show n
            Float f    -> "Float: " ++ show f
            Bool b     -> "Bool: " ++ show b
            String s   -> "String: " ++ show s
            List []    -> "Empty List"
            List (x:_) -> "List (first element: " ++ show x ++ ")"
            Pair a b   -> "Pair: (" ++ show a ++ " . " ++ show b ++ ")"
            Lambda _ _ _ -> "Lambda Function"
            BuiltinFunc _ -> "Builtin Function"
            BuiltinFuncIO _ -> "Builtin IO Function"
    throwError $ BadSpecialForm ("Unrecognized form of type: " ++ typeName) badForm




-- | Basic math
numericAdd, numericSub, numericMul, numericDiv :: [LispVal] -> ThrowsError LispVal
numericAdd [Number a, Number b] = return $ Number (a + b)
numericAdd [Float a, Float b] = return $ Float (a + b)
numericAdd [Number a, Float b] = return $ Float (fromIntegral a + b)
numericAdd [Float a, Number b] = return $ Float (a + fromIntegral b)
numericAdd args = throwError $ TypeMismatch "Expected numbers" (List args)

numericSub [Number a, Number b] = return $ Number (a - b)
numericSub [Float a, Float b] = return $ Float (a - b)
numericSub [Number a, Float b] = return $ Float (fromIntegral a - b)
numericSub [Float a, Number b] = return $ Float (a - fromIntegral b)
numericSub args = throwError $ TypeMismatch "Expected numbers" (List args)

numericMul [Number a, Number b] = return $ Number (a * b)
numericMul [Float a, Float b] = return $ Float (a * b)
numericMul [Number a, Float b] = return $ Float (fromIntegral a * b)
numericMul [Float a, Number b] = return $ Float (a * fromIntegral b)
numericMul args = throwError $ TypeMismatch "Expected numbers" (List args)

numericDiv [Number a, Number b]
    | b == 0    = throwError $ TypeMismatch "Division by zero" (Number b)
    | a `mod` b == 0 = return $ Number (a `div` b)
    | otherwise = return $ Float (fromIntegral a / fromIntegral b)
numericDiv [Float a, Float b]
    | b == 0    = throwError $ TypeMismatch "Division by zero" (Float b)
    | otherwise = return $ Float (a / b)
numericDiv [Number a, Float b]
    | b == 0    = throwError $ TypeMismatch "Division by zero" (Float b)
    | otherwise = return $ Float (fromIntegral a / b)
numericDiv [Float a, Number b]
    | b == 0    = throwError $ TypeMismatch "Division by zero" (Number b)
    | otherwise = return $ Float (a / fromIntegral b)
numericDiv args = throwError $ TypeMismatch "Expected numbers" (List args)

numericSin, numericCos, numericTan, numericExp, numericLog, numericSqrt :: [LispVal] -> ThrowsError LispVal
numericSin [Number a] = return $ Float (sin (fromIntegral a))
numericSin [Float a]  = return $ Float (sin a)
numericSin [arg]      = throwError $ TypeMismatch "Expected a number" arg
numericSin args       = throwError $ NumArgs 1 args

numericCos [Number a] = return $ Float (cos (fromIntegral a))
numericCos [Float a]  = return $ Float (cos a)
numericCos [arg]      = throwError $ TypeMismatch "Expected a number" arg
numericCos args       = throwError $ NumArgs 1 args

numericTan [Number a] = return $ Float (tan (fromIntegral a))
numericTan [Float a]  = return $ Float (tan a)
numericTan [arg]      = throwError $ TypeMismatch "Expected a number" arg
numericTan args       = throwError $ NumArgs 1 args

numericExp [Number a] = return $ Float (exp (fromIntegral a))
numericExp [Float a]  = return $ Float (exp a)
numericExp [arg]      = throwError $ TypeMismatch "Expected a number" arg
numericExp args       = throwError $ NumArgs 1 args

numericLog [Number a] =
    if a <= 0 then throwError $ TypeMismatch "Logarithm requires positive number" (Number a)
    else return $ Float (log (fromIntegral a))
numericLog [Float a] =
    if a <= 0 then throwError $ TypeMismatch "Logarithm requires positive number" (Float a)
    else return $ Float (log a)
numericLog [arg]      = throwError $ TypeMismatch "Expected a number" arg
numericLog args       = throwError $ NumArgs 1 args

numericSqrt [Number a] =
    if a < 0 then throwError $ TypeMismatch "Square root requires non-negative number" (Number a)
    else return $ Float (sqrt (fromIntegral a))
numericSqrt [Float a] =
    if a < 0 then throwError $ TypeMismatch "Square root requires non-negative number" (Float a)
    else return $ Float (sqrt a)
numericSqrt [arg]      = throwError $ TypeMismatch "Expected a number" arg
numericSqrt args       = throwError $ NumArgs 1 args

-- | General Comparison Functions
compareLessThan, compareGreaterThan, compareEquals, compareLessThanEq, compareGreaterThanEq :: [LispVal] -> ThrowsError LispVal

compareLessThan [Number a, Number b] = return $ Bool (a < b)
compareLessThan [Float a, Float b] = return $ Bool (a < b)
compareLessThan [Number a, Float b] = return $ Bool (fromIntegral a < b)
compareLessThan [Float a, Number b] = return $ Bool (a < fromIntegral b)
compareLessThan args = throwError $ TypeMismatch "Expected numbers" (List args)

compareGreaterThan [Number a, Number b] = return $ Bool (a > b)
compareGreaterThan [Float a, Float b] = return $ Bool (a > b)
compareGreaterThan [Number a, Float b] = return $ Bool (fromIntegral a > b)
compareGreaterThan [Float a, Number b] = return $ Bool (a > fromIntegral b)
compareGreaterThan args = throwError $ TypeMismatch "Expected numbers" (List args)

compareEquals [Number a, Number b] = return $ Bool (a == b)
compareEquals [Float a, Float b] = return $ Bool (a == b)
compareEquals [Number a, Float b] = return $ Bool (fromIntegral a == b)
compareEquals [Float a, Number b] = return $ Bool (a == fromIntegral b)
compareEquals args = throwError $ TypeMismatch "Expected numbers" (List args)

compareLessThanEq [Number a, Number b] = return $ Bool (a <= b)
compareLessThanEq [Float a, Float b] = return $ Bool (a <= b)
compareLessThanEq [Number a, Float b] = return $ Bool (fromIntegral a <= b)
compareLessThanEq [Float a, Number b] = return $ Bool (a <= fromIntegral b)
compareLessThanEq args = throwError $ TypeMismatch "Expected numbers" (List args)

compareGreaterThanEq [Number a, Number b] = return $ Bool (a >= b)
compareGreaterThanEq [Float a, Float b] = return $ Bool (a >= b)
compareGreaterThanEq [Number a, Float b] = return $ Bool (fromIntegral a >= b)
compareGreaterThanEq [Float a, Number b] = return $ Bool (a >= fromIntegral b)
compareGreaterThanEq args = throwError $ TypeMismatch "Expected numbers" (List args)

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

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List xs] = return $ List (x : xs)
cons [x, y]       = return $ Pair x y
cons args         = throwError $ NumArgs 2 args

car :: [LispVal] -> ThrowsError LispVal
car [List (x : _)] = return x
car [Pair x _]     = return x
car [List []]      = throwError $ TypeMismatch "Cannot take car of empty list" (List [])
car [arg]          = throwError $ TypeMismatch "Expected a pair or list" arg
car args           = throwError $ NumArgs 1 args

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_ : xs)] = return $ List xs
cdr [Pair _ y]      = return y
cdr [List []]       = throwError $ TypeMismatch "Cannot take cdr of empty list" (List [])
cdr [arg]           = throwError $ TypeMismatch "Expected a pair or list" arg
cdr args            = throwError $ NumArgs 1 args

isNull :: [LispVal] -> ThrowsError LispVal
isNull [List []] = return $ Bool True
isNull [_]       = return $ Bool False
isNull args      = throwError $ NumArgs 1 args

-- | Append two lists together
listAppend :: [LispVal] -> ThrowsError LispVal
listAppend [List xs, List ys] = return $ List (xs ++ ys)
listAppend [String a, String b] = return $ String (a ++ b)
listAppend [List xs, y] = return $ List (xs ++ [y])
listAppend [x, List ys] = return $ List ([x] ++ ys)
listAppend args = throwError $ TypeMismatch "Expected two lists or two strings" (List args)

-- | Get the length of a list
listLength :: [LispVal] -> ThrowsError LispVal
listLength [List xs] = return $ Number (toInteger (length xs))
listLength [arg] = throwError $ TypeMismatch "Expected a list" arg
listLength args = throwError $ NumArgs 1 args

-- | Reverse a list
listReverse :: [LispVal] -> ThrowsError LispVal
listReverse [List xs] = return $ List (reverse xs)
listReverse [arg] = throwError $ TypeMismatch "Expected a list" arg
listReverse args = throwError $ NumArgs 1 args

listMap :: [LispVal] -> IOThrowsError LispVal
listMap [BuiltinFunc f, List xs] = liftThrows $ List <$> mapM (\x -> f [x]) xs
listMap [Lambda params body env, List xs] =
    List <$> mapM (\x -> apply (Lambda params body env) [x]) xs
listMap args = throwError $ TypeMismatch "Expected a function and a list" (List args)

listFilter :: [LispVal] -> IOThrowsError LispVal
listFilter [func@(Lambda _ _ _), List xs] = do
    filtered <- filterM (\x -> do
        result <- apply func [x]
        case result of
            Bool True  -> return True
            Bool False -> return False
            _          -> throwError $ TypeMismatch "Expected boolean return" result
        ) xs
    return $ List filtered
listFilter [func@(BuiltinFunc _), List xs] = do
    filtered <- filterM (\x -> do
        result <- apply func [x]
        case result of
            Bool True  -> return True
            Bool False -> return False
            _          -> throwError $ TypeMismatch "Expected boolean return" result
        ) xs
    return $ List filtered
listFilter args = throwError $ TypeMismatch "Expected a function and a list" (List args)

listFoldL :: [LispVal] -> IOThrowsError LispVal
listFoldL [BuiltinFunc f, initial, List xs] =
    liftThrows $ foldM (\acc x -> f [acc, x]) initial xs
listFoldL [Lambda params body env, initial, List xs] =
    foldM (\acc x -> apply (Lambda params body env) [acc, x]) initial xs
listFoldL args = throwError $ TypeMismatch "Expected a function, initial value, and a list" (List args)

listFoldR :: [LispVal] -> IOThrowsError LispVal
listFoldR [BuiltinFunc f, initial, List xs] =
    liftThrows $ foldM (\acc x -> f [x, acc]) initial (reverse xs)
listFoldR [Lambda params body env, initial, List xs] =
    foldM (\acc x -> apply (Lambda params body env) [x, acc]) initial (reverse xs)
listFoldR args = throwError $ TypeMismatch "Expected a function, initial value, and a list" (List args)

listSort :: [LispVal] -> ThrowsError LispVal
listSort [List xs] =
    case xs of
        [] -> return $ List []
        (Number _:_) -> return $ List (sortBy compareNumbers xs)
        (String _:_) -> return $ List (sortBy compareStrings xs)
        _ -> throwError $ TypeMismatch "Cannot sort mixed types" (List xs)
  where
    compareNumbers (Number a) (Number b) = compare a b
    compareStrings (String a) (String b) = compare a b

listSort [Lambda params body closure, List xs] =
    case xs of
        [] -> return $ List []
        _  -> throwError $ TypeMismatch "Custom sorting requires ThrowsErrorIO" (List xs)
        -- If you later want custom sorting, you'd need `ThrowsErrorIO`
listSort args = throwError $ TypeMismatch "Expected a list (optionally with a comparator function)" (List args)

stringToList :: [LispVal] -> ThrowsError LispVal
stringToList [String s] = return $ List (map (String . (:[])) s)  -- Convert each char into a single-char string
stringToList [arg] = throwError $ TypeMismatch "Expected a string" arg
stringToList args = throwError $ NumArgs 1 args

listToString :: [LispVal] -> ThrowsError LispVal
listToString [List chars] = case mapM extractChar chars of
    Right strList -> return $ String (concat strList)
    Left err -> throwError err
  where
    extractChar (String [c]) = Right [c]  -- Ensure it's a single-character string
    extractChar invalid = Left $ TypeMismatch "Expected a list of single-character strings" invalid
listToString [arg] = throwError $ TypeMismatch "Expected a list of characters" arg
listToString args = throwError $ NumArgs 1 args

-- | Built-in function table
primitives :: [(String, LispVal)]
primitives =
  [ ("+", BuiltinFunc numericAdd),
    ("-", BuiltinFunc numericSub),
    ("*", BuiltinFunc numericMul),
    ("/", BuiltinFunc numericDiv),

    ("sin", BuiltinFunc numericSin),
    ("cos", BuiltinFunc numericCos),
    ("tan", BuiltinFunc numericTan),
    ("exp", BuiltinFunc numericExp),
    ("log", BuiltinFunc numericLog),
    ("sqrt", BuiltinFunc numericSqrt),

    ("<", BuiltinFunc compareLessThan),
    (">", BuiltinFunc compareGreaterThan),
    ("=", BuiltinFunc compareEquals),
    ("<=", BuiltinFunc compareLessThanEq),
    (">=", BuiltinFunc compareGreaterThanEq),
    ("not", BuiltinFunc logicalNot),
    ("and", BuiltinFunc logicalAnd),
    ("or", BuiltinFunc logicalOr),
    ("xor", BuiltinFunc logicalXor),

    ("cons", BuiltinFunc cons),
    ("car", BuiltinFunc car),
    ("cdr", BuiltinFunc cdr),
    ("null?", BuiltinFunc isNull),

    ("append", BuiltinFunc listAppend),
    ("length", BuiltinFunc listLength),
    ("reverse", BuiltinFunc listReverse),
    ("sort", BuiltinFunc listSort),
    ("map", BuiltinFuncIO listMap),
    ("filter", BuiltinFuncIO listFilter),
    ("foldl", BuiltinFuncIO listFoldL),
    ("foldr", BuiltinFuncIO listFoldR),

    ("string->list", BuiltinFunc stringToList),
    ("list->string", BuiltinFunc listToString)
  ]

-- Initialize environment
primitiveEnv :: IO Env
primitiveEnv = newIORef (Map.fromList primitives)