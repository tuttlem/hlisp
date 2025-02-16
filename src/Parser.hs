module Parser where

import Debug.Trace
import Text.Parsec
import Text.Parsec.String (Parser)
import Expr
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Numeric

-- | Parses an atom (symbol) in Lisp.
-- Atoms start with a letter or specific special characters and can contain
-- alphanumeric characters or symbols such as `!$%&|*+-/:<=>?@^_~`.
--
-- Examples:
--
-- >>> parseTest parseAtom "foo"
-- Atom "foo"
--
-- >>> parseTest parseAtom "+add"
-- Atom "+add"
--
parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> oneOf "!$%&|*+-/:<=>?@^_~"
    rest <- many (letter <|> digit <|> oneOf "!$%&|*+-/:<=>?@^_~")
    return $ Atom (first : rest)

-- | Parses a number.
-- Currently, only decimal integer literals are supported.
--
-- Examples:
--
-- >>> parseTest parseNumber "123"
-- Number 123
--
parseNumber :: Parser LispVal
parseNumber = Number . read <$> many1 digit

-- | Parses a string.
-- Strings are enclosed in double quotes (`"`), and can contain any characters
-- except an unescaped `"` inside.
--
-- Examples:
--
-- >>> parseTest parseString "\"hello\""
-- String "hello"
--
-- >>> parseTest parseString "\"lisp is cool\""
-- String "lisp is cool"
--
parseString :: Parser LispVal
parseString = do
    char '"'  -- Match opening quote
    str <- many (noneOf "\"")  -- Match everything until the closing quote
    char '"'  -- Match closing quote
    return $ String str  -- Return LispVal String

-- | Parses a boolean literal (`#t` for true, `#f` for false).
-- Uses `try` to avoid ambiguity in cases where `#t` or `#f` might be
-- followed by additional characters.
--
-- Examples:
--
-- >>> parseTest parseBool "#t"
-- Bool True
--
-- >>> parseTest parseBool "#f"
-- Bool False
--
parseBool :: Parser LispVal
parseBool =
    try (string "#t" >> return (Bool True)) <|>
    try (string "#f" >> return (Bool False))

-- | Parses a list expression.
-- Lists are enclosed in parentheses and contain zero or more expressions
-- separated by spaces.
--
-- Examples:
--
-- >>> parseTest parseList "(1 2 3)"
-- List [Number 1, Number 2, Number 3]
--
-- >>> parseTest parseList "(+ 1 2)"
-- List [Atom "+", Number 1, Number 2]
--
parseList :: Parser LispVal
parseList = List <$> between (char '(') (char ')') (sepBy (try parseExpr <|> parseString) spaces)

parseQuote :: Parser LispVal
parseQuote = do
    char '\''  -- ✅ Match the single quote `'`
    expr <- parseExpr  -- ✅ Parse the following expression
    return $ List [Atom "quote", expr]  -- ✅ Convert to `(quote expr)`


-- | Parses any valid Lisp expression.
-- This includes numbers, booleans, strings, atoms, and lists.
-- The use of `try` ensures that overlapping parsers backtrack properly.
parseExpr :: Parser LispVal
parseExpr = try parseString
        <|> try parseNumber
        <|> try parseBool
        <|> try parseAtom
        <|> try parseList
        <|> parseQuote

-- | Parses a Lisp expression from a given string input.
-- Wraps the parsing process in `IOThrowsError` to integrate with
-- error-handling in the interpreter.
--
-- Examples:
--
-- >>> readExpr "(+ 1 2)"
-- Right (List [Atom "+", Number 1, Number 2])
--
-- >>> readExpr "foo"
-- Right (Atom "foo")
--
readExpr :: String -> IOThrowsError LispVal
readExpr input = liftThrows $ case parse parseExpr "lisp" input of
    Left err  -> Left $ ParserError (show err)
    Right val -> Right val
