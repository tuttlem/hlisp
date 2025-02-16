module Parser where

import Debug.Trace
import Text.Parsec
import Text.Parsec.String (Parser)
import Expr
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Numeric

-- Parse an atom (symbol)
parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> oneOf "!$%&|*+-/:<=>?@^_~"
  rest <- many (letter <|> digit <|> oneOf "!$%&|*+-/:<=>?@^_~")
  return $ Atom (first : rest)

-- Parse a number
parseNumber :: Parser LispVal
parseNumber = Number . read <$> many1 digit

parseString :: Parser LispVal
parseString = do
    char '"'  -- Match opening quote
    str <- many (noneOf "\"")  -- Match everything until the closing quote
    char '"'  -- Match closing quote
    return $ String str  -- Return LispVal String

-- Parse booleans
parseBool :: Parser LispVal
parseBool =
  (string "#t" >> return (Bool True)) <|> (string "#f" >> return (Bool False))

-- Parse lists
parseList :: Parser LispVal
parseList = List <$> between (char '(') (char ')') (sepBy (try parseExpr <|> parseString) spaces)

-- General parser for any expression
parseExpr :: Parser LispVal
parseExpr = try parseString
        <|> try parseNumber
        <|> try parseBool
        <|> try parseAtom
        <|> parseList

-- Top-level function to run parser
readExpr :: String -> IOThrowsError LispVal
readExpr input = liftThrows $ case parse parseExpr "lisp" input of
    Left err  -> Left $ ParserError (show err)
    Right val -> Right val
