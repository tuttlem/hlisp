module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import Expr
import Control.Monad
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

-- Parse booleans
parseBool :: Parser LispVal
parseBool =
  (string "#t" >> return (Bool True)) <|> (string "#f" >> return (Bool False))

-- Parse lists
parseList :: Parser LispVal
parseList = List <$> between (char '(') (char ')') (sepBy parseExpr spaces)

-- General parser for any expression
parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseNumber <|> parseBool <|> parseList

-- Top-level function to run parser
readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> Left $ ParserError (show err)
  Right val -> Right val
