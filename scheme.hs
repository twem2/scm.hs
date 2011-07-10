module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Numeric

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal 
             | Number Integer
             | String String
             | Bool Bool

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

escapedChars :: Parser Char
escapedChars = do char '\\'
                  x <- oneOf "\\\"nrt"
                  return $ case x of
                             'n' -> '\n'
                             'r' -> '\r'
                             't' -> '\t'
                             _   -> x

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (noneOf "\"" <|> escapedChars)
                 char '"'
                 return $ String x

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol 
               rest <- many (letter <|> digit <|> symbol)
               return $ Atom (first:rest)

parseBool :: Parser LispVal
parseBool = do x <- string "#t" <|> string "#f"
               return $ case x of
                          "#t" -> Bool True
                          "#f" -> Bool False

oct2dec :: String -> Integer
oct2dec x = fst $ (readOct x) !! 0

hex2dec :: String -> Integer
hex2dec x = fst $ (readHex x) !! 0 

readBinReversed :: String -> Integer
readBinReversed "" = 0
readBinReversed (x:xs) = case x of
                          '0' -> 2 * readBinReversed xs
                          '1' -> 2 * readBinReversed xs + 1

readBin :: String -> Integer
readBin x = readBinReversed $ reverse x

parseOct :: Parser LispVal
parseOct = do string "#o" 
              x <- many1 $ oneOf "01234567"
              (return . Number . oct2dec) x
              
parseHex :: Parser LispVal
parseHex = do string "#x"
              x <- many1 $ digit <|> oneOf "abcdefABCDEF"
              (return . Number . hex2dec) x
              
parseDec :: Parser LispVal
parseDec = do string "#d"
              x <- many1 digit
              (return . Number . read) x

parseBin :: Parser LispVal
parseBin = do string "#b"
              x <- many1 $ oneOf "01" 
              (return . Number . readBin) x

parseNumber :: Parser LispVal
--parseNumber = liftM (Number . read) $ many1 digit
--parseNumber = do num <- many1 digit
--                 (return . Number . read) num
parseNumber = many1 digit >>= return . Number . read

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseOct
        <|> parseHex
        <|> parseDec
        <|> parseBin
        
readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value"

main :: IO ()
main = do args <- getArgs
          putStrLn (readExpr (args !! 0))