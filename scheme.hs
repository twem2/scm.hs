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
             | Character Char

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
parseBool = do x <- try (string "#t") <|> try (string "#f")
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
parseOct = do try $ string "#o" 
              x <- many1 $ oneOf "01234567"
              (return . Number . oct2dec) x
              
parseHex :: Parser LispVal
parseHex = do try $ string "#x"
              x <- many1 $ digit <|> oneOf "abcdefABCDEF"
              (return . Number . hex2dec) x
              
parseDec :: Parser LispVal
parseDec = do try $ string "#d"
              x <- many1 digit
              (return . Number . read) x

parseBin :: Parser LispVal
parseBin = do try $ string "#b"
              x <- many1 $ oneOf "01" 
              (return . Number . readBin) x

parseNumber :: Parser LispVal
parseNumber = do x <- parseOct <|> parseHex <|> parseBin <|> parseDec <|> parseDec2
                 return $ x

parseDec2 :: Parser LispVal
parseDec2 = many1 digit >>= return . Number . read

-- parseCharacter :: Parser LispVal
-- parseCharacter = do string "#\\"
--                     x <- anyChar <|> 
--                       -- string "space" <|> string "newline" <|> (alphaNum >>= not alphaNum)
--                     case x of
--                       "space" -> return $ Character ' '
--                       "newline" -> return $ Character '\n'
--                       _ -> return $ Character (x !! 0)
                    
                    

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseBool
        <|> parseNumber

        
readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value"

main :: IO ()
main = do args <- getArgs
          putStrLn (readExpr (args !! 0))
          
test :: String -> IO ()
test x = putStrLn $ x ++ " : " ++ readExpr x

runTests :: IO ()
runTests = do test "x" -- Atom
              test "\"foobar$\"" -- String
              test "\"out\\\"in\\\"out\"" -- String with quotes
              test "\"\\\\\"" -- String with \
              test "\"\\n\\t\\r\"" -- String with \n\t\r
              test "#t" -- true
              test "#f" -- false
              test "1251261171"
              test "#o712315"
              test "#x87AaFf"
              test "#d136125"
              test "#b101010011"
        

  