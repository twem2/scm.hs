module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces, parseTest)
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

parseCharacter :: Parser LispVal
parseCharacter = do string "#\\"
                    x <- (try $ string "space") <|> (try $ string "newline") 
                         <|> do { c <- anyChar; notFollowedBy alphaNum; return [c] }
                    case x of
                       "space" -> return $ Character ' '
                       "newline" -> return $ Character '\n'
                       _ -> return $ Character (x !! 0)

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do head <- endBy parseExpr spaces
                     tail <- char '.' >> spaces >> parseExpr
                     return $ DottedList head tail
        
parseQuoted = do char '\''
                 x <- parseExpr
                 return $ List [Atom "quote", x]
                  
parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseBool
        <|> parseNumber
        <|> parseCharacter
        <|> parseQuoted
        <|> do char '('
               x <- try parseList <|> parseDottedList
               char ')'
               return x


readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value"

main :: IO ()
main = do args <- getArgs
          putStrLn (readExpr (args !! 0))
          
parseTest :: String -> IO ()
parseTest x = case (readExpr x) of 
  "Found value" -> putStrLn $ "pass [" ++ x ++ "]"
  _ -> putStrLn $ "FAIL [" ++ x ++ "]"

parseTestFail :: String -> IO ()
parseTestFail x = case (readExpr x) of 
  "Found value" -> putStrLn $ "FAIL [" ++ x ++ "]"
  _ -> putStrLn $ "pass [" ++ x ++ "]"


runTests :: IO ()
runTests = do parseTest "x" -- Atom
              parseTest "\"foobar$\"" -- String
              parseTest "\"out\\\"in\\\"out\"" -- String with quotes
              parseTest "\"\\\\\"" -- String with \
              parseTest "\"\\n\\t\\r\"" -- String with \n\t\r
              parseTest "#t" -- true
              parseTest "#f" -- false
              parseTest "1251261171"
              parseTest "#o712315"
              parseTest "#x87AaFf"
              parseTest "#d136125"
              parseTest "#b101010011"
              parseTest "#\\space"
              parseTest "#\\newline"
              parseTest "#\\s"
              parseTestFail "#\\sp" -- should fail
              putStrLn $ if (readBin "1010") == 10 then "pass [binary test]" else "FAIL [binary test]"  
              parseTest "(a parseTest)"
              parseTest "(a (nester) parseTest)"
              parseTest "(a (dotted . list) parseTest)"
              parseTest "(a '(quoted (dotted . list)) parseTest)"
              parseTestFail "(a '(imbalanced parens)" -- should fail