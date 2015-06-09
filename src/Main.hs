module Main where
 import Text.ParserCombinators.Parsec hiding (spaces)
 import Control.Monad
 import Numeric
 import Data.Char (digitToInt)

 data LispVal = Atom String
              | List [LispVal]
              | DottedList [LispVal] LispVal
              | Number Integer
              | String String
              | Bool Bool
              deriving Show

 symbol :: Parser Char
 symbol = oneOf "!$%&|*+-/:<=>?@^_~"

 spaces :: Parser ()
 spaces = skipMany1 space

 escapedChar :: Parser Char
 escapedChar = do
     _ <- char '\\'
     c <- oneOf "\"\\ntr"
     return $ case c of
         'n'  -> '\n'
         't'  -> '\t'
         'r'  -> '\r'
         _    -> c

 parseString :: Parser LispVal
 parseString = do
     _ <- char '"'
     x <- many (escapedChar <|> noneOf "\"")
     _ <- char '"'
     return $ String x

 parseBool :: Parser LispVal
 parseBool = try $ ((string "#t" >> return (Bool True)) <|> (string "#f" >> return (Bool False)))

 parseAtom :: Parser LispVal
 parseAtom = do
     first <- letter <|> symbol
     rest <- many $ letter <|> digit <|> symbol
     let atom = first:rest
     return $ case atom of
         _    -> Atom atom

 parseNumber :: Parser LispVal
 parseNumber = liftM (Number . read) $ many1 digit

 parseNumber' :: Parser LispVal
 parseNumber' = do
     digits <- many1 digit
     return . Number . read $ digits

 parseNumber'' :: Parser LispVal
 parseNumber'' = many1 digit >>= return . Number . read

 parseNumber''' :: Parser LispVal
 parseNumber''' = do
     x <- parseDecimal <|> parseDecimalWithRedix <|> parseOctal <|> parseHexadecimal <|> parseBinary
     return x
     where
         parseDecimal = many1 digit >>= return . Number . read
         parseDecimalWithRedix = try (string "#d") >> parseDecimal

         parseOctal = try (string "#o") >> many1 octDigit >>=
             return . Number . fst . (\x -> readOct x !! 0)

         parseHexadecimal = try (string "#x") >> many1 hexDigit >>=
             return . Number . fst . (\x -> readHex x !! 0)

         parseBinary = try (string "#b") >> many1 (oneOf "01") >>=
             return . Number . (foldl (\z a -> a + 2 * z) 0) . (map $ toInteger . digitToInt)

 parseExpr :: Parser LispVal
 parseExpr = parseBool <|> parseAtom <|> parseString <|> parseNumber'''

 readExpr :: String -> String
 readExpr input = case parse parseExpr "lisp" input of
     Left err -> "No match: " ++ show err
     Right val -> "Parsed: " ++ show val

 main :: IO ()
 main = do
     putStrLn $ show $ head args
     putStrLn $ readExpr $ head args
     where args = ["\"This \\n is \\\" a string\""]
