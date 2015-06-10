module Main where
 import Text.ParserCombinators.Parsec hiding (spaces)
 import Control.Monad
 import Numeric
 import Data.Char

 data LispVal = Atom String
              | List [LispVal]
              | DottedList [LispVal] LispVal
              | Number Integer
              | String String
              | Bool Bool
              | Character Char
              | Float Double
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
     return $ Atom (first:rest)

 parseNumber' :: Parser LispVal
 parseNumber' = do
     digits <- many1 digit
     return . Number . read $ digits

 parseNumber'' :: Parser LispVal
 parseNumber'' = many1 digit >>= return . Number . read

 parseNumber''' :: Parser LispVal
 parseNumber''' = liftM (Number . read) $ many1 digit

 parseNumber :: Parser LispVal
 parseNumber = do
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

 parseFloat :: Parser LispVal
 parseFloat = do
     x <- many1 digit
     _ <- char '.'
     y <- many1 digit
     return . Float . fst $ ((readFloat (x ++ "." ++ y)) !! 0)

 parseCharacter :: Parser LispVal
 parseCharacter = do
     _ <- try (string "#\\")
     c <- try singleChar <|> try characterName
     return $ Character c
     where
         -- character names are case-insensitive
         characterName = (ciString "space" >> return ' ')
                     <|> (ciString "newline" >> return '\n')
         singleChar = do
             c <- anyChar
             notFollowedBy alphaNum
             return c
         ciString = mapM (\c -> char (toLower c) <|> char (toUpper c)) -- case-insensitive string

 parseExpr :: Parser LispVal
 parseExpr = parseAtom
         <|> try parseString
         <|> try parseFloat
         <|> try parseNumber
         <|> try parseBool
         <|> try parseCharacter

 readExpr :: String -> String
 readExpr input = case parse parseExpr "lisp" input of
     Left err -> "No match: " ++ show err
     Right val -> show val

 main :: IO ()
 main = mapM readArg args >> return ()
     where
         args = ["12.33", "12", "#\\NeWline", "#\\a", "#x123", "\"This \\n is \\\" a string\""]
         readArg a = do
             putStrLn $ show a ++ " --> " ++ show (readExpr a)

