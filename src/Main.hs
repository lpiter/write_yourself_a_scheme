module Main where
  import Text.ParserCombinators.Parsec hiding (spaces)
  import Control.Monad
  import Numeric
  import Data.Char
  import Data.Ratio
  import Data.Complex

  data LispVal = Atom String
               | List [LispVal]
               | DottedList [LispVal] LispVal
               | Number Integer
               | String String
               | Bool Bool
               | Character Char
               | Float Double
               | Ratio Rational
               | Complex (Complex Double)
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
  parseBool = try $ (string "#t" >> return (Bool True)) <|> (string "#f" >> return (Bool False))

  parseAtom :: Parser LispVal
  parseAtom = do
      first <- letter <|> symbol
      rest <- many $ letter <|> digit <|> symbol
      return $ Atom (first:rest)

  parseDecimal = liftM (Number . read) (many1 digit)

  parseNumber :: Parser LispVal
  parseNumber = parseDecimal <|> parseDecimalWithRedix <|> parseOctal <|> parseHexadecimal <|> parseBinary
    where
      parseDecimalWithRedix = try (string "#d") >> parseDecimal
      parseOctal =       liftM (Number . fst . head . readOct) (try (string "#o") >> many1 octDigit)
      parseHexadecimal = liftM (Number . fst . head . readHex) (try (string "#x") >> many1 hexDigit)

  parseBinary = liftM (Number . foldl (\z a -> 2*z + a) 0 . map (toInteger . digitToInt)) readBinaryString
    where
      readBinaryString = try (string "#b") >> many1 (oneOf "01")

  parseFloat :: Parser LispVal
  parseFloat = do
      x <- many1 digit
      _ <- char '.'
      y <- many1 digit
      return . Float . fst $ head $ readFloat (x ++ "." ++ y)

  parseRatio :: Parser LispVal
  parseRatio = do
      x <- many1 digit
      char '/'
      y <- many1 digit
      return $ Ratio (read x % read y)

  parseComplex :: Parser LispVal
  parseComplex = do
      x <- try parseFloat <|> parseDecimal
      char '+'
      y <- try parseFloat <|> parseDecimal
      char 'i'
      return $ Complex (toDouble x :+ toDouble y)
      where
        toDouble (Float f) = f
        toDouble (Number n) = fromIntegral n

  parseCharacter :: Parser LispVal
  parseCharacter = do
      _ <- try (string "#\\")
      c <- try singleChar <|> try characterName
      return $ Character c
      where
        -- character names are case-insensitive
        characterName = (ciString "space" >> return ' ') <|> (ciString "newline" >> return '\n')
          where
            ciString = mapM (\c -> char (toLower c) <|> char (toUpper c)) -- case-insensitive string
        singleChar = do
          c <- anyChar
          notFollowedBy alphaNum
          return c

  parseList :: Parser LispVal
  parseList = liftM List $ sepBy parseExpr spaces

  parseExpr :: Parser LispVal
  parseExpr = parseAtom
    <|> try parseString
    <|> try parseFloat
    <|> try parseRatio
    <|> try parseComplex
    <|> try parseNumber
    <|> try parseBool
    <|> try parseCharacter
    <|> try parseList

  readExpr :: String -> String
  readExpr input = case parse parseExpr "lisp" input of
                     Left err -> "No match: " ++ show err
                     Right val -> show val

  main :: IO ()
  main = mapM_ readArg args
    where
      args = ["12.33", "12", "#\\NeWline", "#\\a", "#x123", "\"This \\n is \\\" a string\"",
              "12/45", "12+44.6i"]
      readArg a = putStrLn $ show a ++ " --> " ++ show (readExpr a)
