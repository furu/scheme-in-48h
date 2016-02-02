module Main where

-- 2. 構文解析

-- 練習問題2
-- 4. parseNumberがScheme standard for different basesもサポートするようにしなさい。
--    それにあたってはreadOctとreadHexが便利でしょう。

-- Text.ParserCombinators.Parsec から spaces を除くすべての関数をインポート
-- spaces は後で自分で定義するため
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric
import Data.Char

-- Lisp の値を表すデータ型
--   Int: 整数。有界 (最小値と最大値がある)。
--   Integer: 整数。有界でない (とても大きい数を表すことができる)。
--            Int の方が効率的。
data LispVal =   Atom String
               | List [LispVal]
               | DottedList [LispVal] LispVal
               | Number Integer
               | String String
               | Bool Bool deriving (Show)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

escapedChars :: Parser Char
escapedChars = do
  char '\\'
  x <- oneOf "\\\"nrt"
  return $ case x of
                'n' -> '\n'
                'r' -> '\r'
                't' -> '\t'
                _   -> x

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many $ escapedChars <|> noneOf "\"\\"
  char '"'
  return $ String x

-- アトムは1つの文字か記号のあとに0個以上の文字、数字、または記号が連なったもの
parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest  <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
             "#t" -> Bool True
             "#f" -> Bool False
             _    -> Atom atom

-- | parseNumber
--
-- >>> parseTest parseNumber "#b101"
-- Number 5
--
-- >>> parseTest parseNumber "#o777"
-- Number 511
--
-- >>> parseTest parseNumber "364"
-- Number 364
--
-- >>> parseTest parseNumber "#d114"
-- Number 114
--
-- >>> parseTest parseNumber "#x11F"
-- Number 287
parseNumber :: Parser LispVal
parseNumber = parseBin <|> parseOct <|> parseDec <|> parseHex

-- | parseBin
--
-- >>> parseTest parseBin "#b101"
-- Number 5
--
-- >>> parseTest parseBin "#b1111"
-- Number 15
parseBin :: Parser LispVal
parseBin = do
  try $ string "#b"
  x <- many1 $ oneOf "01"
  return $ Number (readBin x)

readBin :: String -> Integer
readBin xs = toInteger $ sum $ zipWith (*) digits weights
  where
    digits  = map digitToInt xs
    weights = map (\x -> 2 ^ x) (reverse [0..(length xs - 1)])

-- | parseOct
--
-- >>> parseTest parseOct "#o777"
-- Number 511
--
-- >>> parseTest parseOct "#o10"
-- Number 8
parseOct :: Parser LispVal
parseOct = do
  try $ string "#o"
  x <- many1 octDigit
  return $ Number (fst $ head $ readOct x)

-- | parseDec
--
-- >>> parseTest parseDec "364"
-- Number 364
--
-- >>> parseTest parseDec "#d114"
-- Number 114
parseDec :: Parser LispVal
parseDec = do
  x <- try (string "#d" >> many1 digit) <|> many1 digit
  return $ Number (read x)

-- | parseHex
--
-- >>> parseTest parseHex "#x11F"
-- Number 287
--
-- >>> parseTest parseHex "#xAA"
-- Number 170
parseHex :: Parser LispVal
parseHex = do
  try $ string "#x"
  x <- many1 hexDigit
  return $ Number (fst $ head $ readHex x)

-- <|> は1つ目のパーサを試し、それが失敗したら2つ目を試し、
-- それも失敗したら3つ目を試し…、成功したパーサから返ってきた値を返す
parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
                   Left err  -> "No match: " ++ show err
                   Right val -> "Found value: " ++ show val

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ readExpr $ head args
