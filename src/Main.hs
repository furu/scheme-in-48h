module Main where

-- 2. 構文解析

-- 練習問題2
-- 5. CharacterコンストラクタをLispValに加え、
--    R5RSに書かれているようにcharacter literalsのパーサを実装しなさい。

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
               | Bool Bool
               | Character Char deriving (Show)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

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

parseBool :: Parser LispVal
parseBool = do
  char '#'
  x <- oneOf "tf"
  return $ case x of
                't' -> Bool True
                'f' -> Bool False

-- アトムは1つの文字か記号のあとに0個以上の文字、数字、または記号が連なったもの
parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest  <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ Atom atom

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

parseChar :: Parser LispVal
parseChar = try parseCharacterName <|> parseCharacter

-- | parseCharacter
--
-- >>> parseTest parseCharacter "#\\a"
-- Character 'a'
--
-- >>> parseTest parseCharacter "#\\A"
-- Character 'A'
--
-- >>> parseTest parseCharacter "#\\("
-- Character '('
--
-- >>> parseTest parseCharacter "#\\ "
-- Character ' '
parseCharacter :: Parser LispVal
parseCharacter = do
  try $ string "#\\"
  x <- anyChar
  notFollowedBy alphaNum
  return $ Character x

-- | parseCharacterName
--
-- >>> parseTest parseCharacterName "#\\space"
-- Character ' '
--
-- >>> parseTest parseCharacterName "#\\newline"
-- Character '\n'
parseCharacterName :: Parser LispVal
parseCharacterName = do
  try $ string "#\\"
  x <- try (string "space" <|> string "newline")
  notFollowedBy alphaNum
  return $ case x of
                "space"   -> Character ' '
                "newline" -> Character '\n'

-- <|> は1つ目のパーサを試し、それが失敗したら2つ目を試し、
-- それも失敗したら3つ目を試し…、成功したパーサから返ってきた値を返す
-- The parser p <|> q first applies p. If it succeeds, the value of p is returned.
-- If p fails without consuming any input, parser q is tried.
parseExpr :: Parser LispVal
parseExpr =
  parseAtom <|> parseString <|> parseNumber <|> try parseBool <|> parseChar

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
                   Left err  -> "No match: " ++ show err
                   Right val -> "Found value: " ++ show val

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ "Parsed input: " ++ head args
  putStrLn $ readExpr $ head args
