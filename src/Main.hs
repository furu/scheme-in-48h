module Main where

-- 2. 構文解析

-- 練習問題2
-- 3. \n、\r、\t、\\などのエスケープ文字も認識するようにしなさい。
--
-- エスケープ文字というか制御文字？

-- Text.ParserCombinators.Parsec から spaces を除くすべての関数をインポート
-- spaces は後で自分で定義するため
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad

-- Lisp の値を表すデータ型
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

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) (many1 digit)

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
