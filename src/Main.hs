module Main where

-- 2. 構文解析

-- 空白文字

-- Text.ParserCombinators.Parsec から spaces を除くすべての関数をインポート
-- spaces は後で自分で定義するため
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> String
readExpr input = case parse (spaces >> symbol) "lisp" input of
                   Left err  -> "No match: " ++ show err
                   Right val -> "Found value"

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ readExpr $ head args
