module Main where

import System.Environment

-- 最初の Haskell プログラム
-- コマンドラインから名前を読み込み挨拶を表示する

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ "Hello, " ++ head args
