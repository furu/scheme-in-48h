module Main where

import System.Environment

-- 練習問題1
-- 1. コマンドラインから2つの引数を取り、
-- その両方を使ってメッセージを出力するようにプログラムを変更しなさい。
--
-- 1番目の引数として挨拶の言葉を受け取り、
-- 2番目の引数として名前を受け取り、
-- {挨拶の言葉}, {名前} を表示するように変更した。

main :: IO ()
main = do
  greeting:name:_ <- getArgs
  putStrLn $ greeting ++ ", " ++ name
