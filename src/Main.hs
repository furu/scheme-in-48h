module Main where

import System.IO

-- 練習問題1
-- 3. getLineはコンソールから一行読み込み、文字列として返すIOアクションです。
--    名前を入力を促し、名前を読み、コマンドライン引数の代わりにそれを出力する
--    ようにプログラムを変更しなさい。

main :: IO ()
main = do
  putStr "put your name: "
  hFlush stdout
  name <- getLine
  putStrLn $ "Hi, " ++ name
