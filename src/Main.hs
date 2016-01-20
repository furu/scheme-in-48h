module Main where

import System.Environment

-- 練習問題1
-- 2. 2つの引数に対して簡単な算術演算を行うようにプログラムを変更しなさい。
--    文字列を数値に変換するにはreadが、数値を文字列に戻すにはshowが使えます。
--    いくつか違う演算を行うよう試してみてください。

main :: IO ()
main = do
  x:y:_ <- getArgs
  putStrLn $ "加算: " ++ (show $ read x + read y)
  putStrLn $ "減算: " ++ (show $ read x - read y)
  putStrLn $ "乗算: " ++ (show $ read x * read y)
  putStrLn $ "除算: " ++ (show $ read x / read y)
