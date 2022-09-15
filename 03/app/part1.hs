module Main where

import Data.Bits (Bits (shiftL))
import Data.List (transpose)
import Data.Word (Word32)

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let columns = transpose $ lines contents
      mostCommon = map (\s -> length (filter (== '1') s) > div (length s) 2) columns
      gamma = foldl (\acc x -> acc `shiftL` 1 + if x then 1 else 0) (0 :: Word32) mostCommon
      epsilon = foldl (\acc x -> acc `shiftL` 1 + if x then 0 else 1) (0 :: Word32) mostCommon

  print $ gamma * epsilon