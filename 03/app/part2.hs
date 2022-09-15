module Main where

import Data.Bits (Bits (shiftL))
import Data.List (transpose)
import Data.Word (Word32)

mostCommon :: [Char] -> Char
mostCommon ls = if length (filter (== '1') ls) >= length (filter (== '0') ls) then '1' else '0'

leastCommon :: [Char] -> Char
leastCommon ls = if mostCommon ls == '1' then '0' else '1'

findRating :: ([Char] -> Char) -> [[Char]] -> Int -> [Char]
findRating criteria [l] idx = l
findRating criteria ls idx =
  let c = criteria (transpose ls !! idx)
   in findRating criteria (filter (\s -> s !! idx == c) ls) (idx + 1)

bitsToNum :: [Char] -> Word32
bitsToNum = foldl (\acc x -> acc `shiftL` 1 + if x == '1' then 1 else 0) (0 :: Word32)

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let ls = lines contents
      oxygenRating = bitsToNum $ findRating mostCommon ls 0
      co2Rating = bitsToNum $ findRating leastCommon ls 0

  print $ oxygenRating * co2Rating