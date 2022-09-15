{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Either (rights)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Read (decimal)

textToNumList :: T.Text -> T.Text -> [Integer]
textToNumList text sep = map fst $ rights $ map decimal $ T.splitOn sep text

toNum :: T.Text -> Integer
toNum x =
  case decimal x of
    Left e -> error e
    Right (n, _) -> n

applyCommand :: (Integer, Integer) -> [T.Text] -> (Integer, Integer)
applyCommand (pos, depth) ["forward", n] = (pos + toNum n, depth)
applyCommand (pos, depth) ["down", n] = (pos, depth + toNum n)
applyCommand (pos, depth) ["up", n] = (pos, depth - toNum n)
applyCommand _ _ = error "Invalid command"

main :: IO ()
main = do
  contents <- TIO.readFile "input.txt"
  let commands = T.splitOn " " <$> T.splitOn "\n" contents
      (pos, depth) = foldl applyCommand (0, 0) commands

  print $ pos * depth