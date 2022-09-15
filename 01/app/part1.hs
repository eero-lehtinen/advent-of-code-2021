{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Either (rights)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Read (decimal)

textToNumList :: T.Text -> T.Text -> [Integer]
textToNumList text sep = map fst $ rights $ map decimal $ T.splitOn sep text

main :: IO ()
main = do
  contents <- TIO.readFile "input.txt"
  let nums = textToNumList contents "\n"
      increasedCount =
        fst $
          foldl
            (\(acc, last) cur -> (if cur > last then acc + 1 else acc, cur))
            (0, head nums)
            (tail nums)
  print increasedCount