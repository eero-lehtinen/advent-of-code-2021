{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TupleSections #-}

import Data.Either (rights)
import Data.List (transpose)
import Data.Maybe (catMaybes)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Read (decimal)

newtype Board = Board {elems :: [[(Bool, Integer)]]} deriving (Show)

registerNum :: Integer -> Board -> Board
registerNum n board = Board $ (map . map) (\(b, x) -> if x == n then (True, x) else (b, x)) (elems board)

rotl :: [[a]] -> [[a]]
rotl = transpose . map reverse

hasWon :: Board -> Bool
hasWon board = calc (elems board) || (calc . rotl $ elems board)
  where
    calc :: [[(Bool, Integer)]] -> Bool
    calc e = any null (filter (not . fst) <$> e)

calcScore :: Board -> Integer
calcScore board = sum $ map snd $ concat (filter (not . fst) <$> elems board)

textLineToNumList :: T.Text -> T.Text -> [Integer]
textLineToNumList text sep = map fst $ rights $ map decimal $ T.splitOn sep text

main :: IO ()
main = do
  contents <- TIO.readFile "input.txt"
  let ls = T.lines contents
      nums = textLineToNumList (head ls) (T.pack ",")
      boardParts = T.splitOn (T.pack "\n\n") (T.unlines $ drop 2 ls)
      boards = map toBoard boardParts

  print (process boards nums)
  where
    toBoard :: T.Text -> Board
    toBoard text = Board $ (map . map) (False,) $ (\t -> textLineToNumList t (T.pack " ")) <$> T.lines text

process :: [Board] -> [Integer] -> Integer
process boards nums =
  let newBoards = map (registerNum $ head nums) boards
   in case filter hasWon newBoards of
        [b] -> calcScore b * head nums
        _ -> process newBoards (tail nums)
