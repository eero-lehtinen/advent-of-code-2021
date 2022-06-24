import Data.Either ()
import Data.List (transpose)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Read (decimal)

data Board = Board {elems :: [[(Bool, Integer)]]} deriving (Show)

registerNum :: Integer -> Board -> Board
registerNum n board = Board $ (map . map) (\(b, x) -> if x == n then (True, x) else (b, x)) (elems board)

rotl :: [[a]] -> [[a]]
rotl = transpose . map reverse

hasWon :: Board -> Bool
hasWon board = or [calc $ elems board, calc $ rotl $ elems board]
  where
    calc :: [[(Bool, Integer)]] -> Bool
    calc e = any null (filter (\(b, n) -> b == False) <$> e)

calcScore :: Board -> Integer
calcScore board = sum $ map snd $ concat (filter (\(b, n) -> b == False) <$> (elems board))

process :: [Board] -> [Integer] -> Integer
process boards nums =
  let newBoards = map (registerNum $ head nums) boards
      firstBoard = head newBoards
   in if and [length newBoards == 1, hasWon firstBoard]
        then (calcScore firstBoard) * (head nums)
        else process (filter (\b -> not $ hasWon b) newBoards) (tail nums)

toNum :: T.Text -> Maybe Integer
toNum x = case decimal x of
  Left _ -> Nothing
  Right (n, _) -> Just n

textLineToNumList :: T.Text -> T.Text -> [Integer]
textLineToNumList text sep = catMaybes $ map toNum (T.splitOn sep text)

main :: IO ()
main = do
  contents <- TIO.readFile "input.txt"
  let ls = T.lines contents
      nums = textLineToNumList (head ls) (T.pack ",")
      boardParts = T.splitOn (T.pack "\n\n") (T.unlines $ drop 2 ls)
      boards = map toBoard boardParts

  putStrLn $ show $ process boards nums
  where
    toBoard :: T.Text -> Board
    toBoard text = Board $ (map . map) (\x -> (False, x)) $ (\t -> textLineToNumList t (T.pack " ")) <$> T.lines text