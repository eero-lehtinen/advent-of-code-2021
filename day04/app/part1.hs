import Data.Either ()
import Data.List (transpose)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Read (decimal)

data Board = Board {elems :: [[(Bool, Integer)]]} deriving (Show)

registerNum :: Integer -> Board -> Board
registerNum n board = Board $ (map . map) (\(b, x) -> if x == n then (True, x) else (b, x)) (elems board)

rotl = transpose . map reverse

hasWon :: Board -> Bool
hasWon board = or $ (calc $ elems board) : [calc $ rotl $ elems board]
  where
    calc :: [[(Bool, Integer)]] -> Bool
    calc e = any null (filter (\(b, n) -> b == False) <$> e)

calcScore :: Board -> Integer
calcScore board = sum $ map snd $ concat (filter (\(b, n) -> b == False) <$> (elems board))

process :: [Board] -> [Integer] -> Integer
process boards nums =
  let newBoards = map (registerNum $ head nums) boards
   in case filter hasWon newBoards of
        [b] -> (calcScore b) * (head nums)
        _ -> process newBoards (tail nums)

main :: IO ()
main = do
  contents <- TIO.readFile "input.txt"
  let ls = T.lines contents
      nums = catMaybes $ map toNum $ T.splitOn (T.pack ",") (ls !! 0)
      boardParts = T.splitOn (T.pack "\n\n") (T.unlines $ drop 2 ls)
      boards = map toBoard boardParts

  putStrLn $ show $ process boards nums
  where
    toNum :: T.Text -> Maybe Integer
    toNum x = case decimal x of
      Left _ -> Nothing
      Right (n, _) -> Just n
    toBoard :: T.Text -> Board
    toBoard text = Board $ (map . map) (\x -> (False, x)) $ map lineToNumList (T.lines text)
    lineToNumList l = catMaybes $ map toNum (T.words l)
