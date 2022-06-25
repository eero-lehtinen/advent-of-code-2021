import Data.Either (rights)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Read (decimal)

textLineToNumList :: T.Text -> T.Text -> [Int]
textLineToNumList text sep = map fst $ rights $ map decimal $ T.splitOn sep text

count1478 :: [Text] -> Int
count1478 words = length $ filter (\w -> T.length w `elem` [2, 4, 3, 7]) words

main :: IO ()
main = do
  contents <- TIO.readFile "input.txt"
  let ls = T.lines contents
      ws = map (T.words . T.drop 61) ls

  -- putStrLn . show $ ws
  putStrLn . show $ sum (map count1478 ws)