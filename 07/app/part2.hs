import Data.Either (rights)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Read (decimal)
import GHC.Float (double2Int, int2Double)

textLineToNumList :: T.Text -> T.Text -> [Int]
textLineToNumList text sep = map fst $ rights $ map decimal $ T.splitOn sep text

findShortestDist :: [Int] -> Int
findShortestDist nums = minimum [totalDistances nums x | x <- [minimum nums .. maximum nums]]

totalDistances :: [Int] -> Int -> Int
totalDistances nums target = sum $ map (`calcDist` target) nums
  where
    calcDist :: Int -> Int -> Int
    calcDist a target = let n = int2Double $ abs $ target - a in double2Int (n * (n + 1) / 2)

main :: IO ()
main = do
  contents <- TIO.readFile "input.txt"
  let nums = textLineToNumList contents (T.pack ",")
      center = findShortestDist nums

  print $ findShortestDist nums