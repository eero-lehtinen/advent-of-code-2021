import Data.Char (digitToInt)
import Data.List (elemIndex, sort)
import Data.Matrix (Matrix, fromLists, ncols, nrows, safeGet, (!))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Debug.Trace (trace)

digitStringToNumList :: String -> [Int]
digitStringToNumList line = map digitToInt line

-- scuffed dfs
collectBasin :: (Int, Int) -> [(Int, Int)] -> Matrix Int -> [(Int, Int)]
collectBasin (y, x) visited hMap =
  case (cond (y + 1, x), cond (y - 1, x), cond (y, x + 1), cond (y, x - 1)) of
    (True, _, _, _) -> collectBasin (y + 1, x) (insert ++ visited) hMap
    (_, True, _, _) -> collectBasin (y - 1, x) (insert ++ visited) hMap
    (_, _, True, _) -> collectBasin (y, x + 1) (insert ++ visited) hMap
    (_, _, _, True) -> collectBasin (y, x - 1) (insert ++ visited) hMap
    -- backtracking
    _ -> case elemIndex (y, x) visited of
      Just n ->
        if n == length visited - 1
          then reverse visited
          else collectBasin (visited !! (n + 1)) visited hMap
      _ -> collectBasin (visited !! 0) (insert ++ visited) hMap
  where
    notVisited (y2, x2) = not $ elem (y2, x2) visited
    cond (y2, x2) =
      notVisited (y2, x2)
        && y2 <= nrows hMap
        && y2 >= 1
        && x2 <= ncols hMap
        && x2 >= 1
        && hMap ! (y2, x2) > hMap ! (y, x)
        && hMap ! (y2, x2) < 9
    insert = if notVisited (y, x) then [(y, x)] else []

main :: IO ()
main = do
  contents <- TIO.readFile "input.txt"
  let ls = lines $ T.unpack contents
      heightMap = fromLists $ map digitStringToNumList ls

      cols = ncols heightMap
      rows = nrows heightMap

      lowPoints =
        [ (y, x)
          | y <- [1 .. rows],
            x <- [1 .. cols],
            let center = heightMap ! (y, x),
            y + 1 > rows || center < heightMap ! (y + 1, x),
            y - 1 < 1 || center < heightMap ! (y - 1, x),
            x + 1 > cols || center < heightMap ! (y, x + 1),
            x - 1 < 1 || center < heightMap ! (y, x - 1)
        ]

      basins = map (\p -> collectBasin p [] heightMap) lowPoints
      basinCounts = map length basins

  -- putStrLn . show $ heightMap
  -- putStrLn . show $ lowPoints
  -- putStrLn . show $ basins
  -- putStrLn . show $ basinCounts
  putStrLn . show . product . take 3 . reverse . sort $ basinCounts