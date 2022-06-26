import Data.Char (digitToInt)
import Data.Matrix (Matrix, fromLists, ncols, nrows, (!))
-- import qualified Data.Matrix as Mat
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

digitStringToNumList :: String -> [Int]
digitStringToNumList line = map digitToInt line

main :: IO ()
main = do
  contents <- TIO.readFile "input.txt"
  let ls = lines $ T.unpack contents

      heightMap = fromLists $ map digitStringToNumList ls

      cols = ncols heightMap
      rows = nrows heightMap

      lowPoints =
        [ center
          | x <- [1 .. cols],
            y <- [1 .. rows],
            let center = heightMap ! (y, x),
            y + 1 > rows || center < heightMap ! (y + 1, x),
            y - 1 < 1 || center < heightMap ! (y - 1, x),
            x + 1 > cols || center < heightMap ! (y, x + 1),
            x - 1 < 1 || center < heightMap ! (y, x - 1)
        ]

      riskLevel = foldr (\n acc -> acc + n + 1) 0 lowPoints

  -- putStrLn . show $ ws
  putStrLn . show $ heightMap
  putStrLn . show $ lowPoints
  putStrLn . show $ riskLevel