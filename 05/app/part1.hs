import Data.Either (rights)
import Data.List (transpose)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Read (decimal)

data Coord = Coord
  { x :: Int,
    y :: Int
  }
  deriving (Show, Eq, Ord)

newtype Vent = Vent
  { coveredCoords :: [Coord]
  }
  deriving (Show)

type CoordCount = Map Coord Int

newtype Board = Board {elems :: [[(Bool, Int)]]} deriving (Show)

toPair :: [b] -> (b, b)
toPair [a, b] = (a, b)
toPair _ = error "two elements required for a pair"

getCoveredCoords :: Coord -> Coord -> [Coord]
getCoveredCoords start end
  | start == end = [end]
  | (y start /= y end) && (x start /= x end) = []
  | y start < y end = start : helper id (+ 1)
  | y start > y end = start : helper id (+ (-1))
  | x start < x end = start : helper (+ 1) id
  | x start > x end = start : helper (+ (-1)) id
  | otherwise = error "unreachable"
  where
    helper fx fy = getCoveredCoords (Coord (fx $ x start) (fy $ y start)) end

parseCoord :: Text -> Coord
parseCoord text =
  let pointPair = toPair $ map fst $ rights $ map decimal $ T.splitOn (T.pack ",") text
   in uncurry Coord pointPair

parseVent :: Text -> Vent
parseVent text =
  let coordPair = toPair $ map parseCoord $ T.splitOn (T.pack " -> ") text
   in Vent $ uncurry getCoveredCoords coordPair

countCoveredCoords :: [Vent] -> CoordCount
countCoveredCoords = foldr countVent Map.empty
  where
    countVent :: Vent -> CoordCount -> CoordCount
    countVent v acc = foldr countCoords acc (coveredCoords v)

    countCoords :: Coord -> CoordCount -> CoordCount
    countCoords c acc = Map.insertWith (+) c 1 acc

main :: IO ()
main = do
  contents <- TIO.readFile "input.txt"
  let ls = T.lines contents
      vents = map parseVent ls
      coordCount = countCoveredCoords vents

  print (Map.size $ Map.filter (> 1) coordCount)
