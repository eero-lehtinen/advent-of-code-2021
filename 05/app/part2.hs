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

data Vent = Vent
  { coveredCoords :: [Coord]
  }
  deriving (Show)

type CoordCount = Map Coord Int

data Board = Board {elems :: [[(Bool, Int)]]} deriving (Show)

toPair :: [b] -> (b, b)
toPair [a, b] = (a, b)
toPair _ = error "two elements required for a pair"

getCoveredCoords :: Coord -> Coord -> [Coord]
getCoveredCoords start end
  | start == end = [end]
  | otherwise = start : getCoveredCoords (Coord (x1 + getDir x1 x2) (y1 + getDir y1 y2)) end
  where
    x1 = x start
    y1 = y start
    x2 = x end
    y2 = y end
    getDir a b = case compare a b of
      LT -> 1
      EQ -> 0
      GT -> -1

parseCoord :: Text -> Coord
parseCoord text =
  let pointPair = toPair $ map fst $ rights $ map decimal $ T.splitOn (T.pack ",") text
   in Coord (fst pointPair) (snd pointPair)

parseVent :: Text -> Vent
parseVent text =
  let coordPair = toPair $ map parseCoord $ T.splitOn (T.pack " -> ") text
   in Vent $ getCoveredCoords (fst coordPair) (snd coordPair)

countCoveredCoords :: [Vent] -> CoordCount
countCoveredCoords vs = foldr countVent Map.empty vs
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

  -- putStrLn $ show vents
  -- putStrLn $ show coordCount
  putStrLn $ show $ Map.size $ Map.filter (> 1) coordCount
