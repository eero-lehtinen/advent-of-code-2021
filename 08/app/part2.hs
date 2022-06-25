import Data.Char (ord)
import Data.Either (rights)
import Data.Foldable
import Data.Foldable (toList)
import Data.List (elemIndex, intersect, sort, union, (\\))
import Data.Maybe (catMaybes)
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Read (decimal)

textLineToNumList :: T.Text -> T.Text -> [Int]
textLineToNumList text sep = map fst $ rights $ map decimal $ T.splitOn sep text

segmentPositions :: [[Int]]
segmentPositions = (map . map) (charToPos) segmentLetters

charToPos :: Char -> Int
charToPos c = ord c - 97

segmentLetters :: [String]
segmentLetters =
  [ "abcefg",
    "cf",
    "acdeg",
    "acdfg",
    "bcdf",
    "abdfg",
    "abdefg",
    "acf",
    "abcdefg",
    "abcdfg"
  ]

filterFromSingleValues :: Seq Text -> Seq Text
filterFromSingleValues poss =
  foldr
    ( \x acc ->
        if T.length x == 1
          then fmap (\a -> if T.length a == 1 then a else T.filter (/= T.head x) a) acc
          else acc
    )
    poss
    poss

solvePossibilities :: Seq Text -> [Text] -> Seq Text
solvePossibilities poss ws =
  foldr
    ( \w acc ->
        let fittingSegments = filter (\s -> length s == T.length w) segmentPositions
            seg = foldr1 union fittingSegments
            ignores = seg \\ foldr1 intersect fittingSegments
         in filterFromSingleValues $
              S.mapWithIndex
                ( \i text ->
                    case (i `elem` ignores, i `elem` seg) of
                      (True, _) -> text
                      (False, True) -> T.filter (\c -> c `T.elem` w) text
                      (False, False) -> T.filter (\c -> not $ c `T.elem` w) text
                )
                acc
    )
    poss
    ws

fromDigits :: [Int] -> Int
fromDigits = foldl addDigit 0
  where
    addDigit num d = 10 * num + d

mapWord :: Seq Char -> String -> String
mapWord segmentData word =
  map
    ( \c -> case S.findIndexL (== c) segmentData of
        Just i -> ['a' .. 'g'] !! i
        _ -> error "invalid segmentData"
    )
    word

main :: IO ()
main = do
  contents <- TIO.readFile "input.txt"
  let ls = T.lines contents
      -- words before '|'
      leftWords = map (T.words . T.take 59) ls
      -- words after '|'
      rightWords = (map . map) (T.unpack) $ map (T.words . T.drop 61) ls

      possibilities = S.fromList $ take 7 $ repeat (T.pack "abcdefg")

      segments = map (\w -> S.fromList $ concat $ T.unpack <$> solvePossibilities possibilities w) leftWords

      mappedWords = map (\(s, ws) -> map (sort . mapWord s) ws) (zip segments rightWords)

      nums = map (\slist -> fromDigits $ catMaybes $ map (\s -> elemIndex s segmentLetters) slist) mappedWords

  -- putStrLn . show $ segments
  -- putStrLn . show $ rightWords
  -- putStrLn . show $ mappedWords
  putStrLn . show $ nums
  putStrLn . show $ sum nums