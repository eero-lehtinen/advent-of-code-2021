import Data.Either (rights)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Read (decimal)

type FishCount = Seq Int

textLineToNumList :: T.Text -> T.Text -> [Int]
textLineToNumList text sep = map fst $ rights $ map decimal $ T.splitOn sep text

countOccur :: Eq a => a -> [a] -> Int
countOccur x = length . filter (x ==)

simulateFishDay :: FishCount -> FishCount
simulateFishDay count =
  let zeroCount = Seq.index count 0
   in Seq.adjust (+ zeroCount) 6 $ Seq.drop 1 count |> zeroCount

main :: IO ()
main = do
  contents <- TIO.readFile "input.txt"
  let nums = textLineToNumList contents (T.pack ",")
      count = fmap (`countOccur` nums) (Seq.fromList [0 .. 8])

  print . sum $ iterate simulateFishDay count !! 256
