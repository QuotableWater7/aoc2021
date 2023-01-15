import Data.List.Split
import Text.RE.Summa (parseInteger)

parseValues = map parseInteger . splitOn "\n"

computeScore :: [Int] -> Int
computeScore [] = 0
computeScore [_] = 0
computeScore (x:y:xs) = (if y > x then 1 else 0) + computeScore (y:xs)

computeScorePart2 :: [Int] -> Int
computeScorePart2 [] = 0
computeScorePart2 (a:b:c:d:xs) = (if (b + c + d) > (a + b + c) then 1 else 0) + computeScorePart2 (b:c:d:xs)
computeScorePart2 _ = 0

main = do
  contents <- readFile "input.txt"
  let values = sequence $ parseValues contents

  -- part 1
  let score = computeScore <$> values
  print score

  -- part 2
  let score_part_2 = computeScorePart2 <$> values
  print score_part_2