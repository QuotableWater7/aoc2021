binaryToDecimal :: [Int] -> Int
binaryToDecimal = foldl (\x y -> 2*x + y) 0

getGamma :: [String] -> [Int]
getGamma ("":_) = []
getGamma bitList = do
  let splits = map (splitAt 1) bitList
  let firstValues = map fst splits
  let restOfLists = map snd splits

  let onesCount = length . filter (== "1") $ firstValues
  let gamma = if onesCount > round (fromIntegral (length bitList) / 2.0) then 1 else 0

  gamma : getGamma restOfLists

main = do
  contents <- readFile "input.txt"

  -- part 1
  let gammas = getGamma $ lines contents
  let epsilons = map (\x -> if x == 1 then 0 else 1) gammas
  print $ show $ (*) (binaryToDecimal gammas) (binaryToDecimal epsilons)