import qualified Data.List.Split as S
import Text.RE.Summa (parseInteger)

data Direction = Up | Down | Forward deriving(Show)

parseDirection :: String -> Either String Direction
parseDirection str
  | str == "up"        = return Up
  | str == "down"      = return Down
  | str == "forward"   = return Forward
  | otherwise          = Left $ "Could not parse: " ++ str

data Move = Move Direction Int deriving(Show)

parseMove :: String -> Either String Move
parseMove str = do
    let [direction, amount] = S.splitOn " " str
    parsed_amount <- (case parseInteger amount of 
        Just x -> Right x 
        Nothing -> Left $ "Could not parse amount: " ++ amount)
    parsed_direction <- parseDirection direction
    return $ Move parsed_direction parsed_amount

calculatePosition :: [Move] -> (Int, Int) 
calculatePosition = foldr calculatePositionHelper (0, 0)
  where 
    calculatePositionHelper (Move Down n) (x, y) = (x, y + n)
    calculatePositionHelper (Move Up n) (x, y) = (x, y - n)
    calculatePositionHelper (Move Forward n) (x, y) = (x + n, y)

calculatePositionPart2 :: [Move] -> (Int, Int, Int)
calculatePositionPart2 = foldl calculatePositionHelper2 (0, 0, 0)
  where
    calculatePositionHelper2 (x, y, z) (Move Down n) = (x, y, z + n)
    calculatePositionHelper2 (x, y, z) (Move Up n) = (x, y, z - n)
    calculatePositionHelper2 (x, y, z) (Move Forward n) = (x + n, y + z * n, z)

main = do
  contents <- readFile "input.txt"
  let moves = mapM parseMove $ lines contents

  -- part 1
  let position = calculatePosition <$> moves
  case position of
    Right (x, z) -> print $ "(" ++ show x ++ ", " ++ show z ++ ") = " ++ show (x * z) 
    Left reason -> print $ "Could not calculate position: " ++ reason

  -- part 2
  let position = calculatePositionPart2 <$> moves
  case position of
    Right (x, y, _) -> print $ "(" ++ show x ++ ", " ++ show y ++ ") = " ++ show (x * y) 
    Left reason -> print $ "Could not calculate position: " ++ reason