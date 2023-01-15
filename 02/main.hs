import qualified Data.List.Split as S
import Text.RE.Summa (parseInteger)

data Direction = Up | Down | Forward | Backward deriving(Show)

parseDirection :: String -> Either String Direction
parseDirection str
  | str == "up"        = return Up
  | str == "down"      = return Down
  | str == "forward"   = return Forward
  | str == "backward"  = return Backward
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
    calculatePositionHelper (Move Down n) (x, z) = (x, z + n)
    calculatePositionHelper (Move Up n) (x, z) = (x, z - n)
    calculatePositionHelper (Move Forward n) (x, z) = (x + n, z)
    calculatePositionHelper (Move Backward n) (x, z) = (x - n, z)

main = do
  contents <- readFile "input.txt"
  let moves = mapM parseMove $ lines contents

  -- part 1
  let position = calculatePosition <$> moves
  case position of
    Right (x, z) -> print $ "(" ++ show x ++ ", " ++ show z ++ ") = " ++ show (x * z) 
    Left reason -> print $ "Could not calculate position: " ++ reason