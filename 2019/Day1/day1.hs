import Prelude hiding (Right, Left)
import Data.Char (isDigit)

data Turn
  = Right Int 
  | Left Int
 deriving (Show)
  
data Direction 
  = North
  | South
  | East
  | West
 deriving (Show, Eq)
  
type Coordinate = (Direction, Int, Int)

eqCord :: Coordinate -> Coordinate -> Bool
eqCord (_, x, y) (_, x', y') = x == x' && y == y'
  
changeDirection :: Turn -> Coordinate -> Coordinate 
changeDirection (Right x) (North, x', y') = (East, x' + x, y')
changeDirection (Right x) (South, x', y') = (West, x' - x, y')
changeDirection (Right x) (East, x', y')  = (South, x', y' - x)  
changeDirection (Right x) (West, x', y')  = (North, x', y' + x)
changeDirection (Left x) (North, x', y')  = (West, x' - x, y')
changeDirection (Left x) (South, x', y')  = (East, x' + x, y')
changeDirection (Left x) (East, x', y')   = (North, x', y' + x)
changeDirection (Left x) (West, x', y')   = (South, x', y' - x)

calculateCoordinate :: [Turn] -> Coordinate
calculateCoordinate = foldl (flip changeDirection) (North, 0, 0)

sumCoordinate :: Coordinate -> Int
sumCoordinate (_, x, y) = (abs x) + (abs y)

stringToTurn :: String -> Maybe Turn
stringToTurn [] = Nothing
stringToTurn (s:s':str) 
  | s == 'R'  = Just $ Right (read num :: Int)
  | s == 'L'  = Just $ Left (read num :: Int)
  | otherwise = stringToTurn (s':str)
 where 
  (num, _) = span isDigit (s':str)
  
parse :: String -> [Turn]
parse = unmaybe 
  . (map stringToTurn) 
  . (split ',')

split :: (Eq a) => a -> [a] -> [[a]]
split delim xs = case splitOnce delim xs of 
                      (val, Nothing)        -> [val]
                      (val, Just remainder) -> val:split delim remainder 

splitOnce :: (Eq a) => a -> [a] -> ([a], Maybe [a])
splitOnce _ [] = ([], Nothing)
splitOnce delim (x:xs) 
  | x == delim = ([], Just xs)
  | otherwise  = let (before, after) = splitOnce delim xs
                 in (x:before, after)
                 
unmaybe :: [Maybe a] -> [a]
unmaybe ms = [x | Just x <- ms] 

-- Part 2

expandSteps :: Coordinate -> Coordinate -> [Coordinate]
expandSteps (_, x1, y1) (dir, x2, y2) =
  case dir of
    North -> [(North, x1, y) | y <- [y1 + 1 .. y2]]
    South -> [(South, x1, y) | y <- [y1 - 1, y1 - 2 .. y2]]
    East  -> [(East, x, y1)  | x <- [x1 + 1 .. x2]]
    West  -> [(West, x, y1)  | x <- [x1 - 1, x1 - 2 .. x2]]

changeDirectionPart2 :: Turn -> Coordinate -> [Coordinate]
changeDirectionPart2 (Right x) (North, x', y') = expandSteps (North, x', y') (East, x' + x, y')
changeDirectionPart2 (Right x) (South, x', y') = expandSteps (South, x', y') (West, x' - x, y')
changeDirectionPart2 (Right x) (East, x', y')  = expandSteps (East, x', y')  (South, x', y' - x)
changeDirectionPart2 (Right x) (West, x', y')  = expandSteps (West, x', y')  (North, x', y' + x)
changeDirectionPart2 (Left x) (North, x', y')  = expandSteps (North, x', y') (West, x' - x, y')
changeDirectionPart2 (Left x) (South, x', y')  = expandSteps (South, x', y') (East, x' + x, y')
changeDirectionPart2 (Left x) (East, x', y')   = expandSteps (East, x', y')  (North, x', y' + x)
changeDirectionPart2 (Left x) (West, x', y')   = expandSteps (West, x', y')  (South, x', y' - x)

calculateCoordinatePart2 :: [Turn] -> Maybe Coordinate
calculateCoordinatePart2 turns = go (North, 0, 0) [] turns
  where
    go _ _ [] = Nothing
    go pos visited (t:ts) =
      let steps = changeDirectionPart2 t pos
      in case firstDuplicate visited steps of
           Just coord -> Just coord
           Nothing    -> go (last steps) (visited ++ steps) ts

    firstDuplicate :: [Coordinate] -> [Coordinate] -> Maybe Coordinate
    firstDuplicate visited [] = Nothing
    firstDuplicate visited (c:cs)
      | (snd3 c, trd3 c) `elem` [(snd3 v, trd3 v) | v <- visited] = Just c
      | otherwise = firstDuplicate (visited ++ [c]) cs

    snd3 (_, x, _) = x
    trd3 (_, _, y) = y
    
main :: IO ()
main = do 
  let test1 = "R2, L3"
  let test2 = "R2, R2, R2"
  let test3 = "R5, L5, R5, R3" 
  input <- readFile "input.txt"
  print $ calculateCoordinate $ parse input 
  print $ (sumCoordinate (calculateCoordinate (parse test1))) == 5
  print $ (sumCoordinate (calculateCoordinate (parse test2))) == 2
  print $ (sumCoordinate (calculateCoordinate (parse test3))) == 12
  
  --Part 2
  let testP2 = "R8, R4, R4, R8"
  print $ (case calculateCoordinatePart2 (parse testP2) of
              Just (dir, x, y) -> sumCoordinate (dir, x, y)
              Nothing -> 0) == 4
  print $ sumCoordinate $ maybe (North, 0, 0) id (calculateCoordinatePart2 (parse input))
