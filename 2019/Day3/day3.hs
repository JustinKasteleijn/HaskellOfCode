import Data.List (transpose)

validLine :: [Int] -> Bool
validLine (x:y:z:xs) = x + y > z && y + z > x && x + z > y
validLine _          = False 

parseLine :: String -> [Int]
parseLine = (map read) . words

parse :: [String] -> [[Int]] 
parse = map parseLine

valid :: [[Int]] -> [Bool]
valid = map validLine

countTrue :: [Bool] -> Int
countTrue = length . filter (\x -> x)

groupByThree :: [a] -> [[a]] 
groupByThree (x:y:z:xs) = [x,y,z] : groupByThree xs
groupByThree _ = []

parseCols :: [[Int]] -> [[Int]]
parseCols xs = concatMap transpose (groupByThree xs)

main :: IO ()
main = do
  -- Part 1
  
  let test = "5 10 25"
  print ((validLine (parseLine test)) == False)
  
  input <- readFile "input.txt"
  print $ countTrue $ valid $ parse $ lines input 
  
  -- Part 2
  print $ countTrue $ valid $ parseCols $ parse $ lines input
  
