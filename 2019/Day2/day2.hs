import Prelude hiding (Left, Right)


type KeyPad = [[Int]]
type Position = (Int, Int)

keyPad :: KeyPad
keyPad = [[1,2,3],[4,5,6],[7,8,9]]

data Dir 
  = Up
  | Down
  | Left
  | Right
 deriving (Show)
 
getDigits :: Position -> [[Dir]] -> [Int]
getDigits _ []           = []
getDigits start (d:dirs) = let (v, loc) = getDigit start d
                           in v : (getDigits loc dirs) 
  
getDigit :: Position -> [Dir] -> (Int, Position)
getDigit loc dirs           = let (y, x) = getDigit' loc dirs
                              in ((keyPad !! y !! x), (y,x))
  where 
    getDigit' :: Position -> [Dir] -> Position
    getDigit' loc []            = loc 
    getDigit' loc (Up:dirs)    = case loc of 
                                  (0, x) -> getDigit' (0, x) dirs
                                  (y, x) -> getDigit' (y - 1, x) dirs
    getDigit' loc (Down:dirs)  = case loc of 
                                  (2, x) -> getDigit' (2, x) dirs
                                  (y, x) -> getDigit' (y + 1, x) dirs
    getDigit' loc (Left:dirs)  = case loc of 
                                  (y, 0) -> getDigit' (y, 0) dirs
                                  (y, x) -> getDigit' (y, x - 1) dirs
    getDigit' loc (Right:dirs) = case loc of 
                                  (y, 2) -> getDigit' (y, 2) dirs
                                  (y, x) -> getDigit' (y, x + 1) dirs

parse :: String -> [[Dir]]
parse = map (map toDir) 
  . lines 

toDir :: Char -> Dir 
toDir 'U' = Up
toDir 'D' = Down
toDir 'L' = Left
toDir 'R' = Right
toDir _ = error "Invalid digit"

-- Part 2
fancyKeypad :: [String]
fancyKeypad =
  [ 
    "  1  "
    , " 234 "
    , "56789"
    , " ABC "
    , "  D  "
  ]

move :: Position -> Char -> Position
move (r, c) 'U' = if valid (r - 1, c) then (r - 1, c) else (r, c)
move (r, c) 'D' = if valid (r + 1, c) then (r + 1, c) else (r, c)
move (r, c) 'L' = if valid (r, c - 1) then (r, c - 1) else (r, c)
move (r, c) 'R' = if valid (r, c + 1) then (r, c + 1) else (r, c)
move pos _      = pos

valid :: Position -> Bool
valid (r, c) = r >= 0 && r < 5 && c >= 0 && c < 5 && (fancyKeypad !! r !! c /= ' ')

followInstructions :: Position -> String -> Position
followInstructions = foldl move

solve :: [String] -> String
solve instructions = map (\(r, c) -> fancyKeypad !! r !! c) (tail (scanl followInstructions (2, 0) instructions))

main :: IO ()
main = do
  input <- readFile "input.txt"
  
  -- Part 1
  let test = "ULL\nRRDDD\nLURDL\nUUUUD"
  print ((getDigits (1, 1) (parse test)) == [1,9,8,5])
  print $ getDigits (1, 1) (parse input)
  
  -- Part 2
  print $ (solve (lines test)) == "5DB3"
  print $ solve $ lines input
  
  
  
