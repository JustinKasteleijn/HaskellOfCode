import Data.Char (isDigit, ord, chr)
import Data.List (sortBy, isInfixOf)
import Data.Ord (comparing)

type Distribution = [(Char, Int)]  

data Room = Room 
 { 
  distribution :: Distribution 
  , id :: Int                      
  , order :: String                 
 } 
 deriving (Show, Eq)
 
parseRooms :: [String] -> [Room]
parseRooms = map parseRoom
 
parseRoom :: String -> Room 
parseRoom str = let (dist, rest) = parseDistribution str
                    id           = parseId rest
                    order        = parseOrder rest
                in Room dist id order

parseDistribution :: String -> (Distribution, String)
parseDistribution str =
    let split' = split '-' str
        rest  = last split'
        dist  = concat $ init split'
    in (go dist [], rest)
  where
    go :: String -> Distribution -> Distribution
    go [] acc = acc
    go (c:cs) acc =
        let updated = case lookup c acc of
                        Just x  -> (c, x + 1) : filter ((/= c) . fst) acc
                        Nothing -> (c, 1) : acc
        in go cs updated
        
parseId :: String -> Int 
parseId = read . (takeWhile isDigit)

parseOrder :: String -> String 
parseOrder = (takeWhile (/=']')) 
  . (drop 1) 
  . (dropWhile (/='['))

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

realRoom :: Room -> Int
realRoom (Room dist id order) = let checksum' = checksum $ orderDist dist 
                                in case take 5 checksum' == order of
                                    True  -> id 
                                    False -> 0
                                    

orderDist :: Distribution -> Distribution
orderDist = sortBy (comparing (negate . snd) <> comparing fst) 

checksum :: Distribution -> String 
checksum = map fst 

realRooms :: [Room] -> [Int]
realRooms = map realRoom

parsePart2 :: String -> (String, Int)
parsePart2 str = let spaces = map (\c -> if c == '-' then ' ' else c) str
                     name   = takeWhile (not . isDigit) spaces
                     id     = read ((takeWhile isDigit) $ dropWhile (not . isDigit) spaces) :: Int 
                 in (name, id) 
                 
decrypt :: String -> Int -> (String, Int)
decrypt room id = (map (`decrypt'` modId id) room, id)
  where 
    modId :: Int -> Int 
    modId x = x `mod` 26
    
    decrypt' :: Char -> Int -> Char
    decrypt' c shift
      | c == ' '  = c 
      | c >= 'a' && c <= 'z' = shiftChar c 'a' shift  
      | c >= 'A' && c <= 'Z' = shiftChar c 'A' shift  
      | otherwise = c  

    shiftChar :: Char -> Char -> Int -> Char
    shiftChar c base shift = let offset = ord c - ord base 
                                 newChar = chr (ord base + (offset + shift) `mod` 26)  
                             in newChar

findNorthPole :: [(String, Int)] -> Maybe Int
findNorthPole [] = Nothing
findNorthPole ((name, id):xs) 
  | "north" `isInfixOf` name = Just id
  | otherwise                = findNorthPole xs    

main :: IO ()
main = do 
  -- Part 1
  let test = "aaaaa-bbb-z-y-x-123[abxyz]\na-b-c-d-e-f-g-h-987[abcde]\nnot-a-real-room-404[oarel]\ntotally-real-room-200[decoy]"
  print ((sum (realRooms (parseRooms (lines test)))) == 1514) 
  
  input <- readFile "input.txt"
  print $ sum $ realRooms $ parseRooms $ lines input
  
  -- Part 2
  print $ parsePart2 "aaaaa-bbb-z-y-x-123[abxyz]"
  
  let parsed = map parsePart2 (lines input)
  let decrypted = map (\(name, id) -> decrypt name id) parsed
  print $ maybe 0 Prelude.id (findNorthPole decrypted)
