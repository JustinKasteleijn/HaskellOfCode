import Data.List (group, sort, transpose, maximumBy, minimumBy)
import Data.Ord (comparing)

mostCommon :: (Eq a, Ord a) => [a] -> a
mostCommon = fst 
  . maximumBy (comparing snd) 
  . map (\xs -> (head xs, length xs)) 
  . group 
  . sort
  
leastCommon :: (Eq a, Ord a) => [a] -> a
leastCommon = fst 
  . minimumBy (comparing snd) 
  . map (\xs -> (head xs, length xs)) 
  . group 
  . sort

main :: IO ()
main = do 
  -- Part 1
  let test = "eedadn\ndrvtee\neandsr\nraavrd\natevrs\ntsrnev\nsdttsa\nrasrtv\nnssdts\nntnada\nsvetve\ntesnvt\nvntsnd\nvrdear\ndvrsen\nenarar"
  print ((map mostCommon (transpose (lines test))) == "easter")
  
  input <- readFile "input.txt" 
  print $ map mostCommon $ transpose $ lines input 
  
  -- Part 2
  print $ map leastCommon $ transpose $ lines input 
  
