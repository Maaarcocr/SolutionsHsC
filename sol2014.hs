import Test.QuickCheck
matches:: Eq a => a -> [a] -> [a]
matches _ [] = []
matches a (x:xs) 
  | x == a = x:(matches a xs)
  | otherwise = matches a xs

matches':: Eq a => a -> [a] -> [a]
matches' a list = [x|x <- list, x == a]

squareSum:: [Int] -> Int
squareSum list = foldr (+) 0 (map (\x -> x*x) list)

isort:: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs) 
  where 
    insert x [] = [x]
    insert x list = [y|y<-list, y <= x] ++ [x] ++ [y|y<-list, y > x]

data Set a = Set [a] deriving (Show)

makeSet:: Ord a => [a] -> Set a
makeSet list = Set $ isort (foldr (\x acc -> if elem x acc then acc else x:acc) [] list) -- this was not asked, but in order to test the code I have made this function

mapSet:: Ord b => Set a -> (a -> b) -> Set b
mapSet (Set list) f = makeSet $ map f list 

filterSet:: Ord a => Set a -> (a -> Bool) -> Set a
filterSet (Set list) p = makeSet $ filter p list

memSet:: Ord a => a -> Set a -> Bool
memSet _ (Set []) = False
memSet x (Set list) 
  | x < (list !! half) = memSet x $ makeSet $ take half list
  | x > (list !! half) = memSet x $ makeSet $ drop (half+1) list
  | otherwise = True
  where half = (div (length list) 2)

sum':: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + (sum' xs)

sumAcc:: [Int] -> Int 
sumAcc list  = helper list 0
  where 
    helper [] acc = acc
    helper (x:xs) acc = helper xs $ acc + x

sumInts:: Int -> IO()
sumInts acc = do
  y <- getLine
  let x = read y
  if x == 0 then putStrLn $ show acc else sumInts $ acc + x
  return ()  

prop_sum:: [Int] -> Bool
prop_sum x = sum' x == sumAcc x 
