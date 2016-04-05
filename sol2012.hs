ones:: a -> Int
ones _ = 1

myLength:: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1+myLength(xs)

myLength' list = foldr (+) 0 (map ones list) 

mod':: Int -> Int -> Int
mod' a b
	| a >= b = mod' (a-b) b
	| otherwise = a

divides:: Int -> Int -> Bool
divides a b = if mod b a == 0 then True else False

strictF:: Int -> [Int]
strictF n = helper n n
helper n 1 = [1]
helper n t 
	|divides t n = t:(helper n (t-1))
	|otherwise = helper n (t-1)
primesUpTo:: Int -> [Int]
primesUpTo n 
	|n < 2 = []
	|n == 2 = [2]
	|otherwise = if (myLength $ strictF n) == 2 then n:(primesUpTo (n-1)) else primesUpTo (n-1)
	
primeList:: IO()
primeList = do
	putStrLn "Write a number"
	number <- getLine
	putStrLn (show $ primesUpTo $ read number)

elem':: Eq a => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs) = if a == x then True else (elem' a xs) 

alphanums:: String -> String
alphanums [] = ""
alphanums (x:xs)
	|elem' x ".,;' " = alphanums xs
	|otherwise = x:(alphanums xs)

-- using list-compre.
alphanums' :: [Char] -> [Char]
alphanums' xs = [ x | x <- xs , (x `elem` ['a'..'z']) || (x `elem` ['A'..'Z']) ]

foldr':: (a->b->b) -> b -> [a] -> b 
foldr' _ e [] = e
foldr' f e (x:xs) = f x (foldr' f e xs)

cube:: Num a => a -> a
cube x = x*x*x

mySum::Num a => [a] -> a
mySum list = foldr (+) 0 (map cube list)

-- without helper function
mySum' :: Num a => [a] -> a
mySum' xs = foldr (+) 0 $ map (\x -> x^3) xs
