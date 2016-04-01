sameHead:: Eq a => [a] -> [a] -> Bool
sameHead [] _ = False
sameHead _ [] = False
sameHead (x:xs) (y:ys) = x == y 


--biggerThan:: Num a => Int -> [a] -> [a]
--biggerThan _ [] = []
--biggerThan x (y:ys) = if y > x then y:(biggerThan x ys) else biggerThan x ys

sameList:: Eq a => [a] -> [a] -> Bool
sameList [] [] = True
sameList [] _ = False
sameList _ [] = False
sameList (x:xs) (y:ys) = if x == y then sameList xs ys else False

sumPosSqr:: [Int] -> Int
sumPosSqr list = foldr (+) 0 (map (\x -> x*x) $ filter (\x -> x >= 0) list)

wonkyLen:: IO()
wonkyLen = do
	putStrLn "Write a string and I'll tell the length"
	s <- getLine
	let l = length s
	putStrLn $ show $ (l+1) * (l+1)
	
zipWith':: (a->b->c) -> [a] -> [b] -> [c]
zipWith' f as bs = [f a b| (a,b) <- zip as bs]

absDiff:: Float -> Float -> Float
absDiff a b = abs $ a-b

nearEq:: Float -> Float -> Float -> Bool
nearEq a b c = a > (absDiff b c) 

rv:: [a] -> [Float] -> [(a, Float)]
rv list numbers 
	|(length list == length numbers) && (nearEq 0.0002 1 (sum numbers)) = zip list numbers
	|otherwise = [] 
