xor:: Bool -> Bool -> Bool
xor x y
  | x == y = False
  | otherwise = True

combi::Real a => [(a,a)] -> [a]
combi xs = (sum [x | (x,y) <- xs, x > y]):(sum [y | (x,y) <- xs, x > y]):[]
zipWith':: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f xs ys = map (\(x,y) -> f x y) (zip xs ys) 

qSort:: Ord a => [a] -> [a]
qSort [] = []
qSort (x:xs) = (qSort [y| y <- xs, y <= x]) ++ [x] ++ (qSort [y| y <- xs, y > x])

curry':: ((a,b) -> c) -> (a -> b -> c)
curry' f = \x y -> f(x,y)

uncurry:: (a->b->c) -> ((a,b) -> c)
uncurry f = \(x,y) -> f x y

filterFirst:: (a -> Bool) -> [a] -> [a]
filterFirst _ [] = []
filterFirst p (x:xs)
  | p x = x:(filterFirst p xs)
  | otherwise = xs 

filterLast:: (a->Bool) -> [a] -> [a]
filterLast p (list) = 
  reverse (filterFirst p (reverse list)) 

backwards:: IO()
backwards = do
  putStrLn "Tell me something"
  text <- getLine
  putStrLn $ reverse text
  return ()

mapFuns:: [(a->b)] -> a -> [b]
mapFuns [] _ = []
mapFuns (f:fs) x = (f x):(mapFuns fs x)

mapFuns':: [(a->b)] -> a -> [b]
mapFuns' fs x = foldr (\f acc ->(f x):acc ) [] fs

foldr':: (a -> b -> b) -> b -> [a] -> b
foldr' f base [] = base
foldr' f base (x:xs) = f x $ foldr' f base xs

split:: [a] -> ([a], [a])
split list = helper list ([], [])
  where 
    helper [] sol = sol
    helper [x] (a, b) = (a ++ [x], b)
    helper (x:y:s) (a,b) = helper s (a ++ [x], b ++ [y])
