-- 3.2

-- a
and2 :: [Bool] -> Bool
and2 [] = True
and2 (x:xs) = if (x==True) then and xs else False

and3 :: [Bool] -> Bool
and3 [] = True
and3 (x:xs) | x==True = and xs
			| otherwise = False

-- b
or2 :: [Bool] -> Bool
or2 [] = False
or2 (x:xs) | x==False = or2 xs
		   | otherwise = True

-- c
concat2 :: [[a]] -> [a]
concat2 [] = []
concat2 (x:xs) = x ++ concat2 xs

-- d
replicate2 :: Int -> a -> [a]
replicate2 0 x = []
replicate2 n x = x : replicate2 (n-1) x

-- e
(!?) :: [a] -> Int -> a
(x:_) !? 0  = x
(_:xs) !? n = xs !! (n-1)

-- f
elem2 :: Eq a => a -> [a] -> Bool
elem2 v [] = False
elem2 v (x:xs) | v/=x  = elem2 v xs
			   | otherwise = True

-- 3.3
concat3 :: [[a]] -> [a]
concat3 xss = [x | xs <- xss, x <-xs]

replicate3 :: Int -> a -> [a]
replicate3 n x= [x | i<-[0..n-1]]


-- 3.4
--nub :: Eq a => [a] -> [a]
--nub (x:xs) = 

-- 3.5
intersperse :: a -> [a] ->[a]
intersperse k [] = []
intersperse k (x:xs) = x :
					   if length xs >= 1 then k : (intersperse k xs)
					   else (intersperse k xs)

-- 3.6 

-- a
insert :: Ord a => a -> [a] -> [a]
insert n [] = n : []
insert n (x:xs)	| n <= x = n : x : xs
				| otherwise = x : insert n xs


-- b
isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)


-- 3.7

-- a
minimo :: Ord a => [a] -> a
--minimo l = head (isort l)
minimo [x] = x
minimo (x:xs) = min x (minimo xs)


-- b
delete :: Eq a => a -> [a] -> [a]
delete n [] = []
delete n (x:xs) | x==n = xs
				| otherwise = x : (delete n xs)


-- c
ssort :: Ord a => [a] -> [a]
ssort [] = []
ssort l = x : ssort (delete x l)
	where x = minimo l



-- 3.8

-- a
merge :: Ord a => [a] -> [a] -> [a]
merge x [] = x
merge [] y = y
merge (x:xs) (y:ys) | x <= y = x : (merge xs (y:ys))
					| y <= x = y : (merge (x:xs) ys)


-- b
msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort l = merge (msort x) (msort y)
	where
		x = take (length l `div` 2) l
		y = drop (length l `div` 2) l
