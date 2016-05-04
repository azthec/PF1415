-- 2.1
-- a
max3 :: Ord a => a -> a -> a -> a
max3 x y z = if (x >= y && x >= z) then x else
				if (y >= x && y >= z) then y else z

min3 :: Ord a => a -> a -> a -> a
min3 x y z = if (x <= y && x <= z) then x else
				if (y <= x && y <= z) then y else z

-- b
maxx :: Ord a => a -> a -> a -> a
maxx x y z = max x (max y z)

minx :: Ord a => a -> a -> a -> a
minx x y z = min x (min y z)


-- 2.2
classifica :: Int -> String
classifica x | x<= 9 = "reprovado"
			 | x<=12 = "suficiente"
			 | x<=15 = "bom"
			 | x<=18 = "muito bom"
			 | otherwise = "muito bom com distinção"


-- 2.3
xor :: Bool -> Bool -> Bool
xor x y | x == y = False
		| otherwise = True


-- 2.4
safetail :: [a] -> [a]
safetail [] = []
safetail (x:xs) = xs


-- 2.5
-- a
curta :: [a] -> Bool
curta x | length x <=2 = True
		| otherwise = False

-- b
curta2 :: [a] -> Bool
curta2 [] = True
curta2 [_] = True
curta2 [_,_] = True
curta2 l = False

--2.6

--2.7

-- 2.8
-- a
aprox :: (Integral a, Fractional b) => a -> b
aprox x = 4 * (sum [(-1)^n/fromIntegral(2*n+1) | n<-[0..x]])

-- b
aprox' :: (Integral a, Floating b) => a -> b
aprox' x = sqrt (12 * sum [(-1)^k/fromIntegral((k+1)^2) | k<-[0..x]])


-- 2.9
divprop :: Int -> [Int]
divprop n = [x | x<-[1..(n `div` 2)], n `mod` x == 0]


-- 2.10
perfeitos :: Int -> [Int]
perfeitos n = [x | x<-[2..n], sum (divprop x) == x]

-- 2.11
divprop' :: Int -> [Int]
divprop' n = [x | x<-[1..n], n `mod` x == 0]

primo :: Int -> Bool
primo n | divprop' n == [1,n] = True
		| otherwise = False

-- 2.13
binom :: Integral a => a -> a -> a
binom n k = product [(k+1)..n] `div` product [1..(n-k)]

pascal :: Int -> [[Int]]
pascal x = [ [binom n k | k <- [0..n]] | n <-[0..x] ]


-- 2.14
-- aux
prodsum :: Num a => [(a,a)] -> a
prodsum [] = 0
prodsum (x:xs) = prod x + prodsum xs
prod (x,y) = x*y


dotproduct :: Num a => [a] -> [a] -> a
dotproduct x y = prodsum (zip x y)


-- 2.15
pitagoricos :: Int -> [(Int,Int,Int)]
pitagoricos n = [ (x,y,z) | x <-[1..n], y <-[1..n], z <- [1..n], x*x + y*y == z*z ]
