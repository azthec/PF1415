-- 1.1
inc :: Num a => a -> a
inc x = x + 1

dobro :: Num a => a -> a
dobro x = x + x

quadrado :: Num a => a -> a -- Pode dar resultados errados. Usar tipos inteiros para evitar erros
quadrado x = x*x

media :: Fractional a => a -> a -> a
media x y = (x + y)/2

-- 1.2
triangulo :: Int -> Int -> Int -> Bool
triangulo a b c | a < b+c && b < a+c && c < a+b = True
				| otherwise = False


-- 1.3
heron :: Floating a => a -> a -> a -> a
heron a b c = sqrt (s*(s-a)*(s-b)*(s-c))
				where s = (a+b+c)/2


-- 1.4
metades :: [a] -> ([a],[a])
metades x = (take m x, drop m x)
			where m = length x `div` 2

-- 1.5
-- a
last1 :: [a] -> a
last1 x = head (reverse x)

last2 :: [a] -> a
last2 x = head (drop (length x -1) x)

-- b
init1 :: [a] -> [a]
init1 x = take (length x -1) x

init2 :: [a] -> [a]
init2 x = reverse (drop 1 (reverse x))


-- 1.6
-- a
binom :: Integral a => a -> a -> a
binom n k = product [1..n] `div` (product [1..k] * product [1..(n-k)])

-- b
binom2 :: Integral a => a -> a -> a
binom2 n k = product [(k+1)..n] `div` product [1..(n-k)]


segundo xs = head (tail xs)
trocar (x, y) = (y, x)
par x y = (x, y)
dobro2 x = 2 * x
metade x = x/2
minuscula x = x >= 'a' && x <= 'z'
intervalo x a b = x >= a && x <= b
palindromo xs = reverse xs == xs
twice f x = f (f x)
