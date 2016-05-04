--f4
--4.9
primos :: [Int]
primos = crivo [2..]

crivo :: [Int] -> [Int]
crivo (p:xs) = p : crivo [x | x<-xs, x`mod`p/=0]

decompor :: Int -> [Int] -> [Int]
decompor n (p:ps)                  
    |n==1         = []
    |n`mod`p==0   = [p]++decompor (n`div`p) (p:ps)
    |otherwise    = decompor n ps

factores :: Int -> [Int]
factores n = decompor n primos

--f5
--5.1

-- executar uma lista de acções em sequência
seqn :: [IO a] -> IO ()
seqn []     =  return ()
seqn (a:as) =  do a
                  seqn as

frases:: Int -> IO ()
frases n = putStr ("Se " ++ show n ++ " elefantes incomodam muita gente,\n" ++ show (n+1) ++ " incomodam muito mais!\n")

elefantes :: Int -> IO ()
elefantes n = seqn [frases p | p <- [2..n-1]]

/
--5.1
life :: Board -> Int -> IO()
life b 0     = return ()
life [] _ = return ()
life b (n+1) = do cls
                  showcells b
                  wait 100
                  life (nextgen b) n

--5.2 
life :: Board -> Int -> IO Int
life b 0     = return (length b)
life [] _ = return 0
life b (n+1) = do cls
                  showcells b
                  wait 100
                  life (nextgen b) n

--5.2
-- evolucao duma colonia
-- nao e´ o numero de geracoes 
life :: Board -> Int -> IO Int
life b 0     = return (length b)
life [] _ = return 0
life b (n+1) = do cls
                  showcells b
                  wait 100
                  life (nextgen b) n

--5.5
printsol :: Sol -> IO ()
printsol n = 


