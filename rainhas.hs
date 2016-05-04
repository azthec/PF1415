{- 
   Solução do problema das oito rainhas
   Colocar 8 rainhas num tabuleiro de Xadrez 
   de forma a que nenhuma rainha esteja em linha de ataque de outra
   Baseada no solução do livro 
   "Introduction to Functional Programming" de Bird & Wadler
   Pedro Vasconcelos, 2011
-}


type Pos = (Int,Int) -- posição no tabuleiro (coluna, linha)

type Sol = [Int]     -- uma solução: lista das linhas das rainhas


-- colocar n rainhas no tabuleiro
rainhas :: Int -> [Sol]
rainhas 0 = [[]]   -- caso base: uma só solução (vazia)
rainhas k          -- caso recursivo
    | k>0 = [sol ++ [n] | sol<-rainhas (k-1),  -- coloca k-1 rainhas
                          n<-[1..8],        -- linha para nova rainha
                          verifica sol n]   -- verifica se pode colocar 

-- verifica se pode colocar uma nova rainha 
verifica :: Sol -> Int -> Bool
verifica sol m
    = and [not (linha (i,j) (n+1,m)) | (i,j)<-zip [1..n] sol]
    where n = length sol  -- número de colunas já colocadas

-- verificar se duas rainhas estão em linha
linha :: Pos -> Pos -> Bool
linha (i,j) (n,m) 
    =  -- i==n ||   -- mesma coluna (desnecessário)
       j==m ||      -- mesma linha
       i+j==n+m ||  -- mesmas diagonais
       i-j==n-m

