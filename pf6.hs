import Data.List


data Prop = Const Bool      --constantes
          | Var Char        --variaveis
          | Neg Prop        --negacao
          | Conj Prop Prop  --conjucao
          | Disj Prop Prop  --disjuncao
          | Impl Prop Prop  --implicacao
            deriving (Eq,Show)

--listas de associacoes entre chaves e valores
type Assoc ch v = [(ch,v)]
--funcao para procurar valor associado numa chave
find2 :: Eq ch => ch -> Assoc ch v -> v
find2 ch assocs = head [v | (ch',v) <- assocs, ch==ch']

--atribuicao de valores as variaveis
type Atrib = Assoc Char Bool
--para correr todas as proposicoes logicas
valor :: Atrib -> Prop -> Bool
valor s (Const b) = b
valor s (Var x) = find2 x s
valor s (Neg p) = not (valor s p)
valor s (Conj p q) = valor s p && valor s q
valor s (Disj p q) = valor s p || valor s q
valor s (Impl p q ) = not (valor s p) || valor s q


--gera todas as sequencias de n boleanos, recursiva
bits :: Int -> [[Bool]]
bits 0 = [[]]
bits n = [b:bs | bs <-bits (n-1), b<-[False,True]]

--gera atribuicoes, lista todas as variaveis numa proposicao
vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Neg p) = vars p
vars (Conj p q) = vars p ++ vars q
vars (Disj p q) = vars p ++ vars q
vars (Impl p q) = vars p ++ vars q

--gera todas as atribuicoes de variaveis duma proposicao
atribs :: Prop -> [Atrib]
atribs p = map (zip vs) (bits (length vs))
           where vs = nub (vars p)
--6.1
satisfaz :: Prop -> Bool
satisfaz p = or [valor s p | s<- atribs p]

--6.2
-- 2 props sao equiv se p => s && s => p for tautologia

tautologia :: Prop -> Bool
tautologia p = and [valor s p | s<-atribs p]

equiv :: Prop -> Prop -> Bool
equiv p s = tautologia (Conj (Impl p s) (Impl s p))

--6.3

--tautologia2 :: Prop -> bool
--tautologia2 p = 

--6.4
--showProp :: Prop -> String
--showProp Const 


--Const Bool print Bool
--Var Char print Char
--Neg Prop pring ~showProp Prop
--Conj Prop Prop print showProp Prop && showProp Prop
--Disj Prop Prop print showProp Prop || showProp Prop
--Impl Prop Prop print showProp Prop -> showProp Prop

--valor :: Atrib -> Prop -> Bool
--valor s (Const b) = b
--valor s (Var x) = find2 x s
--valor s (Neg p) = not (valor s p)
--valor s (Conj p q) = valor s p && valor s q
--valor s (Disj p q) = valor s p || valor s q
--valor s (Impl p q ) = not (valor s p) || valor s q


--6.5
--tabela :: Prop -> IO()


data Arv a = Vazia 
           | No a (Arv a) (Arv a)
             deriving Show
--6.6
sumArv :: Num a => Arv a -> a
sumArv Vazia = 0
sumArv (No x esq dir) = x + (sumArv esq) + (sumArv dir)

--6.7
decres :: Arv a -> [a]
decres Vazia = []
decres (No x esq dir) = decres dir ++ [x] ++ decres esq

--6.8
cres :: Int -> Int -> Arv a -> [a]
cres _ _ Vazia = []
cres n x (No y esq dir) = if (n == x) then cres n (x+1) esq ++ [y] ++ cres n (x+1) dir else cres n (x+1) esq ++ cres n (x+1) dir

nivel :: Int -> Arv a -> [a]
nivel n arv = cres n 0 arv

