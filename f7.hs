import Data.Char
import qualified Data.Set as Set
{- 
module Parens where

import Data.Map (Map)
import qualified Data.Map as Map

matchingParens :: Map Char Char
matchingParens = Map.fromList [
    ('(', ')')
  , ('{', '}')
  , ('[', ']')
  ]

isOpening :: Char -> Bool
isOpening c = maybe False (const True) $ Map.lookup c matchingParens

type Stack a = [a]

balanced :: String -> Bool
balanced = balanced' []

balanced' :: Stack Char -> String -> Bool
balanced' [] ""     = True
balanced' _  ""     = False
balanced' [] (c:cs) = balanced' [c] cs
balanced' (o:os) (c:cs)
  | isOpening c = balanced' (c:o:os) cs
  | otherwise   = case Map.lookup o matchingParens of
      Nothing -> False
      Just closing -> if closing == c
        then balanced' os cs
        else False
-}

{- Implementação de um tipo abstrato para pilhas 
   Pedro Vasconcelos, 2011-2013
   
   "Interface" exportada:

   data Stack a 
   
   empty :: Stack a
   push :: a -> Stack a -> Stack a
   pop :: Stack a -> Stack a
   top :: Stack a -> a
   isEmpty :: Stack a -> Bool

-}
{-
module Stack (Stack,
              empty, isEmpty,
              pop, push, top) where

data Stack a = Stk [a]

empty :: Stack a
empty = Stk []

push :: a -> Stack a -> Stack a
push x (Stk xs) = Stk (x:xs)

pop :: Stack a -> Stack a
pop (Stk (x:xs)) =  Stk xs
pop _  = error "Stack.pop: empty stack"

top :: Stack a -> a
top (Stk (x:xs)) = x
top _  = error "Stack.top: empty stack"

isEmpty :: Stack a -> Bool
isEmpty (Stk []) = True
isEmpty _        = False



--7.1
parent :: String -> Bool
parent [] = True                        
parent str = parent' str empty

--TODO [ & {
--funcoes auxiliares
parent' :: String -> Stack Char -> Bool 
parent' [] stk = isEmpty stk
parent' (c:str) stk
        | (c == '(') = parent' str (push c stk)
        | (c == ')') = if isEmpty stk then False
                       else if top stk == '(' then parent' str (pop stk)
                       else False

--7.2
solveRPN :: (Num a, Read a) => String -> a  
solveRPN = head . foldl foldingFunction [] . words   --foldl L->R --words parser whitespace, words :: String -> [String]
    where   foldingFunction (x:y:ys) "*" = (x * y):ys  
            foldingFunction (x:y:ys) "+" = (x + y):ys  
            foldingFunction (x:y:ys) "-" = (y - x):ys  
            foldingFunction xs numberString = read numberString:xs  
-}
--7.3
--cenas teoricas
{- 
1) pop(push x s) = s
   --pela def de s
   pop(push x (Stk xs) = Stk xs
   -- pela def de push
   pop(Stk(x:xs)
   -- pela def de pop
   Stk xs

2) top(push x s) = x
   top(push x (Stk xs)
   top(Stk x:xs)
   x

3) isEmpty empty = True
   isEmpty (Stk [])
   True

4) isEmpty (push x s) = False
   isEmpty (push x Stk s)
   isEmpty (Stk x:s)
   False
-}

--7.4


main :: IO ()
main = do txt <- getContents
                 let ws = map clean (words txt) --lista
                 let s = Set.fromList ws --construir conjunto
                 print (Set.size s)  --numero de palavras
      where
        clean = map toLower . filter isLetter
