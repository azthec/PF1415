data Prop = Const Bool
            Var Char
            Neg Prop
            Conj Prop Prop

--Funcoes Uteis - Valor bits vars atribs

-- Exemplo (Neg(Var 'a'))
--         Conj(Const True)(Var 'b')
