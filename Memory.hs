module Memory where

import Lexer



-- id, tipo, valor
type Symtable = (Token, [Token], Token)
-- nome do escopo
type ActivStack = String


-- funções para a tabela de símbolos

get_default_value :: [Token] -> Token
get_default_value ([Type pos "int" ]) = Int pos 0   
get_default_value ([Type pos "float" ]) = Float pos 0.0   
get_default_value ([Type pos "bool" ]) = Bool pos True 
get_default_value ([Type pos "string" ]) = String pos "" 
get_default_value ([Type pos "array", BeginIndex pos1 , Type pos2 _, EndIndex pos4  ]) = Array pos []
get_default_value ([Type pos "matrix", BeginIndex pos1 , Type pos2 _, EndIndex pos4  ]) = Matrix pos [[]]
 


symtable_insert :: Symtable -> ([ActivStack], [Symtable])-> ([ActivStack], [Symtable])
symtable_insert symbol (activ,[])  = (activ,[symbol])
symtable_insert symbol (activ,table)  = (activ,[symbol]++table)



symtable_update :: Symtable -> ([ActivStack], [Symtable])-> ([ActivStack], [Symtable])
symtable_update _ (activ, []) = fail "variable not found"
symtable_update (id1, t1, v1) ( activ, symt ) = 
                               (activ, symtable_update_auxiliar (id1, t1, v1) symt )

symtable_update_auxiliar :: Symtable -> ([Symtable])-> ([Symtable])
symtable_update_auxiliar (id1, t1, v1) ((id2, t2, v2):t) = 
                                if id1 == id2 && t1 == t2 then ((id1, t1, v1) : t)
                                else (id2, t2, v2) : symtable_update_auxiliar (id1, t1, v1) t


--symtable_remove :: (Token,Token) ->  ([ActivStack], [Symtable])->  [(ActivStack, Symtable)]
--symtable_remove _ [] = fail "variable not found"
--symtable_remove (id1, v1) ((id2, v2):t) = 
--                               if id1 == id2 then t
--                               else (id2, v2) : symtable_remove (id1, v1) t                               
