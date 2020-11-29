module Memory where

import Lexer



-- escopo, id, tipo, valor
type Symtable = (String, Token, [Token], Token)
-- nome do escopo
type ActivStack = String



get_default_value :: [Token] -> Token
get_default_value ([Type pos "int" ]) = Int pos 0   
get_default_value ([Type pos "float" ]) = Float pos 0.0   
get_default_value ([Type pos "bool" ]) = Bool pos True 
get_default_value ([Type pos "string" ]) = String pos "" 
get_default_value ([Type pos "array", BeginIndex pos1 , Type pos2 _, EndIndex pos4  ]) = Array pos []
get_default_value ([Type pos "matrix", BeginIndex pos1 , Type pos2 _, EndIndex pos4  ]) = Matrix pos [[]]
 

-- funções para verificação de tipos
compatible :: Token -> Token -> Bool
compatible (Int _ _) (Int _ _) = True
compatible (Float _ _) (Float _ _) = True
compatible (Bool _ _) (Bool _ _) = True
compatible (String _ _) (String _ _) = True
compatible _ _ = False



-- recebe o id, compara id e escopo e retorna o valor
get_type :: Token -> ([ActivStack], [Symtable]) -> Token
get_type _ (activ,[]) = error "variable not found"
get_type id1 (activ,symt) = get_type_auxiliar (get_top activ, id1) symt
    


get_type_auxiliar :: (String, Token) -> ([Symtable])-> Token
get_type_auxiliar _ ([]) = error "variable not found"
get_type_auxiliar (es1, Id pos1 id1) ((es2, Id pos2 id2, t2, v2):t) =     
                                if id1 == id2 && es1 == es2 then v2
                                else get_type_auxiliar (es1, Id pos1 id1) t




-- funções para a tabela de símbolos

symtable_insert :: (Token, [Token], Token) -> ([ActivStack], [Symtable])-> ([ActivStack], [Symtable])
symtable_insert (id1, t1, v1) (activ,[])  = (activ,[(get_top activ, id1, t1, v1)])
symtable_insert (id1, t1, v1) (activ,table)  = (activ,[(get_top activ, id1, t1, v1)]++table)



symtable_update :: (Token, Token) -> ([ActivStack], [Symtable])-> ([ActivStack], [Symtable])
symtable_update _ (activ, []) = fail "variable not found"
symtable_update (id1, v1) ( activ, symt ) = 
                               (activ, symtable_update_auxiliar ( get_top activ, id1, v1) symt )



symtable_update_auxiliar :: (String, Token, Token) -> ([Symtable])-> ([Symtable])
symtable_update_auxiliar _ ([]) = fail "variable not found"
symtable_update_auxiliar (es1, Id pos1 id1, v1) ((es2, Id pos2 id2, t2, v2):t) = 
                                if id1 == id2 && es1 == es2 then ((es1, Id pos1 id1, t2, v1) : t)
                                else (es2, Id pos2 id2, t2, v2) : symtable_update_auxiliar (es1, Id pos1 id1, v1) t


stack_insert :: ActivStack -> ([ActivStack], [Symtable])-> ([ActivStack], [Symtable])
stack_insert scope ([],symt)  = ([scope],symt)
stack_insert scope (activ,symt)  = ([scope] ++ activ,symt)


get_top :: [ActivStack]-> String
get_top [] = ""
get_top (h:b) = h


stack_remove :: ActivStack -> ([ActivStack], [Symtable])-> ([ActivStack], [Symtable])
stack_remove scope ([],symt) = fail "variable not found"
stack_remove scope ((h:b),symt) = (b, symt)

-- Uma variável tem um escopo, tem que salvar esse escopo.  
-- Quando uma variável for ser salva, acho que dá pra pegar a pilha de ativação
-- Ver qual é o escopo de lá, o topo da pilha, e salvar com esse escopo


-- A parte dos subprogramas. Tem que criar outra tabela que vai salvar o nome do subprograma
-- o header e o corpo. Aí quando for feita uma chamada pra esse subprograma, procura nessa 
-- tabela o nome, vê se o header se encaixa e executa a sequencia de tokens salvas nesse body
