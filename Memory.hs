module Memory where

import Lexer



-- escopo, id, tipo, valor
type Symtable = (String, Token, [Token], Token)
-- nome do escopo
type ActivStack = String

-- flag que indica que deve executar
type Execute = Bool



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
get_type :: Token -> (Execute, [ActivStack], [Symtable]) -> Token
get_type _ (e, activ,[]) = error "variable not found"
get_type id1 (e, activ,symt) = get_type_auxiliar (get_top activ, id1) symt
    


get_type_auxiliar :: (String, Token) -> ([Symtable])-> Token
get_type_auxiliar _ ([]) = error "variable not found"
get_type_auxiliar (es1, Id pos1 id1) ((es2, Id pos2 id2, t2, v2):t) =     
                                if id1 == id2 && es1 == es2 then v2
                                else get_type_auxiliar (es1, Id pos1 id1) t




-- funções para a tabela de símbolos

symtable_insert :: (Token, [Token], Token) -> (Execute, [ActivStack], [Symtable])-> (Execute, [ActivStack], [Symtable])
symtable_insert (id1, t1, v1) (e, activ,[])  = (e, activ,[(get_top activ, id1, t1, v1)])
symtable_insert (id1, t1, v1) (e, activ,table)  = (e, activ,[(get_top activ, id1, t1, v1)]++table)



symtable_update :: (Token, Token) -> (Execute, [ActivStack], [Symtable])-> (Execute, [ActivStack], [Symtable])
symtable_update _ (e, activ, []) = error "variable not found"
symtable_update (id1, v1) (e, activ, symt ) = 
                               (e, activ, symtable_update_auxiliar ( get_top activ, id1, v1) symt )



symtable_update_auxiliar :: (String, Token, Token) -> ([Symtable])-> ([Symtable])
symtable_update_auxiliar _ ([]) = fail "variable not found"
symtable_update_auxiliar (es1, Id pos1 id1, v1) ((es2, Id pos2 id2, t2, v2):t) = 
                                if id1 == id2 && es1 == es2 then ((es1, Id pos1 id1, t2, v1) : t)
                                else (es2, Id pos2 id2, t2, v2) : symtable_update_auxiliar (es1, Id pos1 id1, v1) t


symtable_get :: (Token) -> (Execute, [ActivStack], [Symtable]) -> Token
symtable_get id1 ( e, activ, []) = id1 -- Tem que tratar isso
symtable_get (Id pos1 id1) (e, activ, (es2, Id pos2 id2, t2, v2):t) = 
                                if id1 == id2 && get_top activ == es2 then v2
                                else symtable_get (Id pos1 id1) (e, activ, t)


stack_insert :: ActivStack -> (Execute, [ActivStack], [Symtable])-> (Execute, [ActivStack], [Symtable])
stack_insert scope (e, [],symt)  = (e, [scope],symt)
stack_insert scope (e, activ,symt)  = (e, [scope] ++ activ,symt)


get_top :: [ActivStack]-> String
get_top [] = ""
get_top (h:b) = h


stack_remove :: ActivStack -> (Execute, [ActivStack], [Symtable])-> (Execute, [ActivStack], [Symtable])
stack_remove scope (e, [], symt) = error "variable not found"
stack_remove scope (e, (h:b),symt) = (e, b, symt)


begin_execute :: (Execute, [ActivStack], [Symtable]) -> (Execute, [ActivStack], [Symtable])
begin_execute (e, a:b, c:d) = (True, a:b, c:d)
begin_execute (e, [], c:d) = (True, [], c:d)
begin_execute (e, a:b, []) = (True, a:b, [])
begin_execute (e, [], []) = (True, [], [])

end_execute :: (Execute, [ActivStack], [Symtable]) -> (Execute, [ActivStack], [Symtable])
end_execute (e, a:b, c:d) = (False, a:b, c:d)
end_execute (e, [], c:d) = (False, [], c:d)
end_execute (e, a:b, []) = (False, a:b, [])
end_execute (e, [], []) = (False, [], [])

is_executing :: (Execute, [ActivStack], [Symtable]) -> Bool
is_executing (e, a:b, c:d) = e
is_executing (e, [], c:d) = e
is_executing (e, a:b, []) = e
is_executing (e, [], []) = e

check_execute :: Token -> Bool
check_execute (Bool p e) = e
check_execute _ = error "type mismatch"

-- Uma variável tem um escopo, tem que salvar esse escopo.  
-- Quando uma variável for ser salva, acho que dá pra pegar a pilha de ativação
-- Ver qual é o escopo de lá, o topo da pilha, e salvar com esse escopo


-- A parte dos subprogramas. Tem que criar outra tabela que vai salvar o nome do subprograma
-- o header e o corpo. Aí quando for feita uma chamada pra esse subprograma, procura nessa 
-- tabela o nome, vê se o header se encaixa e executa a sequencia de tokens salvas nesse body
