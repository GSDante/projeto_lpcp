module Main (main) where

import Lexer
import Tokens
import Memory 

import Expressions
import Types

import Text.Parsec
import Control.Monad.IO.Class

import System.IO.Unsafe

import Data.List


-- parsers para os não-terminais

primTypeToken = intToken <|> stringToken <|> floatToken <|> boolToken

arrayToken :: ParsecT [Token]  ([ActivStack], [Symtable])IO([Token])
arrayToken =
    do
      lbrack <- beginIndexToken
      innercontent <- innerContentArray
      rbrack <- endIndexToken
      
      return (lbrack:innercontent ++ [rbrack])

innerContentArray = 
    do first <- primTypeToken
       next <- remainingContent
       return ([first] ++ next)

remainingContent =
    try ( do a <- commaToken 
             b <- innerContentArray
             return (a: b)) 
    <|> (return [])  

matrixToken :: ParsecT [Token]  ([ActivStack], [Symtable])IO([Token])
matrixToken  =
    do
      lbrack <- beginIndexToken
      innercontent <- innerContentMatrix
      rbrack <- endIndexToken
      
      return (lbrack : innercontent ++ [rbrack])

innerContentMatrix = 
    try (do first <- arrayToken
            next <- remainingContentMatrix
            return (first ++ next))
    <|> do first <- matrixToken
           next <- remainingContentMatrix
           return (first ++ next)

remainingContentMatrix =
    try ( do a <- commaToken 
             b <- innerContentMatrix
             return (a : b)) 
    <|> (return [])  


arrayTypeToken :: ParsecT [Token] ([ActivStack], [Symtable]) IO([Token])
arrayTypeToken =  do l <- beginIndexToken
                     t <- typeToken
                     r <- endIndexToken
                     return (l : t : [r])

generalTypeToken :: ParsecT [Token] ([ActivStack], [Symtable]) IO([Token])
generalTypeToken = try ( do a <- typeToken
                            b <- beginIndexToken
                            c <- typeToken
                            d <- endIndexToken
                            return (a : b: c:[d]))
                    <|> do a <- typeToken
                           return ([a])


program ::  ParsecT [Token]  ([ActivStack], [Symtable])IO([Token])
program = do
            a <- global_variable
            b <- subprograms 
            c <- mainProgram
            eof
            return (a ++ b ++ c) 

global_variable ::  ParsecT [Token]  ([ActivStack], [Symtable])IO([Token])
global_variable = try(do
                a <- try assign <|> try declaration
                b <- semiColonToken
                c <- remaining_global_variable
                return (a ++ [b] ++ c))
                <|> return []
                
remaining_global_variable ::  ParsecT [Token]  ([ActivStack], [Symtable])IO([Token])
remaining_global_variable = try (do
                a <- global_variable
                return(a))
                <|>
                return []

subprograms :: ParsecT [Token]  ([ActivStack], [Symtable])IO([Token])
subprograms = try (do
          a <- subprogram
          b <- subprograms
          return (a ++ b))
          <|> 
          return []


subprogram :: ParsecT [Token]  ([ActivStack], [Symtable])IO([Token])
subprogram = (do 
            a <- funcToken 
            b <- typeToken
            c <- idToken
            d <- beginParenthesisToken
            e <- parameters
            f <- endParenthesisToken
            g <- beginToken 
            h <- stmts
            i <- endToken
            j <- subprograms
            return (a:b:c:[d]++e++[f]++[g]++h++[i] ++ j))
            <|>
            (do
            a <- procToken 
            c <- idToken
            d <- beginParenthesisToken
            e <- parameters
            f <- endParenthesisToken
            g <- beginToken 
            h <- stmts_proc
            i <- endToken
            j <- subprograms
            return (a:c:[d]++e++[f]++[g]++h++[i] ++ j))


parameters :: ParsecT [Token]  ([ActivStack], [Symtable])IO([Token])
parameters = try (do
            a <- generalTypeToken
            b <- idToken
            c <- semiColonToken
            d <- parameters 
            return (a ++ b :[c]++d) )
            <|>
            return []


mainProgram :: ParsecT [Token]  ([ActivStack], [Symtable])IO([Token])
mainProgram = do
            a <- programToken 
            updateState(stack_insert ("main"))
            s <- getState
            liftIO (print s)
            b <- beginToken 
            c <- stmts
            d <- endToken
            return (a:[b] ++ c ++ [d])

stmts :: ParsecT [Token]  ([ActivStack], [Symtable])IO([Token])
stmts = try( do
          first <- stmt
          next <- remaining_stmts
          return (first ++ next))
          <|>
          return []


stmts_proc :: ParsecT [Token]  ([ActivStack], [Symtable])IO([Token])
stmts_proc = try( do
                   first <- stmt_proc
                   next <- remaining_stmts_proc
                   return (first ++ next))
                <|>
                return []

stmt :: ParsecT [Token]  ([ActivStack], [Symtable])IO([Token])
stmt = try assign <|> try declaration <|> try invoking_expression <|> try return_expression <|> 
       print_exp <|> read_exp <|> while <|> dowhile <|> for <|> ifs

stmt_proc :: ParsecT [Token]  ([ActivStack], [Symtable])IO([Token])
stmt_proc = try assign <|> try declaration <|>  try invoking_expression <|> 
            print_exp <|> while_proc <|> dowhile_proc <|> for_proc <|> ifs_proc


declaration :: ParsecT [Token]  ([ActivStack], [Symtable])IO([Token])
declaration = try(do a <- typeToken
                     b <- idToken
                     updateState(symtable_insert (b, [a], get_default_value [a] ))
                     s <- getState
                     liftIO (print s)
                     return (a:[b]))
                 

return_expression = do a <- returnToken
                       b <- intToken <|> stringToken <|> boolToken <|> floatToken <|> idToken
                       return (a:[b])

invoking_expression = do a <- idToken
                         b <- beginParenthesisToken
                         c <- parameters_invoke
                         d <- endParenthesisToken
                         return (a:b:c ++ [d])


parameters_invoke :: ParsecT [Token]  ([ActivStack], [Symtable])IO([Token])
parameters_invoke = try (do
            a <- idToken <|> primTypeToken
            b <- remaining_parameters
            return (a:b) )
            <|>
            return []

remaining_parameters = try (do a <- commaToken 
                               b <- parameters_invoke 
                               return (a:b))
                               <|> return []

----- OPERACOES 
-- bool
operacao_boolean :: ParsecT [Token]  ([ActivStack], [Symtable])IO( Token )
operacao_boolean = (do
              a <- greaterToken <|> lessToken <|> greaterEqualToken <|> lessEqualToken 
                <|> equalToken <|> diffToken
              return (a))

operacao_logica :: ParsecT [Token]  ([ActivStack], [Symtable])IO([Token])
operacao_logica = (do
                  a <- orToken <|> andToken
                  return [a])

expressao_logica :: ParsecT [Token]  ([ActivStack], [Symtable])IO([Token])
expressao_logica = try(do
                  a <- expressao_boolean
                  b <- operacao_logica
                  c <- expressao_boolean 
                  return (a++b++c))
                  <|>try(do
                    a <- expressao_boolean
                    return (a))
            


expressao_boolean :: ParsecT [Token]  ([ActivStack], [Symtable])IO([Token])
expressao_boolean = try (do
                a <- intToken <|> floatToken <|> boolToken <|> stringToken 
                b <- operacao_boolean
                c <- intToken <|> floatToken <|> boolToken <|> stringToken 
                return [eval a b c] )
                <|>
                (do a <- idToken
                    b <- operacao_boolean
                    c <- intToken 

                    s <- getState
                    if (not (compatible (get_type a s) c)) then fail "type mismatch"
                    else 
                      do 
                      s <- getState
                      liftIO (print s)
                      return [eval (symtable_get a s) b c] )
                  



-- int
int_operation :: ParsecT [Token]  ([ActivStack], [Symtable])IO(Token)
int_operation = do
        a <- sumToken <|> subToken <|> multToken <|> divToken <|> expToken <|> radToken <|> restoDivToken
        return (a)


add_expression :: Token -> ParsecT [Token]  ([ActivStack], [Symtable])IO(Token)
add_expression n = try (do
                    a <- int_operation
                    b <- intToken <|> floatToken
                    c <- add_expression (eval n a b)
                    return (c)
                  )
                  <|> 
                    return (n)
                  

expressions_int :: ParsecT [Token]  ([ActivStack], [Symtable]) IO(Token)
expressions_int = --try(do
                  --a <- expression_int
                  --b <- add_expression
                  --return (a++b))
                -- <|>
                  do
                    a <- expression_int
                    return (a)

int_values :: ParsecT [Token]  ([ActivStack], [Symtable]) IO(Token)
int_values = -- try(do
              -- a <- beginParenthesisToken
              -- b <- expression_int
              -- c <- endParenthesisToken
              -- return ([a]++b++[c]))
              -- <|>
              try(do
              a <- intToken <|> floatToken
              b <- add_expression a
              return (b) )
              <|>
              do 
                a <- idToken
                b <- int_operation
                c <- intToken <|> floatToken

                s <- getState
                if (not (compatible (get_type a s) c)) then fail "type mismatch"
                else 
                  do 
                    s <- getState
                    liftIO (print s)
                    return (eval (symtable_get a s) b c)
                  


expression_int :: ParsecT [Token]  ([ActivStack], [Symtable])IO(Token )
expression_int = try (do
                  a <- int_values
                  return (a))
                  <|>
                  
                  --try (do
                   -- a <- absToken
                    --b <- beginParenthesisToken
                    --c <- intToken <|> idToken
                    --d <- endParenthesisToken
                    --return ([a]++[b]++[c]++[d]) )
                  -- <|>
                  --try (do
                    --a <- absToken
                    ----b <- beginParenthesisToken
                    --c <- subToken
                    --d <- intToken <|> idToken
                    --e <- endParenthesisToken
                    --return ([a]++[b]++[c]++[d]++[e]) )
                  -- <|>
                   try(do
                    a <- intToken
                    return a)
                  

float_operation :: ParsecT [Token]  ([ActivStack], [Symtable])IO([Token])
float_operation = do
        a <- sumToken <|> subToken <|> multToken <|> divToken
        return [a]

add_expression_float ::Token -> ParsecT [Token]  ([ActivStack], [Symtable])IO(Token)
add_expression_float n = try (do
                    a <- int_operation
                    b <- intToken <|> floatToken
                    c <- add_expression (eval n a b)
                    return (c)
                  )
                  <|> 
                    return (n)
                  
expressions_float :: ParsecT [Token]  ([ActivStack], [Symtable])IO(Token)
expressions_float = --try(do
                  --a <- expression_int
                  --b <- add_expression
                  --return (a++b))
                -- <|>
                  do
                    a <- expression_int
                    return (a)


float_values :: ParsecT [Token]  ([ActivStack], [Symtable])IO(Token)
float_values = -- try(do
              -- a <- beginParenthesisToken
              -- b <- expression_float
              -- c <- endParenthesisToken
              -- return ([a]++b++[c]))
              -- <|>
              try(do
              a <- intToken <|> floatToken
              b <- add_expression a
              return (b) )
              <|>
              do 
                a <- idToken
                b <- int_operation
                c <- intToken <|> floatToken

                s <- getState
                if (not (compatible (get_type a s) c)) then fail "type mismatch"
                else 
                  do 
                    s <- getState
                    liftIO (print s)
                    return (eval (symtable_get a s) b c)
                  


expression_float :: ParsecT [Token]  ([ActivStack], [Symtable])IO(Token)
expression_float = try (do
                  a <- float_values
                  return (a))
                  <|>
                  
                  --try (do
                   -- a <- absToken
                    --b <- beginParenthesisToken
                    --c <- intToken <|> idToken
                    --d <- endParenthesisToken
                    --return ([a]++[b]++[c]++[d]) )
                  -- <|>
                  --try (do
                    --a <- absToken
                    ----b <- beginParenthesisToken
                    --c <- subToken
                    --d <- intToken <|> idToken
                    --e <- endParenthesisToken
                    --return ([a]++[b]++[c]++[d]++[e]) )
                  -- <|>
                   try(do
                    a <- floatToken
                    return a)
                  



string_operation :: ParsecT [Token]  ([ActivStack], [Symtable])IO([Token])
string_operation = do
           a <- equalToken <|> diffToken <|> sumToken
           return [a]

string_values ::ParsecT [Token]  ([ActivStack], [Symtable])IO([Token])
string_values = try(do
              a <- beginParenthesisToken
              b <- try expression_string
              c <- endParenthesisToken
              return ([a]++b++[c]))
              <|>
              (do
              a <- stringToken <|> idToken
              return [a])

add_expression_string :: ParsecT [Token]  ([ActivStack], [Symtable])IO([Token])
add_expression_string = try (do
                    a <- string_operation
                    b <- string_values
                    c <- try add_expression_string
                    return (a ++ b)
                  )
                  <|> 
                    return []

expressions_string :: ParsecT [Token]  ([ActivStack], [Symtable])IO([Token])
expressions_string = try(do
                  a <- try expression_string
                  b <- try add_expression_string
                  return (a++b))
                <|>
                  (do
                    a <- expression_string
                    return (a))

sum_string::ParsecT [Token]  ([ActivStack], [Symtable])IO([Token])
sum_string = try(do
                  a <- string_values
                  b <- sumToken
                  c <- string_values
                  return (a ++ [b] ++ c))

mult_string::ParsecT [Token]  ([ActivStack], [Symtable])IO([Token])
mult_string = try(do
                    a <- string_values
                    b <- multToken
                    c <- intToken
                    return (a ++ [b] ++ [c]))
                <|>
                do a <- intToken
                   b <- multToken
                   c <- string_values
                   return ([a] ++ [b] ++ c)

len_string::ParsecT [Token]  ([ActivStack], [Symtable])IO([Token])
len_string = try(do
                    a <- lenghtToken
                    b <- beginParenthesisToken
                    c <- string_values
                    d <- endParenthesisToken
                    return ([a] ++ [b] ++ c ++ [d]))

string_comparation::ParsecT [Token]  ([ActivStack], [Symtable])IO([Token])
string_comparation = try(do
                         a <- string_values
                         b <- string_operation
                         c <- string_values
                         return (a++b++c))

substr_operation::ParsecT [Token]  ([ActivStack], [Symtable])IO([Token])
substr_operation = try(do
                         a <- substrToken
                         b <- beginParenthesisToken
                         c <- string_values
                         d <- commaToken
                         e <- intToken
                         f <- commaToken
                         g <- intToken
                         h <- endParenthesisToken
                         return ([a] ++ [b] ++ c ++ [d] ++ [e] ++ [f] ++ [g] ++ [h]))

expression_string :: ParsecT [Token]  ([ActivStack], [Symtable])IO([Token])
expression_string = len_string <|> try sum_string <|> try mult_string <|> try string_operation <|> try string_comparation <|> try substr_operation
                    <|> 
                        try(do
                            a <- stringToken
                            return [a])
                            
-- array, matrix
array_expression :: ParsecT [Token]  ([ActivStack], [Symtable])IO([Token])
array_expression = len_operation <|> try transpose_operation <|> try inner_prod_operation <|> 
                   try swap_lines_operation <|> try index_operation <|> 
                   do a <- try arrayToken <|> matrixToken
                      b <- array_operators 
                      c <- try arrayToken <|> matrixToken
                      return (a ++ b ++ c) 

len_operation :: ParsecT [Token]  ([ActivStack], [Symtable])IO([Token])
len_operation = do a <- lenToken
                   b <- idToken
                   return (a:[b])

transpose_operation :: ParsecT [Token]  ([ActivStack], [Symtable])IO([Token])
transpose_operation = do a <- idToken
                         b <- transposeToken
                         return (a:[b])

listify_id :: ParsecT [Token]  ([ActivStack], [Symtable])IO([Token])
listify_id = do a <- idToken
                return ([a])

inner_prod_operation :: ParsecT [Token]  ([ActivStack], [Symtable])IO([Token])
inner_prod_operation = do a <- listify_id <|> try arrayToken <|> matrixToken
                          b <- innerProdToken
                          c <- listify_id <|> try arrayToken <|> matrixToken
                          return (a ++ b: c) 
                          
swap_lines_operation :: ParsecT [Token]  ([ActivStack], [Symtable])IO([Token])
swap_lines_operation = do a <- matrixToken
                          b <- swapLinesToken
                          c <- beginParenthesisToken
                          d <- intToken
                          e <- commaToken
                          f <- intToken
                          g <- endParenthesisToken
                          return (a ++ b : c:d:e:f: [g]) 

index_operation :: ParsecT [Token]  ([ActivStack], [Symtable])IO([Token])
index_operation = try (do a <- idToken
                          b <- beginIndexToken
                          c <- primTypeToken <|> idToken
                          d <- endIndexToken
                          return (a: b: c : [d]))
                     <|>
                     do a <- idToken
                        b <- beginIndexToken
                        c <- index_expression
                        d <- endIndexToken
                        return (a: b: c ++ [d])

-- [1], [1:1], [1:1, 1:1], [1,1], 
index_expression =  try slice_expression <|> arrayToken

slice_expression :: ParsecT [Token]  ([ActivStack], [Symtable])IO([Token])
slice_expression = do a <- intToken 
                      b <- colonToken
                      c <- intToken 
                      d <- remaining_slice
                      return (a:b:c:d)

remaining_slice = try (do a <- commaToken
                          b <- slice_expression 
                          return (a: b))
                  <|> return []

array_operators = do a <- sumToken <|> subToken <|> multToken <|> divToken 
                          <|> equalToken <|> diffToken
                     return [a]

-- COMANDOS

assign :: ParsecT [Token]  ([ActivStack], [Symtable]) IO([Token])
assign = try (do
          t <- generalTypeToken
          a <- idToken
          b <- assignToken
          --c <- intToken <|> stringToken <|> boolToken <|> floatToken <|> idToken
          c <- expression
          updateState(symtable_insert (a, t, c))
          s <- getState
          liftIO (print s)
          return (t ++ a:b:[c]))
          <|>
          try (do
          m <- constToken
          t <- generalTypeToken
          a <- idToken
          b <- assignToken
          c <- intToken <|> stringToken <|> boolToken <|> floatToken <|> idToken
          --c <- expression
          updateState(symtable_insert (a, t, c))
          s <- getState
          liftIO (print s)
          return (m : t ++a:b:[c]))
          <|>
          do
          a <- idToken
          b <- assignToken
          c <- intToken <|> stringToken <|> boolToken <|> floatToken <|> idToken
          --c <- expression
          s <- getState
          if (not (compatible (get_type a s) c)) then fail "type mismatch"
          else 
            do 
            updateState(symtable_update (a, c))
            s <- getState
            liftIO (print s)
            return (a:b:[c])
          
          

expression:: ParsecT [Token]  ([ActivStack], [Symtable])IO(Token)
expression = 
            try( do
                  a <- try expressions_int
                  -- <|> try array_expression <|> try expressions_int <|> try expressions_float <|> invoking_expression
                  return (a) )
                  <|>
             --try( do
                 -- a <- try arrayToken <|> matrixToken
                ----  return (a) )
              --    <|>
          -----   try( do
               --   a <- try expressions_string
              --    return (a))
              --    <|>
            do a <- intToken <|> stringToken <|> boolToken <|> floatToken <|> idToken
               return a
              --  <|>
              --  return []


print_exp :: ParsecT [Token]  ([ActivStack], [Symtable])IO([Token])
print_exp = do 
        a <- printToken
        b <- idToken <|> stringToken
        return (a:[b])

read_exp :: ParsecT [Token]  ([ActivStack], [Symtable])IO([Token])
read_exp = do 
        a <- readToken
        b <- greaterToken
        c <- idToken
        s <- liftIO $ getLine -- recupera texto inserido pela linha de comando
        st <- getState
        -- atualiza na tabela de simbolos
        updateState(symtable_update (c, getTokenFrom (get_type c st) (show s) ))
        st <- getState
        liftIO (print st)
        return (a:[b])

getTokenFrom :: Token -> String -> Token
getTokenFrom (String  p _) s = (String p s)
getTokenFrom (Int p _) s = (Int p (read (read s)) )
getTokenFrom (Float p _) s = (Float p (read (read s)) )

while :: ParsecT [Token]  ([ActivStack], [Symtable])IO([Token])
while = do
       a <- whileToken
       b <- beginParenthesisToken
       c <- expressao_logica
       d <- endParenthesisToken
       e <- beginWhileToken
       f <- stmts
       g <- endWhileToken
       return (a:[b] ++ c ++ d:[e]++ f ++ [g]) <|> (return [])

while_proc :: ParsecT [Token]  ([ActivStack], [Symtable])IO([Token])
while_proc = do
       a <- whileToken
       b <- beginParenthesisToken
       c <- expressao_logica
       d <- endParenthesisToken
       e <- beginWhileToken
       f <- stmts_proc
       g <- endWhileToken
       return (a:[b] ++ c ++ d:[e]++ f ++ [g]) <|> (return [])

dowhile :: ParsecT [Token]  ([ActivStack], [Symtable])IO([Token])
dowhile = do
       a <- doToken
       b <- beginWhileToken
       c <- stmts
       d <- endWhileToken
       e <- whileToken
       f <- beginParenthesisToken
       g <- expressao_logica
       h <- endParenthesisToken
       return ([a]++[b]++c++[d]++[e]++[f]++g++[h]) <|> (return [])

dowhile_proc :: ParsecT [Token]  ([ActivStack], [Symtable])IO([Token])
dowhile_proc = do
       a <- doToken
       b <- beginWhileToken
       c <- stmts_proc
       d <- endWhileToken
       e <- whileToken
       f <- beginParenthesisToken
       g <- expressao_logica
       h <- endParenthesisToken
       return ([a]++[b]++c++[d]++[e]++[f]++g++[h]) <|> (return [])

for :: ParsecT [Token]  ([ActivStack], [Symtable])IO([Token])
for = do
       a <- forToken
       b <- beginParenthesisToken
       c <- idToken
       d <- inToken
       e <- idToken
       f <- endParenthesisToken
       g <- beginForToken
       h <- stmts
       i <- endForToken
       return (a:b:c:d:e:f:[g]++h++[i]) <|> (return [])

for_proc :: ParsecT [Token]  ([ActivStack], [Symtable])IO([Token])
for_proc = do
       a <- forToken
       b <- beginParenthesisToken
       c <- idToken
       d <- inToken
       e <- idToken
       f <- endParenthesisToken
       g <- beginForToken
       h <- stmts_proc
       i <- endForToken
       return (a:b:c:d:e:f:[g]++h++[i]) <|> (return [])

ifs :: ParsecT [Token]  ([ActivStack], [Symtable])IO([Token])
ifs = 
       try (do
         a <- ifToken
         b <- beginParenthesisToken
         c <- expressao_logica  
         d <- endParenthesisToken
         e <- beginIfToken
         f <- stmts
         g <- endIfToken
         h <- elseToken
         i <- beginIfToken
         j <- stmts
         k <- endIfToken
         return (a:[b] ++ c ++ d:[e] ++ f ++ g:h:[i] ++ j ++ [k]))
         <|>
         (do
           a <- ifToken
           b <- beginParenthesisToken
           c <- expressao_logica 
           d <- endParenthesisToken
           e <- beginIfToken
           f <- stmts
           g <- endIfToken
           return (a:[b] ++ c ++ d:[e]++ f ++ [g])) <|> (return [])

ifs_proc :: ParsecT [Token]  ([ActivStack], [Symtable])IO([Token])
ifs_proc = 
       try (do
         a <- ifToken
         b <- beginParenthesisToken
         c <- expressao_logica  
         d <- endParenthesisToken
         e <- beginIfToken
         f <- stmts_proc
         g <- endIfToken
         h <- elseToken
         i <- beginIfToken
         j <- stmts
         k <- endIfToken
         return (a:[b] ++ c ++ d:[e] ++ f ++ g:h:[i] ++ j ++ [k]))
         <|>
         (do
           a <- ifToken
           b <- beginParenthesisToken
           c <- expressao_logica 
           d <- endParenthesisToken
           e <- beginIfToken
           f <- stmts_proc
           g <- endIfToken
           return (a:[b] ++ c ++ d:[e]++ f ++ [g])) <|> (return [])

remaining_stmts :: ParsecT [Token]  ([ActivStack], [Symtable])IO([Token])
remaining_stmts = (do a <- semiColonToken
                      b <- stmts
                      return (a:b)) <|> (return [])


remaining_stmts_proc :: ParsecT [Token]  ([ActivStack], [Symtable])IO([Token])
remaining_stmts_proc = (do a <- semiColonToken
                           b <- stmts_proc
                           return (a:b)) <|> (return [])


-- invocação do parser para o símbolo de partida 

parser :: [Token] -> IO (Either ParseError [Token])
--parser tokens = runParserT program [] "Error message" tokens
parser tokens = runParserT program ([],[]) "Error message" tokens

main :: IO ()
main = case unsafePerformIO (parser (getTokens "Examples/program6.pe")) of
            { Left err -> print err; 
              Right ans -> print ans
            }