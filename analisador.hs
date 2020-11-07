module Main (main) where

import Lexer
import Text.Parsec

-- parsers para os tokens

programToken = tokenPrim show update_pos get_token where
  get_token (Program p)  = Just (Program p) 
  get_token _       = Nothing

subprogramToken = tokenPrim show update_pos get_token where
  get_token (Func p) = Just (Func p)
  get_token _       = Nothing

idToken = tokenPrim show update_pos get_token where
  get_token (Id p s) = Just (Id p s)
  get_token _      = Nothing

constToken = tokenPrim show update_pos get_token where
  get_token (Const p) = Just (Const p)
  get_token _      = Nothing


whileToken = tokenPrim show update_pos get_token where
  get_token (While p)    = Just (While p)
  get_token _      = Nothing

doToken = tokenPrim show update_pos get_token where
  get_token (Do p)   = Just (Do p)
  get_token _      = Nothing

forToken = tokenPrim show update_pos get_token where
  get_token (For p)   = Just (For p)
  get_token _      = Nothing

inToken = tokenPrim show update_pos get_token where
  get_token (In p)   = Just (In p)
  get_token _      = Nothing

ifToken = tokenPrim show update_pos get_token where
  get_token (If p)    = Just (If p)
  get_token _      = Nothing

elseToken = tokenPrim show update_pos get_token where
  get_token (Else p)   = Just (Else p)
  get_token _      = Nothing

beginToken = tokenPrim show update_pos get_token where
  get_token (Begin p) = Just (Begin p)
  get_token _     = Nothing

endToken = tokenPrim show update_pos get_token where
  get_token (End p) = Just (End p)
  get_token _   = Nothing

beginWhileToken = tokenPrim show update_pos get_token where
  get_token (BeginWhile p) = Just (BeginWhile p)
  get_token _     = Nothing

endWhileToken = tokenPrim show update_pos get_token where
  get_token (EndWhile p) = Just (EndWhile p)
  get_token _   = Nothing


beginForToken = tokenPrim show update_pos get_token where
  get_token (BeginFor p) = Just (BeginFor p)
  get_token _     = Nothing

endForToken = tokenPrim show update_pos get_token where
  get_token (EndFor p) = Just (EndFor p)
  get_token _   = Nothing

beginIfToken = tokenPrim show update_pos get_token where
  get_token (BeginIf p) = Just (BeginIf p)
  get_token _     = Nothing

endIfToken = tokenPrim show update_pos get_token where
  get_token (EndIf p) = Just (EndIf p)
  get_token _   = Nothing

beginIndexToken :: Parsec [Token] st Token
beginIndexToken = tokenPrim show update_pos get_token where
  get_token (BeginIndex p) = Just (BeginIndex p)
  get_token _     = Nothing

endIndexToken :: Parsec [Token] st Token
endIndexToken = tokenPrim show update_pos get_token where
  get_token (EndIndex p) = Just (EndIndex p)
  get_token _     = Nothing
beginParenthesisToken = tokenPrim show update_pos get_token where
  get_token (BeginParenthesis p) = Just (BeginParenthesis p)
  get_token _   = Nothing

endParenthesisToken = tokenPrim show update_pos get_token where
  get_token (EndParenthesis p) = Just (EndParenthesis p)
  get_token _   = Nothing  

semiColonToken :: Parsec [Token] st Token
semiColonToken = tokenPrim show update_pos get_token where
  get_token (SemiColon p) = Just (SemiColon p)
  get_token _         = Nothing

colonToken :: Parsec [Token] st Token
colonToken = tokenPrim show update_pos get_token where
  get_token (Colon p) = Just (Colon p)
  get_token _         = Nothing

commaToken :: Parsec [Token] st Token
commaToken = tokenPrim show update_pos get_token where
  get_token (Comma p) = Just (Comma p)
  get_token _     = Nothing

assignToken = tokenPrim show update_pos get_token where
  get_token (Assign p) = Just (Assign p)
  get_token _      = Nothing

greaterToken = tokenPrim show update_pos get_token where
  get_token (Greater p) = Just (Greater p)
  get_token _       = Nothing

lessToken = tokenPrim show update_pos get_token where
  get_token (Less p) = Just (Less p)
  get_token _    = Nothing

greaterEqualToken = tokenPrim show update_pos get_token where
  get_token (GreaterOrEqual p) = Just (GreaterOrEqual p)
  get_token _              = Nothing

lessEqualToken = tokenPrim show update_pos get_token where
  get_token (LessOrEqual p) = Just (LessOrEqual p)
  get_token _              = Nothing

equalToken = tokenPrim show update_pos get_token where
  get_token (Equal p) = Just (Equal p)
  get_token _     = Nothing

diffToken = tokenPrim show update_pos get_token where
  get_token (Diff p) = Just (Diff p)
  get_token _              = Nothing

printToken = tokenPrim show update_pos get_token where
  get_token (Print p) = Just (Print p)
  get_token _       = Nothing

intToken = tokenPrim show update_pos get_token where
  get_token (Int p s) = Just (Int p s)
  get_token _       = Nothing

stringToken = tokenPrim show update_pos get_token where
  get_token (String p s) = Just (String p s)
  get_token _       = Nothing


boolToken = tokenPrim show update_pos get_token where
  get_token (Bool p s) = Just (Bool p s)
  get_token _       = Nothing


orToken = tokenPrim show update_pos get_token where
  get_token (Or p) = Just (Or p)
  get_token _  = Nothing


andToken = tokenPrim show update_pos get_token where
  get_token (And p) = Just (And p)
  get_token _   = Nothing


floatToken = tokenPrim show update_pos get_token where
  get_token (Float p s) = Just (Float p s)
  get_token _       = Nothing

lenghtToken :: Parsec [Token] st Token
lenghtToken = tokenPrim show update_pos get_token where
  get_token (Lenght p) = Just (Lenght p)
  get_token _  = Nothing

substrToken :: Parsec [Token] st Token
substrToken = tokenPrim show update_pos get_token where
  get_token (Substr p) = Just (Substr p)
  get_token _  = Nothing

primTypeToken = intToken <|> stringToken <|> floatToken <|> boolToken

arrayToken :: Parsec [Token] st [Token]
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

matrixToken :: Parsec [Token] st [Token]
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


sumToken = tokenPrim show update_pos get_token where
  get_token (Sum p ) = Just (Sum p )
  get_token _   = Nothing

subToken = tokenPrim show update_pos get_token where
  get_token (Sub p) = Just (Sub p)
  get_token _       = Nothing


multToken = tokenPrim show update_pos get_token where
  get_token (Multi p)= Just (Multi p)
  get_token _       = Nothing

divToken = tokenPrim show update_pos get_token where
  get_token (Div p) = Just (Div p)
  get_token _       = Nothing

expToken = tokenPrim show update_pos get_token where
  get_token (Pow p)= Just (Pow p)
  get_token _       = Nothing

radToken = tokenPrim show update_pos get_token where
  get_token (Rad p) = Just (Rad p)
  get_token _       = Nothing

restoDivToken = tokenPrim show update_pos get_token where
  get_token (Mod p) = Just (Mod p)
  get_token _       = Nothing

absToken = tokenPrim show update_pos get_token where
  get_token (Abs p) = Just (Abs p)
  get_token _       = Nothing

returnToken = tokenPrim show update_pos get_token where
  get_token (Return p) = Just (Return p)
  get_token _       = Nothing

lenToken = tokenPrim show update_pos get_token where
  get_token (Len p) = Just (Len p)
  get_token _   = Nothing

transposeToken = tokenPrim show update_pos get_token where
  get_token (Transpose p) = Just (Transpose p)
  get_token _   = Nothing

innerProdToken = tokenPrim show update_pos get_token where
  get_token (InnerProd p) = Just (InnerProd p)
  get_token _         = Nothing

swapLinesToken :: Parsec [Token] st Token
swapLinesToken = tokenPrim show update_pos get_token where
  get_token (SwapLines p) = Just (SwapLines p)
  get_token _             = Nothing

typeToken = tokenPrim show update_pos get_token where
  get_token (Type p s) = Just (Type p s)
  get_token _       = Nothing

update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos pos _ (tok:_) = pos -- necessita melhoria
update_pos pos _ []      = pos

-- parsers para os não-terminais

program :: Parsec [Token] st [Token]
program = do
            a <- subprograms 
            b <- mainProgram
            eof
            return (a ++ b) 

subprograms :: Parsec [Token] st [Token]
subprograms = try (do
          a <- subprogram
          b <- subprograms
          return (a ++ b))
          <|> 
          return []

subprogram :: Parsec [Token] st [Token]
subprogram = do 
            a <- subprogramToken 
            b <- typeToken
            c <- idToken
            d <- beginParenthesisToken
            e <- parameters
            f <- endParenthesisToken
            g <- beginToken 
            h <- stmts
            i <- endToken
            j <- subprograms
            return (a:b:c:[d]++e++[f]++[g]++h++[i] ++ j)

parameters :: Parsec [Token] st [Token]
parameters = try (do
            a <- typeToken
            b <- idToken
            c <- semiColonToken
            d <- parameters 
            return (a:b:[c]++d) )
            <|>
            return []


mainProgram :: Parsec [Token] st [Token]
mainProgram = do
            a <- programToken 
            b <- beginToken 
            c <- stmts
            d <- endToken
            return (a:[b] ++ c ++ [d])

stmts :: Parsec [Token] st [Token]
stmts = try( do
          first <- stmt
          next <- remaining_stmts
          return (first ++ next))
          <|>
          return []

stmt :: Parsec [Token] st [Token]
stmt = try assign <|> try invoking_expression <|> try return_expression <|> 
       print_exp <|> while <|> dowhile <|> for <|> ifs 

return_expression = do a <- returnToken
                       b <- expression
                       return (a: b)

invoking_expression = do a <- idToken
                         b <- beginParenthesisToken
                         c <- parameters_invoke
                         d <- endParenthesisToken
                         return (a:b:c ++ [d])


parameters_invoke :: Parsec [Token] st [Token]
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
operacao_boolean :: Parsec[Token] st [Token]
operacao_boolean = (do
              a <- greaterToken <|> lessToken <|> greaterEqualToken <|> lessEqualToken 
                <|> equalToken <|> diffToken
              return [a])

operacao_logica :: Parsec[Token] st [Token]
operacao_logica = (do
                  a <- orToken <|> andToken
                  return [a])

expressao_logica :: Parsec[Token] st [Token]
expressao_logica = try(do
                  a <- expressao_boolean
                  b <- operacao_logica
                  c <- expressao_boolean 
                  return (a++b++c))<|>try(do
                    a <- expressao_boolean
                    return (a))
            


expressao_boolean :: Parsec[Token] st [Token]
expressao_boolean = do
                a <- intToken <|> idToken
                b <- operacao_boolean
                c <- intToken <|> idToken
                return ([a]++b++[c])

-- int
int_operation :: Parsec [Token] st [Token]
int_operation = do
        a <- sumToken <|> subToken <|> multToken <|> divToken <|> expToken <|> radToken <|> restoDivToken
        return [a]

expression_int :: Parsec [Token] st [Token]
expression_int = try (do
                  a <- intToken <|> idToken
                  b <- int_operation
                  c <- intToken <|> idToken
                  return ([a]++b++[c]))
                  <|>
                  try (do
                    a <- absToken
                    b <- beginParenthesisToken
                    c <- intToken <|> idToken
                    d <- endParenthesisToken
                    return ([a]++[b]++[c]++[d]) )
                  <|>
                  (do
                    a <- absToken
                    b <- beginParenthesisToken
                    c <- subToken
                    d <- intToken <|> idToken
                    e <- endParenthesisToken
                    return ([a]++[b]++[c]++[d]++[e]) )


float_operation :: Parsec [Token] st [Token]
float_operation = do
        a <- sumToken <|> subToken <|> multToken <|> divToken
        return [a]


expression_float :: Parsec [Token] st [Token]
expression_float = try (do
                    a <- floatToken <|> idToken
                    b <- float_operation
                    c <- floatToken <|> idToken
                    return ([a]++b++[c]))
                  <|>
                  try (do
                    a <- absToken
                    b <- beginParenthesisToken
                    c <- floatToken
                    d <- endParenthesisToken
                    return ([a]++[b]++[c]++[d]) )
                  <|>
                  (do
                    a <- absToken
                    b <- beginParenthesisToken
                    c <- subToken
                    d <- floatToken <|> idToken
                    e <- endParenthesisToken
                    return ([a]++[b]++[c]++[d]++[e]) )






expression_string :: Parsec [Token] st [Token]
expression_string = try(do
                      a <- stringToken
                      b <- sumToken
                      c <- stringToken
                      return ([a] ++ [b] ++ [c]))
                    <|>
                    try(do 
                        a <- stringToken
                        b <- multToken
                        c <- intToken
                        return ([a] ++ [b] ++ [c]))
                    <|>
                         try(do
                         a <- intToken
                         b <- multToken
                         c <- stringToken
                         return ([a] ++ [b] ++ [c]))
                    <|>
                        try(do
                           a <- lenghtToken
                           b <- beginParenthesisToken
                           c <- stringToken
                           d <- endParenthesisToken
                           return ([a] ++ [b] ++ [c] ++ [d]))
                    <|> do 
                            a <- substrToken
                            b <- beginParenthesisToken
                            c <- stringToken
                            d <- commaToken
                            e <- intToken
                            f <- commaToken
                            g <- intToken
                            h <- endParenthesisToken
                            return ([a] ++ [b] ++ [c] ++ [d] ++ [e] ++ [f] ++ [g] ++ [h])
                            
-- array, matrix
array_expression :: Parsec [Token] st [Token]
array_expression = len_operation <|> try transpose_operation <|> try inner_prod_operation <|> 
                   try swap_lines_operation <|> try index_operation <|> 
                   do a <- try arrayToken <|> matrixToken
                      b <- array_operators 
                      c <- try arrayToken <|> matrixToken
                      return (a ++ b ++ c) 

len_operation :: Parsec [Token] st [Token]
len_operation = do a <- lenToken
                   b <- idToken
                   return (a:[b])

transpose_operation :: Parsec [Token] st [Token]
transpose_operation = do a <- idToken
                         b <- transposeToken
                         return (a:[b])

listify_id :: Parsec [Token] st [Token]
listify_id = do a <- idToken
                return ([a])

inner_prod_operation :: Parsec [Token] st [Token]
inner_prod_operation = do a <- listify_id <|> try arrayToken <|> matrixToken
                          b <- innerProdToken
                          c <- listify_id <|> try arrayToken <|> matrixToken
                          return (a ++ b: c) 
                          
swap_lines_operation :: Parsec [Token] st [Token]
swap_lines_operation = do a <- matrixToken
                          b <- swapLinesToken
                          c <- beginParenthesisToken
                          d <- intToken
                          e <- commaToken
                          f <- intToken
                          g <- endParenthesisToken
                          return (a ++ b : c:d:e:f: [g]) 

index_operation :: Parsec [Token] st [Token]
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

slice_expression :: Parsec [Token] st [Token]
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

assign :: Parsec [Token] st [Token]
assign = try (do
          t <- typeToken
          a <- idToken
          b <- assignToken
          c <- expression
          return (t:a:b:c))
          <|>
          try (do
          m <- constToken
          t <- typeToken
          a <- idToken
          b <- assignToken
          c <- expression
          return (m:t:a:b:c))
          <|>
          do
          a <- idToken
          b <- assignToken
          c <- expression
          return (a:b:c)


expression :: Parsec [Token] st [Token]
expression = try( do
                  a <- try array_expression <|> try expression_int <|> try expression_float <|> invoking_expression
                  return (a) )
                  <|>
             try( do
                  a <- try arrayToken <|> matrixToken
                  return (a) )
                  <|>
             try( do
                  a <- try expression_string
                  return (a))
                  <|>
                  do 
                    a <- intToken <|> stringToken <|> boolToken <|> floatToken <|> idToken
                    return [a]


print_exp :: Parsec [Token] st [Token]
print_exp = do 
        a <- printToken
        b <- idToken <|> stringToken
        return (a:[b])

while :: Parsec [Token] st [Token]
while = do
       a <- whileToken
       b <- beginParenthesisToken
       c <- expressao_logica
       d <- endParenthesisToken
       e <- beginWhileToken
       f <- stmts
       g <- endWhileToken
       return (a:[b] ++ c ++ d:[e]++ f ++ [g]) <|> (return [])

dowhile :: Parsec [Token] st [Token]
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

for :: Parsec [Token] st [Token]
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

ifs :: Parsec [Token] st [Token]
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

remaining_stmts :: Parsec [Token] st [Token]
remaining_stmts = (do a <- semiColonToken
                      b <- stmts
                      return (a:b)) <|> (return [])

-- invocação do parser para o símbolo de partida 

parser :: [Token] -> Either ParseError [Token]
parser tokens = runParser program () "Error message" tokens

main :: IO ()
main = case parser (getTokens "program1.pe") of
            { Left err -> print err; 
              Right ans -> print ans
            }