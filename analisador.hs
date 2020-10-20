module Main (main) where

import Lexer
import Text.Parsec

-- parsers para os tokens

programToken = tokenPrim show update_pos get_token where
  get_token Program = Just Program
  get_token _       = Nothing

subprogramToken = tokenPrim show update_pos get_token where
  get_token Func = Just Func
  get_token _       = Nothing

idToken = tokenPrim show update_pos get_token where
  get_token (Id x) = Just (Id x)
  get_token _      = Nothing

whileToken = tokenPrim show update_pos get_token where
  get_token While     = Just While
  get_token _      = Nothing

doToken = tokenPrim show update_pos get_token where
  get_token Do     = Just Do
  get_token _      = Nothing

forToken = tokenPrim show update_pos get_token where
  get_token For    = Just For
  get_token _      = Nothing

inToken = tokenPrim show update_pos get_token where
  get_token In     = Just In
  get_token _      = Nothing

ifToken = tokenPrim show update_pos get_token where
  get_token If     = Just If
  get_token _      = Nothing

elseToken = tokenPrim show update_pos get_token where
  get_token Else     = Just Else
  get_token _      = Nothing

beginToken = tokenPrim show update_pos get_token where
  get_token Begin = Just Begin
  get_token _     = Nothing

endToken = tokenPrim show update_pos get_token where
  get_token End = Just End
  get_token _   = Nothing

beginIndexToken :: Parsec [Token] st Token
beginIndexToken = tokenPrim show update_pos get_token where
  get_token BeginIndex = Just BeginIndex
  get_token _     = Nothing

endIndexToken :: Parsec [Token] st Token
endIndexToken = tokenPrim show update_pos get_token where
  get_token EndIndex = Just EndIndex
  get_token _     = Nothing
beginParenthesisToken = tokenPrim show update_pos get_token where
  get_token BeginParenthesis = Just BeginParenthesis
  get_token _   = Nothing

endParenthesisToken = tokenPrim show update_pos get_token where
  get_token EndParenthesis = Just EndParenthesis
  get_token _   = Nothing  

semiColonToken :: Parsec [Token] st Token
semiColonToken = tokenPrim show update_pos get_token where
  get_token SemiColon = Just SemiColon
  get_token _         = Nothing

colonToken :: Parsec [Token] st Token
colonToken = tokenPrim show update_pos get_token where
  get_token Colon = Just Colon
  get_token _         = Nothing

assignToken = tokenPrim show update_pos get_token where
  get_token Assign = Just Assign
  get_token _      = Nothing

greaterToken = tokenPrim show update_pos get_token where
  get_token Greater = Just Greater
  get_token _       = Nothing

lessToken = tokenPrim show update_pos get_token where
  get_token Less = Just Less
  get_token _    = Nothing

greaterEqualToken = tokenPrim show update_pos get_token where
  get_token GreaterOrEqual = Just GreaterOrEqual
  get_token _              = Nothing

lessEqualToken = tokenPrim show update_pos get_token where
  get_token LessOrEqual = Just LessOrEqual
  get_token _              = Nothing

equalToken = tokenPrim show update_pos get_token where
  get_token Equal = Just Equal
  get_token _     = Nothing

diffToken = tokenPrim show update_pos get_token where
  get_token Diff = Just Diff
  get_token _              = Nothing

printToken = tokenPrim show update_pos get_token where
  get_token Print = Just Print
  get_token _       = Nothing

intToken = tokenPrim show update_pos get_token where
  get_token (Int x) = Just (Int x)
  get_token _       = Nothing

stringToken = tokenPrim show update_pos get_token where
  get_token (String x) = Just (String x)
  get_token _       = Nothing


boolToken = tokenPrim show update_pos get_token where
  get_token (Bool x) = Just (Bool x)
  get_token _       = Nothing

floatToken = tokenPrim show update_pos get_token where
  get_token (Float x) = Just (Float x)
  get_token _       = Nothing

primTypeToken = intToken <|> stringToken <|> floatToken <|> boolToken

arrayToken :: Parsec [Token] st [Token]
arrayToken =
    do
      lbrack <- beginIndexToken
      innercontent <- innerContentArray
      rbrack <- endIndexToken
      
      return (lbrack:innercontent ++ [rbrack])

-- cells :: GenParser Char st [String]
innerContentArray = 
    do first <- primTypeToken
       next <- remainingContent
       return (first: next)

remainingContent =
    try ( do a <- colonToken 
             b <- innerContentArray
             return (a: b)) 
    <|> (return [])  

sumToken = tokenPrim show update_pos get_token where
  get_token Sum = Just Sum
  get_token _       = Nothing

subToken = tokenPrim show update_pos get_token where
  get_token Sub = Just Sub
  get_token _       = Nothing


multToken = tokenPrim show update_pos get_token where
  get_token Multi = Just Multi
  get_token _       = Nothing

divToken = tokenPrim show update_pos get_token where
  get_token Div = Just Div
  get_token _       = Nothing

expToken = tokenPrim show update_pos get_token where
  get_token Pow = Just Pow
  get_token _       = Nothing

radToken = tokenPrim show update_pos get_token where
  get_token Rad = Just Rad
  get_token _       = Nothing

restoDivToken = tokenPrim show update_pos get_token where
  get_token Mod = Just Mod
  get_token _       = Nothing

returnToken = tokenPrim show update_pos get_token where
  get_token Return = Just Return
  get_token _       = Nothing


typeToken = tokenPrim show update_pos get_token where
  get_token (Type x) = Just (Type x)
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
          b <- semiColonToken
          c <- subprograms
          return (a ++ [b] ++ c))
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
            i <- returnToken
            j <- idToken
            k <- semiColonToken
            l <- endToken
            return (a:b:c:[d]++e++[f]++[g]++h++[i]++[j]++[k]++[l])

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
stmt = assign <|> print_exp <|> while <|> dowhile <|> for <|> ifs 

operacao_boolean :: Parsec[Token] st [Token]
operacao_boolean = (do
              a <- greaterToken <|> lessToken <|> greaterEqualToken <|> lessEqualToken 
                <|> equalToken <|> diffToken
              return [a])

expressao_boolean :: Parsec[Token] st [Token]
expressao_boolean = do
                a <- intToken <|> idToken
                b <- operacao_boolean
                c <- intToken <|> idToken
                return ([a]++b++[c])

int_operation :: Parsec [Token] st [Token]
int_operation = do
        a <- sumToken <|> subToken <|> multToken <|> divToken <|> expToken <|> radToken <|> restoDivToken
        return [a]


expression_int :: Parsec [Token] st [Token]
expression_int = do
        a <- intToken <|> idToken
        b <- int_operation
        c <- intToken <|> idToken
        return ([a]++b++[c])

assign :: Parsec [Token] st [Token]
assign = try (do
          t <- typeToken
          a <- idToken
          b <- assignToken
          c <- expression
          return (t:a:b:c))
          <|>
          do
          a <- idToken
          b <- assignToken
          c <- expression
          return (a:b:c)


expression :: Parsec [Token] st [Token]
expression = try( do
                  a <- expression_int
                  return (a) )
                  <|>
             try( do
                  a <- arrayToken
                  return (a) )
                  <|>  
                  do 
                    a <- intToken <|> stringToken <|> boolToken <|> floatToken 
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
       c <- expressao_boolean
       d <- endParenthesisToken
       e <- beginToken
       f <- stmts
       g <- endToken
       return (a:[b] ++ c ++ d:[e]++ f ++ [g]) <|> (return [])

dowhile :: Parsec [Token] st [Token]
dowhile = do
       a <- doToken
       b <- beginToken
       c <- stmts
       d <- endToken
       e <- whileToken
       f <- beginParenthesisToken
       g <- expressao_boolean
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
       g <- beginToken
       h <- stmts
       i <- endToken
       return (a:b:c:d:e:f:[g]++h++[i]) <|> (return [])

ifs :: Parsec [Token] st [Token]
ifs = 
       try (do
         a <- ifToken
         b <- beginParenthesisToken
         c <- expressao_boolean 
         d <- endParenthesisToken
         e <- beginToken
         f <- stmts
         g <- endToken
         h <- elseToken
         i <- beginToken
         j <- stmts
         k <- endToken
         return (a:[b] ++ c ++ d:[e] ++ f ++ g:h:[i] ++ j ++ [k]))
         <|>
         (do
           a <- ifToken
           b <- beginParenthesisToken
           c <- expressao_boolean
           d <- endParenthesisToken
           e <- beginToken
           f <- stmts
           g <- endToken
           return (a:[b] ++ c ++ d:[e]++ f ++ [g])) <|> (return [])

remaining_stmts :: Parsec [Token] st [Token]
remaining_stmts = (do a <- semiColonToken
                      b <- stmts
                      return (a:b)) <|> (return [])

-- operacoes enter matrix e array
-- cardinalidade(#), produto escalar(.*),indexação. Tipos(array e matriz)

-- invocação do parser para o símbolo de partida 

parser :: [Token] -> Either ParseError [Token]
parser tokens = runParser program () "Error message" tokens

main :: IO ()
main = case parser (getTokens "program1.pe") of
            { Left err -> print err; 
              Right ans -> print ans
            }