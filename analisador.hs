module Main (main) where

import Lexer
import Text.Parsec

-- parsers para os tokens

programToken = tokenPrim show update_pos get_token where
  get_token Program = Just Program
  get_token _       = Nothing

idToken = tokenPrim show update_pos get_token where
  get_token (Id x) = Just (Id x)
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


typeToken = tokenPrim show update_pos get_token where
  get_token (Type x) = Just (Type x)
  get_token _       = Nothing

update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos pos _ (tok:_) = pos -- necessita melhoria
update_pos pos _ []      = pos

-- parsers para os não-terminais

program :: Parsec [Token] st [Token]
program = do
            a <- programToken 
            b <- beginToken 
            c <- stmts
            d <- endToken
            eof
            return (a:[b] ++ c ++ [d])

stmts :: Parsec [Token] st [Token]
stmts = try( do
          first <- stmt
          next <- remaining_stmts
          return (first ++ next))
          <|>
          return []

stmt :: Parsec [Token] st [Token]
stmt = assign <|> print_exp

assign :: Parsec [Token] st [Token]
assign = do
          t <- typeToken
          a <- idToken
          b <- assignToken
          c <- expression
          return (t:a:b:c)


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

int_operation :: Parsec [Token] st [Token]
int_operation = do
        a <- sumToken <|> subToken <|> multToken <|> divToken <|> expToken <|> radToken <|> restoDivToken
        return [a]


expression_int :: Parsec [Token] st [Token]
expression_int = do
        a <- intToken
        b <- int_operation
        c <- intToken 
        return ([a]++b++[c])


print_exp :: Parsec [Token] st [Token]
print_exp = do 
        a <- printToken
        b <- idToken <|> stringToken
        return (a:[b])

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