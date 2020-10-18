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

assignToken = tokenPrim show update_pos get_token where
  get_token Assign = Just Assign
  get_token _      = Nothing

intToken = tokenPrim show update_pos get_token where
  get_token (Int x) = Just (Int x)
  get_token _       = Nothing

--arrayToken :: Parsec [Token] st [Token]
--arrayToken =
--    do
--        lbrack <- beginIndexToken
--        elementtype <- many (noneOf ",")
--        rbrack <- endIndexToken
        
--        return (lbrack:elementtype : rbrack)

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
stmts = try(do
          first <- stmt
          next <- remaining_stmts
          return (first ++ next))
          <|> return []

stmt :: Parsec [Token] st [Token]
stmt = assign 

assign :: Parsec [Token] st [Token]
assign = do
          a <- idToken
          b <- assignToken
          c <- intToken <|> beginIndexToken
          return (a:b:[c])

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