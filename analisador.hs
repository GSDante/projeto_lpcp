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

semiColonToken :: Parsec [Token] st Token
semiColonToken = tokenPrim show update_pos get_token where
  get_token SemiColon = Just SemiColon
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
          c <- intToken <|> stringToken <|> boolToken <|> floatToken
          return (t:a:b:[c])

print_exp :: Parsec [Token] st [Token]
print_exp = do 
        a <- printToken
        b <- idToken <|> stringToken
        return (a:[b])

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