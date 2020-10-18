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

whileToken = tokenPrim show update_pos get_token where
  get_token While     = Just While
  get_token _      = Nothing

beginToken = tokenPrim show update_pos get_token where
  get_token Begin = Just Begin
  get_token _     = Nothing

endToken = tokenPrim show update_pos get_token where
  get_token End = Just End
  get_token _   = Nothing

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

intToken = tokenPrim show update_pos get_token where
  get_token (Int x) = Just (Int x)
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
stmt = assign <|> while



operacao_boolean :: Parsec[Token] st [Token]
operacao_boolean = (do
              a <- greaterToken <|> lessToken <|> greaterEqualToken <|> lessEqualToken 
                <|> equalToken <|> diffToken
              return [a])

expressao_boolean :: Parsec[Token] st [Token]
expressao_boolean = do
                a <- intToken
                b <- operacao_boolean
                c <- intToken
                return ([a]++b++[c])

assign :: Parsec [Token] st [Token]
assign = do
          a <- idToken
          b <- assignToken
          c <- intToken
          return (a:b:[c])

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