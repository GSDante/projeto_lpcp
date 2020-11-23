module Tokens where

import Lexer

import Text.Parsec
import Control.Monad.IO.Class

import System.IO.Unsafe

import Data.List


programToken :: ParsecT [Token] st IO (Token)
programToken = tokenPrim show update_pos get_token where
  get_token (Program p)  = Just (Program p) 
  get_token _       = Nothing

funcToken :: ParsecT [Token] st IO (Token)
funcToken = tokenPrim show update_pos get_token where
  get_token (Func p) = Just (Func p)
  get_token _       = Nothing

procToken :: ParsecT [Token] st IO (Token)
procToken = tokenPrim show update_pos get_token where
  get_token (Proc p) = Just (Proc p)
  get_token _       = Nothing

idToken :: ParsecT [Token] st IO (Token)
idToken = tokenPrim show update_pos get_token where
  get_token (Id p s) = Just (Id p s)
  get_token _      = Nothing

constToken :: ParsecT [Token] st IO (Token)
constToken = tokenPrim show update_pos get_token where
  get_token (Const p) = Just (Const p)
  get_token _      = Nothing

whileToken :: ParsecT [Token] st IO (Token)
whileToken = tokenPrim show update_pos get_token where
  get_token (While p)    = Just (While p)
  get_token _      = Nothing

doToken :: ParsecT [Token] st IO (Token)
doToken = tokenPrim show update_pos get_token where
  get_token (Do p)   = Just (Do p)
  get_token _      = Nothing

forToken :: ParsecT [Token] st IO (Token)
forToken = tokenPrim show update_pos get_token where
  get_token (For p)   = Just (For p)
  get_token _      = Nothing

inToken :: ParsecT [Token] st IO (Token)
inToken = tokenPrim show update_pos get_token where
  get_token (In p)   = Just (In p)
  get_token _      = Nothing

ifToken :: ParsecT [Token] st IO (Token)
ifToken = tokenPrim show update_pos get_token where
  get_token (If p)    = Just (If p)
  get_token _      = Nothing

elseToken :: ParsecT [Token] st IO (Token)
elseToken = tokenPrim show update_pos get_token where
  get_token (Else p)   = Just (Else p)
  get_token _      = Nothing

beginToken :: ParsecT [Token] st IO (Token)
beginToken = tokenPrim show update_pos get_token where
  get_token (Begin p) = Just (Begin p)
  get_token _     = Nothing

endToken :: ParsecT [Token] st IO (Token)
endToken = tokenPrim show update_pos get_token where
  get_token (End p) = Just (End p)
  get_token _   = Nothing

beginWhileToken :: ParsecT [Token] st IO (Token)
beginWhileToken = tokenPrim show update_pos get_token where
  get_token (BeginWhile p) = Just (BeginWhile p)
  get_token _     = Nothing

endWhileToken :: ParsecT [Token] st IO (Token)
endWhileToken = tokenPrim show update_pos get_token where
  get_token (EndWhile p) = Just (EndWhile p)
  get_token _   = Nothing

beginForToken :: ParsecT [Token] st IO (Token)
beginForToken = tokenPrim show update_pos get_token where
  get_token (BeginFor p) = Just (BeginFor p)
  get_token _     = Nothing

endForToken :: ParsecT [Token] st IO (Token)
endForToken = tokenPrim show update_pos get_token where
  get_token (EndFor p) = Just (EndFor p)
  get_token _   = Nothing

beginIfToken :: ParsecT [Token] st IO (Token)
beginIfToken = tokenPrim show update_pos get_token where
  get_token (BeginIf p) = Just (BeginIf p)
  get_token _     = Nothing

endIfToken :: ParsecT [Token] st IO (Token)
endIfToken = tokenPrim show update_pos get_token where
  get_token (EndIf p) = Just (EndIf p)
  get_token _   = Nothing

beginParenthesisToken :: ParsecT [Token] st IO (Token)
beginParenthesisToken = tokenPrim show update_pos get_token where
  get_token (BeginParenthesis p) = Just (BeginParenthesis p)
  get_token _   = Nothing

endParenthesisToken :: ParsecT [Token] st IO (Token)
endParenthesisToken = tokenPrim show update_pos get_token where
  get_token (EndParenthesis p) = Just (EndParenthesis p)
  get_token _   = Nothing  

semiColonToken :: ParsecT [Token] st IO (Token)
semiColonToken = tokenPrim show update_pos get_token where
  get_token (SemiColon p) = Just (SemiColon p)
  get_token _         = Nothing



colonToken :: ParsecT [Token] st IO (Token)
colonToken = tokenPrim show update_pos get_token where
  get_token (Colon p) = Just (Colon p)
  get_token _         = Nothing

commaToken :: ParsecT [Token] st IO (Token)
commaToken = tokenPrim show update_pos get_token where
  get_token (Comma p) = Just (Comma p)
  get_token _     = Nothing

assignToken :: ParsecT [Token] st IO (Token)
assignToken = tokenPrim show update_pos get_token where
  get_token (Assign p) = Just (Assign p)
  get_token _      = Nothing

greaterToken :: ParsecT [Token] st IO (Token)
greaterToken = tokenPrim show update_pos get_token where
  get_token (Greater p) = Just (Greater p)
  get_token _       = Nothing

lessToken :: ParsecT [Token] st IO (Token)
lessToken = tokenPrim show update_pos get_token where
  get_token (Less p) = Just (Less p)
  get_token _    = Nothing

greaterEqualToken :: ParsecT [Token] st IO (Token)
greaterEqualToken = tokenPrim show update_pos get_token where
  get_token (GreaterOrEqual p) = Just (GreaterOrEqual p)
  get_token _              = Nothing

lessEqualToken :: ParsecT [Token] st IO (Token)
lessEqualToken = tokenPrim show update_pos get_token where
  get_token (LessOrEqual p) = Just (LessOrEqual p)
  get_token _              = Nothing

equalToken :: ParsecT [Token] st IO (Token)
equalToken = tokenPrim show update_pos get_token where
  get_token (Equal p) = Just (Equal p)
  get_token _     = Nothing

diffToken :: ParsecT [Token] st IO (Token)
diffToken = tokenPrim show update_pos get_token where
  get_token (Diff p) = Just (Diff p)
  get_token _              = Nothing

printToken :: ParsecT [Token] st IO (Token)
printToken = tokenPrim show update_pos get_token where
  get_token (Print p) = Just (Print p)
  get_token _       = Nothing


intToken :: ParsecT [Token] st IO (Token)
intToken = tokenPrim show update_pos get_token where
  get_token (Int p s) = Just (Int p s)
  get_token _       = Nothing

stringToken :: ParsecT [Token] st IO (Token)
stringToken = tokenPrim show update_pos get_token where
  get_token (String p s) = Just (String p s)
  get_token _       = Nothing


boolToken :: ParsecT [Token] st IO (Token)
boolToken = tokenPrim show update_pos get_token where
  get_token (Bool p s) = Just (Bool p s)
  get_token _       = Nothing


orToken :: ParsecT [Token] st IO (Token)
orToken = tokenPrim show update_pos get_token where
  get_token (Or p) = Just (Or p)
  get_token _  = Nothing


andToken :: ParsecT [Token] st IO (Token)
andToken = tokenPrim show update_pos get_token where
  get_token (And p) = Just (And p)
  get_token _   = Nothing


floatToken :: ParsecT [Token] st IO (Token)
floatToken = tokenPrim show update_pos get_token where
  get_token (Float p s) = Just (Float p s)
  get_token _       = Nothing


beginIndexToken :: ParsecT [Token] st IO (Token)
beginIndexToken = tokenPrim show update_pos get_token where
  get_token (BeginIndex p) = Just (BeginIndex p)
  get_token _     = Nothing

endIndexToken :: ParsecT [Token] st IO (Token)
endIndexToken = tokenPrim show update_pos get_token where
  get_token (EndIndex p) = Just (EndIndex p)
  get_token _     = Nothing

lenghtToken :: ParsecT [Token] st IO (Token)
lenghtToken = tokenPrim show update_pos get_token where
  get_token (Lenght p) = Just (Lenght p)
  get_token _  = Nothing


substrToken :: ParsecT [Token] st IO (Token)
substrToken = tokenPrim show update_pos get_token where
  get_token (Substr p) = Just (Substr p)
  get_token _  = Nothing

sumToken :: ParsecT [Token] st IO (Token)
sumToken = tokenPrim show update_pos get_token where
  get_token (Sum p ) = Just (Sum p )
  get_token _   = Nothing

subToken :: ParsecT [Token] st IO (Token)
subToken = tokenPrim show update_pos get_token where
  get_token (Sub p) = Just (Sub p)
  get_token _       = Nothing

multToken :: ParsecT [Token] st IO (Token)
multToken = tokenPrim show update_pos get_token where
  get_token (Multi p)= Just (Multi p)
  get_token _       = Nothing

divToken :: ParsecT [Token] st IO (Token)
divToken = tokenPrim show update_pos get_token where
  get_token (Div p) = Just (Div p)
  get_token _       = Nothing

expToken :: ParsecT [Token] st IO (Token)
expToken = tokenPrim show update_pos get_token where
  get_token (Pow p)= Just (Pow p)
  get_token _       = Nothing

radToken :: ParsecT [Token] st IO (Token)
radToken = tokenPrim show update_pos get_token where
  get_token (Rad p) = Just (Rad p)
  get_token _       = Nothing

restoDivToken :: ParsecT [Token] st IO (Token)
restoDivToken = tokenPrim show update_pos get_token where
  get_token (Mod p) = Just (Mod p)
  get_token _       = Nothing

absToken :: ParsecT [Token] st IO (Token)
absToken = tokenPrim show update_pos get_token where
  get_token (Abs p) = Just (Abs p)
  get_token _       = Nothing

returnToken :: ParsecT [Token] st IO (Token)
returnToken = tokenPrim show update_pos get_token where
  get_token (Return p) = Just (Return p)
  get_token _       = Nothing

lenToken :: ParsecT [Token] st IO (Token)
lenToken = tokenPrim show update_pos get_token where
  get_token (Len p) = Just (Len p)
  get_token _   = Nothing

transposeToken :: ParsecT [Token] st IO (Token)
transposeToken = tokenPrim show update_pos get_token where
  get_token (Transpose p) = Just (Transpose p)
  get_token _   = Nothing

innerProdToken :: ParsecT [Token] st IO (Token)
innerProdToken = tokenPrim show update_pos get_token where
  get_token (InnerProd p) = Just (InnerProd p)
  get_token _         = Nothing

swapLinesToken :: ParsecT [Token] st IO (Token)
swapLinesToken = tokenPrim show update_pos get_token where
  get_token (SwapLines p) = Just (SwapLines p)
  get_token _             = Nothing

typeToken :: ParsecT [Token] st IO (Token)
typeToken = tokenPrim show update_pos get_token where
  get_token (Type p s) = Just (Type p s)
  get_token _       = Nothing

update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos pos _ (tok:_) = pos -- necessita melhoria
update_pos pos _ []      = pos
