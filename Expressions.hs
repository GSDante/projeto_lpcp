module Expressions where

import Lexer
import Tokens
import Memory
import Data.List

compare_token :: Token -> Token -> Bool
compare_token a b 
    | a == b  = True
    | otherwise = False

eval :: [Token] -> Type
eval (BeginIndex p :b) = eval b 
eval ((Float p i): b) = Vector (FloatType i:[(eval b)]) 
eval ((Int p i): b) = Vector (IntType i : [(eval b)])
eval (Comma p: b) = eval b 
eval ([EndIndex p]) = Vector [] 
