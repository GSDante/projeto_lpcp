module Expressions where


import Lexer



eval :: Token -> Token -> Token -> Token
eval (Int pos x) (Greater _ ) (Int _ y) = Bool pos (x > y) 
eval (Float pos x) (Greater _ ) (Float _ y) = Bool pos (x > y) 

eval (Int pos x) (Less _ ) (Int _ y) = Bool pos (x < y) 
eval (Float pos x) (Less _ ) (Float _ y) = Bool pos (x < y) 

eval (Int pos x) (GreaterOrEqual _ ) (Int _ y) = Bool pos (x >= y) 
eval (Float pos x) (GreaterOrEqual _ ) (Float _ y) = Bool pos (x >= y) 

eval (Int pos x) (LessOrEqual _ ) (Int _ y) = Bool pos (x <= y) 
eval (Float pos x) (LessOrEqual _ ) (Float _ y) = Bool pos (x <= y) 

eval (Int pos x) (Equal _ ) (Int _ y) = Bool pos (x == y) 
eval (Float pos x) (Equal _ ) (Float _ y) = Bool pos (x == y) 

eval (Int pos x) (Diff _ ) (Int _ y) = Bool pos (x /= y) 
eval (Float pos x) (Diff _ ) (Float _ y) = Bool pos (x /= y) 

eval (Int pos x) (Sum _) (Int _ y) =  Int pos (x+y)
eval (Int pos x) (Sub _) (Int _ y) = Int pos(x-y)

eval (Float pos x) (Sum _) (Float _ y) =  Float pos (x+y)
eval (Float pos x) (Sub _) (Float _ y) = Float pos(x-y)

eval (Float pos x) (Sum _) (Int _ y) =  Float pos (x+ fromIntegral y)
eval (Float pos x) (Sub _) (Int _ y) = Float pos(x-fromIntegral y)

eval (Int pos x) (Sum _) (Float _ y) =  Float pos (fromIntegral x+y)
eval (Int pos x) (Sub _) (Float _ y) = Float pos(fromIntegral x-y)

eval (Float pos x) (Div _) (Float _ y) =  Float pos (x/y)
eval (Int pos x) (Div _) (Int _ y) =  Int pos (div x y)

eval (Int pos x) (Pow _) (Int _ y) = Int pos(x ^ y)
eval (Float pos x) (Pow _) (Float _ y) = Float pos(x ** y)
eval (Float pos x) (Pow _) (Int _ y) = Float pos(x ** fromIntegral y)

eval (Int pos x) (Multi _) (Int _ y) =  Int pos ( x * y)
eval (Float pos x) (Multi _) (Float _ y) =  Float pos ( x * y)

eval (Float pos x) (Multi _) (Int _ y) =  Float pos ( x * fromIntegral y)
eval (Int pos x) (Multi _) (Float _ y) =  Float pos (fromIntegral x * y)

eval (Int pos x) (Sum _) (Sum _) = Int pos ( x + 1 )
