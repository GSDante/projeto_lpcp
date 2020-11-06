{
module Lexer where

import System.IO
import System.IO.Unsafe
}

%wrapper "posn"

$digit = 0-9      -- digits
$alpha = [a-zA-Z]   -- alphabetic characters
$alphanum = [a-zA-Z0-9]

tokens :-

  $white+                                ;
  "--".*                                 ;
  main                                { \p s -> Program p }
  "{"                                    { \p s -> Begin p}
  "}"                                    { \p s -> End p}
  ";"                                    { \p s -> SemiColon p}
  ":"                                    { \p s -> Colon p}
  ","                                    { \p s -> Comma p}
  void                                   { \p s -> Type p s}
  int                                    { \p s -> Type p s}
  float                                  { \p s -> Type p s}
  bool                                   { \p s -> Type p s}
  string                                 { \p s -> Type p s}
  array                                  { \p s -> Type p s}
  matrix                                 { \p s -> Type p s}
  const                                  { \p s -> Const p}
  =                                      { \p s -> Assign p}
  "("				                             { \p s-> BeginParenthesis p}
  ")"				                             { \p s-> EndParenthesis p}
  "["                                    { \p s-> BeginIndex p}
  "]"                                    { \p s-> EndIndex p}
  if                                     { \p s -> If p}
  else                                   { \p s -> Else p}
  print                                  { \p s -> Print p}
  while                                  { \p s -> While p}
  func                                   { \p s -> Func p}
  >                                      { \p s -> Greater p}
  "<"                                    { \p s -> Less p}
  ">="                                   { \p s -> GreaterOrEqual p}
  "<="                                   { \p s -> LessOrEqual p}
  "=="                                   { \p s -> Equal p}
  "!="                                   { \p s -> Diff p}
  "+"                                    { \p s -> Sum p}
  "+="                                   { \p s -> Increment p}
  "-="                                   { \p s -> Decrement p}
  "*="                                   { \p s -> MultEqual p}
  "/="                                   { \p s -> DivEqual p}
  "-"                                    { \p s -> Sub p}
  "*"                                    { \p s -> Multi p}
  "%"                                    { \p s -> Mod p}
  "^"                                    { \p s -> Pow p}
  "abs"                                  { \p s -> Abs p}
  "\-"                                   { \p s -> Rad p}
  "/"                                    { \p s -> Div p}
  "#"                                    { \p s -> Len p} 
  ".*"                                   { \p s -> InnerProd p} 
  "OR"                                   { \p s -> Or  p}
  "AND"                                  { \p s -> And  p}
  lenght                                 { \p s -> Lenght p}
  substr                                 { \p s -> Substr p}
  for                                    { \p s -> For p}
  do                                     { \p s -> Do  p}
  in                                     { \p s -> In  p}
  return                                 { \p s -> Return  p}
  $digit+                                { \p s -> Int p(read s)} 
  $digit+.$digit+                        { \p s -> Float p(read s)}
  "True"                                 { \p s -> Bool p(read s) }
  "False"                                { \p s -> Bool p(read s) }
  $alpha+[$alpha $digit \_ \']*          { \p s -> Id p s }
  \" $alphanum [$alphanum ! \_ \ \p s']* \"  { \p s -> String p(read s)}
{
-- Each action has type :: String -> Token

-- The token type:
data Token =
  Program  AlexPosn|
  Begin    AlexPosn|
  End      AlexPosn|
  BeginParenthesis  AlexPosn|
  EndParenthesis AlexPosn|
  BeginIndex AlexPosn|
  EndIndex AlexPosn|
  SemiColon AlexPosn|
  Colon AlexPosn|
  Comma AlexPosn|
  Assign    AlexPosn| 
  If  AlexPosn|
  Else AlexPosn|
  Print AlexPosn|
  Greater AlexPosn|
  Increment AlexPosn|
  Decrement AlexPosn|
  MultEqual AlexPosn|
  DivEqual AlexPosn|
  GreaterOrEqual AlexPosn|
  Less AlexPosn|
  LessOrEqual AlexPosn|
  Equal AlexPosn|
  Diff AlexPosn|
  Sum AlexPosn|
  Sub AlexPosn|
  Div AlexPosn|
  Mod AlexPosn|
  Abs AlexPosn|
  Multi AlexPosn|
  Pow AlexPosn|
  Rad AlexPosn|
  Len AlexPosn|
  InnerProd AlexPosn|
  Or AlexPosn|
  And AlexPosn|
  Lenght AlexPosn|
  Substr AlexPosn|
  In AlexPosn|
  For AlexPosn|
  While AlexPosn|
  Func AlexPosn|
  Do  AlexPosn|
  Return  AlexPosn|
  Const AlexPosn|
  Type AlexPosn String |
  Id AlexPosn String |
  Int AlexPosn Int |
  Float AlexPosn Float|
  Bool AlexPosn Bool |
  String AlexPosn String  
  deriving (Eq,Show)


 
getTokens fn = unsafePerformIO (getTokensAux fn)

getTokensAux fn = do {fh <- openFile fn ReadMode;
                      s <- hGetContents fh;
                      return (alexScanTokens s)}
}