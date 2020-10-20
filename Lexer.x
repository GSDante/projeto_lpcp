{
module Lexer where

import System.IO
import System.IO.Unsafe
}

%wrapper "basic"

$digit = 0-9      -- digits
$alpha = [a-zA-Z]   -- alphabetic characters
$alphanum = [a-zA-Z0-9]

tokens :-

  $white+                                ;
  "--".*                                 ;
  main                                { \s -> Program }
  "{"                                    { \s -> Begin}
  "}"                                    { \s -> End}
  ";"                                    { \s -> SemiColon}
  ":"                                    { \s -> Colon}
  ","                                    { \s -> Comma}
  int                                    { \s -> Type s}
  float                                  { \s -> Type s}
  bool                                   { \s -> Type s}
  string                                 { \s -> Type s}
  array                                  { \s -> Type s}
  matrix                                 { \s -> Type s}
  =                                      { \s -> Assign}
  "("				                             { \s -> BeginParenthesis}
  ")"				                             { \s -> EndParenthesis}
  "["                                    { \s -> BeginIndex}
  "]"                                    { \s -> EndIndex}
  if                                     { \s -> If}
  else                                   { \s -> Else}
  print                                  { \s -> Print}
  while                                  { \s -> While}
  func                                   { \s -> Func}
  >                                      { \s -> Greater}
  "<"                                    { \s -> Less}
  ">="                                   { \s -> GreaterOrEqual}
  "<="                                   { \s -> LessOrEqual}
  "=="                                   { \s -> Equal}
  "!="                                   { \s -> Diff}
  "+"                                    { \s -> Sum}
  "+="                                   { \s -> Increment}
  "-="                                   { \s -> Decrement}
  "*="                                   { \s -> MultEqual}
  "/="                                   { \s -> DivEqual}
  "-"                                    { \s -> Sub}
  "*"                                    { \s -> Multi}
  "%"                                    { \s -> Mod}
  "^"                                    { \s -> Pow}
  "\-"                                   { \s -> Rad}
  "/"                                    { \s -> Div}
  "#"                                    { \s -> Len} 
  ".*"                                   { \s -> InnerProd} 
  "OR"                                   { \s -> Or }
  "AND"                                  { \s -> And }
  for                                    { \s -> For}
  do                                     { \s -> Do }
  $digit+                                { \s -> Int (read s)} 
  $digit+.$digit+                        { \s -> Float (read s)}
  "True"                                 { \s -> Bool (read s) }
  "False"                                { \s -> Bool (read s) }
  $alpha+[$alpha $digit \_ \']*          { \s -> Id s }
  \" $alphanum [$alphanum ! \_ \ \s']* \"  { \s -> String s}
{
-- Each action has type :: String -> Token

-- The token type:
data Token =
  Program |
  Begin   |
  End     |
  BeginParenthesis |
  EndParenthesis |
  BeginIndex |
  EndIndex |
  SemiColon |
  Colon |
  Comma |
  Assign    | 
  If  |
  Else |
  Print |
  Greater |
  Increment |
  Decrement|
  MultEqual |
  DivEqual |
  GreaterOrEqual |
  Less|
  LessOrEqual |
  Equal |
  Diff |
  Sum |
  Sub|
  Div|
  Mod|
  Multi|
  Pow|
  Rad|
  Len |
  InnerProd |
  Or |
  And |
  For|
  While|
  Func|
  Do |
  Type String |
  Id String |
  Int Int |
  Float Float|
  Bool Bool |
  String String  
  deriving (Eq,Show)


getTokens fn = unsafePerformIO (getTokensAux fn)

getTokensAux fn = do {fh <- openFile fn ReadMode;
                      s <- hGetContents fh;
                      return (alexScanTokens s)}
}