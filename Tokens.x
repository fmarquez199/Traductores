{
module Tokens_posn (Token(..), AlexPosn(..), alexScanTokens, token_posn) where
}

%wrapper "posn"

$keyword = [with begin end var int bool char array of if otherwise while for from to step read print not true false]
$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters
$literal = [\n \t \' \\]
--$identifier = $alpha [$alpha $digit \_]
$aritmethic = [\+ \- \* \/ \% \-$digit]
$relational = [\< \= \>]
$boolean = [ \/\ \\\/ ]
$separator = [\( \) \[ \] \{ \} \, \. \:]
$instruction = [\;]
$arrays = [\$ \#]
$symbol = [$aritmethic $relational $boolean $separator $instruction $arrays]
$invalid = [$digit $alpha $symbol $literal]

tokens :-

  $white+                       ;
  $keyword                      { tok (\p s -> Var p s) }
  $digit+                       { tok (\p s -> Int p (read s)) }
  -- \- $digit+                    { tok (\p s -> Var p (head s)) }
  \-\>                          { tok (\p s -> Var p s) } -- TkHacer
  \<\-                          { tok (\p s -> Var p s) } -- TkAsignacion
  \<\=                          { tok (\p s -> Var p s) } -- TkMenorIgual
  \>\=                          { tok (\p s -> Var p s) } -- TkMayorIgual
  \/\=                          { tok (\p s -> Var p s) } -- TkDesigual
  \+\+                          { tok (\p s -> Var p s) } -- TkSiguienteCar
  \-\-                          { tok (\p s -> Var p s) } -- TkAnteriorCar
  \:\:                          { tok (\p s -> Var p s) } -- TkConcatenacion
  \\\/                          { tok (\p s -> Var p s) } -- TkDisyuncion
  \/\\                          { tok (\p s -> Var p s) } -- TkConjuncion
  $symbol                       { tok (\p s -> Sym p (head s)) }
  \'.\'                         { tok (\p s -> Var p (read (shows s ""))) } -- char
  $alpha [$alpha $digit \_]*    { tok (\p s -> Var p s) }
  ~$invalid                     { tok (\p s -> Sym p (head s)) }

{
-- Each right-hand side has type :: AlexPosn -> String -> Token
-- Some action helpers:
tok f p s = f p s
-- The token type:
data Token =
    Keyword AlexPosn String   |
    Id  AlexPosn              |
    Sym AlexPosn Char         |
    Var AlexPosn String       |
    Int AlexPosn Int          |
    Invalid AlexPosn Char
    deriving (Eq,Show)

token_posn (Sym p _) = p
token_posn (Var p _) = p
token_posn (Int p _) = p

}