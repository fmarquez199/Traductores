{
module Tokens_posn (Token(..), AlexPosn(..), alexScanTokens, token_posn) where
}

%wrapper "posn"

$keyword = [with begin end var int bool char array of if otherwise while for from to step read print not true false]
$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters
$literal = [\n \t \' \\]
--$identifier = $alpha [$alpha $digit \_]
$aritmethic = [\+ \- \* \/ \%]
$relational = [\< \= \>]
-- $boolean = [ \/\ \\\/ ]
$separator = [\( \) \[ \] \{ \} \, \. \:]
$instruction = [\;]
$arrays = [\$ \#]
$symbol = [$aritmethic $relational $separator $instruction $arrays]
-- $invalid = [$digit$alpha ^$symbol]

tokens :-

  $white+                       ;
  $keyword                      { tok (\p s -> Var p s) }
  $digit+                       { tok (\p s -> Int p (read s)) }
  [ [\<][\=] [\>][\=] [\/][\=]
    [\+][\+] [\-][\-] [\:][\:]
    [\\][\/] [\/][\\] [\<][\-]
    [\-][\>] ]         { tok (\p s -> Var p (read s)) } -- sym dobles.
  $symbol                       { tok (\p s -> Sym p (head s)) }
  \'.\'                         { tok (\p s -> Var p (read s)) } -- char
  $alpha [$alpha $digit \_]*    { tok (\p s -> Var p s) }
  [$digit$alpha ~$symbol]       { tok (\p s -> Var p (read s)) }

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