{-  Titulo: Lexer.hs
 -
 -  Descripicion: Analizador lexicografico (lexer) del lenguaje, basado en  
 -  Fortran, BasicTran. El lexer reconoce y representa los Tokens del lenguaje
 -  como se sigue (ordenadas por orden alfabetico y agrupadas por categorias
 -  ordenadas a su vez por precedencia):
 -
 -             Tipo         |        Tokens          |     Representacion.
 -  ------------------------+------------------------+-------------------------
 -      Palabra reservada   |         array          |        TkArray
 -      Palabra reservada   |         begin          |        TkBegin
 -      Palabra reservada   |         bool           |        TkBool
 -      Palabra reservada   |         char           |        TkChar
 -      Palabra reservada   |         end            |        TkEnd
 -      Palabra reservada   |         false          |        TkFalse
 -      Palabra reservada   |         for            |        TkFor
 -      Palabra reservada   |         from           |        TkFrom
 -      Palabra reservada   |         if             |        TkIf
 -      Palabra reservada   |         int            |        TkInt
 -      Palabra reservada   |         not            |        TkNegacion
 -      Palabra reservada   |         of             |        TkOf
 -      Palabra reservada   |         otherwise      |        TkOtherwise
 -      Palabra reservada   |         print          |        TkPrint
 -      Palabra reservada   |         read           |        TkRead
 -      Palabra reservada   |         step           |        TkStep
 -      Palabra reservada   |         to             |        TkTo
 -      Palabra reservada   |         true           |        TkTrue
 -      Palabra reservada   |         var            |        TkVar
 -      Palabra reservada   |         while          |        TkWhile
 -      Palabra reservada   |         with           |        TkWith
 -  ------------------------+------------------------+-------------------------
 -            Numero        |            n           |        TkNum(n)
 -  ------------------------+------------------------+-------------------------
 -           Caracter       |            \'          |        TkCaracter('\'')
 -           Caracter       |            \\          |        TkCaracter('\\')
 -           Caracter       |            \n          |        TkCaracter('\n')
 -           Caracter       |            \t          |        TkCaracter('\t')
 -           Caracter       |            x           |        TkCaracter('x')
 -  ------------------------+------------------------+-------------------------
 -         Identificador    |            id          |        TkId("id")
 -  ------------------------+------------------------+-------------------------
 -           Simbolo        |            #           |        TkValorAscii
 -           Simbolo        |            $           |        TkShift
 -           Simbolo        |            %           |        TkMod
 -           Simbolo        |            (           |        TkParAbre
 -           Simbolo        |            )           |        TkParCierra
 -           Simbolo        |            *           |        TkMult
 -           Simbolo        |            +           |        TkSuma
 -           Simbolo        |            ++          |        TkSiguienteCar
 -           Simbolo        |            ,           |        TkComa
 -           Simbolo        |            -           |        TkResta
 -           Simbolo        |            --          |        TkAnteriorCar
 -           Simbolo        |            ->          |        TkHacer
 -           Simbolo        |            .           |        TkPunto
 -           Simbolo        |            /           |        TkDiv
 -           Simbolo        |            /=          |        TkDesigual
 -           Simbolo        |            /\          |        TkConjuncion
 -           Simbolo        |            :           |        TkDosPuntos
 -           Simbolo        |            ::          |        TkConcatenacion
 -           Simbolo        |            ;           |        TkPuntoYComa
 -           Simbolo        |            <           |        TkMenor
 -           Simbolo        |            <-          |        TkAsignacion
 -           Simbolo        |            <=          |        TkMenorIgual
 -           Simbolo        |            =           |        TkIgual
 -           Simbolo        |            >           |        TkMayor
 -           Simbolo        |            >=          |        TkMayorIgual
 -           Simbolo        |            [           |        TkCorcheteAbre
 -           Simbolo        |            \/          |        TkDisyuncion
 -           Simbolo        |            ]           |        TkCorcheteCierra
 -           Simbolo        |            {           |        TkLlaveAbre
 -           Simbolo        |            }           |        TkLlaveCierra
 -
 -  Autores: Francisco Marquez    12-11163
 -           Angel Morante        13-10931
 -
 -  Fecha: 10 de mayo de 2 018.
 -
 -}

import System.Environment
import System.IO
import Tokens_posn
-- token_show :: Token -> String
token_show (Sym p s)
  | s == '#'   = "TkValorAscii "                ++ posx
  | s == '$'   = "TkShift "                     ++ posx
  | s == '%'   = "TkMod "                       ++ posx
  | s == '('   = "TkParAbre "                   ++ posx
  | s == ')'   = "TkParCierra "                 ++ posx
  | s == '*'   = "TkMult "                      ++ posx
  | s == '+'   = "TkSuma "                      ++ posx
  | s == ','   = "TkComa "                      ++ posx
  | s == '-'   = "TkResta "                     ++ posx
  | s == '.'   = "TkPunto "                     ++ posx
  | s == '/'   = "TkDiv "                       ++ posx
  | s == ':'   = "TkDosPuntos "                 ++ posx
  | s == ';'   = "TkPuntoYComa "                ++ posx
  | s == '<'   = "TkMenor "                     ++ posx
  | s == '='   = "TkIgual "                     ++ posx
  | s == '>'   = "TkMayor"                      ++ posx
  | s == '['   = "TkCorcheteAbre "              ++ posx
  | s == ']'   = "TkCorcheteCierra "            ++ posx
  | s == '{'   = "TkLlaveAbre "                 ++ posx
  | s == '}'   = "TkLlaveCierra "               ++ posx
  | otherwise  = "TkCaracter(" ++ show s ++ ") " ++ posx
  where posx = posn_show p
token_show (Var p s)
  | s == "array"     = "TkArray "          ++ posx
  | s == "begin"     = "TkBegin "          ++ posx
  | s == "bool"      = "TkBool "           ++ posx
  | s == "char"      = "TkChar "           ++ posx
  | s == "end"       = "TkEnd "            ++ posx
  | s == "false"     = "TkFalse "          ++ posx
  | s == "for"       = "TkFor "            ++ posx
  | s == "from"      = "TkFrom "           ++ posx
  | s == "if"        = "TkIf "             ++ posx
  | s == "int"       = "TkInt "            ++ posx
  | s == "not"       = "TkNegacion "       ++ posx
  | s == "of"        = "TkOf "             ++ posx
  | s == "otherwise" = "TkOtherwise "      ++ posx
  | s == "print"     = "TkPrint "          ++ posx
  | s == "read"      = "TkRead "           ++ posx
  | s == "Step"      = "TkStep "           ++ posx
  | s == "to"        = "TkTo "             ++ posx
  | s == "true"      = "TkTrue "           ++ posx
  | s == "var"       = "TkVar "            ++ posx
  | s == "while"     = "TkWhile "          ++ posx
  | s == "with"      = "TkWith "           ++ posx
  | s == "++"        = "TkSiguienteCar "   ++ posx
  | s == "--"        = "TkAnteriorCar "    ++ posx
  | s == "->"        = "TkHacer "          ++ posx
  | s == "/="        = "TkDesigual"        ++ posx
  | s == "/\\"       = "TkConjuncion "     ++ posx
  | s == "::"        = "TkConcatenacion "  ++ posx
  | s == "<-"        = "TkAsignacion"      ++ posx
  | s == "<="        = "TkMenorIgual "     ++ posx
  | s == ">="        = "TkMayorIgual "     ++ posx
  | s == "\\/"       = "TkDisyuncion "     ++ posx
  | otherwise        = "TkId(" ++ s ++ ") " ++ posx
  where posx = posn_show p
token_show (Int p s) = "TkNum(" ++ (show s) ++ ") " ++ (posn_show p)

posn_show (AlexPn _ row colum) = show row ++ " " ++ show colum

print_list [] = ""
print_list (x:xs) = x ++ "\n" ++ (print_list xs)

main = do
  args <- getArgs
  file <- openFile (head args) ReadMode
  content <- hGetContents file
  let tokens = (alexScanTokens content)
  let graphic = map token_show tokens
  putStrLn (print_list graphic)
  hClose file
  