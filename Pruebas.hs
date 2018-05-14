{- Titulo: suit de pruebas.
 -
 -}

import System.Environment
import System.IO

main = do
  arg <- getArgs
  -- if length arg /= 1 then
  --   error "Debe introducir un solo nombre de archivo."
  let salidaPrograma = "Respuestas\\"
  let salidaEsperada = "Respuestas esperadas\\"
  let resultFile = salidaPrograma ++ (head arg)
  let referenceFile = salidaEsperada ++ (head arg)
  file1 <- readFile resultFile
  file2 <- readFile referenceFile
  if file1 == file2 then
    putStrLn "Comportamiento esperado."
  else
    putStrLn "Comportamiento no esperado.\nRevisar archivos y arreglar."
  putStrLn "Fin del programa."