{-# LANGUAGE ScopedTypeVariables #-}
module TestSolucion where

import Intro
import Test.QuickCheck
import Data.Char (toLower, isSpace)
import Data.List (sort, isSuffixOf)

----------------------------
-- Pruebas para firstVowels
----------------------------

-- La función debe separar las vocales (en el orden en que aparecen) y luego los demás caracteres.
prop_firstVowels_correct :: String -> Bool
prop_firstVowels_correct xs =
  firstVowels xs ==
    (filter (`elem` "aeiouAEIOU") xs ++ filter (not . (`elem` "aeiouAEIOU")) xs)

----------------------------
-- Pruebas para isAnagram
----------------------------

-- Reflexividad: toda cadena es anagrama de sí misma.
prop_isAnagramRefl :: String -> Bool
prop_isAnagramRefl s = isAnagram s s

-- Simetría: si s es anagrama de t, entonces t es anagrama de s.
prop_isAnagramSym :: String -> String -> Bool
prop_isAnagramSym s t = isAnagram s t == isAnagram t s

-- Los espacios se ignoran: agregar espacios extra no debe alterar el resultado.
prop_isAnagramSpaces :: String -> Bool
prop_isAnagramSpaces s = isAnagram s (s ++ "  ")

----------------------------
-- Pruebas para commonSuffix
----------------------------

-- Para una lista de cadenas, el sufijo común debe ser, efectivamente, sufijo de cada cadena.
prop_commonSuffixIsSuffix :: [String] -> Bool
prop_commonSuffixIsSuffix xs =
  all (isSuffixOf suf) xs
  where suf = commonSuffix xs

----------------------------
-- Pruebas para interseccion
----------------------------

-- La intersección debe contener, en el mismo orden que en la segunda lista, 
-- todos los elementos que estén en ambas listas.
prop_interseccionElements :: [Int] -> [Int] -> Bool
prop_interseccionElements xs ys =
  let res = interseccion xs ys
  in  all (`elem` xs) res &&
      length res == length (filter (`elem` xs) ys)

----------------------------
-- Pruebas para ackerman
----------------------------

-- Para m == 0: ackerman 0 n debe ser n+1.
prop_ackermanBase :: NonNegative Integer -> Bool
prop_ackermanBase (NonNegative n) = ackerman 0 n == n + 1

-- Para m == 1: se espera ackerman 1 n == n+2.
prop_ackerman1 :: NonNegative Integer -> Bool
prop_ackerman1 (NonNegative n) = ackerman 1 n == n + 2

-- Para m == 2: se cumple que ackerman 2 n == 2*n+3 (para n pequeños, para evitar cálculos demasiado costosos).
prop_ackerman2 :: NonNegative Integer -> Property
prop_ackerman2 (NonNegative n) =
  n < 10 ==> ackerman 2 n == 2 * n + 3

----------------------------
-- Pruebas para quicksort
----------------------------

-- quicksort debe ordenar la lista igual que sort.
prop_quicksortSorted :: [Int] -> Bool
prop_quicksortSorted xs = quicksort xs == sort xs

----------------------------
-- Pruebas para BTree (bTreeInsert, bTreeSearch, bTreeMap, bTreeHeight)
----------------------------

-- Función auxiliar: recorrido inorden de un árbol BTree.
bTreeToList :: BTree a -> [a]
bTreeToList Empty = []
bTreeToList (Node s l r) = bTreeToList l ++ [s] ++ bTreeToList r

-- Luego de insertar todos los elementos, cada uno debe poder encontrarse en el árbol.
prop_bTreeSearchAll :: [Int] -> Bool
prop_bTreeSearchAll xs =
  let tree = foldr bTreeInsert Empty xs
  in  all (\x -> bTreeSearch x tree) xs

-- Al insertar un elemento, este debe ser encontrado en el árbol resultante.
prop_bTreeInsertSearch :: Int -> [Int] -> Bool
prop_bTreeInsertSearch x xs =
  let tree = foldr bTreeInsert Empty xs
      tree' = bTreeInsert x tree
  in  bTreeSearch x tree'

-- bTreeMap debe aplicar una función a todos los elementos del árbol.
prop_bTreeMap :: Fun Int Int -> [Int] -> Bool
prop_bTreeMap (Fun _ f) xs =
  let tree = foldr bTreeInsert Empty xs
  in  sort (map f (bTreeToList tree))
      == sort (bTreeToList (bTreeMap f tree))

-- bTreeHeight debe ser 0 para un árbol vacío.
prop_bTreeHeightEmpty :: Bool
prop_bTreeHeightEmpty = bTreeHeight Empty == 0

-- Para un árbol no vacío, la altura debe ser al menos 1.
prop_bTreeHeightNonEmpty :: [Int] -> Property
prop_bTreeHeightNonEmpty xs = (not (null xs)) ==> bTreeHeight tree >= 1
  where tree = foldr bTreeInsert Empty xs

-- La altura del árbol no puede exceder el número de nodos (caso peor: lista ordenada).
prop_bTreeHeightBound :: [Int] -> Bool
prop_bTreeHeightBound xs =
  let tree = foldr bTreeInsert Empty xs
  in  bTreeHeight tree <= length xs

----------------------------
-- Función main: ejecución de pruebas y calificación
----------------------------

main :: IO ()
main = do
  let n = pruebas
  putStrLn "Pruebas firstVowels:"
  fv1 <- quickCheckResult $ withMaxSuccess n prop_firstVowels_correct

  putStrLn "\nPruebas isAnagram:"
  an1 <- quickCheckResult $ withMaxSuccess n prop_isAnagramRefl
  an2 <- quickCheckResult $ withMaxSuccess n prop_isAnagramSym
  an3 <- quickCheckResult $ withMaxSuccess n prop_isAnagramSpaces

  putStrLn "\nPruebas commonSuffix:"
  cs1 <- quickCheckResult $ withMaxSuccess n prop_commonSuffixIsSuffix

  putStrLn "\nPruebas interseccion:"
  int1 <- quickCheckResult $ withMaxSuccess n prop_interseccionElements

  putStrLn "\nPruebas ackerman:"
  ack1 <- quickCheckResult $ withMaxSuccess n prop_ackermanBase
  ack2 <- quickCheckResult $ withMaxSuccess n prop_ackerman1
  ack3 <- quickCheckResult $ withMaxSuccess n prop_ackerman2

  putStrLn "\nPruebas quicksort:"
  qs1 <- quickCheckResult $ withMaxSuccess n prop_quicksortSorted

  putStrLn "\nPruebas bTreeSearch:"
  bst1 <- quickCheckResult $ withMaxSuccess n prop_bTreeSearchAll

  putStrLn "\nPruebas bTreeInsert (bTreeSearch tras inserción):"
  bst2 <- quickCheckResult $ withMaxSuccess n prop_bTreeInsertSearch

  putStrLn "\nPruebas bTreeMap:"
  btmap1 <- quickCheckResult $ withMaxSuccess n prop_bTreeMap

  putStrLn "\nPruebas bTreeHeight:"
  bth1 <- quickCheckResult $ withMaxSuccess n prop_bTreeHeightEmpty
  bth2 <- quickCheckResult $ withMaxSuccess n prop_bTreeHeightNonEmpty
  bth3 <- quickCheckResult $ withMaxSuccess n prop_bTreeHeightBound

  -- Reunimos los resultados de todas las pruebas.
  let resultados = [ fv1, an1, an2, an3, cs1, int1, ack1, ack2, ack3
                   , qs1, bst1, bst2, btmap1, bth1, bth2, bth3 ]
  let totalPruebas = length resultados
  let exitos = length $ filter isSuccess resultados

  putStrLn $ "\nPruebas exitosas: " ++ show exitos ++ "/" ++ show totalPruebas

  -- Cálculo de calificación tentativa:
  -- Asignamos los siguientes pesos:
  --   firstVowels:      1 prueba
  --   isAnagram:        3 pruebas
  --   commonSuffix:     1 prueba
  --   interseccion:     1 prueba
  --   ackerman:         3 pruebas
  --   quicksort:        1 prueba
  --   BTree:            6 pruebas (bTreeSearch, bTreeInsert, bTreeMap, bTreeHeightEmpty,
  --                                bTreeHeightNonEmpty, bTreeHeightBound)
  --
  -- Puntaje máximo = 1+3+1+1+3+1+6 = 16
  let score_firstVowels = if isSuccess fv1 then 1 else 0
      score_isAnagram   = fromIntegral . length . filter isSuccess $ [an1, an2, an3]
      score_commonSuffix = if isSuccess cs1 then 1 else 0
      score_interseccion = if isSuccess int1 then 1 else 0
      score_ackerman    = fromIntegral . length . filter isSuccess $ [ack1, ack2, ack3]
      score_quicksort   = if isSuccess qs1 then 1 else 0
      score_bTree       = fromIntegral . length . filter isSuccess $ [bst1, bst2, btmap1, bth1, bth2, bth3]
      puntajeTotal      = score_firstVowels + score_isAnagram + score_commonSuffix +
                          score_interseccion + score_ackerman + score_quicksort + score_bTree
      calificacion      = (puntajeTotal / 16) * 10
  putStrLn $ "Calificación tentativa: " ++ show calificacion
