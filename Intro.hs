-- | Práctica 1: Introducción a Haskell
-- Profesor: Manuel Soto Romero
-- Ayudante: Diego Méndez Medina
-- Ayudante: José Alejandro Pérez Marquez
-- Laboratorio: Erick Daniel Arroyo Martínez
-- Laboratorio: Erik Rangel Limón

module Intro where

import Data.Char
import Data.List
import GHC.Generics

-- first vowelsq 1
firstVowels :: String -> String
firstVowels str = [c | c <- str, toLower c `elem` "aeiou"] ++ [c | c <- str, toLower c `notElem` "aeiou"]

-- is anagram 2
isAnagram :: String -> String -> Bool
isAnagram s1 s2 = sort (map toLower (filter (/= ' ') s1)) == sort (map toLower (filter (/= ' ') s2))

-- common suffix 3
commonSuffix :: [String] -> String
commonSuffix [] = ""
commonSuffix strs = reverse (foldl1 commonPrefix (map reverse strs))
  where
    commonPrefix a b = takeWhile (uncurry (==)) (zip a b) >>= (\(x, _) -> [x])

-- intersection 4
interseccion :: (Eq a) => [a] -> [a] -> [a]
interseccion xs ys = [y | y <- ys, y `elem` xs]
-- ackerman 5
ackerman :: Integer -> Integer -> Integer
ackerman 0 n = n + 1
ackerman m 0 = ackerman (m - 1) 1
ackerman m n = ackerman (m - 1) (ackerman m (n - 1))

-- quicksort
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (piv:xs) = quicksort min ++ [piv] ++ quicksort max
    where
        max = filter (>= piv) xs
        min = filter (< piv) xs

-- Definición de un árbol binario de búsqueda (BST)
data BTree a = Empty
             | Node a (BTree a) (BTree a)
             deriving (Show, Eq)

-- bTreeInsert
bTreeInsert :: (Ord a) => a -> BTree a -> BTree a
bTreeInsert x Empty = Node x Empty Empty
bTreeInsert x (Node y izq der) | x < y = Node y (bTreeInsert x izq) der
                               | x > y = Node y izq (bTreeInsert x der)
                               | x == y = Node y izq der
-- bTreeSearch
bTreeSearch :: (Ord a) => a -> BTree a -> Bool
bTreeSearch x Empty = False
bTreeSearch x (Node y izq der) | x < y = bTreeSearch x izq
                               | x > y = bTreeSearch x der
                               | x == y = True

-- bTreeMap
bTreeMap :: (a -> b) -> BTree a -> BTree b
bTreeMap f Empty = Empty
bTreeMap f (Node a izq der) = Node (f a) (bTreeMap f izq) (bTreeMap f der)

-- bTreeHeight
bTreeHeight :: BTree a -> Int
bTreeHeight Empty = 0
bTreeHeight (Node a izq der) = 1 + max (bTreeHeight izq) (bTreeHeight der)

-- Ejemplo de arbol
bTree1 = Node 6 
            (Node 4 
                (Node 2 
                    (Node 1 Empty Empty) 
                    (Node 3 Empty Empty)
                ) 
                (Node 5 Empty Empty)
            ) 
            (Node 8 
                (Node 7 Empty Empty) 
                (Node 9 
                    Empty 
                    (Node 10 Empty Empty)
                )
            )

-- | Número de pruebas
pruebas :: Int
pruebas = 1000
