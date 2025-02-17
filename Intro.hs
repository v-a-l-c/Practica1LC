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

-- first vowelsq
firstVowels :: String -> String
firstVowels = undefined

-- is anagram
isAnagram :: String -> String -> Bool
isAnagram = undefined

-- common suffix
commonSuffix :: [String] -> String
commonSuffix = undefined

-- intersection 
interseccion :: (Eq a) => [a] -> [a] -> [a]
interseccion = undefined

-- ackerman
ackerman :: Integer -> Integer -> Integer
ackerman = undefined

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
bTreeInsert x (Node y izq der) | x <= y = bTreeInsert x izq
                               | x > y = bTreeInsert x der
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
