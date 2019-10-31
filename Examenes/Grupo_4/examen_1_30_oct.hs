-- Informática (1º del Grado en Matemáticas, Grupo 4)
-- 1º examen de evaluación continua (30 de octubre de 2019)
-- ---------------------------------------------------------------------

-- Nota: La puntuación de cada ejercicio es 2.5 puntos.

import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1. La distancia de Hamming entre dos listas es el número de
-- posiciones en que los correspondientes elementos son distintos. Por
-- ejemplo, la distancia de Hamming entre "roma" y "loba" es 2 (porque
-- hay 2 posiciones en las que los elementos correspondientes son
-- distintos: la 1ª y la 3ª). 
--    
-- Definir la función
--    distancia :: Eq a => [a] -> [a] -> Int
-- tal que (distancia xs ys) es la distancia de Hamming entre xs e
-- ys. Por ejemplo,
--    distancia "romano" "comino"  ==  2
--    distancia "romano" "camino"  ==  3
--    distancia "roma"   "comino"  ==  2
--    distancia "roma"   "camino"  ==  3
--    distancia "romano" "ron"     ==  1
--    distancia "romano" "cama"    ==  2
--    distancia "romano" "rama"    ==  1
-- ---------------------------------------------------------------------

-- 1ª definición (por comprensión):
distancia :: Eq a => [a] -> [a] -> Int
distancia xs ys = sum [1 | (x,y) <- zip xs ys, x /= y] 

-- 2ª definición (por recursión):
distancia2 :: Eq a => [a] -> [a] -> Int
distancia2 [] _ = 0
distancia2 _ [] = 0
distancia2 (x:xs) (y:ys) | x /= y    = 1 + distancia2 xs ys
                         | otherwise = distancia2 xs ys

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    esPotencia :: Integer -> Integer -> Bool
-- tal que (esPotencia x a) se verifica si x es una potencia de a. Por
-- ejemplo, 
--    esPotencia 32 2  ==  True
--    esPotencia 42 2  ==  False
-- ---------------------------------------------------------------------

-- 1ª definición (por comprensión):
esPotencia :: Integer -> Integer -> Bool
esPotencia x a = x `elem` [a^n | n <- [0..x]]

-- 2ª definición (por recursión):
esPotencia2 :: Integer -> Integer -> Bool
esPotencia2 x a = aux x a 0
  where aux x a b | b > x     = False
                  | otherwise = x == a ^ b || aux x a (b+1)

-- 3ª definición (por recursión):
esPotencia3 :: Integer -> Integer -> Bool
esPotencia3 0 _ = False
esPotencia3 1 a = True
esPotencia3 _ 1 = False
esPotencia3 x a = rem x a == 0 && esPotencia3 (div x a) a

-- La propiedad de equivalencia es
prop_equiv_esPotencia :: Integer -> Integer -> Property
prop_equiv_esPotencia x a =
    x > 0 && a > 0 ==> 
    esPotencia2 x a == b &&
    esPotencia3 x a == b
    where b = esPotencia x a

-- La comprobación es
--    ghci> quickCheck prop_equiv_esPotencia
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Definir la función 
--    sumaListas :: [Int] -> [Int] -> [Int]
-- tal que (sumaListas xs ys) es la suma de las elementos
-- correspondientes de las lista xs e ys. Por ejemplo,
--    sumaListas [2,3,4] [1,2,5]  ==  [3,5,9]
--    sumaListas [2,3,4] [1,2]    ==  [3,5,4]
--    sumaListas [2,3]   [1,2,5]  ==  [3,5,5]
-- ------------------------------------------------------------------------

-- 1ª definición 
sumaListas :: [Int] -> [Int] -> [Int]
sumaListas [] ys         = ys
sumaListas xs []         = xs
sumaListas (x:xs) (y:ys) = x+y : sumaListas xs ys

-- 2ª definición
sumaListas2 :: [Int] -> [Int] -> [Int]
sumaListas2 xs ys = [x+y | (x,y) <- zip (xs ++ replicate (k-m) 0)
                                        (ys ++ replicate (k-n) 0)]
  where m = length xs
        n = length ys
        k = max m n

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Comprobar con QuickCheck que el número de elementos de
-- (sumaListas xs ys) es el máximo de los números de elementos de xs e ys.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_sumaListas :: [Int] -> [Int] -> Bool
prop_sumaListas xs ys =
  length (sumaListas xs ys) == max (length xs) (length ys)

-- La comprobación es
--    λ> quickCheck prop_sumaListas
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 4. Un número primo equilibrado es un número primo que es la
-- media aritmética de su primo anterior y siguiente. Por ejemplo, 5 es
-- un primo equilibrado porque es la media de 3 y 7; pero 7 no lo es
-- porque no es la media de 5 y 11.
--
-- Definir la función 
--    primosEquilibrados :: Int -> [Int]
-- tal que (primosEquilibrados n) es la lista de los números primos
-- equilibrados menores o iguales que n. Por ejemplo, 
--    ghci> primosEquilibrados 1000
--    [5,53,157,173,211,257,263,373,563,593,607,653,733,947,977]
-- ---------------------------------------------------------------------

-- 1ª solución
-- ===========

primosEquilibrados :: Int -> [Int]
primosEquilibrados n =
  [x | x <- [5,7..n]
     , esPrimo x
     , esEquilibrado x]

-- (esPrimo n) se verifica si n es primo. Por ejemplo,
--    esPrimo 5  ==  True
--    esPrimo 6  ==  False
esPrimo :: Int -> Bool
esPrimo n = divisores n == [1,n]

-- (primos n) es la lista de los números primos menores o iguales que
-- n. Por ejemplo,
--    primos 50  ==  [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47]
primos :: Int -> [Int]
primos n = [x | x <- [1..n], esPrimo x]

-- (divisores n) es la lista de los divisores de n Por ejemplo,
--    divisores 36  ==  [1,2,3,4,6,9,12,18,36]
divisores :: Int -> [Int]
divisores n = [x | x <- [1..n], n `mod` x == 0]

-- (esEquilibrado x) se verifica si x es la media de su primo anterior y
-- su primo siguiente. Por ejemplo,
--    esEquilibrado 5  ==  True
--    esEquilibrado 7  ==  False
esEquilibrado :: Int -> Bool
esEquilibrado x = 2 * x == primoAnterior x + primoSiguiente x

-- (primoAnterior x) es el primo anterior a x. Por ejemplo,
--    primoAnterior 22  ==  19
primoAnterior :: Int -> Int
primoAnterior x = head [y | y <- [x-1,x-2..]
                          , esPrimo y]

-- (primoSiguiente x) es el primo siguiente a x. Por ejemplo,
--    primoSiguiente 22  ==  23
--    primoSiguiente 23  ==  29
primoSiguiente :: Int -> Int
primoSiguiente x = head [y | y <- [x+1..]
                           , esPrimo y]

-- 2ª solución
-- ===========

primosEquilibrados2 :: Int -> [Int]
primosEquilibrados2 n = aux (primos n)
  where aux (x1:x2:x3:xs)
          | 2*x2 == x1+x3 = x2 : aux (x2:x3:xs)
          | otherwise     = aux (x2:x3:xs)
        aux _             = []
        
