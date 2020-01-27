-- Informática (1º del Grado en Matemáticas, Grupos 1 y 4)
-- 3º examen de evaluación continua (23 de enero de 2020)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.List
import Data.Numbers.Primes
import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Un número de Munchausen es un número entero positivo
-- que es igual a la suma de sus dígitos elevados a sí mismo. Por
-- ejemplo, 3435 es un número de Munchausen ya qu e 
--    3³ + 4⁴ + 3³ + 5⁵ = 27 + 256 + 27 + 3125 = 3435
--
-- Definir la función
--    esMunchausen :: Integer -> Bool
-- tal que (esMunchausen n) se verifica si n es un número de
-- Munchausen. Por ejemplo,
--    esMunchausen 3435  ==  True
--    esMunchausen 2020  ==  False
-- ---------------------------------------------------------------------

esMunchausen :: Integer -> Bool
esMunchausen n =
  n == sum [x^x | x <- digitos n]

-- (digitos n) es la lista de los dígitos de n. Por ejemplo,
--    digitos 3435  ==  [3,4,3,5]
digitos :: Integer -> [Integer]
digitos n = [read [c] | c <- show n]

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Comprobar con QuickCheck que que los únicos números de
-- Munchausen son 1 y 3435.
--
-- Nota: No usar la propiedad en la definición de esMunchausen.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_Munchausen :: Integer -> Property
prop_Munchausen n =
  n > 0
  ==>
  esMunchausen n == elem n [1, 3435]

-- La comprobación es
--    λ> quickCheck prop_Munchausen
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 2. La lista ps = [p(1),...,p(k)] es una sucesión de Grim
-- para la lista xs = [x(1),...,x(k)] si los p(i) son números primos
-- distintos y p(i) divide a x(i), para 1 ≤ i ≤ k. Por ejemplo, 2, 5,
-- 13, 3, 7 es una sucesión de Grim de 24, 25, 26, 27, 28.
--
-- Definir la función
--    sucesionesDeGrim :: [Integer] -> [[Integer]]
-- tal que (sucesionesDeGrim xs) es la lista de las sucesiones de Grim
-- de xs. Por ejemplo, 
--    sucesionesDeGrim [15,16]          == [[3,2],[5,2]]
--    sucesionesDeGrim [8,9,10]         == [[2,3,5]]
--    sucesionesDeGrim [9,10]           == [[3,2],[3,5]]
--    sucesionesDeGrim [24,25,26,27,28] == [[2,5,13,3,7]]
--    sucesionesDeGrim [25,26,27,28]    == [[5,2,3,7],[5,13,3,2],[5,13,3,7]]
-- ---------------------------------------------------------------------

sucesionesDeGrim :: [Integer] -> [[Integer]]
sucesionesDeGrim [] = [[]]
sucesionesDeGrim (x:xs) =
  [y:ys | y <- divisoresPrimos x
        , ys <- sucesionesDeGrim xs
        , y `notElem` ys]

-- (divisoresPrimos n) es la lista de los divisores primos de n. Por
-- ejemplo, 
--    divisoresPrimos 60  ==  [2,3,5]
divisoresPrimos :: Integer -> [Integer]
divisoresPrimos = nub . primeFactors

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Las primeras sumas alternas de los factoriales son
-- números primos; en efecto, 
--    3! - 2! + 1! = 5
--    4! - 3! + 2! - 1! = 19
--    5! - 4! + 3! - 2! + 1! = 101
--    6! - 5! + 4! - 3! + 2! - 1! = 619
--    7! - 6! + 5! - 4! + 3! - 2! + 1! = 4421
--    8! - 7! + 6! - 5! + 4! - 3! + 2! - 1! = 35899
-- son primos, pero
--    9! - 8! + 7! - 6! + 5! - 4! + 3! - 2! + 1! = 326981
-- no es primo.
--
-- Definir la función
--    sumaAlterna         :: Integer -> Integer
-- tal que (sumaAlterna n) es la suma alterna de los factoriales desde n hasta
-- 1. Por ejemplo,
--    sumaAlterna 3  ==  5
--    sumaAlterna 4  ==  19
--    sumaAlterna 5  ==  101
--    sumaAlterna 6  ==  619
--    sumaAlterna 7  ==  4421
--    sumaAlterna 8  ==  35899
--    sumaAlterna 9  ==  326981
-- ---------------------------------------------------------------------

-- 1ª solución
-- ===========

sumaAlterna1 :: Integer -> Integer
sumaAlterna1 1 = 1
sumaAlterna1 n = factorial n - sumaAlterna1 (n-1)

factorial :: Integer -> Integer
factorial n = product [1..n]

-- 2ª solución
-- ===========

sumaAlterna2 :: Integer -> Integer
sumaAlterna2 n = sum (zipWith (*) signos (tail factoriales))
  where
    signos | odd n     = 1 : concat (replicate (m `div` 2) [-1,1])
           | otherwise = concat (replicate (m `div` 2) [-1,1])
    m = fromIntegral n

-- factoriales es la lista de los factoriales. Por ejemplo,
--    take 7 factoriales  ==  [1,1,2,6,24,120,720]
factoriales :: [Integer]
factoriales = 1 : scanl1 (*) [1..]

-- 3ª definición
-- =============

sumaAlterna3 :: Integer -> Integer
sumaAlterna3 n = 
  sum (genericTake n (zipWith (*) signos (tail factoriales)))
  where signos | odd n     = cycle [1,-1]
               | otherwise = cycle [-1,1]

-- Comparación de eficiencia
-- =========================

--    λ> sumaAlterna1 3000 `mod` (10^6)
--    577019
--    (5.33 secs, 7,025,937,760 bytes)
--    λ> sumaAlterna2 3000 `mod` (10^6)
--    577019
--    (0.03 secs, 15,738,480 bytes)
--    λ> sumaAlterna3 3000 `mod` (10^6)
--    577019
--    (0.05 secs, 16,520,896 bytes)

-- En lo que sigue se usa la 2ª definición
sumaAlterna :: Integer -> Integer
sumaAlterna = sumaAlterna2

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Definir la función
--    conSumaAlternaPrima :: [Integer]
-- tal que conSumaAlternaPrima es la sucesión de los números cuya suma
-- alterna de factoriales es prima. Por ejemplo, 
--    λ> take 8 conSumaAlternaPrima
--    [3,4,5,6,7,8,10,15]
-- ---------------------------------------------------------------------

conSumaAlternaPrima :: [Integer]
conSumaAlternaPrima =
  [n | n <- [0..], isPrime (sumaAlterna n)]

-- ---------------------------------------------------------------------
-- Ejercicio 4. Los árboles se pueden representar mediante el siguiente
-- tipo de datos 
--    data Arbol a = N a [Arbol a]
--                   deriving Show
-- Por ejemplo, los árboles
--      1               3
--     / \             /|\ 
--    6   3           / | \
--        |          5  4  7
--        5          |     /\ 
--                   6    2  1
-- se representan por
--    ej1, ej2 :: Arbol Int
--    ej1 = N 1 [N 6 [],N 3 [N 5 []]]
--    ej2 = N 3 [N 5 [N 6 []], N 4 [], N 7 [N 2 [], N 1 []]]
-- 
-- Definir la función
--    emparejaArboles :: (a -> b -> c) -> Arbol a -> Arbol b -> Arbol c
-- tal que (emparejaArboles f a1 a2) es el árbol obtenido aplicando la
-- función f a los elementos de los árboles a1 y a2 que se encuentran en
-- la misma posición. Por ejemplo,
--    ghci> emparejaArboles (+) (N 1 [N 2 [], N 3[]]) (N 1 [N 6 []])
--    N 2 [N 8 []]
--    ghci> emparejaArboles (+) ej1 ej2
--    N 4 [N 11 [],N 7 []]
--    ghci> emparejaArboles2 (*) ej1 ej2
--    N 3 [N 30 [],N 12 []]
-- ---------------------------------------------------------------------

data Arbol a = N a [Arbol a]
  deriving (Show, Eq)

ej1, ej2 :: Arbol Int
ej1 = N 1 [N 6 [],N 3 [N 5 []]]
ej2 = N 3 [N 5 [N 6 []], N 4 [], N 7 [N 2 [], N 1 []]]

-- 1ª solución
emparejaArboles :: (a -> b -> c) -> Arbol a -> Arbol b -> Arbol c
emparejaArboles f (N x l1) (N y l2) = 
  N (f x y) (zipWith (emparejaArboles f) l1 l2)

-- 2ª solución
emparejaArboles2 :: (a -> b -> c) -> Arbol a -> Arbol b -> Arbol c
emparejaArboles2 f (N x l1) (N y l2) = 
  N (f x y) [emparejaArboles f xs ys | (xs,ys) <- zip l1 l2]

