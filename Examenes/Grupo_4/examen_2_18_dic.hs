-- Informática (1º del Grado en Matemáticas, Grupo 4)
-- 2º examen de evaluación continua (18 de diciembre de 2019)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Librerías auxiliares
-- ---------------------------------------------------------------------

import Data.Char
import Data.List
import Data.Numbers.Primes
import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Definir la función
--    ultimoNoNuloFactorial :: Integer -> Integer
-- tal que (ultimoNoNuloFactorial n) es el último dígito no nulo del
-- factorial de n. Por ejemplo,
--    ultimoNoNuloFactorial  7  == 4
--    ultimoNoNuloFactorial 10  == 8
--    ultimoNoNuloFactorial 12  == 6
--    ultimoNoNuloFactorial 97  == 2
--    ultimoNoNuloFactorial  0  == 1
-- ---------------------------------------------------------------------

-- 1ª solución
-- ===========

ultimoNoNuloFactorial :: Integer -> Integer
ultimoNoNuloFactorial = ultimoNoNulo . factorial

-- (factorial n) es el factorial de n. Por ejemplo,
--    factorial 7  ==  5040
factorial :: Integer -> Integer
factorial n = product [1..n]

-- (ultimoNoNulo n) es el último dígito no nulo de n. Por ejemplo,
--    ultimoNoNulo 5040  ==  4
ultimoNoNulo :: Integer -> Integer
ultimoNoNulo n | r /= 0    = r
               | otherwise = ultimoNoNulo q
  where (q,r) = n `quotRem` 10

-- 2ª solución
-- ===========

ultimoNoNuloFactorial2 :: Integer -> Integer
ultimoNoNuloFactorial2 = last . filter (/= 0) . digitos . factorial

digitos :: Integer -> [Integer]
digitos n = [read [x] | x <- show n]

-- 3ª solución
-- ===========

ultimoNoNuloFactorial3 :: Integer -> Integer
ultimoNoNuloFactorial3 = last . filter (/= 0) . digitos3 . factorial3

digitos3 :: Integer -> [Integer]
digitos3 = map (fromIntegral . digitToInt) . show

factorial3 :: Integer -> Integer
factorial3 = product . enumFromTo 1

-- 4ª solución
-- ===========

ultimoNoNulo4 :: Integer -> Integer
ultimoNoNulo4 n = read [head (dropWhile (=='0') (reverse (show n)))]

-- 5ª solución
-- ===========

ultimoNoNulo5 :: Integer -> Integer
ultimoNoNulo5 =
  read . return . head . dropWhile ('0' ==) . reverse . show

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Comprobar con QuickCheck que si n es mayor que 4,
-- entonces el último dígito no nulo del factorial de n es par.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_ultimoNoNuloFactorial :: Integer -> Property
prop_ultimoNoNuloFactorial n = 
  n > 4 ==> even (ultimoNoNuloFactorial n)
                  
-- La comprobación es
--    ghci> quickCheck prop_ultimoNoNuloFactorial
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 2. Una forma de aproximar el número π es usando la
-- siguiente igualdad:  
--     π         1     1*2     1*2*3     1*2*3*4     
--    --- = 1 + --- + ----- + ------- + --------- + ....
--     2         3     3*5     3*5*7     3*5*7*9
-- Es decir, la serie cuyo término general n-ésimo es el cociente entre el
-- producto de los primeros n números y los primeros n números impares:
--                Π i   
--    s(n) =  -----------
--             Π (2*i+1)
--
-- Definir la función
--    aproximaPi :: Double -> Double
-- tal que (aproximaPi n) es la aproximación del número π calculada con la
-- serie anterior hasta el término n-ésimo. Por ejemplo,
--    aproximaPi 10   ==  3.141106021601377
--    aproximaPi
30   ==  3.1415926533011587
--    aproximaPi 50   ==  3.1415926535897922
-- ---------------------------------------------------------------------

-- 1ª solución (por comprensión):
aproximaPi :: Double -> Double
aproximaPi n = 
  2 * sum [product [1..i] / product [1,3..2*i+1] | i <- [0..n]]

-- 2ª solución (por recursión):
aproximaPi2 :: Double -> Double
aproximaPi2 0 = 2
aproximaPi2 n = 
  aproximaPi2 (n-1) + 2 * product [1..n] / product [3,5..2*n+1]

-- ---------------------------------------------------------------------
-- Ejercicio 3. La descomposición prima de 600 es
--    600 = 2³ * 3 * 5²
-- 
-- Definir la función
--    factorizacion :: Integer -> [(Integer,Integer)]
-- tal que (factorizacion x) ses la lista de las bases y exponentes de
-- la descomposición prima de x. Por ejemplo,
--    factorizacion 600  == [(2,3),(3,1),(5,2)]
--    factorizacion 5500 == [(2,2),(5,3),(11,1)]
-- ---------------------------------------------------------------------

-- 1ª solución
-- ===========

factorizacion :: Integer -> [(Integer,Integer)]
factorizacion n =
  [(x,nOcurrencias x xs) | x <- elementos xs]
  where xs = factoresPrimos n

-- (factores primos n) es la lista de los factores primos de n. Por
-- ejemplo, 
--   factoresPrimos 600  ==  [2,2,2,3,5,5]
factoresPrimos :: Integer -> [Integer]
factoresPrimos 1 = []
factoresPrimos n = x : factoresPrimos (n `div` x)
  where x = menorFactor n

-- (menorFactor n) es el menor factor primo de n. Por ejemplo,
--   menorFactor 10  ==  2
--   menorFactor 11  ==  11
menorFactor :: Integer -> Integer
menorFactor n = head [x | x <- [2..n], n `mod` x == 0]

-- (elementos xs) es la lista de los elementos, sin repeticiones, de
-- xs. Por ejemplo,
--   elementos [3,2,3,5,2]  ==  [3,2,5]
elementos :: Eq a => [a] -> [a]
elementos [] = []
elementos (x:xs) = x : elementos (filter (/=x) xs)

-- (nOcurrencias x ys) es el número de ocurrencias de x en ys. Por
-- ejemplo, 
--   nOcurrencias 'a' "Salamanca"  ==  4
nOcurrencias :: Eq a => a -> [a] -> Integer
nOcurrencias _ [] = 0
nOcurrencias x (y:ys) | x == y    = 1 + nOcurrencias x ys
                      | otherwise = nOcurrencias x ys

-- 2ª solución
-- ===========

factorizacion2 :: Integer -> [(Integer,Integer)]
factorizacion2 n =
  [(head xs,genericLength xs) | xs <- group (primeFactors n)]

-- 3ª solución
-- ===========

factorizacion3 :: Integer -> [(Integer,Integer)]
factorizacion3 = map primeroYlongitud
               . group
               . primeFactors

-- (primeroYlongitud xs) es el par formado por el primer elemento de xs
-- y la longitud de xs. Por ejemplo,
--    primeroYlongitud [3,2,5,7] == (3,4)
primeroYlongitud :: [a] -> (a,Integer)
primeroYlongitud (x:xs) =
  (x, 1 + genericLength xs)

-- Comparación de eficiencia
-- =========================

--   λ> length (factorizacion (product [1..10^4]))
--   1229
--   (4.84 secs, 2,583,331,768 bytes)
--   λ> length (factorizacion2 (product [1..10^4]))
--   1229
--   (0.24 secs, 452,543,360 bytes)
--   λ> length (factorizacion3 (product [1..10^4]))
--   1229
--   (0.23 secs, 452,433,504 bytes)
--   
--   λ> length (factorizacion (product (take (2*10^3) primes)))
--   2000
--   (6.58 secs, 3,415,098,552 bytes)
--   λ> length (factorizacion2 (product (take (2*10^3) primes)))
--   2000
--   (0.02 secs, 23,060,512 bytes)
--   λ> length (factorizacion3 (product (take (2*10^3) primes)))
--   2000
--   (0.02 secs, 22,882,080 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 4. Las expresiones aritméticas se pueden representar como
-- árboles con números en las hojas y operaciones en los nodos. Por
-- ejemplo, la expresión "9-2*4" se puede representar por el árbol
--      - 
--     / \
--    9   *
--       / \
--      2   4
-- 
-- Definiendo el tipo de dato Arbol por 
--    data Arbol = H Int | N (Int -> Int -> Int) Arbol Arbol
-- la representación del árbol anterior es
--    N (-) (H 9) (N (*) (H 2) (H 4))
--
-- Definir la función
--    valor :: Arbol -> Int
-- tal que (valor a) es el valor de la expresión aritmética
-- correspondiente al árbol a. Por ejemplo,
--    valor (N (-) (H 9) (N (*) (H 2) (H 4)))    ==  1
--    valor (N (+) (H 9) (N (*) (H 2) (H 4)))    ==  17
--    valor (N (+) (H 9) (N (div) (H 4) (H 2)))  ==  11
--    valor (N (+) (H 9) (N (max) (H 4) (H 2)))  ==  13
-- ---------------------------------------------------------------------

data Arbol = H Int | N (Int -> Int -> Int) Arbol Arbol

valor :: Arbol -> Int
valor (H x)     = x
valor (N f i d) = f (valor i) (valor d)

