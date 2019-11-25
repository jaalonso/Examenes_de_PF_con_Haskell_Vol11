-- Informática (1º del Grado en Matemáticas, Grupo 2)
-- 1º examen de evaluación continua (6 de noviembre de 2019)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías                                                        --
-- ---------------------------------------------------------------------

import Test.QuickCheck
import Data.List

-- ----------------------------------------------------------------------------
-- Ejercicio 1.1. La suma de la siguiente serie es 1:
--     1/2 + 1/6 + 1/12 + ... + 1/(n*(n+1)) + ...

-- Definir la función 
--    sumaS :: Double -> Double
-- tal que (sumaS n) es la suma de los n primeros términos de dicha
-- serie. Por ejemplo, 
--    sumaSG 10   == 0.9090909090909091
--    sumaSG 100  == 0.9900990099009898
--    sumaSG 1000 == 0.9990009990009997
-- ----------------------------------------------------------------------------

sumaS :: Double -> Double
sumaS n = sum [1/(k*(k+1)) | k <- [1..n]]

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Definir la función 
--    errorS :: Double -> Double
-- tal que (errorS x) es el menor número de términos de la serie
-- necesarios para obtener su límite con un error menor que x.
-- Por ejemplo, 
--    errorS 0.01   == 100.0
--    errorS 0.001  == 999.0
-- ---------------------------------------------------------------------

errorS :: Double -> Double
errorS x = head [m | m <- [1..] , abs ((sumaS m) - 1) < x]

-- ---------------------------------------------------------------------
-- Ejercicio 2. ¿Cuántos números de tres dígitos tienen al menos un
-- dígito par? 
-- ---------------------------------------------------------------------

solucion :: Int
solucion = length [n | n <- [100..999], tieneAlgunDP n]

-- (tieneAlgunDP n) se verifica si n tiene agún dígito par. Por ejemplo,
--    tieneAlgunDP 254  ==  True
--    tieneAlgunDP 35  ==  False
tieneAlgunDP :: Int -> Bool
tieneAlgunDP n = or [even x | x <- digitos n]

-- (digitos n) es la lista de los dígitos de n. Por ejemplo,
--    digitos 325  ==  [3,2,5]
digitos :: Int -> [Int]
digitos n = [read [x] | x <- show n]

-- El cálculo es
--    λ> solucion
--    775

-- 2ª solución
-- ===========

solucion2 :: Int
solucion2 = length [n | n <- [100..999], tieneAlgunDP2 n]

-- (tieneAlgunDP2 n) se verifica si n tiene agún dígito par. Por ejemplo,
--    tieneAlgunDP2 254  ==  True
--    tieneAlgunDP2 35  ==  False
tieneAlgunDP2 :: Int -> Bool
tieneAlgunDP2 n = not (null (intersect (show n) "02468"))

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Decimos que un número n es curioso si la suma de sus
-- dígitos termina igual que el producto de los mismos.
-- 
-- Definir la función
--    esCurioso :: Int -> Bool
-- tal que (esCurioso n) se verifica si n es un número curioso. Por
-- ejemplo,
--    esCurioso 109 = True
--    esCurioso 121 = False
-- ---------------------------------------------------------------------

-- 1ª solución
-- ===========

esCurioso :: Int -> Bool
esCurioso n = x `rem` 10 == y `rem` 10
  where x = sumaDigitos n
        y = prodDigitos n

-- (sumaDigitos n) es la suma de los dígitos de n. Por ejemplo,
--    sumaDigitos 325  ==  10
sumaDigitos :: Int -> Int
sumaDigitos n | n < 10    = n
              | otherwise = (n `rem` 10) + sumaDigitos (n `div` 10)

-- (prodDigitos n) es el producto de los dígitos de n. Por ejemplo,
--    prodDigitos 325  ==  30
prodDigitos :: Int -> Int
prodDigitos n | n < 10    = n
              | otherwise = (n `rem` 10) * prodDigitos (n `div` 10)

-- 2ª solución
-- ===========

esCurioso2 :: Int -> Bool
esCurioso2 n = x `rem` 10 == y `rem` 10
  where x = sum (digitos n)
        y = product (digitos n)

-- Equivalencia
prop_esCuriosoEquiv :: Int -> Property
prop_esCuriosoEquiv n =
  n >= 0 ==> esCurioso n == esCurioso2 n

-- La comprobación es
--    λ> quickCheck prop_esCuriosoEquiv
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Definir la función
--    curiososHasta :: Int -> [Int]
-- tal que (curiososHasta n) es la lista de los números curiosos menores
-- o iguales que n. Por ejemplo,
--    curiososHasta 125 == [1,2,3,4,5,6,7,8,9,22,48,84,109,123]
-- ---------------------------------------------------------------------

curiososHasta :: Int -> [Int]
curiososHasta n = [x | x <- [1..n], esCurioso x]

-- ---------------------------------------------------------------------
-- Ejercicio 3.3. Comprobar con QuickCheck la siguiente propiedad: un
-- número n curioso si, y sólo si, lo es el número obtenido al invertir
-- sus dígitos.
-- ---------------------------------------------------------------------

-- La propiedad es
propCurioso :: Int -> Property
propCurioso n =
  n >= 0 ==> esCurioso n ==> esCurioso (read (reverse (show n)))

-- La comprobación es
--    λ> quickCheck propCurioso
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. Definir la función
--    mayorPre :: Eq a => [a] -> [a] -> [a]
-- tal que (mayorPre xs ys) calcula el mayor prefijo común a xs e ys. Por
-- ejemplo,
--    mayorPre "masa" "madre"       == "ma"
--    mayorPre "masa" "padre"       == ""
--    mayorPre "hola" "hielo"       == "h"
--    mayorPre "helado" "heladeria" == "helad"
-- ---------------------------------------------------------------------

mayorPre :: Eq a => [a] -> [a] -> [a]
mayorPre _  [] = []
mayorPre [] _  = []
mayorPre (x:xs) (y:ys) | x == y    = x : mayorPre xs ys
                       | otherwise = []

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Definir la función
--    mayorSuf :: Eq a => [a] -> [a] -> [a]
-- tal que (mayorSuf xs ys) calcula el mayor sufijo común a xs e ys. Por
-- ejemplo,
--    mayorSuf "masa" "madre"          == ""
--    mayorSuf "masa" "madera"         == "a"
--    mayorSuf "masa" "mesa"           == "sa"
--    mayorSuf "panaderia" "heladeria" == "aderia"
-- ---------------------------------------------------------------------

mayorSuf :: Eq a => [a] -> [a] -> [a]
mayorSuf xs ys = reverse (mayorPre (reverse xs) (reverse ys))

