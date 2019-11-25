-- Informática (1º del Grado en Matemáticas, Grupo 1)
-- 1º examen de evaluación continua (11 de noviembre de 2019)
-- ---------------------------------------------------------------------

import Data.List

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función de3en3 tal que (de3en3 x) es la lista
-- de dígitos de x agrupados de 3 en 3 consecutivamente. Por ejemplo,
--    de3en3 12      == []
--    de3en3 123     == [[1,2,3]]
--    de3en3 1234    == [[1,2,3],[2,3,4]]
--    de3en3 12345   == [[1,2,3],[2,3,4],[3,4,5]]
--    de3en3 123450  == [[1,2,3],[2,3,4],[3,4,5],[4,5,0]]
--    de3en3 1234500 == [[1,2,3],[2,3,4],[3,4,5],[4,5,0],[5,0,0]]
-- ---------------------------------------------------------------------

-- 1ª solución
-- ===========

de3en3 :: Integer -> [[Integer]]
de3en3 x = aux (digitos x)
  where aux (a:b:c:xs) = [a,b,c] : aux (b:c:xs)
        aux _          = []

-- (digitos x) es la lista de los dígitos de x. Por ejemplo,
--    digitos 325  ==  [3,2,5]
digitos :: Integer -> [Integer]
digitos x = [read [y] | y <- show x]

-- 2ª solución
-- ===========

de3en3_2 x = [[a,b,c] | (a,(b,c)) <- zip xs (zip (tail xs) (tail (tail xs)))]
  where xs = digitos x

-- 3ª solución
-- ===========

de3en3_3 x =[[a,b,c] | (a,b,c) <- zip3 xs (tail xs) (drop 2 xs)]
  where xs = digitos x

-- ---------------------------------------------------------------------
-- Ejercicio 2. Un número es endiablado si su doble contiene al 666. Por
-- ejemplo, el número 1333 es endiablado ya que su doble 2666 contiene
-- al 666. 
-- 
-- Definir la función esEndiablado tal que (esEndiablado x) se verifica
-- si x es endiablado. Por ejemplo,
--    esEndiablado 1333     ==  True
--    esEndiablado 23331    == True
--    esEndiablado 28331114 == True
--    esEndiablado 3339     == False
-- ---------------------------------------------------------------------

-- 1ª solución
esEndiablado :: Integer -> Bool
esEndiablado x = [6,6,6] `elem` de3en3 (2*x)

-- 2ª solución
esEndiablado2 :: Integer -> Bool
esEndiablado2 x = "666" `isInfixOf` show (2*x)

-- ---------------------------------------------------------------------
-- Ejercicio 3. Calcular la lista de los 3 primeros números endiablados
-- mayores que 456.
-- ---------------------------------------------------------------------

-- El cálculo es
--    λ> take 3 [x | x <- [457 ..], esEndiablado x] 
--    [833,1333,1833]

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--    entreMedias :: [Float] -> [Float]
-- tal que (entreMedias xs) es la lista obtenida intercalando entre cada
-- dos elementos consecutivos de xs su media aritmética. Por ejemplo,
--     entreMedias [2,7,1,8]  ==  [2.0,4.5,7.0,4.0,1.0,4.5,8.0]
--     entreMedias [2]        ==  [2.0]
--     entreMedias []         ==  []
-- ---------------------------------------------------------------------

entreMedias :: [Float] -> [Float]
entreMedias []       = []
entreMedias [x]      = [x]
entreMedias (x:y:xs) = x: media x y : entreMedias (y:xs)
  where media x y = (x+y)/2

-- ---------------------------------------------------------------------
-- Ejercicio 5.1. La serie de potencias 1 + x + x^2/2! + x^3/3! +...
-- define la función exponencial e^x que es exp x en Haskell.
-- Para  calcular e^3 bastará calcular la serie para x=3
--
-- Definir la función serieExp tal que (serieExp x n) es el valor
-- aproximado de e^x que resulta de sumar n términos de la serie
-- comenzando en 0. Por ejemplo, 
--    serieExp 3 5 = 1 + 3 + 3^2/2! + 3^3/3! + 3^4/4!
--                 = 16.375
-- ---------------------------------------------------------------------

serieExp :: Integer -> Integer -> Double
serieExp x m =
  sum [fromIntegral(x^n)/fromIntegral(fact n) | n <- [0 ..m-1]]

-- (fact n) es el factorial de n. Por ejemplo,
--    fact 3  ==  6
fact :: Integer -> Integer
fact n = product [1 .. n]

-- ---------------------------------------------------------------------
-- Ejercicio 5.2.  Definir la función errorExp tal que (errorExp n) es
-- el errorExp que se comente en el cálculo de e^3 si tenemos en cuenta
-- sólo n términos de la serie. Por ejemplo,
--    errorExp 27 == 7.105427357601002e-15
--    errorExp 10 == 2.2144066044813115e-2
-- ---------------------------------------------------------------------

errorExp :: Integer -> Double
errorExp n = abs (exp 3 - serieExp 3 n)

-- ---------------------------------------------------------------------
-- Ejercicio 5.3. Definir la funcion minimoA tal que (minimoA x) es el
-- menor número de términos tal que el errorExp en el calculo de e^3 con
-- n términos no supera a x. Por ejemplo, el errorExp que cometemos
-- calculando e^3 con la suma de 6 términos es 
--    errorExp 6 == 1.6855369231876693 
-- y el menor n tal que el errorExp no es mayor que 2 es 6
--    minimoA 2  == 6
-- ya que con 5 términos el errorExp es (errorExp 5 = 3.710536923187668)
-- ---------------------------------------------------------------------

minimoA :: Double -> Integer
minimoA x = head [n | n  <- [0 ..], errorExp n <= x]

