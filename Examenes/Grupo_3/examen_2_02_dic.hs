-- Informática (1º del Grado en Matemáticas, Grupo 3)
-- 2º examen de evaluación continua (2 de diciembre de 2019) 
-- ---------------------------------------------------------------------

import Data.List

-- ---------------------------------------------------------------------
-- Ejercicio 1. Una cadena de Gozinta es una secuencia de números
-- positivos no nulos en la que cada uno es un divisor del siguiente.
-- Por ejemplo, 
-- · [1,2,4,12,24] es una cadena de Gozinta pues 1 es un divisor de 2, 2
--   es un divisor de 4, 4 es un divisor de 12 y 12 es un divisor de 24.
-- · [1,2,2,4] es una cadena de Gozinta pues 1 es un divisor de 2, 2 es
--   un divisor de 2 y 2 es un divisor de 4.
-- · [1,2,3,4] no es una cadena de Gozinta pues 2 no es un divisor de 3.
--
-- Definir la función
--    cadenaGozinta :: [Integer] -> Bool
-- tal que (cadenaGozinta xs) es cierto si y solo si la lista xs es una
-- cadena de Gozinta. Por ejemplo,
--   cadenaGozinta [1,2,4,12,24]  ==  True
--   cadenaGozinta [1,2,2,4]      ==  True
--   cadenaGozinta [1,2,3,4]      ==  False
-- ---------------------------------------------------------------------

-- 1ª solución
cadenaGozinta :: [Integer] -> Bool
cadenaGozinta xs =
  all (\(x,y) -> y `mod `x == 0) (zip xs (tail xs))

-- 2ª solución
cadenaGozinta2 :: [Integer] -> Bool
cadenaGozinta2 xs =
  all (== 0) (zipWith mod (tail xs) xs)

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    seccionPar :: Integer -> Integer
-- tal que (seccionPar n) es el número que se obtiene con las cifras del
-- número n que se encuentran en una posición par. Por ejemplo,
--    seccionPar 13579  ==  159
--    seccionPar  8765  ==  75
--    seccionPar   249  ==  29
--    seccionPar     9  ==  9
-- ----------------------------------------------------------------------------

seccionPar :: Integer -> Integer
seccionPar n
  | n < 10    = n
  | otherwise = n `rem` 10 + 10 * seccionPar (n `div` 100) 

-- ---------------------------------------------------------------------
-- Ejercicio 3. Todo polinomio en una variable se puede representar
-- con la lista de sus coeficientes en orden creciente. Por ejemplo, el
-- polinomio 7x^4+x^2+(-2)x+4 se puede representar con la lista
-- [4,-2,1,0,7], donde el coeficiente del término que falta (x^3) es un
-- cero.
--
-- El valor de un polinomio en un número es el resultado que se obtiene
-- al evaluar el polinomio para dicho número. Por ejemplo, el valor de
-- 7x^4+x^2+(-2)x+4 en el número 2 es el resultado de 7*2^4+2^2+(-2)*2+4.
--
-- Definir la función
--    valorPolinomio :: [Integer] -> Integer -> Integer
-- tal que (valorPolinomio xs y) es el valor del polinomio cuyos
-- coeficientes en orden creciente son los de la lista xs en el número
-- y. Por ejemplo,
--    valorPolinomio [4,-2,1,0,7] 2  ==  116
-- ---------------------------------------------------------------------

-- 1ª solución (por recursión)
valorPolinomio :: [Integer] -> Integer -> Integer
valorPolinomio [] _     = 0
valorPolinomio (x:xs) y =  x + y * valorPolinomio xs y

-- 2ª solución (por plegado)
valorPolinomio2 :: [Integer] -> Integer -> Integer
valorPolinomio2 xs y = foldr (\x r -> x+y*r) 0 xs

-- 3ª solucion (por comprensión)
valorPolinomio3 :: [Integer] -> Integer -> Integer
valorPolinomio3 xs y = sum [x*y^n | (x,n) <- zip xs [0..]]

-- ---------------------------------------------------------------------
-- Ejercicio 4. Dada una lista de números positivos no nulos
-- [A1,...,An], decimos que el número Ai tiene la propiedad de Gozinta
-- en dicha lista, si Ai es un divisor de todos los que le siguen en la
-- lista. Por ejemplo, en la lista [7,3,5,15,25], el número 5 tiene la
-- propiedad de  Gozinta pues es un divisor de todos los que le siguen
-- (15 y 25). Sin embargo, en esa misma lista el número 3 no tiene la
-- propiedad de Gozinta, pues no divide a 5 ni a 25.
--
-- Definir la función
--    numerosGozinta :: [Integer] -> [Integer]
-- tal que (numerosGozinta xs) es la lista de los números de la lista xs
-- que tienen la propiedad de Gozinta en dicha lista. Por ejemplo,
--    numerosGozinta [4,2,1]        ==  [1]
--    numerosGozinta [1,3,5]        ==  [1,5]
--    numerosGozinta [7,3,5,15,25]  ==  [5,25]
--    numerosGozinta [3,6,12,6,36]  ==  [3,6,6,36]
--    numerosGozinta [1,2,4,8,24]   ==  [1,2,4,8,24]
-- ---------------------------------------------------------------------

-- 1ª solución
-- ===========

numerosGozinta :: [Integer] -> [Integer]
numerosGozinta xs =
  [y | (y:ys) <- init (tails xs)
     , all (y `divideA`) ys]
  
divideA :: Integer -> Integer -> Bool
divideA x y = y `mod` x == 0

-- 2ª solución
-- ===========

numerosGozinta2 :: [Integer] -> [Integer]
numerosGozinta2 [] = []
numerosGozinta2 (x:xs)
  | all (x `divideA`) xs = x : numerosGozinta2 xs
  | otherwise            = numerosGozinta2 xs


