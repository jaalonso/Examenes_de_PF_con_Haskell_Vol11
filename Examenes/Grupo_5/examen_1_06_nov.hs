-- Informática (1º del Grado en Matemáticas), Grupo 5
-- 1º examen de evaluación continua (6 de noviembre de 2019)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    numerosLongitudDivisores :: Int -> Int -> [Int]
-- tal que (numerosLongitudDivisores n k) es la lista de los números de
-- n dígitos que  ienen k o más divisores. Por ejemplo:
--    numerosLongitudDivisores 3 25 == [720,840,900,960]
--    numerosLongitudDivisores 2 10 == [48,60,72,80,84,90,96]
-- ---------------------------------------------------------------------

numerosLongitudDivisores :: Int -> Int -> [Int]
numerosLongitudDivisores n k =
  [x |  x <- [10^(n-1)..10^n-1], length (divisores x) >= k]

-- (divisores x) es la lista de los divisores de x. Por ejemplo,
--    divisores 60  ==  [1,2,3,4,5,6,10,12,15,20,30,60]
divisores :: Int -> [Int]
divisores x = [z | z <- [1..x], mod x z == 0]

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Se consideran listas de pares con información sobre
-- las notas obtenidas por alumnos en un examen. Por ejemplo:
--    notas :: [(String,Float)]
--    notas = [("Juan",5.6),("Ana",9.5),("Pedro",4),("Luisa",9.5),
--             ("Mateo",9),("Isabel",3.5)]
-- 
-- Definir la función
--    candidatos :: [(String,Float)] -> [String]
-- tal que (candidatos xs) es la lista de los nombres de los alumnos con
-- la nota máxima en la lista de notas xs. Por ejemplo,
--    candidatos lista = ["Ana","Luisa"]
-- ---------------------------------------------------------------------

notas :: [(String,Float)]
notas = [("Juan",5.6),("Ana",9.5),("Pedro",4),("Luisa",9.5),
         ("Mateo",9),("Isabel",3.5)]

candidatos :: [(String,Float)] -> [String]
candidatos xs = [cs | (cs,n) <- xs, n == k]
  where k = maximum [z | (_,z) <- xs]

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Definir la función
--    media :: [(String,Float)] -> Float
-- tal que (media xs) es la nota media correspondiente a la lista de
-- notas xs. Por ejemplo, 
--    media notas == 6.85
-- ---------------------------------------------------------------------

media :: [(String,Float)] -> Float
media xs = sum [z | (_,z) <- xs] / fromIntegral (length xs)
              
-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Un número capicúa es aquel que se lee igual de 
-- izquierda a derecha que de derecha a izquierda. Una forma de 
-- conseguir capicúas a partir de un número entero x es sumarle su
-- reverso (x con las cifras invertidas) y repetir el proceso hasta 
-- conseguir un número capicúa. Por ejemplo:
-- + para el 15 tenemos:
--       15 + 51 = 66
-- + para el 87 tenemos:
--         87 +   78 = 165,
--        165 +  561 = 726,
--        726 +  627 = 1353,
--       1353 + 3531 = 4884                              
--                 
-- Definir la función
--    unPaso :: Integer -> Integer
-- talque (unPaso x) es el entero obtenido al sumar x y su reverso. Por
-- ejemplo, 
--    unPaso 87  == 165  
--    unPaso 121 == 242
-- ---------------------------------------------------------------------

unPaso :: Integer -> Integer
unPaso x = x + reverso x

-- (reverso x) es el número obtenido al invertir los dígitos de x. Por
-- ejemplo, 
--    reverso 325  ==  523
reverso :: Integer -> Integer
reverso x = read (reverse (show x))

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Definir la función
--    hastaCapicua:: Integer -> Integer
-- tal que (hastaCapicua x) es el número de veces que debemos repetir
-- el proceso anterior hasta conseguir un número capicúa. Por ejemplo, 
--    hastaCapicua 15   == 1
--    hastaCapicua 87   == 4
--    hastaCapicua 1221 == 0
-- ---------------------------------------------------------------------

hastaCapicua :: Integer -> Integer  
hastaCapicua x
  | esCapicua x = 0
  | otherwise   = 1 + hastaCapicua (unPaso x)

-- (esCapicua x) se verifica i x es un número capicúa. Por ejemplo,
--    esCapicua 32523  ==  True
--    esCapicua 32532  ==  False
esCapicua :: Integer -> Bool
esCapicua x = x == reverso x      

-- ---------------------------------------------------------------------
-- Ejercicio 4. Se dice que una lista de números zigzaguea si el primer
-- elemento de la lista es (estrictamente) menor que el segundo, el 
-- segundo es mayor que el tercero, el tercero es menor que el cuarto,
-- el cuarto es mayor que el quinto y así sucesivamente. (Si la lista 
-- tiene menos de dos elementos, entonces asumimos que zigzaguea).
-- 
-- Definir el predicado
--    zigzaguea :: (Num a,Ord a) -> [a] -> Bool
-- tal que (zigzaguea xs) se verifica si la lista xs zigzaguea. Por 
-- ejemplo,
--   zigzaguea [3]            == True
--   zigzaguea [4,3]          == False
--   zigzaguea [2,4,1,3,-1,5] == True
--   zigzaguea [1,2,2,-3,7]   == False
-- ------------------------------------------------------------------

-- 1ª solución (por recursión)
zigzaguea1 :: (Num a,Ord a) => [a] -> Bool
zigzaguea1 (x:y:z:xs) = x < y && y > z && zigzaguea1 (z:xs)
zigzaguea1 [x,y]      = x < y
zigzaguea1 _          = True

-- 2ª solución (por comprensión)
zigzaguea2 :: (Num a,Ord a) => [a] -> Bool
zigzaguea2 xs 
  | length xs < 2 = True
  | otherwise = and [x < y | (x,y) <- posImpares ys] 
                && and [x > y | (x,y) <- posPares ys]
     where ys = zip xs (tail xs)

-- (posImpares xs) es la lista de los elementos de xs en las posiciones
-- impares (empezando a contar en 1). Por ejemplo,
--    posImpares [2,4,1,3,-1,5]  ==  [2,1,-1]
posImpares :: [a] -> [a]
posImpares xs = [x | (x,n) <- zip xs [1..], odd n]

-- (posPares xs) es la lista de los elementos de xs en las posiciones
-- pares (empezando a contar en 1). Por ejemplo,
--    posPares [2,4,1,3,-1,5]  ==  [4,3,5]
posPares :: [a] -> [a]
posPares xs =  [x | (x,n) <- zip xs [1..], even n]

-- 3ª solución (por comprensión)

zigzaguea3 :: (Num a,Ord a) => [a] -> Bool
zigzaguea3 xs =
  (length xs < 2)
  || (and [x < y | (x,y) <- paresImpares xs] &&
      and [x > y | (x,y) <- imparesPares xs])


-- (paresImpares xs) es la lista de los pares formado por los elementos
-- de xs que están en las posiciones pares junto con su siguiente
-- elemento. Por ejemplo,
--    paresImpares [0..10]  ==  [(0,1),(2,3),(4,5),(6,7),(8,9)]
paresImpares :: [a] -> [(a,a)]
paresImpares (x:y:zs) = (x,y) : paresImpares zs
paresImpares _        = []

-- (imparesPares xs) es la lista de los pares formado por los elementos
-- de xs que están en las posiciones impares junto con su siguiente
-- elemento. Por ejemplo,
--    imparesPares [0..10]  ==  [(1,2),(3,4),(5,6),(7,8),(9,10)]
imparesPares :: [a] -> [(a,a)]
imparesPares (_:x:y:zs) = (x,y) : imparesPares (y:zs)
imparesPares _          = []

