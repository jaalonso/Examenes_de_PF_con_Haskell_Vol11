-- Informática (1º del Grado en Matemáticas, Grupo 2)
-- 2º examen de evaluación continua (27 de noviembre de 2019)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Los números 545, 5995 y 15151 son los tres menores
-- palíndromos (capicúas) que son divisibles por 109.
--
-- Definir la función
--    multiplosPalindromos :: Integer -> [Integer]
-- tal que (multiplosPalindromos n) es la lista de los palíndromos
-- divisibles por n. Por ejemplo,
--    take 5 (multiplosPalindromos 109) == [545,5995,15151,64746,74447]
--
-- Nota: Basado en el 655 del Proyecto Euler.
-- ---------------------------------------------------------------------

-- 1ª definición
-- =============

multiplosPalindromos :: Integer -> [Integer]
multiplosPalindromos n =
  [x | x <- multiplos n
     , esPalindromo x]

-- (esPalindromo n) se verifica si n es palíndromo. Por ejemplo, 
--    esPalindromo 32523  ==  True
--    esPalindromo 32533  ==  False
esPalindromo :: Integer -> Bool
esPalindromo n = reverse xs == xs
  where xs = show n

-- (multiplos n) es la lista de los múltiplos de n. Por ejemplo, 
--    take 12 (multiplos 5)  ==  [5,10,15,20,25,30,35,40,45,50,55,60]
multiplos :: Integer -> [Integer]
multiplos n = [n,2*n..]

-- 2ª definición
-- =============

multiplosPalindromos2 :: Integer -> [Integer]
multiplosPalindromos2 = filter esPalindromo . multiplos 

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Definir la función
--    multiplosPalindromosMenores :: Integer -> Integer -> [Integer]
-- tal que (multiplosPalindromosMenoresx n) es la lista de los
-- palíndromos divisibles por n, menores que x. Por ejemplo,
--    λ> multiplosPalindromosMenores (10^5) 109
--    [545,5995,15151,64746,74447,79897,84148,89598,99299]
-- ---------------------------------------------------------------------

-- 1ª definición
multiplosPalindromosMenores :: Integer -> Integer -> [Integer]
multiplosPalindromosMenores x n =
  takeWhile (<x) (multiplosPalindromos n)

-- 2ª definición
multiplosPalindromosMenores2 :: Integer -> Integer -> [Integer]
multiplosPalindromosMenores2 x =
  takeWhile (<x) . multiplosPalindromos

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    menorM90 :: Int -> Int
-- tal que (menorM90 n) es el menor múltiplo de n, mayor que 0, formado
-- por 9 y 0. Por ejemplo,
--    menorM90 5   == 90
--    menorM90 7   == 9009
--    menorM90 713 == 9900000009
-- ---------------------------------------------------------------------

-- 1ª solución
-- ===========

menorM90 :: Int -> Int
menorM90 n = head [x | x <- [9..]
                     , solo9y0 x
                     , x `mod` n == 0]

solo9y0 :: Int -> Bool
solo9y0 n = and [x == '0' || x == '9' | x <- show n]


-- 2ª solución
-- ===========

menorM90b :: Int -> Int
menorM90b n = head [x | x <- [n,n+n..]
                      , solo9y0 x]


-- 3ª solución
-- ===========

menorM90c :: Int -> Int
menorM90c n = head [x | x <- numerosCon0y9
                      , x `mod` n == 0]

-- numerosCon0y9 es la lista de los números con los dígitos 0 y 9. Por
-- ejemplo, 
numerosCon0y9 :: [Int]
numerosCon0y9 = map int2Con0y9 [1..]

-- (int2Con0y9 n) es el n-ésimo teŕmino de la sucesión de los números
-- con los dígitos 0 y p. Por ejemplo,
--   λ> map int2Con0y9 [0..11]
--   [0,9,90,99,900,909,990,999,9000,9009,9090,9099]
int2Con0y9 :: Int -> Int
int2Con0y9 n = read (reverse (aux n))
  where aux n | n == 0    = "0"
              | n == 1    = "9"
              | even n    = '0' : aux (n `div` 2)
              | otherwise = '9' : aux (n `div` 2)

-- Observación: el patrón de los números es el de los números en base 2,
-- cambiando el 1 por 9 y el orden.

-- Comparación de eficiencia
-- =========================

--   λ> menorM90 29
--   9909909
--   (7.31 secs, 9,177,134,208 bytes)
--   λ> menorM90b 29
--   9909909
--   (0.26 secs, 316,551,296 bytes)
--   λ> menorM90_2 29
--   9909909
--   (0.01 secs, 114,824 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. La sucesión de los números triangulares se obtiene
-- sumando los números naturales. Así, los 5 primeros números
-- triangulares son
--     1 = 1
--     3 = 1+2
--     6 = 1+2+3
--    10 = 1+2+3+4
--    15 = 1+2+3+4+5

-- Definir la lista
--    triangulares :: [Integer]
-- tal que triangulares es la lista de los números triangulares. Por
-- ejemplo, 
--    take 5 triangulares          == [1,3,6,10,15]
--    takeWhile (<30) triangulares == [1,3,6,10,15,21,28]
-- 
-- Nota: Basado en el 647 del Proyecto Euler.
-- ---------------------------------------------------------------------

triangulares :: [Integer]
triangulares = [n*(n+1) `div` 2 | n <- [1..]]

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Para cada número triangular n existen números
-- naturales a y b, tales que a*n + b también es triangular.
-- 
-- Definir la función
--    paresT :: Integer -> [(Integer,Integer)]
-- tal que si n es triangular, (paresT n) es la lista de los pares (a,b)
-- tales que a es un entero positivo y b el menor número tal que
-- a*n + b es triangular. Por ejemplo,
--    take 5 (paresT 6)  == [(1,0),(2,3),(3,3),(4,4),(5,6)]
--    take 5 (paresT 15) == [(1,0),(2,6),(3,10),(4,6),(5,3)]
-- ---------------------------------------------------------------------

-- 1ª solución
paresT :: Integer -> [(Integer,Integer)]
paresT n = (1,0) : [(a, f a) | a <- [2..]]
  where f a = head (dropWhile (<= a*n) triangulares) - a*n

-- 2ª solución
paresT2 :: Integer -> [(Integer,Integer)]
paresT2 n = (1,0): map g [2..]
  where g a = (a, head (dropWhile (<= a*n) triangulares) - a*n)

-- Comparación de eficiencia
-- =========================

--   λ> paresT 17 !! (2*10^7)
--   (20000001,17986)
--   (3.13 secs, 4,322,738,880 bytes)
--   λ> paresT2 17 !! (2*10^7)
--   (20000001,17986)
--   (0.43 secs, 3,200,112,272 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--    factorizaInv :: Int -> [Int] -> [[Int]]
-- tal (factorizaInv n xs) es la lista de listas de elementos distintos
-- de la lista ordenada creciente xs cuyo producto es el número natural
-- n. Por ejemplo, 
--    λ> factorizaInv 72 [2,3,4,5,6,7,9,10,16]
--    [[2,4,9],[3,4,6]]
--    λ> factorizaInv 720 [2,3,4,5,6,7,9,10,16]
--    [[2,3,4,5,6],[2,4,9,10],[3,4,6,10],[5,9,16]]
-- ---------------------------------------------------------------------

factorizaInv :: Int -> [Int] -> [[Int]]
factorizaInv _ [] = []
factorizaInv n (x:xs)
  | x > n     = []
  | x == n    = [[x]]
  | r == 0    = map (x:) (factorizaInv q xs) ++ factorizaInv n xs
  | otherwise = factorizaInv n xs
  where (q,r) = quotRem n x
