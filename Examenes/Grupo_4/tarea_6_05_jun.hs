-- Informática (1º del Grado en Matemáticas)
-- Tarea evaluable (5 de junio de 2020)
-- =====================================================================

-- ---------------------------------------------------------------------
-- § Indicaciones                                                     --
-- ---------------------------------------------------------------------

-- Es necesario que se pueda cargar el fichero y documentar todas las
-- funciones; es decir,
-- 1. Elegir un nombre para sugerir lo que hace
-- 2. Escribir, en lenguaje natural, qué hace la función y no cómo lo hace. 
-- 3. Poner ejemplos de uso.
--
-- En cada ejercicio se valorará la corrección, claridad y eficiencia de
-- la solución propuesta.

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.Numbers.Primes
import Test.QuickCheck
import Data.Matrix
import Data.List

-- ---------------------------------------------------------------------
-- Ejercicio 1. Dado un número entero n > 1, se puede transformar en una
-- lista como sigue:
-- + Se comienza con la lista xs formada por los divisores primos de n
--   ordenados de menor a mayor.
-- + A continuación, cada x elemento de xs se sustituye por la lista
--   vacía si x es igual a 2 o por su índice en la sucesión de los
--   números primos si x es distinto de 2. 
-- La lista obtenida se llama la codificación de n. Por ejemplo, si n es
-- 3300 en el primer paso se obtiene la lista
--    [2,2,3,5,5,11]
-- y en el segundo,
--    [[],[],[1],[2],[2],[4]]
--
-- Definir las funciones
--    codificacion   :: Int -> [[Int]]
--    decodificacion :: [[Int]] -> Int
-- tales que
-- + (codificacion n) es la codificación del número n. Por ejemplo,
--      codificacion 3300  ==  [[],[],[1],[2],[2],[4]]
--      codificacion 12    ==  [[],[],[1]]
--      codificacion 324   ==  [[],[],[1],[1],[1],[1]]
--      codificacion 525   ==  [[1],[2],[2],[3]]
-- + (decodificacion xss) es el número n tal que su codificación es
--   xss. Por ejemplo,
--      decodificacion [[],[],[1],[2],[2],[4]] ==  3300
--      decodificacion [[],[],[1]]             ==  12
--      decodificacion [[],[],[1],[1],[1],[1]] ==  324
--      decodificacion [[1],[2],[2],[3]]       ==  525
--
-- Comprobar con QuickChek para todo número entero n > 1, si decodifica
-- la codificación de n se obtiene el número n.
-- ---------------------------------------------------------------------

codificacion :: Int -> [[Int]]
codificacion n = map f (primeFactors n)
  where f p | p == 2    = []
            | otherwise = [indicePrimo p]

-- (indicePrimo p) es la posición de p en la sucesión de los números
-- primos. Por ejemplo,
--    indicePrimo 3 ==  1
--    indicePrimo 7 ==  3
indicePrimo :: Int -> Int
indicePrimo p = head [i | (i,q) <- zip [0..] primes, q == p]

decodificacion :: [[Int]] -> Int
decodificacion = product . map g
  where g []     = 2
        g (x:xs) = primes !! x

-- La propieda es
propCodificacion :: Int -> Bool
propCodificacion n = decodificacion (codificacion m) == m
  where m = 2 + abs n

-- La comprobación es
--    λ> quickCheck propCodificacion
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Decimos que un número natural es creciente si sus
-- dígitos son distintos de cero y están ordenados de forma creciente;
-- es decir, cada uno es menor o igual que su siguiente. Por ejemplo,
-- 34468 es número creciente pero 34648 no lo es.
-- 
-- Definir la función
--    crecientes :: Int -> Integer
-- tal que (crecientes n) es la cantidad de números crecientes con n
-- dígitos. Por ejemplo, 
--    crecientes 1    ==  9
--    crecientes 2    ==  45
--    crecientes 3    ==  165
--    crecientes 7    ==  6435
--    crecientes 15   ==  490314
--    crecientes 30   ==  48903492
--    crecientes 100  ==  352025629371
-- ---------------------------------------------------------------------

-- 1ª solución
-- ===========

crecientes1 :: Int -> Integer
crecientes1 = genericLength . sucCrecientes

-- (sucCrecientes n) es la lista de las sucesiones crecientes con n
-- dígitos. Por ejemplo, 
--    λ> sucCrecientes 2
--    [11,12,13,14,15,16,17,18,19,22,23,24,25,26,27,28,29,33,34,35,36,37,38,
--     39,44,45,46,47,48,49,55,56,57,58,59,66,67,68,69,77,78,79,88,89,99]
--    λ> take 10 (sucCrecientes 3)
--    [111,112,113,114,115,116,117,118,119,122]
--    λ> take 10 (sucCrecientes 4)
--    [1111,1112,1113,1114,1115,1116,1117,1118,1119,1122]
sucCrecientes :: Int -> [Int]
sucCrecientes n = filter esCreciente [10^(n-1)..10^n-1]

-- (esCreciente n) se verifica si n es creciente. Por ejemplo,
--    esCreciente 3446 ==  True
--    esCreciente 3464 ==  False
esCreciente :: Int -> Bool
esCreciente n = sort ns == ns
  where ns = show n

-- 2ª solución
-- ===========

crecientes2 :: Int -> Integer
crecientes2 n = sum [crecientesDesde n k | k <- [1..9]]

-- (crecientesDesde n k) es la cantidad de números crecientes con
-- n dígitos cuyo primer dígito es k. Por ejemplo, 
--    crecientesDesde 3 9 ==  1
--    crecientesDesde 3 8 ==  3
--    crecientesDesde 2 8 ==  2
--    crecientesDesde 3 8 ==  3
--    crecientesDesde 3 7 ==  6
crecientesDesde :: Int -> Int -> Integer
crecientesDesde 0 _ = 0
crecientesDesde 1 _ = 1
crecientesDesde n k = sum [crecientesDesde (n-1) j | j <- [k..9]]

-- 3ª solución
-- ===========

crecientes3 :: Int -> Integer
crecientes3 n = sum [p!(n,k) | k <- [1..9]]
  where p = matrizCrecientes3 n

-- (matrizCrecientes3 n) es la matriz de n filas y 9 columnas p tal que
-- el valor en la posición (i,j) es cantidad de números crecientes3 con i
-- dígitos, cuyo primer dígito es j. Por ejemplo,
--    λ> matrizCrecientes3 1
--    ┌                   ┐
--    │ 1 1 1 1 1 1 1 1 1 │
--    └                   ┘
--    λ> matrizCrecientes3 2
--    ┌                   ┐
--    │ 1 1 1 1 1 1 1 1 1 │
--    │ 9 8 7 6 5 4 3 2 1 │
--    └                   ┘
--    λ> matrizCrecientes3 3
--    ┌                            ┐
--    │  1  1  1  1  1  1  1  1  1 │
--    │  9  8  7  6  5  4  3  2  1 │
--    │ 45 36 28 21 15 10  6  3  1 │
--    └                            ┘
--    λ> matrizCrecientes3 4
--    ┌                                     ┐
--    │   1   1   1   1   1   1   1   1   1 │
--    │   9   8   7   6   5   4   3   2   1 │
--    │  45  36  28  21  15  10   6   3   1 │
--    │ 165 120  84  56  35  20  10   4   1 │
--    └                                     ┘
matrizCrecientes3 :: Int -> Matrix Integer
matrizCrecientes3 n = p where
  p = matrix n 9 f
  f (1,_) = 1
  f (i,j) = sum [p!(i-1,k) | k <- [j..9]]

-- 4ª solución
-- ===========

crecientes :: Int -> Integer
crecientes n = sum [p!(n,k) | k <- [1..9]]
  where p = matrizCrecientes n

-- (matrizCrecientes n) es la matriz de n filas y 9 columnas p tal que
-- el valor en la posición (i,j) es cantidad de números crecientes con i
-- dígitos, cuyo primer dígito es j. Por ejemplo,
--    λ> matrizCrecientes 1
--    ┌                   ┐
--    │ 1 1 1 1 1 1 1 1 1 │
--    └                   ┘
--    λ> matrizCrecientes 2
--    ┌                   ┐
--    │ 1 1 1 1 1 1 1 1 1 │
--    │ 9 8 7 6 5 4 3 2 1 │
--    └                   ┘
--    λ> matrizCrecientes 3
--    ┌                            ┐
--    │  1  1  1  1  1  1  1  1  1 │
--    │  9  8  7  6  5  4  3  2  1 │
--    │ 45 36 28 21 15 10  6  3  1 │
--    └                            ┘
--    λ> matrizCrecientes 4
--    ┌                                     ┐
--    │   1   1   1   1   1   1   1   1   1 │
--    │   9   8   7   6   5   4   3   2   1 │
--    │  45  36  28  21  15  10   6   3   1 │
--    │ 165 120  84  56  35  20  10   4   1 │
--    └                                     ┘
matrizCrecientes :: Int -> Matrix Integer
matrizCrecientes n = p where
  p = matrix n 9 f
  f (1,j) = 1
  f (i,9) = 1
  f (i,j) = p!(i,j+1) + p!(i-1,j)

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> crecientes1 7
--    6435
--    (4.97 secs, 12,711,235,024 bytes)
--    λ> crecientes2 7
--    6435
--    (0.04 secs, 5,723,952 bytes)
--    λ> crecientes3 7
--    6435
--    (0.01 secs, 225,704 bytes)
--    λ> crecientes 7
--    6435
--    (0.01 secs, 125,400 bytes)
--
--    λ> crecientes2 17
--    1081575
--    (2.17 secs, 1,714,280,592 bytes)
--    λ> crecientes3 17
--    1081575
--    (0.01 secs, 429,152 bytes)
--    λ> crecientes 17
--    1081575
--    (0.01 secs, 173,024 bytes)
--
--    λ> crecientes3 (10^5)
--    248105172272950452504148938027057501
--    (3.51 secs, 2,288,167,424 bytes)
--    λ> crecientes (10^5)
--    248105172272950452504148938027057501
--    (1.02 secs, 441,295,880 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Calcular la cantidad de números crecientes con 123456
-- dígitos.
-- ---------------------------------------------------------------------

-- El cálculo es
--    λ> crecientes4 123456
--    1338763082311070449095794652369476313
