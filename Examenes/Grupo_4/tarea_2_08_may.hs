-- Informática (1º del Grado en Matemáticas)
-- Tarea del 8 de mayo de 2020
-- =====================================================================

-- ---------------------------------------------------------------------
-- Disponemos de n dados con m caras cada uno, numeradas de 1 a m.
-- Cuando se tiran los n dados, cada uno tiene una cara a la vista.
-- Se desea calcular el número de formas de sumar x, considerando una
-- cara de cada dado.
-- 
-- Ejercicio 1. Definir, por recursión, la función
--    numeroSumasR :: Int -> Int -> Int -> Int
-- tal que (numeroSumasR m n x) es el número de tiradas en las que la
-- suma de las caras visibles es x, considerando n dados con m caras
-- cada uno. Por ejemplo,
--    numeroSumasR 6 3 4 == 3
--    numeroSumasR 3 4 5 == 4
--    numeroSumasR 4 3 5 == 6
-- ---------------------------------------------------------------------

import Data.Array

-- 1ª solución
-- ===========

numeroSumasR1 :: Int -> Int -> Int -> Int
numeroSumasR1 m n x = length (sumas m n x)

-- (sumas m n x) es la lista de tiradas en las que la suma de las caras
-- visibles es x, considerando n dados con m caras cada uno. Por ejemplo,
--    λ> sumas 6 3 4
--    [[1,1,2],[1,2,1],[2,1,1]]
--    λ> sumas 3 4 5
--    [[1,1,1,2],[1,1,2,1],[1,2,1,1],[2,1,1,1]]
--    λ> sumas 4 3 5
--    [[1,1,3],[1,2,2],[1,3,1],[2,1,2],[2,2,1],[3,1,1]]
sumas :: Int -> Int -> Int -> [[Int]]
sumas m n x = [ys | ys <- variacionesR n [1..m],
                    sum ys == x]

-- (variacionesR k xs) es la lista de las variaciones de orden k de los
-- elementos de xs con repeticiones. Por ejemplo, 
--    λ> variacionesR 2 [1..3]
--    [[1,1],[1,2],[1,3],[2,1],[2,2],[2,3],[3,1],[3,2],[3,3]]
--    λ> variacionesR 3 [1..2]
--    [[1,1,1],[1,1,2],[1,2,1],[1,2,2],[2,1,1],[2,1,2],[2,2,1],[2,2,2]]
variacionesR :: Int -> [a] -> [[a]]
variacionesR 0 _  = [[]] 
variacionesR k xs =
  [z:ys | z <- xs, ys <- variacionesR (k-1) xs]

-- 2ª solución
-- ===========

numeroSumasR :: Int -> Int -> Int -> Int
numeroSumasR m n 0 = 0
numeroSumasR m 0 x = 0
numeroSumasR m 1 1 = 1
numeroSumasR m n 1 = 0
numeroSumasR m 1 x | x <= m    = 1
                   | otherwise = 0
numeroSumasR m n x = sum [numeroSumasR m (n-1) (x-k) | k <- [1..min m x]]

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir, por programación dinámica, la función
--    numeroSumasPD :: Int -> Int -> Int -> Int
-- tal que (numeroSumasPD m n x) es el número de tiradas en las que la
-- suma de las caras visibles es x, considerando n dados con m caras
-- cada uno. Por ejemplo,
--    numeroSumasPD 6 3 4    == 3
--    numeroSumasPD 10 20 30 == 20029990
-- ---------------------------------------------------------------------

numeroSumasPD :: Int -> Int -> Int -> Int
numeroSumasPD m n x = (matrizSumas m n x) ! (n,x)

-- (matrizSumas m n x) es la matriz de dimensión (n+1)*(x+1) tal que el
-- valor en la posición (i,j) es el número de tiradas en las que la
-- suma de las caras visibles es j, considerando i dados con m caras
-- cada uno. Por ejemplo,
--    λ> elems (matrizSumas 6 3 4)
--    [0,0,0,0,0,
--     0,1,1,1,1,
--     0,0,1,2,3,
--     0,0,0,1,3]
--    λ> elems (matrizSumas 3 4 5)
--    [0,0,0,0,0,0,
--     0,1,1,1,0,0,
--     0,0,1,2,3,2,
--     0,0,0,1,3,6,
--     0,0,0,0,1,4]
--    λ> elems (matrizSumas 4 3 5)
--    [0,0,0,0,0,0,
--     0,1,1,1,1,0,
--     0,0,1,2,3,4,
--     0,0,0,1,3,6]
-- matrizSumas1 m n x = p
matrizSumas m n x = p
  where p = array ((0,0),(n,x)) [((i,j), f i j) | i <- [0..n], j <- [0..x]]
        f i 0 = 0
        f 0 j = 0
        f 1 1 = 1
        f i 1 = 0
        f 1 j | j <= m    = 1
              | otherwise = 0
        f i j = sum [p!(i-1,j-k) | k <- [1..min m j]]
