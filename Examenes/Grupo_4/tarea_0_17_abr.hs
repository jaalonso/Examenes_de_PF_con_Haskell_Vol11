-- Informática (1º del Grado en Matemáticas)
-- 1ª tarea (17 de abril de 2020)
-- =====================================================================

-- ---------------------------------------------------------------------
-- Ejercicio. Se puede representar las posiciones de las personas
-- infectadas mediante una matriz de forma que los posiciones ocupadas
-- por el número 1 corresponde a las de las personas infectadas y las
-- ocupadas por el número 0 a las de las que no lo están.
--
-- Definir la función
--    maximaSeguridad :: Matrix Int -> [(Int, Int)]
-- tal que (maximaSeguridad a) es la lista de las posiciones de a que
-- están más alejadas de las personas infectadas. Por ejemplo,
--    λ> maximaSeguridad (fromLists [[0,0,1],[0,0,1],[0,1,0]])
--    [(1,1)]
--    λ> maximaSeguridad (fromLists [[0,0,1],[0,0,1],[0,0,0]])
--    [(3,1)]
--    λ> maximaSeguridad (fromLists [[0,0,0],[0,1,0],[0,0,0]])
--    [(1,1),(1,3),(3,1),(3,3)]
--    λ> maximaSeguridad (fromLists [[0,0,0],[0,0,0],[0,1,0]])
--    [(1,1),(1,3)]
--    λ> maximaSeguridad (fromLists [[0,0,0]])
--    [(1,1),(1,2),(1,3)]
-- ---------------------------------------------------------------------

import Data.Matrix

maximaSeguridad :: Matrix Int -> [(Int, Int)]
maximaSeguridad a
  | 0 `notElem` a = []
  | 1 `elem` a    = [(i,j) | i <- [1..m]
                           , j <- [1..n]
                           , b!(i,j) == maximum b ]
  | otherwise     = [(i,j) | i <- [1..m]
                           , j <- [1..n]]
  where m = nrows a
        n = ncols a
        b = minimasDistancias a

-- (minimasDistancias a) es la matriz de la mismas dimensiones que a tal
-- que el valor en cada posición es la mínima distancia a la posiciones
-- de los infectados de la matriz a. Por ejemplo,
--    λ> minimasDistancias (fromLists [[0,0,1],[0,0,1],[0,1,0]])
--    ┌                                                          ┐
--    │                2.0                1.0                0.0 │
--    │ 1.4142135623730951                1.0                0.0 │
--    │                1.0                0.0                1.0 │
--    └                                                          ┘
--    λ> minimasDistancias (fromLists [[0,0,1],[0,0,1],[0,0,0]])
--    ┌                                                          ┐
--    │                2.0                1.0                0.0 │
--    │                2.0                1.0                0.0 │
--    │   2.23606797749979 1.4142135623730951                1.0 │
--    └                                                          ┘
--    λ> minimasDistancias (fromLists [[0,0,0],[0,1,0],[0,0,0]])
--    ┌                                                          ┐
--    │ 1.4142135623730951                1.0 1.4142135623730951 │
--    │                1.0                0.0                1.0 │
--    │ 1.4142135623730951                1.0 1.4142135623730951 │
--    └                                                          ┘
--    λ> minimasDistancias (fromLists [[0,0,0],[0,0,0],[0,1,0]])
--    ┌                                                          ┐
--    │   2.23606797749979                2.0   2.23606797749979 │
--    │ 1.4142135623730951                1.0 1.4142135623730951 │
--    │                1.0                0.0                1.0 │
--    └                                                          ┘
minimasDistancias :: Matrix Int -> Matrix Double
minimasDistancias a =
  matrix m n (\(i,j) -> minimum [ distancia (i,j) (i',j')
                                | (i',j') <- posicionesInfectados a])
  where m = nrows a
        n = ncols a
        
-- (distancia p q) es la distancia de p a q. Por ejemplo,
--    distancia (1,4) (4,8)  ==  5.0
distancia :: (Int,Int) -> (Int,Int) -> Double
distancia (x1,y1) (x2,y2) =
  (fromIntegral ((x1-x2)^2+(y1-y2)^2)) ** 0.5

-- (posicionesInfectados a) es la lista de las posiciones de los
-- infectados en la matriz a. Por ejemplo,
--    λ> posicionesInfectados (fromLists [[0,0,1],[0,0,1],[0,1,0]])
--    [(1,3),(2,3),(3,2)]
posicionesInfectados :: Matrix Int -> [(Int,Int)]
posicionesInfectados a =
  [(i,j) | i <- [1..nrows a]
         , j <- [1..ncols a]
         , a!(i,j) == 1]
