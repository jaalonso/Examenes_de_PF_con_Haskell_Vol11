-- Informática (1º del Grado en Matemáticas)
-- Tarea del 15 de mayo de 2020
-- =====================================================================

import Data.Matrix
import Data.List
import I1M.Monticulo

-- ----------------------------------------------------------------------------
-- Ejercicio 1. Definir la función 
--    mapMonticulo :: Ord a => (a -> a) -> Monticulo a -> Monticulo a
-- tal que (mapMonticulo f m) es el montículo obtenido aplicando la
-- función f a los elementos del montículo m. Por ejemplo,
--    λ> m1 = foldr inserta vacio [6,1,4,8]
--    λ> m1
--    M 1 2 (M 4 1 (M 8 1 Vacio Vacio) Vacio) (M 6 1 Vacio Vacio)
--    λ> mapMonticulo (+3) m1
--    M 4 1 (M 7 1 (M 9 1 (M 11 1 Vacio Vacio) Vacio) Vacio) Vacio
--    λ> m2 = foldr inserta vacio [7,5]
--    λ> m2
--    M 5 1 (M 7 1 Vacio Vacio) Vacio
--    λ> mapMonticulo (*2) m2
--    M 10 1 (M 14 1 Vacio Vacio) Vacio
-- ---------------------------------------------------------------------

mapMonticulo :: Ord a => (a -> a) -> Monticulo a -> Monticulo a
mapMonticulo f m
  | esVacio m = m
  | otherwise = inserta (f (menor m)) (mapMonticulo f (resto m))

-- ---------------------------------------------------------------------
-- Ejercicio 2. Se tiene una cuadrícula que representa una mina de oro
-- de dimensiones m*n. Cada campo de esta mina contiene un entero
-- positivo que es su cantidad de oro. Inicialmente el minero está en
-- la primera columna pero puede estar en cualquier fila. Desde una
-- casilla, puede moverse a una de esas tres posiciones:
-- + a la casilla de la derecha
-- + a la casilla a la derecha/arriba
-- + a la casilla a la derecha/abajo
-- Por ejemplo, de la (2,3) se puede mover a (2,4), (1,4) y (3,4).
-- 
-- Se desea calcular la cantidad máxima de oro que puede obtener. Por
-- ejemplo
-- + En la mina definida por
--      [[1, 3, 3],
--       [2, 1, 4],
--       [0, 6, 4]];
--   el máximo es 12, que corresponde al camino
--      (2,1) -> (3,2) -> (3,3) (con suma 2 + 6 + 4 = 12).
-- + En la mina definida por
--      [[1, 3, 1, 5],
--       [2, 2, 4, 1],
--       [5, 0, 2, 3],
--       [0, 6, 1, 2]];
--   el máximo es 16, que corresponde a uno de los caminos siguientes:
--      (3,1) -> (2,2) -> (2,3) -> (1,4) (con suma 5 + 2 + 4 + 5 = 16) 
--      (3,1) -> (4,2) -> (3,3) -> (3,4) (con suma 5 + 6 + 2 + 3 = 16)
-- 
-- Definir, mediante programación dinámica, la función
--    maximoOro :: Matrix Integer -> Integer
-- tal que (maximoOro p) es la cantidad máxima de oro que se puede
-- obtener en la mina definida por la matriz p. Por ejemplo,
--    λ> maximoOro (fromLists [[1,3,3],[2,1,4],[0,6,4]])
--    12
--    λ> maximoOro (fromLists [[1,3,1,5],[2,2,4,1],[5,0,2,3],[0,6,1,2]])
--    16
--    λ> maximoOro (fromLists [[10,33,13,15],[22,21,4,1],[5,0,2,3],[0,6,14,2]])
--    83
--    λ> maximoOro (fromList 100 100 [1..])
--    995050
--    λ> maximoOro (fromList 1000 1000 [1..])
--    999500500
--    λ> maximoOro (fromList 2000 500 [1..])
--    499875250
-- ---------------------------------------------------------------------

maximoOro :: Matrix Integer -> Integer
maximoOro p = maximum [q!(i,n) | i <- [1..m]]
  where m = nrows p
        n = ncols p
        q = matrizOro p

-- (matrizOro p) es la matriz de la misma dimensión que p y tal que el
-- valor en la posición (i,j) es la cantidad máxima que puede obtener
-- cuando el minero ha llegado a la casilla (i,j). Por ejemplo, 
--    λ> matrizOro (fromLists [[1,3,3],[2,1,4],[0,6,4]])
--    ┌          ┐
--    │  1  5  8 │
--    │  2  3 12 │
--    │  0  8 12 │
--    └          ┘
--    λ> matrizOro (fromLists [[1,3,1,5],[2,2,4,1],[5,0,2,3],[0,6,1,2]])
--    ┌             ┐
--    │  1  5  8 16 │
--    │  2  7 11 14 │
--    │  5  5 13 16 │
--    │  0 11 12 15 │
--    └             ┘
--    λ> matrizOro (fromLists [[10,33,13,15],[22,21,4,1],[5,0,2,3],[0,6,14,2]])
--    ┌             ┐
--    │ 10 55 68 83 │
--    │ 22 43 59 69 │
--    │  5 22 45 62 │
--    │  0 11 36 47 │
--    └             ┘
matrizOro :: Matrix Integer -> Matrix Integer
matrizOro p = q where
  (m, n) = (nrows p, ncols p)
  q = matrix m n f
  f (i,1) = p ! (i,1)
  f (i,j) | i == 1    = p!(i,j) + max (q!(i,j-1)) (q!(i+1,j-1))
          | i == m    = p!(i,j) + max (q!(i,j-1)) (q!(i-1,j-1))
          | otherwise = p!(i,j) + maximum [q!(i,j-1), q!(i-1,j-1), q!(i+1,j-1)]

