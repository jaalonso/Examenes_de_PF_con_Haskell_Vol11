-- Informática (1º del Grado en Matemáticas)
-- 2ª tarea (22 de abril de 2020)
-- =====================================================================

import Data.List

-- ---------------------------------------------------------------------
-- Ejercicio 1. Se tienen n cartas numeradas en un único montón en orden
-- creciente desde arriba hacia abajo; es decir, la carta de más arriba
-- es la número 1, luego sigue la número 2, ...
-- 
-- Se hace lo siguiente: 
-- + se coloca la carta número 1 debajo de todas, 
-- + se quita de arriba la carta número 2, 
-- + se coloca la carta número 3 debajo de todas, 
-- + se quita de arriba la carta número 4, 
-- + ...  
-- 
-- El proceso continúa siempre de la misma manera: se coloca la carta de
-- más arriba debajo de todas y se quita la que ha quedado arriba, hasta
-- que quede una única carta.
-- 
-- Definir la función 
--    cartaFinal :: Int -> Int
-- tal que (cartaFinal n) es la carta final que queda en el montón. Por
-- ejemplo, 
--    cartaFinal 4   == 1
--    cartaFinal 7   == 7
--    cartaFinal 10  == 5
--    cartaFinal 20  == 9
--    cartaFinal 100 == 73
-- ---------------------------------------------------------------------

cartaFinal :: Int -> Int
cartaFinal n =
  head (until esUnitaria primeroAlFinalYQuitaSegundo [1..n])

-- (primeroAlFinalYQuitaSegundo xs) es la lista obtenida añadiendo el
-- primer elemento de xs al final y quitando el segundo elemento. Por
-- ejemplo, 
--    primeroAlFinalYQuitaSegundo [2,5,3,7]  ==  [3,7,2]
--    primeroAlFinalYQuitaSegundo [3,7,2]    ==  [2,3]
--    primeroAlFinalYQuitaSegundo [2,3]      ==  [2]
primeroAlFinalYQuitaSegundo :: [a] -> [a]
primeroAlFinalYQuitaSegundo (x:_:xs) = xs ++ [x]

-- (esUnitaria xs) se verifica si la lista xs tiene un único
-- elemento. Por ejemplo,
--    esUnitaria [27]      ==  True
--    esUnitaria [27, 14]  ==  False
--    esUnitaria []        ==  False
esUnitaria :: [Int] -> Bool
esUnitaria [_] = True
esUnitaria _   = False

-- 2ª solución
-- ===========

cartaFinal2 :: Int -> Int
cartaFinal2 n = aplicaProcedimiento [1..n]

-- (aplicaProcedimiento xs) es el elemento final cuando se le aplica a
-- xs el procedimiento de pasar el primero al final y quitar el segundo
-- hasta que sólo quede un elemento. Por ejemplo, 
--    aplicaProcedimiento [2,5,3,7]  ==  2
--    aplicaProcedimiento [5,3,7]    ==  7
aplicaProcedimiento :: [a] -> a
aplicaProcedimiento [x]      = x
aplicaProcedimiento (x:_:xs) = aplicaProcedimiento (xs ++ [x])

-- 3ª solución
-- ===========

cartaFinal3 :: Int -> Int
cartaFinal3 n = aux [1..n]
  where aux [x]      = x
        aux (x:_:xs) = aux (xs ++ [x])

-- ---------------------------------------------------------------------
-- Ejercicio 2. Se considera una retícula rectangular numerada de arriba
-- a abajo y de izquierda a derecha empezando la numeración en 1.
--      1 2
--    1 A ·
--    2 · ·
--    3 · ·
--    4 · B
-- Se consideran dos personas A (que inicialmente está en la esquina
-- superior izquierda) y B (que inicialmente está en la esquina
-- inferior derecha). En cada paso, A se puede mover una posición a la
-- derecha o hacia abajo y B se puede mover una posición a la
-- izquierda o hacia arriba. En ningún paso pueden salir de la
-- retícula. Por ejemplo, las posibles posiciones de A y B después del
-- primer paso se muestra en la siguiente figura  
--      1 2 
--    1 · A 
--    2 A · 
--    3 · B 
--    4 B ·
-- En el siguiente paso se encuentran en las posiciones (2,2) y (3,1) y
-- ya no se vuelven a encontrar.
-- 
-- Definir la función
--    posicionesEncuentro :: Int -> Int -> [(Int,Int)]
-- tal que (posicionesEncuentro m n) es la lista de las posiciones donde
-- se encuentran A y B en una retícula con m filas y n columnas. Por ejemplo,
--    posicionesEncuentro 4 2  ==  [(2,2),(3,1)]
--    posicionesEncuentro 4 3  ==  []
--    posicionesEncuentro 5 3  ==  [(2,3),(3,2),(4,1)]
-- ---------------------------------------------------------------------

posicionesEncuentro :: Int -> Int -> [(Int,Int)]
posicionesEncuentro m n =
  sort (nub (concat (zipWith intersect (posicionesA m n) (posicionesB m n))))

-- (siguientesA m n p) es la lista de las posiciones donde puede estar A
-- después de un paso si actualmente se encuentra en la posición p en una
-- retícula con m filas y n columnas. Por ejemplo, 
--    siguientesA 4 2 (1,1)  ==  [(2,1),(1,2)]
--    siguientesA 4 2 (1,2)  ==  [(2,2)]
--    siguientesA 4 2 (4,2)  ==  []
siguientesA :: Int -> Int -> (Int,Int) -> [(Int,Int)]
siguientesA m n (i,j) = [(i+1,j) | i < m] ++ [(i,j+1) | j < n]

-- (siguientesB m n p) es la lista de las posiciones donde puede estar B
-- después de un paso si actualmente se encuentra en la posición p en una
-- retícula con m filas y n columnas. Por ejemplo, 
--    siguientesB 4 2 (4,2)  ==  [(3,2),(4,1)]
--    siguientesB 4 2 (4,1)  ==  [(3,1)]
--    siguientesB 4 2 (1,1)  ==  []
siguientesB :: Int -> Int -> (Int,Int) -> [(Int,Int)]
siguientesB m n (i,j) = [(i-1,j) | i > 1] ++ [(i,j-1) | j > 1]

-- (posicionesA m n) es la lista cuyo k-ésimo elemento es la lista de
-- las posiciones a donde puede llegar A, en un retícula con m filas y n
-- columnas, después de k pasos. Por ejemplo,
--    λ> posicionesA 4 2
--    [[(1,1)],[(2,1),(1,2)],[(3,1),(2,2)],[(4,1),(3,2)],[(4,2)],[]]
posicionesA :: Int -> Int -> [[(Int,Int)]]
posicionesA m n =
  take (m + n) (iterate (nub . concatMap (siguientesA m n)) [(1,1)])

-- (posicionesB m n) es la lista cuyo k-ésimo elemento es la lista de
-- las posiciones a donde puede llegar B, en un retícula con m filas y n
-- columnas, después de k pasos. Por ejemplo,
--    λ> posicionesB 4 2
--    [[(4,2)],[(3,2),(4,1)],[(2,2),(3,1)],[(1,2),(2,1)],[(1,1)],[]]
posicionesB :: Int -> Int -> [[(Int,Int)]]
posicionesB m n =
  take (m + n) (iterate (nub . concatMap (siguientesB m n)) [(m,n)])
