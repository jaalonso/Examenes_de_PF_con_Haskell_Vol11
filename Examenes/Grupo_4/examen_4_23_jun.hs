-- Informática (1º del Grado en Matemáticas)
-- Tarea evaluable (23 de junio de 2020)
-- =====================================================================

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.List
import Data.Array
import qualified Data.Map as M

-- ---------------------------------------------------------------------
-- Ejercicio 1. Se consideran diccionarios cuyas claves son caracteres y
-- sus valores son listas de números enteros todas ellas de la misma
-- longitud. Por ejemplo,
--    ejDiccionario :: M.Map Char [Int]
--    ejDiccionario = M.fromList [('A', [3, 1, 1]),
--                                ('C', [1, 1, 4]),
--                                ('G', [2, 2, 4]),
--                                ('T', [1, 2, 2])]
-- En el ejemplo,
-- + los elementos en la 1ª posición de los valores son 3, 1, 2, 1; el
--   máximo de ellos es 3 que ocurre como 1º valor de la clave 'A'
-- + los elementos en la 2ª posición de los valores son 1, 1, 2, 2; el
--   máximo de ellos es 2 que ocurre como 2º valor de las claves 'G' y 'T'
-- + los elementos en la 3ª posición de los valores son 1, 4, 4, 2; el
--   máximo de ellos es 4 que ocurre como 3º valor de las claves 'C' y 'G'
--
-- Definir la función
--    clavesMaximas :: M.Map Char [Int] -> [[Char]]
-- tal que (clavesMaximas d) es la lista de las claves con los valores
-- máximos en cada una de sus posiciones. Por ejemplo,
--    clavesMaximas ejDiccionario == ["AGC","AGG","ATC","ATG"]
-- ---------------------------------------------------------------------

import Data.List
import qualified Data.Map as M

ejDiccionario :: M.Map Char [Int]
ejDiccionario = M.fromList [('A', [3, 1, 1]),
                            ('C', [1, 1, 4]),
                            ('G', [2, 2, 4]),
                            ('T', [1, 2, 2])]

clavesMaximas :: M.Map Char [Int] -> [[Char]]
clavesMaximas d = aux (maximosValores d) 0
  where
    aux []     _ = [[]]
    aux (x:xs) k = inserta [c | c <- cs, (d M.! c) !! k == x] (aux xs (k+1))
    cs           = M.keys d

-- (maximosValores d) es la lista de los máximos de los valores de
-- d. Por ejemplo, 
--    maximosValores ejemplo  ==  [3,2,4]
maximosValores :: M.Map Char [Int] -> [Int]
maximosValores d =
  map maximum (transpose (M.elems d))

-- (inserta xs yss) es la lista obtenida insertando los elementos de xs
-- al principio de los de yss. Por ejemplo,
--    inserta [2,3] [[1,5],[7]]  ==  [[2,1,5],[2,7],[3,1,5],[3,7]]
inserta :: [a] -> [[a]] -> [[a]]
inserta xs yss =
  [x : ys | x <- xs, ys <- yss]

-- ---------------------------------------------------------------------
-- Ejercicio 2. Dado un vector de enteros  se consideran sus elementos
-- como las alturas de rectángulos verticales de base 1, empezando desde
-- el origen de coordenadas. Por ejemplo, el vector cuyos elementos son
-- [3,2,5,3,3,4,1,2] representa los siguientes rectángulos
--    
--            +---+
--            | 5 |       +---+
--    +---+   |   |---+---| 4 |
--    | 3 |---|   | 3 | 3 |   |   +---+
--    |   | 2 |   |   |   |   |---| 2 |
--    |   |   |   |   |   |   | 1 |   |
--    +---+-- +---+---+---+---+---+---+
-- 
-- Desde la parte superior de cada rectángulo trazamos una línea
-- horizontal hacia el eje de ordenadas, hasta que o bien se encuentra
-- con otro rectángulo o llega al eje de ordenadas. En el ejemplo
-- anterior, la lista de las longitudes de dichas líneas horizontales es
-- [1,1,3,1,1,3,1,2]. 
--
-- Definir la función
--    horizontales :: Array Int Int -> Array Int Int
-- tal que (horizontales v) es el vector de las longitudes de las líneas
-- horizontales correspondientes a los rectángulos verticales definidos
-- por v. Por ejemplo, 
--    λ> horizontales (listArray (1,8) [3,2,5,3,3,4,1,2])
--    array (1,8) [(1,1),(2,1),(3,3),(4,1),(5,1),(6,3),(7,1),(8,2)]
--    λ> elems (horizontales (listArray (1,8) [3,2,5,3,3,4,1,2]))
--    [1,1,3,1,1,3,1,2]
-- ---------------------------------------------------------------------

-- 1ª solución
-- ===========

horizontales :: Array Int Int -> Array Int Int
horizontales v =
  listArray (bounds v)
            (reverse [1 + length (takeWhile (<x) xs)
                     | (x:xs) <- init (tails (reverse (elems v)))])

-- 2ª solución
-- ===========

horizontales2 :: Array Int Int -> Array Int Int
horizontales2 v = array (1,m) [(i,q ! (i, v!i)) | i <- [1..m]]
  where q = matrizHorizontales v
        (_,m) = bounds v

-- (matrizHorizontales v) es la matriz de orden mxn (donde m es el mayor
-- valor de v y n es la longitud de v) en la que el valor del elemento
-- (i,j) es la máxima longitud desde un segmento de altura j, situado en
-- la posición i. Por ejemplo, 
--    λ> elems (matrizHorizontales (listArray (1,8) [3,2,5,3,3,4,1,2]))
--    [1,1,1,1,1,
--     1,1,1,2,2,
--     1,1,2,3,3,
--     1,1,1,1,1,
--     1,1,1,2,2,
--     1,1,1,3,3,
--     1,1,1,1,4,
--     1,2,2,2,5]
matrizHorizontales :: Array Int Int -> Array (Int,Int) Int
matrizHorizontales v = q 
  where (_,m) = bounds v
        n = maximum (elems v)
        q = array ((1,1), (m,n)) [((i,j), f i j)| i <- [1..m], j <- [1..n]]
          where
            f 1 j = 1
            f i j | j <= v ! (i-1) = 1
                  | otherwise      = 1 + q ! (i-1,j)

-- 3ª solución
-- ===========

horizontales3 :: Array Int Int -> Array Int Int
horizontales3 v = array (1,m) [(i,q ! (i, v!i)) | i <-[1..m]]
  where (_,m) = bounds v
        n = maximum $ elems v
        q = array ((1,1), (m,n)) [((i,j), f i j)| i <-[1..m], j <-[1..n]]
          where
            f 1 j = 1
            f i j | j <= v ! (i-1) = 1
                  | otherwise      = 1 + q ! (i-1,j)

-- Comparación de eficiencia
-- =========================

--    λ> maximum (horizontales (listArray (1,2000) [1..]))
--    2000
--    (0.18 secs, 113,420,880 bytes)
--    λ> maximum (horizontales2 (listArray (1,2000) [1..]))
--    2000
--    (3.70 secs, 2,791,370,264 bytes)
--    λ> maximum (horizontales3 (listArray (1,2000) [1..]))
--    2000
--    (3.72 secs, 2,791,438,072 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 3. Se considera una matriz cuyos elementos son 0 y 1. La
-- posición p1 está conectado con la posición p2 si hay algún camino
-- pasando sólo por 1 desde p1 a p2,  donde los movimientos permitidos
-- son desplazarse una posición hacia abajo o hacia la derecha. Por ejemplo,
-- en la matriz
--    1 0 1 0
--    0 1 0 0
--    1 1 1 1
-- hay un camino desde la (2,2) a la (3,4) pero no hay ninguno de la
-- (1,1) a la (3,4).
--
-- Definir la función
--    conectado :: Array (Int,Int) Int -> (Int,Int) -> (Int,Int) -> Bool
-- tal que (conectado a p1 p2) se verifica si la posición p1 está
-- conectada con la p2 en la matriz a. Por ejemplo,
--    λ> ej = listArray ((1,1),(3,4)) [1,0,1,0,0,1,0,0,1,1,1,1]
--    λ> conectado ej (2,2) (3,4)
--    True
--    λ> conectado ej (1,1) (3,4)
--    False
-- ---------------------------------------------------------------------

conectado :: Array (Int,Int) Int -> (Int,Int) -> (Int,Int) -> Bool
conectado a (i,j) (k,l)
  | (i,j) == (k,l) = True
  | otherwise      = or [ conectado a (i',j') (k,l)
                        | (i',j') <- sucesores a (i,j)]

-- (sucesores a p) es la lista de los sucesores de p en la matriz a. Por
-- ejemplo, 
--    λ> ej = listArray ((1,1),(3,4)) [1,0,1,0,0,1,0,0,1,1,1,1]
--    λ> sucesores ej (2,2)
--    [(3,2)]
--    λ> sucesores ej (1,2)
--    [(2,2),(1,3)]
--    λ> sucesores ej (2,2)
--    [(3,2)]
--    λ> sucesores ej (3,2)
--    [(3,3)]
--    λ> sucesores ej (1,1)
--    []
--    λ> sucesores ej (3,4)
--    []
sucesores :: Array (Int,Int) Int -> (Int,Int) -> [(Int,Int)]
sucesores a (i,j) = [(k,l) | (k,l) <- [(i+1,j),(i,j+1)],
                             inRange d (k,l),
                             a!(k,l) == 1]
  where d = bounds a

-- ---------------------------------------------------------------------
-- Ejercicio 4. El recorrido de una matriz en espiral es el indicado en
-- la figura siguiente:
--     1 ->  2 ->  3 ->  4
--                       |
--                       v 
--     5 ->  6 ->  7     8
--     ^           |     |
--     |           v     v 
--     9    10 <- 11    12 
--     ^                 |
--     |                 v 
--    13 <- 14 <- 15 <- 16 
-- es decir,
--    1, 2, 3, 4, 8, 12, 16, 15, 14, 13, 9, 5, 6, 7, 11, 10
--
-- Ls matrices pueden representarse mediante listas de listas. Por
-- ejemplo, la matriz anterior se puede representar por la lista
--    [[1..4],[5..8],[9..12],[13..16]]
-- 
-- Definir la función
--    espiral :: [[a]] -> [a]
-- tal aue (espiral xss) es el recorrido en espiral de la matriz
-- xss. Por ejemplo,
--    λ> espiral [[1..4],[5..8],[9..12],[13..16]]
--    [1,2,3,4,8,12,16,15,14,13,9,5,6,7,11,10]
--    λ> espiral [[1..3],[4..6],[7..9]]
--    [1,2,3,6,9,8,7,4,5]
--    λ> espiral [[1..4],[5..8],[9..12],[13..16]]
--    [1,2,3,4,8,12,16,15,14,13,9,5,6,7,11,10]
--    λ> espiral ["abc","def","geh"]
--    "abcfhegde"
-- ---------------------------------------------------------------------

espiral :: [[a]] -> [a]
espiral []       = []
espiral (xs:xss) = xs ++ espiral (reverse (transpose xss))

