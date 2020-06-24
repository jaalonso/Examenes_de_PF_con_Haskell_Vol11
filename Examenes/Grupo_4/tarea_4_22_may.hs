-- Informática (1º del Grado en Matemáticas)
-- Tarea del 22 de mayo de 2020
-- =====================================================================

import I1M.PolOperaciones
import Data.Array

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    raicesApol :: (Num a, Eq a) => [(a,Int)] -> Polinomio a
-- tal que (raicesApol xs) es el polinomio p tal que el xs s la lista de
-- los pares (x,m), donde x representa una raíz de p y m su
-- multiplicidad. Por ejemplo,
--    λ> raicesApol [(3,2)]
--    x^2 + -6*x + 9
--    λ> raicesApol [(2,3)]
--    x^3 + -6*x^2 + 12*x + -8
--    λ> raicesApol [(1,2),(2,1)]
--    x^3 + -4*x^2 + 5*x + -2
-- ---------------------------------------------------------------------

raicesApol :: (Num a, Eq a) => [(a,Int)] -> Polinomio a
raicesApol xs = foldr multPol polUnidad ps
  where ps = [potencia (consPol 1 1 (consPol 0 (-x) polCero)) m | (x,m) <- xs]

-- (potencia p n) es a n-ésima potencia del polinomio p. Por ejemplo,
--    λ> p = consPol 3 2 (consPol 0 10 polCero)
--    λ> p
--    2*x^3 + 10
--    λ> potencia p 2
--    4*x^6 + 40*x^3 + 100
--    λ> potencia p 3
--    8*x^9 + 120*x^6 + 600*x^3 + 1000
potencia :: (Num a, Eq a) => Polinomio a -> Int -> Polinomio a
potencia p 0 = polUnidad
potencia p n = multPol p (potencia p (n-1))

-- ---------------------------------------------------------------------
-- Ejercicio 2. Dada una varilla de longitud n y un conjunto de precios
-- que contiene los precios de todas las piezas de tamaño inferior a
-- n, el problema consiste en determinar el valor máximo que se puede
-- obtener cortando la varilla y vendiendo las piezas. Por ejemplo, si
-- la longitud de la varilla es 8 y los valores de las diferentes piezas
-- se dan por la siguiente tabla
--    longitud   | 1   2   3   4   5   6   7   8  
--    -----------+--------------------------------
--    precio     | 1   5   8   9  10  17  17  20
-- entonces el valor máximo que se puede obtener es 22 (cortando en dos
-- piezas de longitudes 2 y 6). Y si los precios son los de la siguiente
-- tabla 
--    longitud   | 1   2   3   4   5   6   7   8  
--    -----------+--------------------------------
--    precio     | 3   5   8   9  10  17  17  20
-- el valor máximo es 24 (cortando en 8 piezas de longitud 1).
--
-- Las dos tablas anteriores se pueden representar mediante los
-- siguientes vectores
--    v1, v2 :: Array Int Int
--    v1 = listArray (1,8) [1,5,8,9,10,17,17,20]
--    v2 = listArray (1,8) [3,5,8,9,10,17,17,20]
--
-- Definir, mediante programación dinámica, la función
--    mejorPrecio :: Array Int Int -> Int
-- tal que (mejorPrecio v) es el valor máximo que se puede obtener
-- cortando la varilla cuya tabla de precios representada por el vector
-- y vendiendo las piezas. Por ejemplo,
--    mejorPrecio v1 == 22
--    mejorPrecio v2 == 24
-- ---------------------------------------------------------------------

v1, v2 :: Array Int Int
v1 = listArray (1,8) [1,5,8,9,10,17,17,20]
v2 = listArray (1,8) [3,5,8,9,10,17,17,20]

-- Por recursión
-- =============

mejorPrecioR :: Array Int Int -> Int
mejorPrecioR v = aux v n where
  n = snd (bounds v) 
  aux v 0 = 0
  aux v 1 = v!1
  aux v j = maximum [v!i + aux v (j-i) | i <- [1..j]]

-- Por programación dinámica
-- =========================

mejorPrecio :: Array Int Int -> Int
mejorPrecio v = (vectorMejorPrecio v) ! n
  where n = snd (bounds v)

-- (vectorMejorPrecio v) es el vector 
--    λ> vectorMejorPrecio v1
--    array (0,8) [(0,0),(1,1),(2,5),(3,8),(4,10),(5,13),(6,17),(7,18),(8,22)]
--    λ> vectorMejorPrecio v2
--    array (0,8) [(0,0),(1,3),(2,6),(3,9),(4,12),(5,15),(6,18),(7,21),(8,24)]
vectorMejorPrecio :: Array Int Int -> Array Int Int
vectorMejorPrecio v = w where
  n = snd (bounds v)
  w = array (0,n) [(i, f i) | i <- [0..n]]
  f 0 = 0
  f 1 = v!1
  f j = maximum [v!i + w!(j-i) | i <-[1..j]]
