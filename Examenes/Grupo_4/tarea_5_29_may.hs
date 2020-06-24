-- Informática (1º del Grado en Matemáticas)
-- Tarea del 29 de mayo de 2020
-- =====================================================================

-- ---------------------------------------------------------------------
-- § Indicaciones                                                     --
-- ---------------------------------------------------------------------

-- Es necesario que se pueda cargar el fichero y documentar todas las
-- funciones; es decir,
-- 1. Elegir un nombre para sugerir lo que hace
-- 2. Escribir, en lenguaje natural, qué hace la función y no cómo lo hace. 
-- 3. Poner ejemplos de uso.

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.List
import Data.Map
import Data.Numbers.Primes

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Definir la función
--    ficheroNumeros :: FilePath -> Int -> IO ()
-- tal que (ficheroNumeros f n) escribe en el fichero f el número
-- obtenido concatenando los dígitos de los números primos menores que
-- n. Por ejemplo,  
--    ficheroNumeros "numeros.txt" 100
-- crea el fichero "numeros.txt" cuyo contenido es
--    2357111317192329313741434753596167717379838997
-- ---------------------------------------------------------------------

ficheroNumeros :: FilePath -> Int -> IO ()
ficheroNumeros f n =
  writeFile f (primosConcatenados n)

-- (primosConcatenados n) es la cadena obtenida concatenando los
-- dígitos de los números primos menores que n. Por ejemplo,  
--    λ> primosConcatenados 100
--    "2357111317192329313741434753596167717379838997"
primosConcatenados :: Int -> String
primosConcatenados n = concatMap show (takeWhile (< n) primes)

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Definir la función
--    posiciones :: FilePath -> Int -> IO [Int]
-- tal que (posiciones f n) es la lista de las posiciones de n en el
-- número contenido en el fichero f. Por ejemplo,
--    λ> ficheroNumeros "numeros.txt" 100
--    λ> posiciones "numeros.txt" 3
--    [1,7,13,16,18,23,27,37,41]
--    λ> ficheroNumeros "numeros.txt" 1000
--    λ> posiciones "numeros.txt" 12
--    [61,111,129,132,144,150]
--    λ> posiciones "numeros.txt" 123
--    []
-- ---------------------------------------------------------------------

posiciones :: FilePath -> Int -> IO [Int]
posiciones f n = do
  ds <- readFile f
  return (posicionesEnLista ds (show n))

-- (posicionesEnLista xs ys) es la lista de las posiciones de ys en
-- xs. Por ejemplo, 
--    λ> posicionesEnLista "2315145156" "15" 
--    [2,7]
posicionesEnLista :: Eq a => [a] -> [a] -> [Int]
posicionesEnLista xs ys = aux xs 0
  where aux [] _ = []
        aux (x:xs) n | ys `isPrefixOf` (x:xs) = n : aux xs (n+1)
                     | otherwise              = aux xs (n+1)

-- ---------------------------------------------------------------------
-- Ejercicio 2. El término
--    maximum (sum [3, y, product [2, z], 7]
-- se puede representar por
--    T "M" [T "S" [N 3, V 'y', T "P" [N 2, V 'z']], N 7
-- donde maximum se representa por "M", sum por "S" y product por "P".
-- Además, el valor del término anterior cuando 'y' se interpreta por 5
-- y 'z' por 2 es 12.
--
-- En general, los términos aritméticos como los anteriores se pueden
-- definir por 
--    data Termino = V Char
--                 | N Int
--                 | T String [Termino]
--      deriving Show
-- las interpretaciones de las variables son diccionarios de caracteres
-- en enteros
--    type InterpretacionVariables = Map Char Int
-- las interpretaciones de las operaciones son diccionarios de cadenas
-- en funciones de listas de enteros en enteros
--    type InterpretacionOperaciones = Map String ([Int] -> Int)
-- y las interpretaciones son pares formados por interpretaciones de
-- variables y de operaciones
--    type Interpretacion = (InterpretacionVariables, InterpretacionOperaciones)
-- Por ejemplo, 
--    ejInt1, ejInt2 :: Interpretacion
--    ejInt1 = (fromList [('x',3),('y',5),('z',2)],
--              fromList [("S", sum), ("P", product), ("M", maximum)])
--    ejInt2 = (fromList [('x',1),('y',2),('z',3)],
--              fromList [("S", sum), ("P", product), ("M", minimum)])
-- donde ejInt1 es la interpretación usada anteriormente.
--
-- Definir la función
--    valor :: Termino -> Interpretacion -> Int
-- tal que (valor t i) es el valor del término t en la interpretación
-- i. Por ejemplo,
--    λ> valor (T "M" [T "S" [N 3, V 'y', T "P" [N 2, V 'z']], N 7]) ejInt1
--    12
--    λ> valor (T "M" [T "S" [N 3, V 'y', T "P" [N 2, V 'z']], N 7]) ejInt2
--    7
-- ---------------------------------------------------------------------

data Termino = V Char
             | N Int
             | T String [Termino]
  deriving Show

type InterpretacionVariables = Map Char Int

type InterpretacionOperaciones = Map String ([Int] -> Int)

type Interpretacion = (InterpretacionVariables, InterpretacionOperaciones)

ejInt1, ejInt2 :: Interpretacion
ejInt1 = (fromList [('x',3),('y',5),('z',2)],
          fromList [("S", sum), ("P", product), ("M", maximum)])
ejInt2 = (fromList [('x',1),('y',2),('z',3)],
          fromList [("S", sum), ("P", product), ("M", minimum)])
               
valor :: Termino -> Interpretacion -> Int
valor (V x) i    = fst i ! x
valor (N n) _    = n
valor (T o ts) i = (snd i ! o) [valor t i | t <- ts]
