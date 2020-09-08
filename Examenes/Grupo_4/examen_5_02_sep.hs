-- Informática (1º del Grado en Matemáticas)
-- Examen 2ª convocatoria (2 de septiembre de 2020)
-- =====================================================================

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.Matrix
import Data.List
import Data.Numbers.Primes
import qualified Data.Set as S

-- ---------------------------------------------------------------------
-- Ejercicio 1. Empezando con el número 1 y moviéndose en el sentido de
-- las agujas del reloj se obtienen las matrices espirales. Por ejemplo, 
--    |1|   |1 2|   |7 8 9|   | 7  8  9 10|   |21 22 23 24 25|
--          |4 3|   |6 1 2|   | 6  1  2 11|   |20  7  8  9 10|
--                  |5 4 3|   | 5  4  3 12|   |19  6  1  2 11|
--                            |16 15 14 13|   |18  5  4  3 12|
--                                            |17 16 15 14 13|
-- son las matrices espirales de orden 1, 2 ,3 , 4 y 5.
-- 
-- Definir la función
--    matrizEspiral :: Int -> Matrix Int
-- tal que (matrizEspiral n) es la matriz espiral de orden n. Por
-- ejemplo,
--    λ> matrizEspiral 5
--    ┌                ┐
--    │ 21 22 23 24 25 │
--    │ 20  7  8  9 10 │
--    │ 19  6  1  2 11 │
--    │ 18  5  4  3 12 │
--    │ 17 16 15 14 13 │
--    └                ┘
-- ---------------------------------------------------------------------

matrizEspiral :: Int -> Matrix Int
matrizEspiral 1 = fromList 1 1 [1]
matrizEspiral n
  | odd n     = f1 <-> b
  | otherwise = b' <-> fn
  where a   = matrizEspiral (n-1)
        m   = (n-1)^2
        c1s = take (n-1) [m+1..]
        c1  = fromList (n-1) 1 (reverse c1s)
        b   = c1  <|> a
        f1s = take n $ drop (n-1) [m+1..]
        f1  = fromList 1 n f1s
        cn  = fromList (n-1) 1 c1s
        b'  = a <|> cn
        fn  = fromList 1 n (reverse f1s)

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. El número 6 se puede descomponer de 8 forma tales que
-- la lista de sus sumandos sea un palíndromo (es decir, que se lee
-- igual en ambos sentidos) como se muestra a continuación 
--    6 = 6
--    6 = 1 + 4 + 1             
--    6 = 1 + 1 + 2 + 1 + 1     
--    6 = 1 + 1 + 1 + 1 + 1 + 1 
--    6 = 1 + 2 + 2 + 1         
--    6 = 2 + 2 + 2             
--    6 = 2 + 1 + 1 + 2         
--    6 = 3 + 3                 
--
-- Definir la función
--    descomposiciones :: Int -> [[Int]]
-- tal que (descomposiciones n) es la lista de los sumandos de las
-- descomposiciones de n que son palíndromas. Por ejemplo,
--    λ> descomposiciones 6
--    [[6],[1,4,1],[1,1,2,1,1],[1,1,1,1,1,1],[2,1,1,2],[1,2,2,1],[2,2,2],[3,3]]
--    λ> descomposiciones 5
--    [[5],[1,3,1],[1,1,1,1,1],[2,1,2]]
--    λ> descomposiciones 1
--    [[1]]
--    λ> length (descomposiciones2 30)
--    32768
-- ---------------------------------------------------------------------

-- 1ª solución
-- ===========

descomposiciones :: Int -> [[Int]]
descomposiciones n =
  filter esPalindromo (descomposicionesSuma n)

-- (descomposicionesSuma n) es la lista de las descomposiciones de n
-- como suma de números naturales. Por ejemplo,
--    descomposicionesSuma 3  ==  [[3],[1,2],[1,1,1],[2,1]]
descomposicionesSuma 1 = [[1]]
descomposicionesSuma n =
  [n]: [n-x:ys | x <- [1..n-1], ys <- descomposicionesSuma x]

-- (esPalindromo xs) se verifica si la lista xs es un palíndromo. Por
-- ejemplo, 
--    esPalindromo [1,2,4,2,1]  ==  True
--    esPalindromo [1,2,2,4,1]  ==  False
esPalindromo :: [Int] -> Bool
esPalindromo xs = reverse xs == xs

-- 2ª solución
-- ===========

descomposiciones2 :: Int -> [[Int]]
descomposiciones2 = reverse . sort . descomposiciones2'

descomposiciones2' :: Int -> [[Int]]
descomposiciones2' 1 = [[1]]
descomposiciones2' n = [n] : concatMap (aux n) [1..n `div` 2]
  where aux n d | n == 2*d  = [[d,d]]
                | otherwise = map (f d) (descomposiciones2' (n - 2*d))
        f d xs = d : xs ++ [d]

-- Comparación de eficiencia
-- =========================

--    λ> length (descomposiciones 22)
--    2048
--    (6.01 secs, 6,761,385,360 bytes)
--    λ> length (descomposiciones2 22)
--    2048
--    (0.01 secs, 7,302,584 bytes)

-- --------------------------------------------------------------------
-- Ejercicio 2.2. Calcular el menor número que tiene más de cien mil
-- descomposiciones cuyas lista de sumandos son palíndromos.
-- ---------------------------------------------------------------------

-- El cálculo es
--    λ> head [n | n <- [1..], length (descomposiciones n) > 10^5]
--    34

-- ---------------------------------------------------------------------
-- Ejercicio 3. Las expresiones aritméticas con variables pueden
-- representarse usando el siguiente tipo de datos  
--    data Expr = C Int 
--               | V Char 
--               | S Expr Expr
--               | P Expr Expr  
--               deriving Show
-- Por ejemplo, la expresión 2*(a+5) se representa por
--    P (C 2) (S (V 'a') (C 5))
--
-- Una expresión está reducida si no contiene una operación en la que
-- los dos operandos son números. Por ejemplo, la expresión 
-- (S (V 'x') (P (C 4) (C 5))) no está reducida porque los dos operandos
-- de (P (C 4) (C 5)) son números; en cambio, la expresión
-- (S (V 'x') (C 20)) está reducida.
-- 
-- Definir la función
--    reduce :: Expr -> Expr
-- tal que (reduce e) es una expresión reducida eqiovalente a e. Por
-- ejemplo, 
--    reduce (S (C 3) (C 4))             == C 7
--    reduce (S (C 3) (V 'x'))           == S (C 3) (V 'x')
--    reduce (S (C 3) (P (C 4) (C 5)))   == C 23
--    reduce (S (V 'x') (P (C 4) (C 5))) == S (V 'x') (C 20)
--    reduce (S (C 3) (P (V 'x') (C 5))) == S (C 3) (P (V 'x') (C 5))
--    reduce (C 3)                       == C 3
--    reduce (V 'x')                     == V 'x'
-- ---------------------------------------------------------------------

data Expr = C Int 
           | V Char 
           | S Expr Expr 
           | P Expr Expr  
           deriving Show

reduce :: Expr -> Expr
reduce e | reducible e = aux e
         | otherwise   = e
  where aux (S (C m) (C n)) = C (m+n)
        aux (S a b)         = reduce (S (aux a) (aux b))
        aux (P (C m) (C n)) = C (m*n)
        aux (P a b)         = reduce (P (aux a) (aux b))
        aux e               = e

-- (reducible e) se verifica si e es una expresión reducible; es decir,
-- contiene una operación en la que los dos operandos son números. Por
-- ejemplo, 
--    reducible (S (C 3) (C 4))             == True
--    reducible (S (C 3) (V 'x'))           == False
--    reducible (S (C 3) (P (C 4) (C 5)))   == True
--    reducible (S (V 'x') (P (C 4) (C 5))) == True
--    reducible (S (C 3) (P (V 'x') (C 5))) == False
--    reducible (C 3)                       == False
--    reducible (V 'x')                     == False
reducible :: Expr -> Bool
reducible (C _)           = False
reducible (V _)           = False
reducible (S (C _) (C _)) = True
reducible (S a b)         = reducible a || reducible b
reducible (P (C _) (C _)) = True
reducible (P a b)         = reducible a || reducible b

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. Un conjunto de primos concatenables es un conjunto S de
-- números  primos tales que al concatenar cualquier par de elementos de
-- S se obtiene un número primo. Por ejemplo, {3, 7, 109, 673} es un
-- conjunto de primos concatenables ya que sus elementos son primos y las
-- concatenaciones de sus parejas son 37, 3109, 3673, 73, 7109, 7673,
-- 1093, 1097, 109673, 6733, 6737 y 673109 y todos son primos.
--
-- Definir la función
--    concatenables :: Integer -> Integer -> [[Integer]]
-- tal que (concatenables n m) es el conjunto de los conjuntos
-- concatenables de n elementos menores que m. Por ejemplo,
--    take 5 (concatenables 2   10)  ==  [[3,7]]
--    take 5 (concatenables 3   10)  ==  []
--    take 5 (concatenables 2  100)  ==  [[3,7],[3,11],[3,17],[3,31],[3,37]]
--    take 5 (concatenables 3  100)  ==  [[3,37,67],[7,19,97]]
--    take 5 (concatenables 4  100)  ==  []
--    take 5 (concatenables 4 1000)  ==  [[3,7,109,673],[23,311,677,827]]
-- ---------------------------------------------------------------------

-- 1ª definición
-- =============

concatenables :: Integer -> Integer -> [[Integer]]
concatenables 0 _ = [[]]
concatenables n m = 
  nub [sort (x:xs) | x <- takeWhile (<=m) primes,
                     xs <- xss,
                     all (x `emparejable`) xs]
  where xss = concatenables (n-1) m

emparejable :: Integer -> Integer -> Bool
emparejable x y =
  isPrime (concatenacion x y) &&
  isPrime (concatenacion y x)

concatenacion :: Integer -> Integer -> Integer
concatenacion x y =
  read (show x ++ show y)

-- 2ª definición
-- =============

concatenables2 :: Integer -> Integer -> [[Integer]]
concatenables2 n m = map reverse (aux n m)
  where aux 1 m = [[x] | x <- takeWhile (<=m) primes]
        aux n m = 
            [p:ys | ys@(x:xs) <- xss,
                    p <- dropWhile (<x) ps,
                    all (p `emparejable`) ys]
          where ps  = takeWhile (<=m) primes
                xss = aux (n-1) m

-- 3ª definición
-- =============

concatenables3 :: Integer -> Integer -> [[Integer]]
concatenables3 n m = map S.toList (aux n m)
  where aux 1 m = [S.singleton x | x <- takeWhile (<=m) primes]
        aux n m = [S.insert x xs | x <- takeWhile (<=m) primes,
                                   xs <- xss,
                                   all (x `emparejable`) xs]
          where xss = aux (n-1) m

-- 4ª definición
-- =============

concatenables4 :: Integer -> Integer -> [[Integer]]
concatenables4 n m = map S.toList (aux n m)
  where aux 1 m = [S.singleton x | x <- takeWhile (<=m) primes]
        aux n m = 
            [S.insert p ys | ys <- xss,
                             let (x,xs) = S.deleteFindMax ys,
                             p <- dropWhile (<x) ps,
                             all (p `emparejable`) ys]
          where ps  = takeWhile (<=m) primes
                xss = aux (n-1) m

-- Comparación de eficiencia
-- =========================

--    λ> head (concatenables 4 1000)
--    [3,7,109,673]
--    (20.36 secs, 11,781,891,120 bytes)
--    
--    λ> head (concatenables2 4 1000)
--    [3,7,109,673]
--    (0.02 secs, 0 bytes)
--    
--    λ> head (concatenables3 4 1000)
--    [3,7,109,673]
--    (38.04 secs, 21,542,334,024 bytes)
--    
--    λ> head (concatenables4 4 1000)
--    [3,7,109,673]
--    (0.03 secs, 0 bytes)

-- --------------------------------------------------------------------
-- Ejercicio 4.2. Calcular la cantidad de conjuntso conjuntos
-- concatenables de 1 2 ó 3 elementos menores que 1000.
-- ---------------------------------------------------------------------

-- El cálculo es
--   λ> sum [length (concatenables x 1000) | x <- [1..3]]
--   1149


