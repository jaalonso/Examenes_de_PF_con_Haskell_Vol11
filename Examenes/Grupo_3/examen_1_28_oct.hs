-- Informática (1º del Grado en Matemáticas, Grupo 3)
-- 1º examen de evaluación continua (28 de octubre de 2019)      
-- =====================================================================

-- ---------------------------------------------------------------------
-- Ejercicio 1. Supongamos un tablero de ajedrez infinito de casillas de
-- tamaño 1x1, con una casilla negra situada entre las posiciones (0,0),
-- (1,0), (0,1) y (1,1). El tablero se extiende en todas direcciones
-- abarcando todo el plano. De esta forma, cada punto del plano está
-- pintado de color negro (el interior de las casillas negras y los
-- bordes) o de color blanco (el interior de las casillas blancas). Por
-- ejemplo, el punto (0.5,0.5) se encuentra pintado de negro (en el
-- interior de una casilla negra), el punto (0.5,1) también se encuentra
-- pintado de negro (en el borde de las casillas) y el punto (0.5,1.5)
-- se encuentra pintado de blanco (en el interior de una casilla blanca). 
--
-- Definir la función
--   posicionNegra :: (Double,Double) -> Bool
-- tal que (posicionNegra (x,y)) se verifica si el punto (x,y) se encuentra
-- pintado de negro en el tablero de ajedrez infinito. Por ejemplo,
--   posicionNegra (0.5,0.5)    ==  True
--   posicionNegra (0.5,1)      ==  True
--   posicionNegra (0.5,1.5)    ==  False
--   posicionNegra (-0.5,0.5)   ==  False
--   posicionNegra (0.5,-0.5)   ==  False
--   posicionNegra (-0.5,-0.5)  ==  True
-- ----------------------------------------------------------------------------

-- 1ª solución
posicionNegra :: (Double,Double) -> Bool
posicionNegra (x,y)
  | floor x == ceiling x = True
  | floor y == ceiling y = True
  | even (floor x + floor y) = True
  | otherwise = False

-- 2ª solución
posicionNegra2 :: (Double,Double) -> Bool
posicionNegra2 (x,y) =
     floor x == ceiling x
  || floor y == ceiling y
  || even (floor x + floor y) 

-- ---------------------------------------------------------------------
-- Ejercicio 2. Una lista de números [a(1),a(2),...] se denomina
-- posicionalmente coprima si para cada i ∈ {1, 2,...} se cumple que i y
-- a(i) son coprimos, es decir, el máximo común divisor de a(i) e i es
-- 1. Por ejemplo, la lista [3,5,7] es posicionalmente coprima pues 3 es
-- coprimo con 1, 5 es coprimo con 2 y 7 es coprimo con 3; por otro
-- lado, la lista [2,4,6] no es posicionalmente coprima pues 4 no es
-- coprimo con 2, ni 6 es coprimo con 3.
--
-- Definir la función
--    posicionalmenteCoprima :: [Integer] -> Bool
-- tal que (posicionalmenteCoprima xs) se cumple si la lista xs es
-- posicionalmente coprima. Por ejemplo,
--    posicionalmenteCoprima [3,5,7]  ==  True
--    posicionalmenteCoprima [2,4,6]  ==  False
--    posicionalmenteCoprima []       ==  True
--    posicionalmenteCoprima [6]      ==  True
-- ----------------------------------------------------------------------------

-- 1ª solución
posicionalmenteCoprima :: [Integer] -> Bool
posicionalmenteCoprima xs =
  and [gcd x i == 1 | (x,i) <- zip xs [1..]]

-- 2ª solución
posicionalmenteCoprima2 :: [Integer] -> Bool
posicionalmenteCoprima2 xs = aux xs [1..]
  where aux [] _          = True
        aux (x:xs) (n:ns) = gcd x n == 1 && aux xs ns

-- ---------------------------------------------------------------------
-- Ejercicio 3. Una forma de aproximar el valor del número π es usando
-- la siguiente igualdad: 
--
--           π     4     16     36     64
--          --- = --- * ---- * ---- * ---- * ...
--           2     3     15     35     63
--
-- Es decir, el producto de los términos de la secuencia cuyo término general
-- n-ésimo es: 
--                    4*k^2
--           s(k) = ---------, para k > 0
--                   4*k^2-1
--
-- Definir la función
--    aproximaPi :: Double -> Double
-- tal que (aproximaPi n) es la aproximación del número π calculada con la
-- serie anterior hasta el término n-ésimo (contando desde 0). Por ejemplo,
--    aproximaPi 10    ==  3.0677038066434985
--    aproximaPi 100   ==  3.133787490628162
--    aproximaPi 1000  ==  3.140807746030402
-- ----------------------------------------------------------------------------

-- 1ª definición
aproximaPi :: Double -> Double
aproximaPi n =
  2 * product [(4*k^2) / (4*k^2-1) | k <- [1..n]]

-- 2ª definición
aproximaPiR :: Double -> Double
aproximaPiR 0 = 2
aproximaPiR n = aproximaPiR (n-1) * 4 * n^2 / (4*n^2-1)
   
-- ---------------------------------------------------------------------
-- Ejercicio 4. La mecánica habitual para sumar números naturales
-- consiste en sumar todas las cifras que ocupan la misma posición,
-- tomadas en  orden desde las unidades, acumulando una 'llevada' desde
-- una posición a la siguiente. Consideremos un proceso de 'suma
-- sesgada' que no acumula la llevada, por ejemplo la suma ordinaria de
-- 1357 y 579 es 1936, sin embargo el valor de la suma sesgada es 1826:
--
--        Suma con llevada:                Suma sesgada:
--              ¹ ¹
--            1 3 5 7                          1 3 5 7  
--         +    5 7 9                       +    5 7 9  
--        ------------                     ------------ 
--            1 9 3 6                          1 8 2 6  
--
-- Definir la función:
--    sumaSesgada :: Integer -> Integer -> Integer
-- tal que (sumaSesgada n m) es la 'suma sesgada' de los números naturales
-- n y m. Por ejemplo,
--    sumaSesgada 1357 579  ==  1826
--    sumaSesgada 9999 999  ==  9888
--    sumaSesgada 0 1234    ==  1234
-- ----------------------------------------------------------------------------

-- 1ª solución
-- ===========

sumaSesgada :: Integer -> Integer -> Integer
sumaSesgada 0 m = m
sumaSesgada n 0 = n
sumaSesgada n m =
  sumaSesgada (div n 10) (div m 10) * 10 + mod (mod n 10 + mod m 10) 10

-- 2ª solución
-- ===========

sumaSesgada2 :: Integer -> Integer -> Integer
sumaSesgada2 n m =
  sum [10^k * ((x + y) `mod` 10)
      | (k,x,y) <- zip3 [c-1,c-2..]
                   (replicate (c-a) 0 ++ xs)
                   (replicate (c-b) 0 ++ ys)]
  where xs = digitos n
        ys = digitos m
        a  = length xs
        b  = length ys
        c  = max a b

digitos :: Integer -> [Integer]
digitos n = [read [c] | c <- show n] 
