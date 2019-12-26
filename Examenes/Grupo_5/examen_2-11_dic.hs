-- Informática (1º del Grado en Matemáticas, Grupo 5)
-- 2º examen de evaluación continua (11 de diciembre de 2019)
-- ---------------------------------------------------------------------

import Data.List
import Data.Char

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Un número entero se dirá un cubo perfecto si es el
-- cubo de un entero positivo; y se dirá un casicubo si es el anterior o
-- el posterior de un cubo perfecto.
-- 
-- Definir la lista infinita
--    casiCubos :: [Integer]
-- de los enteros casicubos. Por ejemplo,
--    λ> take 15 casiCubos
--    [0,2,7,9,26,28,63,65,124,126,215,217,342,344,511]
-- ---------------------------------------------------------------------

casiCubos :: [Integer]
casiCubos = concat [[x^3-1,x^3+1] | x <- [1..]]

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Calcular en qué lugar de la lista casiCubos aparece el
-- casicubo 175615. 
-- ---------------------------------------------------------------------

-- El cálculo es
-- Consulta: 1 + length (takeWhile (/=175615) casiCubos)
--    λ> 1 + length (takeWhile (/=175615) casiCubos)
--    111
--    λ> casiCubos !! 111
--    175617

-- ---------------------------------------------------------------------
-- Definir la función
--     menorRep :: Integer -> Integer
-- tal que (menorRep x) devuelve el menor casicubo cuyos dígitos
-- contienen al menos una cifra distinta del cero y del nueve con x o más
-- repeticiones consecutivas. Por ejemplo,
--    menorRep 2  ==  344
--    menorRep 3  ==  195111
--    menorRep 5  ==  7111117466
-- ---------------------------------------------------------------------

menorRep :: Integer -> Integer
menorRep x =
  head [y | y <- casiCubos,
            any propiedad (group (show y))]
  where propiedad (y:ys) = y /= '0' &&
                           y /= '9' &&
                           genericLength (y:ys) == x

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    palabrasMasLargas :: [String] -> [String]
-- (palabrasMasLargas xs) es la lista de las palabras de mayor longitud
-- en xs sin contar letras repetidas ni diferencias entre mayúsculas
-- y minúsculas. Por ejemplo,
--    λ> palabrasMasLargas ["Ana", "Osa", "ABC", "Lara"]
--    ["Osa","ABC","Lara"]
-- ---------------------------------------------------------------------

palabrasMasLargas :: [String] -> [String]
palabrasMasLargas css =
  [cs | cs <- css, longitudReducida cs == m]
  where longitudReducida = length . nub . map toLower
        m = maximum (map longitudReducida css)

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Una secuencia de números pseudo-aleatorios se puede
-- calcular como sigue_
--   + el primer término es:   X(0) = s
--   + el término n-ésimo es:  X(n) = (a*X(n-1)+c) módulo m 
-- donde s (semilla), a (multiplicador), c (incremento) y m (módulo)
-- son números enteros no negativos.
-- 
-- Definir la función
--    aleatorio :: Integer -> Integer -> Integer -> Integer -> [Integer]
-- tal que (aleatorio s a m c) es la sucesión de números
-- pseudo-aleatorios usando s como semilla, a como multiplicador, m como
-- módulo y c como incremento. Por ejemplo,  
--    λ> take 20 (aleatorio 0 1 10 3)
--    [0,3,6,9,2,5,8,1,4,7,0,3,6,9,2,5,8,1,4,7]
--    λ> take 10 (aleatorio 4 3 (2^25) 0)
--    [4,12,36,108,324,972,2916,8748,26244,78732]
-- ---------------------------------------------------------------------

aleatorio :: Integer -> Integer -> Integer -> Integer -> [Integer]
aleatorio s a m c = iterate (\x -> (a*x+c) `mod` m) s

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. El periodo de una secuencia de números
-- pseudo-aleatorios es el número de términos que aparecen en la
-- secuencia antes de que se repita la semilla de la misma.
-- 
-- Definir la función
--    periodo :: Integer -> Integer -> Integer -> Integer -> Integer
-- tal que (periodo s a m c) es el periodo de la secuencia
-- pseudo-aleatoria correpondiente. Por ejemplo,
--    λ> periodo 0 1 10 3
--    10
--    λ> periodo 4 3 (2^25) 0
--    2097152
-- ---------------------------------------------------------------------

periodo :: Integer -> Integer -> Integer -> Integer -> Integer
periodo s a m c =
  1 + genericLength (takeWhile (/=s) (tail (aleatorio s a m c)))

-- ----------------------------------------------------------------------
-- Ejercicio 4. La codificación unaria de un número n estrictamente
-- positivo se realiza poniendo un cero seguido n unos. Así por ejemplo,
-- el 1 es 01, el 2 es 011, el 3 es 0111, ...
-- 
-- Definir la función
--    unario :: [Int] -> [Int]
-- tal que (unario xs) es la codificación unaria de los números de xs.
-- Por ejemplo,
--    λ> unario [1..5]
--    [0,1,0,1,1,0,1,1,1,0,1,1,1,1,0,1,1,1,1,1]
--    λ> unario [3,1,1,2]
--    [0,1,1,1,0,1,0,1,0,1,1]
-- ---------------------------------------------------------------------

unario :: [Int] -> [Int]
unario = concatMap aux
  where aux x = 0 : replicate x 1

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Definir la función
--    unarioValido :: [Int] -> Bool
-- tal que (unarioValido xs) se verifica si es xs es un código  unario
-- válido; es decir, que comience por 0, acabe por 1, y entre dos 0 debe
-- haber al menos un 1. Por ejemplo,
--    λ> unarioValido [0,1,0,1,1,0,1,1,1,0,1,1,1,1,0,1,1,1,1,1]
--    True
--    λ> unarioValido [3,1,1,2]
--    False
--    λ> unarioValido [1,1,0]
--    False
--    λ> unarioValido [0,1,0,0,1]
--    False

unarioValido :: [Int] -> Bool
unarioValido [] = True
unarioValido (x:xs)
  | x == 0    = not (null (takeWhile (==1) xs)) &&
                unarioValido (dropWhile (==1) xs)
  | otherwise = False
