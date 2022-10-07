{- 
1)
Implementar una función en Haskell que reciba dos parámetros: un número entero
nro, y otro número entero exp. La función deberá devolver True, en caso de que el número nro sea
divisible por el número exp. En caso de que el número nro no sea divisible por el número exp la
función deberá devolver False.
-}
esDivisible :: (Int, Int) -> Bool
esDivisible(a, b) | (a `rem` b) == 0 = True
                  | otherwise = False


{-
2)
Implementar una función en Haskell que reciba un número entero n. La función deberá
generar una lista con todos los números enteros mediante los cuales el número n sea divisible,
incluyendo al 1 y al propio número n entre tales factores. Realizar las validaciones necesarias. Utilizar
recursividad. Reutilizar la función de la consigna 1).
-}

listaFactoresDivisiblesDe :: Int -> [Int]
listaFactoresDivisiblesDe n = listaDivisores(n, n)

listaDivisores :: (Int, Int) -> [Int]
listaDivisores (a, 0) = []
listaDivisores (a, 1) = [1]
listaDivisores (a, b) = if esDivisible(a, b) 
                        then b:listaDivisores(a, b-1)
                        else listaDivisores(a, b-1)

{-
3)
Implementar una función en Haskell que reciba un número entero nro. La función
deberá devolver True, en caso de que el número nro sea un número primo. En caso de que el
número nro no sea primo, la función deberá devolver False. Utilizar recursividad y reutilizar la
función de la consigna 1).
-}

esPrimo :: Int -> Bool
esPrimo n   | length(listaFactoresDivisiblesDe n) == 2 = True
            | otherwise = False

{- 
4)
Implementar una función en Haskell que reciba un número entero n. La función deberá
generar una lista con todos los números primos menores o iguales que n. Utilizar lista por
comprensión. Reutilizar la función de la consigna 2.
-}

listaPrimosMenorIgualA :: Int -> [Int]
listaPrimosMenorIgualA n = [x | x <- [1..n], esPrimo(x)]

{-
5)
Implementar una función en Haskell que reciba un número entero n. La función deberá
generar una lista con todos los factores primos del número n. Es decir, en este caso, el producto de
todos los factores de la lista a generar deberá ser igual a n. Reutilizar las funciones necesarias.
-}

listaFactoresPrimosDe :: Int -> [Int]
listaFactoresPrimosDe n = [x | x <- listaPrimosMenorIgualA n, esDivisible(n, x)]

primerFactorPrimoDe :: Int -> Int
primerFactorPrimoDe n = head(listaFactoresPrimosDe n)

primeraFactorizacion :: Int -> Int
primeraFactorizacion n = div n (primerFactorPrimoDe n)

factorizar :: Int -> [Int]
factorizar n | n < 2 = []
             | otherwise = primerFactorPrimoDe n:factorizar (primeraFactorizacion n)
                              
{-
6)
Implementar una función en Haskell que reciba una lista de tuplas. Cada tupla de la
lista deberá tener 2 elementos: el primer elemento representará una base y el segundo elemento
representará un exponente. La función deberá generar la expansión de la factorización de los
elementos representados en la lista. Es decir, para cada tupla se deberá calcular la potencia
correspondiente a elevar la base (primer elemento de la tupla) al exponente de dicha tupla (segundo
elemento de la tupla). Y a su vez, multiplicar todas las potencias obtenidas entre sí.
-}

expandir :: [(Int, Int)] -> Int
expandir [] = 1
expandir (h:t) = fst h ^ snd h * expandir t