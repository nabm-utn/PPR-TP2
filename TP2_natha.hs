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
listaDivisores (a, b) 	| b == 0					 = []
						| b == 1 					 = [1]
						| esDivisible(a, b) == True  = b:listaDivisores(a, b-1)
						| esDivisible(a, b) == False = listaDivisores(a, b-1)
						| otherwise = []
{-
3)
Implementar una función en Haskell que reciba un número entero nro. La función
deberá devolver True, en caso de que el número nro sea un número primo. En caso de que el
número nro no sea primo, la función deberá devolver False. Utilizar recursividad y reutilizar la
función de la consigna 1).
-}

esPrimo :: Int -> Bool
esPrimo n 	| length(listaFactoresDivisiblesDe n) == 2 = True
			| otherwise = False

{-
4)
Implementar una función en Haskell que reciba un número entero n. La función deberá
generar una lista con todos los números primos menores o iguales que n. Utilizar lista por
comprensión. Reutilizar la función de la consigna 2.
-}
listaFactoresPrimosDe :: Int -> [Int]
listaFactoresPrimosDe n = listaPrimos(n, n)


listaPrimos :: (Int, Int) -> [Int]
listaPrimos(a, b)	| (b < 2) == True							= []
					| (esPrimo b && esDivisible(a, b)) == True 	= b:listaPrimos(a, b-1)
					| (esPrimo b && esDivisible(a, b)) == False = listaPrimos(a, b-1)
					| otherwise = []
{-
5)
Implementar una función en Haskell que reciba un número entero n. La función deberá
generar una lista con todos los factores primos del número n. Es decir, en este caso, el producto de
todos los factores de la lista a generar deberá ser igual a n. Reutilizar las funciones necesarias.
-}
factorizar :: (Int) -> [Int]
factorizar (n)  | (n < 2) == True = []
				| (length(listaFactoresPrimosDe n) > 0) == True = last(listaFactoresPrimosDe n):factorizar ( n `div` last(listaFactoresPrimosDe n))
				| otherwise = []
						      
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
expandir list 	| (length (list) < 1) == True = 1
				| otherwise = fst (head(list)) ^ snd (head(list)) * expandir (tail(list))