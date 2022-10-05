--punto 1
esDivisible :: Int->Int->Bool
esDivisible nro exp = mod nro exp == 0

--punto 2
listaFactoresDivisiblesde:: Int -> [Int]
listaFactoresDivisiblesde n  = listaDivisores n 1

listaDivisores:: Int->Int->[Int]
listaDivisores n div | n==div=[n]
		     | n > div = if esDivisible n div
		          then div :listaDivisores n (div +1)
			     else listaDivisores n (div+1)
--Punto 3

esPrimo:: Int -> Bool
esPrimo n = verificaPrimos n 2


verificaPrimos :: Int -> Int -> Bool
verificaPrimos n div | (n == div || n ==1 )= True 
	               | n > div = if (esDivisible n div) == False
				         then verificaPrimos n (div+1)
				          else  False

--Punto 4
listaPrimosMenoresOIgualesQue::Int->[Int]
listaPrimosMenoresOIgualesQue n = [x | x <-[1..n], esPrimo x == True]



