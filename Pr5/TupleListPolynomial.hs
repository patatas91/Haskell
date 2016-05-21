-- Modulo Lista de Tuplas
-- Autores: Cristian Simon Moreno, NIA: 611487
--          Miguel Allue Baron,    NIA: 593599

module TupleListPolynomial where 

--Esta funcion devolvera un polinomio que solo contiene una x
x :: [(Int,Int)]
x = [(1,1)]

--Esta funcion devolvera un polinomio que contenga como termino independiente
--la constante c
coef :: Int->[(Int,Int)]
coef c = [(c,0)]

--Esta funcion suma todos los polinomios que haya en la lista lp y devuelve
--el resultado
padd :: [[(Int,Int)]]->[(Int,Int)]
padd x 
	| (ord2 x) == True = padd2 x
	| otherwise = error "La lista de tuplas no esta ordenada"

padd2 :: [[(Int,Int)]]->[(Int,Int)]
padd2 [] = []
padd2 (x:[]) = x
padd2 (x:xs) = suma x (padd2 xs)

--Esta funcion suma dos polinomios y devuelve el resultado
suma :: [(Int,Int)]->[(Int,Int)]->[(Int,Int)]
suma [] a = a
suma a [] = a
suma a b
	| snd(head a) > snd(head b) = [(head a)] ++ suma (tail a) b
	| snd(head a) < snd(head b) = [(head b)] ++ suma a (tail b)
	| otherwise = [((fst (head a) + fst (head b)), snd (head a))] ++  suma (tail a) (tail b)

--Esta funcion suma todos los polinomios que haya en la lista lp y devuelve
--el resultado
pmul :: [[(Int,Int)]]->[(Int,Int)]
pmul x 
	| (ord2 x) == True = pmul2 x
	| otherwise = error "La lista de tuplas no esta ordenada"
	
pmul2 :: [[(Int,Int)]]->[(Int,Int)]
pmul2 [] = []
pmul2 (x:[]) = x
pmul2 (x:xs) = mult x (pmul2 xs)

--Esta funcion multiplica dos polinomios y devuelve el resultado
mult :: [(Int,Int)]->[(Int,Int)]->[(Int,Int)]
mult [] a = a
mult a [] = a
mult a b
	| snd(head a) > snd(head b) = [(head a)] ++ mult (tail a) b
	| snd(head a) < snd(head b) = [(head b)] ++ mult a (tail b)
	| otherwise = [((fst (head a) * fst (head b)), snd (head a))] ++  mult (tail a) (tail b)	

--Esta funcion evalua el polinomio p para el valor x, devolviendo como resultado
--el correspondiente numero real resultado
peval :: [(Int,Int)]->Int->Int
peval [] x = 0
peval ((base,exponente):xs) x = ((x^exponente) * base) + (peval xs x)

--Esta funcion calcula la derivada analitica del polinomio p, dando como resultado
--otro polinomio 
pderv :: [(Int,Int)]->[(Int,Int)]
pderv x 
	| (ord x) == True = pderv2 x
	| otherwise = error "La lista de tuplas no esta ordenada"

pderv2 :: [(Int,Int)]->[(Int,Int)]
pderv2 [] = [] 
pderv2 ((base,exponente):xs) 
	| exponente == 0	 = []
	| otherwise = [(base*exponente,exponente-1)] ++ pderv2 xs
	
--Esta funcion comprueba si una lista de tuplas esta ordenada	
ord :: [(Int,Int)]-> Bool
ord [] = True
ord (x:[]) = True
ord ((b1,e1):(b2,e2):xs)
	| e2>=e1 = False
	| otherwise = ord ([(b2,e2)] ++ xs)
	
ord2 ::[[(Int,Int)]]->Bool
ord2 [] = True
ord2 (x:xs) 
	| (ord x) == True = ord2 xs
	| otherwise = False

