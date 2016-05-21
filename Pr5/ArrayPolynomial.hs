-- Modulo Vector de Coeficientes
-- Autores: Cristian Simon Moreno, NIA: 611487
--          Miguel Allue Baron,    NIA: 593599

module ArrayPolynomial where 

--Esta funcion devolvera un polinomio que solo contiene una x
x :: [Int]
x = [1,0]

--Esta funcion devolvera un polinomio que contenga como termino independiente
--la constante c
coef :: Int->[Int]
coef c = [c]

--Esta funcion suma dos polinomios y devuelve el resultado
suma :: [Int]->[Int]->[Int]
suma [] a = a
suma a [] = a
suma a b = (suma (init a) (init b)) ++ [(last a) + (last b)]

--Esta funcion suma todos los polinomios que haya en la lista lp y devuelve
--el resultado
padd :: [[Int]]->[Int]
padd [] = []
padd (x:[]) = x
padd (x:xs) = suma x (padd xs)

--Esta funcion multiplica dos polinomios y devuelve el resultado
mult :: [Int]->[Int]->[Int]
mult [] a = a
mult a [] = a
mult a b = (mult (init a) (init b)) ++ [(last a) * (last b)]

--Esta funcion suma todos los polinomios que haya en la lista lp y devuelve
--el resultado
pmul :: [[Int]]->[Int]
pmul [] = []
pmul (x:[]) = x
pmul (x:xs) = mult x (pmul xs)

--Esta funcion evalua el polinomio p para el valor x, devolviendo como resultado
--el correspondiente numero real resultado
peval :: [Int]->Int->Int
peval [] a = 0
peval p x = (last(p)) + peval (init (map(* x) p)) x

--Esta funcion calcula la derivada analitica del polinomio p, dando como resultado
--otro polinomio
pderv :: [Int]->[Int]
pderv [] = []
pderv a = pderv2 (init a) ((length a)-1)

pderv2 :: [Int]->Int->[Int]
pderv2 [] long = []
pderv2 (x:xs) long = [x*long] ++ pderv2 xs (long-1)



