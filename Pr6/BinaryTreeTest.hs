-- Binary Tree Test
-- Autores: Cristian Simon Moreno, NIA: 611487
--          Miguel Allue Baron,    NIA: 593599

import BinaryTree

--Creamos el arbol
arbol = Node 17 (Node 12 (Node 5 EmptyTree (leaf 8)) (leaf 15))
             (Node 115
                   (Node 32 (leaf 30) (Node 46 EmptyTree (leaf 57)))
                   (leaf 163))
				
--Tamaño arbol
tamaño1 = size arbol

--Creamos el arbol2
arbol2 = Node 17 (Node 12 (Node 5 EmptyTree (leaf 8)) (leaf 15))
                (Node 115
                      (Node 32 (leaf 30) (Node 46 EmptyTree (leaf 57)))
                      (leaf 163))

--Añadir
a = add 4 arbol2
b = add 180 a
arbolMod = add 29 b

--Tamaño arbolMod
tamaño2 = size arbolMod

--Preorden
preorden = preorder arbolMod

--Postorden
postorden = postorder arbolMod

--Inorden
inorden = inorder arbolMod

--Between [5 17]
b1 = between arbol 5 17

--Between [12 32]
b2 = between arbol 12 32

--Between [12 163]
b3 = between arbol 12 163

				
main :: IO()
main = do	
	putStrLn ""
	putStrLn "___________________________________________"
	putStrLn ""
	putStrLn "Listado de nodos del arbol"
	putStrLn "--------------------------"
	putStrLn "[17, 12, 5, 8, 15, 115, 32, 30, 46, 57, 163]"
	putStrLn ""
	putStrLn "___________________________________________"
	putStrLn ""
	putStrLn "Dibujo del arbol binario creado"
	putStrLn "-------------------------------"
	pict arbol
	putStrLn ""
	putStr "-> Tamaño: "
	print tamaño1
	putStrLn ""
	putStrLn "___________________________________________"
	putStrLn ""
	putStrLn "-> Añadimos [4, 180, 29]"
	putStrLn ""	
	putStrLn "Dibujo del arbol binario modificado"
	putStrLn "-----------------------------------"
	pict arbolMod
	putStrLn ""
	putStr "-> Tamaño: "
	print tamaño2
	putStrLn ""
	putStrLn "___________________________________________"
	putStrLn ""
	putStrLn "Recorridos"
	putStrLn "----------"
	putStrLn "-> Preorden:"
	print preorden
	putStrLn ""
	putStrLn "-> Postorden:"
	print postorden
	putStrLn ""
	putStrLn "-> Inorden:"
	print inorden	
	putStrLn ""
	putStrLn "___________________________________________"
	putStrLn ""
	putStrLn "Elementos entre MIN MAX"
	putStrLn "-----------------------"
	putStrLn "-> [5-17]"
	print b1
	putStrLn ""
	putStrLn"-> [12-32]"
	print b2
	putStrLn ""
	putStrLn "-> [12-163]"
	print b3
	putStrLn ""