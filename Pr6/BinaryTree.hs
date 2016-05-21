-- Binary Tree
-- Autores: Cristian Simon Moreno, NIA: 611487
--          Miguel Allue Baron,    NIA: 593599

module BinaryTree where

--ARBOLES BINARIOS

--Constructor BinaryTree
data BTree a = EmptyTree 
               | Node a (BTree a) (BTree a)
               deriving (Eq, Ord, Read, Show)

--Esta función devolverá un árbol que consista en una única hoja que contenga el
--elemento x.	
leaf :: a -> BTree a  
leaf x = Node x EmptyTree EmptyTree

--Esta función devolverá un árbol que contenga en la raíz el elemento x,
--con hijo izquierdo lb e hijo derecho rb.
tree :: a -> BTree a -> BTree a -> BTree a
tree x lb rb = Node x lb rb

--Esta función devuelve el número de elementos del árbol.
size :: BTree a -> Int
size EmptyTree = 0
size (Node _ lb rb) = 1 + size lb + size rb

--RECORRIDOS

--Recorrido en preorden
preorder :: BTree a -> [a]
preorder EmptyTree = []
preorder (Node x lb rb) = [x] ++ preorder lb ++ preorder rb

--Recorrido en postorden
postorder :: BTree a -> [a]
postorder EmptyTree = []
postorder (Node x lb rb) = preorder lb ++ preorder rb ++ [x]

--Recorrido en inorden
inorder :: BTree a -> [a]
inorder EmptyTree = []
inorder (Node x lb rb) = preorder lb ++ [x] ++ preorder rb

--ARBOLES DE BUSQUEDA

--Añade el elemento x al árbol t, devolviendo el resultado.
add :: (Ord a) => a -> BTree a -> BTree a
add x EmptyTree = leaf x
add x (Node a lb rb)
		| (x == a) = (Node x lb rb)
		| (x < a) = (Node a (add x lb) rb)
		| (x > a) = (Node a lb (add x rb))

--Busca en el árbol t y devuelve una lista con todos los
--elementos que están entre xmin y xmax.
between :: (Ord a) => BTree a -> a -> a -> [a]
between EmptyTree min max = []
between (Node x lb rb) min max
		 | x>=min && x<=max = [x] ++ between lb min max ++ between rb min max
		 | otherwise = between lb min max ++ between rb min max
 
 
 
-- Imprime un arbol por pantalla, dandole cierto formato.
pict t = putStr (pic "" t)
         where pic ind EmptyTree = ind ++ "."
               pic ind (Node x EmptyTree EmptyTree) = ind ++ show x
               pic ind (Node x EmptyTree rb) = pic ('\t':ind) rb ++ "\n" ++
                                        ind ++ show x
               pic ind (Node x lb EmptyTree) = ind ++ show x     ++ "\n" ++
                                        pic ('\t':ind) lb
               pic ind (Node x lb rb) = pic ('\t':ind) rb ++ "\n" ++
                                        ind ++ show x     ++ "\n" ++
                                        pic ('\t':ind) lb
										


