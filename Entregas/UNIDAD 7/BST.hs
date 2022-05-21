-- ////////////////////////// BST \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

{-
Los BST son arboles con las siguientes caracteristicas:
todos los elementos de ti son menores que x
todos los elementos de td son mayores que x
ti y td también cumplen el invariante de BST

ej        4 
      /      \ 
    2          6    
   /  \       /  \
   1  3     5     7
-}


{-
Ejercicio 1
Indicar el costo de heapsort :: Ord a => [a] -> [a] (de la práctica anterior) suponiendo que
el usuario utiliza una priority queue con costos logarítmicos de inserción y borrado (o sea, usa una
Heap como tipo de representación)
-}

-- para Pruebas

arbol1   = (NodeT 1 EmptyT EmptyT)
arbol3   = (NodeT 3 EmptyT EmptyT)
arbol7   = (NodeT 7 EmptyT EmptyT)
arbol5   = (NodeT 5 EmptyT EmptyT)
arbol2   = (NodeT 2 arbol1 arbol3)
arbol6   = (NodeT 6 arbol5 arbol7)
arbolBST = (NodeT 4 arbol2 arbol6)

arbolBST2 = deleteBST 4 arbolBST

data Tree a = EmptyT | NodeT a (Tree a) (Tree a) 
  deriving Show

belongsBST :: Ord a => a -> Tree a -> Bool
--Propósito: dado un BST dice si el elemento pertenece o no al árbol.
--Costo: O(log N). Dado que NO recorre todos las hojas (N) sino que en el peor de los casos recorre la rama más larga (log N)

belongsBST _ (EmptyT) = False
belongsBST e (NodeT x ti td) = if e == x
                                then True 
                                else if (e > x)
                                      then belongsBST e td
                                      else belongsBST e ti

insertBST :: Ord a => a -> Tree a -> Tree a
--Propósito: dado un BST inserta un elemento en el árbol.
--Costo: O(log N), verdadero, ya que solo debe buscar hasta encontrar la hoja donde quede ordenado. en el PEOR caso, toda una rama
insertBST e EmptyT = NodeT e EmptyT EmptyT
insertBST e (NodeT x ti td) = if e == x
                                then (NodeT x ti td)
                                else if e > x 
                                    then NodeT x ti (insertBST e td)
                                    else NodeT x (insertBST e ti) td 

deleteBST :: Ord a => a -> Tree a -> Tree a
--Propósito: dado un BST borra un elemento en el árbol.
--Costo: O(log N)
deleteBST e EmptyT           = EmptyT
deleteBST e (NodeT x ti td)  =  if e == x 
                                then rearmarBST ti td
                                else if e > x
                                      then NodeT x ti (deleteBST e td)
                                      else NodeT x (deleteBST e ti) td 



rearmarBST :: Ord a => Tree a -> Tree a -> Tree a
  -- PRECOND: los dos árboles son BSTs
rearmarBST EmptyT td = td
rearmarBST ti     td = let (m, ti') = splitMaxBST ti
                        in NodeT m ti' td

splitMinBST :: Ord a => Tree a -> (a, Tree a)
--Propósito: dado un BST devuelve un par con el mínimo elemento y el árbol sin el mismo.
--Costo: O(log N)
splitMinBST (NodeT x EmptyT td) = (x, td)
splitMinBST (NodeT x ti td)     = let (m, ti') = splitMinBST ti
                                   in (m, NodeT x  ti' td)

splitMaxBST :: Ord a => Tree a -> (a, Tree a)
  -- PRECOND: el árbol es BST, y NO está vacío
--Propósito: dado un BST devuelve un par con el máximo elemento y el árbol sin el mismo.
--Costo: O(log N)
splitMaxBST (NodeT x ti EmptyT) = (x, ti)  
splitMaxBST (NodeT x ti td)     = let (m, td') = splitMaxBST td
                                   in (m, NodeT x ti td')
                              

esBST :: Ord a => Tree a -> Bool
--Propósito: indica si el árbol cumple con los invariantes de BST.
--Costo: O(N2
esBST EmptyT          = True
esBST (NodeT x ti td) = esMenorBST x td && esMayorBST x ti
                       && esBST ti
                       && esBST td

esMenorBST :: Ord a => a -> Tree a -> Bool
esMenorBST _  EmptyT       = True
esMenorBST x (NodeT y _ _) = x <= y

esMayorBST :: Ord a => a -> Tree a -> Bool
esMayorBST _  EmptyT       = True
esMayorBST x (NodeT y _ _) = x >= y

elMaximoMenorA :: Ord a => a -> Tree a -> Maybe a
--Propósito: dado un BST y un elemento, devuelve el máximo elemento que sea menor al elemento dado.
--Costo: O(log N)
elMaximoMenorA x EmptyT          = Nothing
elMaximoMenorA x (NodeT e ti td) = if x > e
                                    then    if esMenorBST x td
                                              then Just e 
                                              else elMaximoMenorA x td  
                                    else elMaximoMenorA x ti
                


{-
EJEMPLOS
             10

     4              15
  1     8        12     18


          4 
      /      \ 
    2          6    
   /  \       /  \
   1  3     5     7
-}


elMinimoMayorA :: Ord a => a -> Tree a -> Maybe a
--Propósito: dado un BST y un elemento, devuelve el mínimo elemento que sea mayor al elemento dado.
--Costo: O(log N)

elMinimoMayorA x EmptyT          = Nothing
elMinimoMayorA x (NodeT e ti td) = if x < e
                                    then    if  esMayorBST x ti
                                              then Just e 
                                              else elMinimoMayorA x ti  
                                    else elMinimoMayorA x td



--balanceado :: Tree a -> Bool
--Propósito: indica si el árbol está balanceado. Un árbol está balanceado cuando para cada
--   nodo la diferencia de alturas entre el subarbol izquierdo y el derecho es menor o igual a 1.
--Costo: O(N2)
--balanceado EmptyT          = 
--balanceado (NodeT x ti td) =    