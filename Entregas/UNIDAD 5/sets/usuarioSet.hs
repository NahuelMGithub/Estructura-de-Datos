-- //////////////////////////////// 2.3 Set (conjunto) \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\


--import Set1
import Set2
--     (Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList)


-- Costo: O(n*m) donde n es la longitud de la primer lista y m la longitud de la segunda lista
losQuePertenecen :: Eq a => [a] -> Set a -> [a]
losQuePertenecen []     _   = []
losQuePertenecen (x:xs) set = if belongs x set
                               then x : losQuePertenecen xs set
                               else losQuePertenecen xs set

--Costo: O(n): donde n es la longitud de la lista. aca tengo una duda... que pasa con la implementacion? porque 
-- con un a implementación esta fn es lineal, pero con otra es cuadrática... y el usuario no puede saberlo. 
-- si contamos SOLO el punto de vista del USUARIO, esta fn es LINEAL . O(n) con n long de la lista 

deListaASet :: Eq a => [a] -> Set a 
deListaASet []     = emptyS
deListaASet (x:xs) = addS x (deListaASet xs)

-- Costo: O(n*n) donde n es la longitud de la  lista
sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos xs =   setToList (deListaASet xs) 

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)

-- Costo: O(n*m*l) donde n es la longitud de la lista del set, m la longitud de la lista del set en t1 y l la long de la lista del set de t2
-- chan... aca tengo duda. porque dentroe del arbol, t1  y t2... no se cuantas otras listas hay. 
unirTodos :: Eq a => Tree (Set a) -> Set a
unirTodos EmptyT             = emptyS
unirTodos (NodeT set t1 t2)  = unionS  set
                               (unionS  
                                 (unirTodos t1)
                                 (unirTodos t2))


