-- //////////////////////////////// 2.3 Set (conjunto) \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\


--import Set1
import Set2
--     (Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList)

losQuePertenecen :: Eq a => [a] -> Set a -> [a]
losQuePertenecen []     _   = []
losQuePertenecen (x:xs) set = if belongs x set
                               then x : losQuePertenecen xs set
                               else losQuePertenecen xs set



deListaASet :: Eq a => [a] -> Set a 
deListaASet []     = emptyS
deListaASet (x:xs) = addS x (deListaASet xs)

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos xs =   setToList (deListaASet xs) 

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)


unirTodos :: Eq a => Tree (Set a) -> Set a
unirTodos EmptyT             = emptyS
unirTodos (NodeT set t1 t2)  = unionS  set
                               (unionS  
                                 (unirTodos t1)
                                 (unirTodos t2))


