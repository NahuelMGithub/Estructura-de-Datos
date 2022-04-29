-- //////////////////////////////// 2.3 Set (conjunto) \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\


module Set2
    (Set, emptyS, addS, belongs,sizeS, removeS, unionS, setToList)
    where   

data Set a = S [a]

emptyS :: Set a --Crea un conjunto vacío.
addS :: Eq a => a -> Set a -> Set a --Dados un elemento y un conjunto, agrega el elemento al conjunto.
belongs :: Eq a => a -> Set a -> Bool --Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.
sizeS :: Eq a => Set a -> Int --Devuelve la cantidad de elementos distintos de un conjunto.
removeS :: Eq a => a -> Set a -> Set a --Borra un elemento del conjunto.
unionS :: Eq a => Set a -> Set a -> Set a --Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos. conjuntos.
setToList :: Eq a => Set a -> [a] --Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.

emptyS = S []

addS x (S ys) = S (x:ys)

belongs x (S xs) = pertenece x xs

pertenece :: Eq a => a -> [a] -> Bool
pertenece x []     = False
pertenece x (y:ys) = x == y || pertenece x ys

sizeS (S xs) = cantidadSinRepetidos xs

cantidadSinRepetidos :: Eq a => [a] -> Int
cantidadSinRepetidos  []    = 0
cantidadSinRepetidos (x:xs) = unoSi (pertenece x xs) + cantidadSinRepetidos xs 

unoSi :: Bool -> Int
unoSi True  = 1
unoSi False = 0

removeS x (S xs) = S (sacarTodos x xs)

sacarTodos :: Eq a => a -> [a] -> [a]
sacarTodos x []     = []
sacarTodos x (y:ys) = if x == y
                        then sacarTodos x ys
                        else y : sacarTodos x ys

unionS (S xs) (S ys) = (S (xs ++ ys))

setToList (S xs) = xs