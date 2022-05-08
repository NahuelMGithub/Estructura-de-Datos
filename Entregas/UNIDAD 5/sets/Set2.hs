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

-- Costo: O(1) 
emptyS = S []

-- Costo: O(n) donde n es la longitud de la lista
addS x (S ys) = S (x:ys)

-- Costo: O(n) donde n es la longitud de la lista
belongs x (S xs) = pertenece x xs

-- Costo: O(n) donde n es la longitud de la lista
pertenece :: Eq a => a -> [a] -> Bool
pertenece x []     = False
pertenece x (y:ys) = x == y || pertenece x ys

-- Costo: O(n*n) donde n es la longitud de la lista
sizeS (S xs) = cantidadSinRepetidos xs

-- Costo: O(n*n) donde n es la longitud de la lista
cantidadSinRepetidos :: Eq a => [a] -> Int
cantidadSinRepetidos  []    = 0
cantidadSinRepetidos (x:xs) = unoSiNo (pertenece x xs) + cantidadSinRepetidos xs 

-- Costo: O(1)
unoSiNo :: Bool -> Int
unoSiNo False = 1
unoSiNo True  = 0

-- Costo: O(n) donde n es la longitud de la lista
removeS x (S xs) = S (sacarTodos x xs)

-- Costo: O(n) donde n es la longitud de la lista
sacarTodos :: Eq a => a -> [a] -> [a]
sacarTodos x []     = []
sacarTodos x (y:ys) = if x == y
                        then sacarTodos x ys
                        else y : sacarTodos x ys

-- Costo: O(n) donde n es la longitud de la lista xs
unionS (S xs) (S ys) = (S (xs ++ ys))

-- Costo: O(n*n) donde n es la longitud de la lista
setToList (S xs) = sinRepetidos xs

-- Costo: O(n*n) donde n es la longitud de la lista
sinRepetidos :: Eq a => [a] -> [a] 
sinRepetidos []     = []
sinRepetidos (x:xs) = if pertenece x xs
                        then sinRepetidos xs
                        else x : sinRepetidos xs