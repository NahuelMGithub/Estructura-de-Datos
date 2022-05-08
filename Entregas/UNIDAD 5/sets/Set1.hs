-- //////////////////////////////// 2.1 Set (conjunto) \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\


module Set1
    (Set, emptyS, addS, belongs,sizeS, removeS, unionS, setToList)
    where   


-- Inv. Rep: La lista NO posee elementos repetidos

data Set a = S [a]

-- pensado para una Usuario que pregunta mucho, pero hace pocos ingresos



-- Costo: O(1)
emptyS :: Set a
emptyS = S []

-- Costo: O(n) donde n es la longitud de la lista 
addS :: Eq a => a -> Set a -> Set a
addS x (S ys) = if pertenece x ys   
                 then S ys
                 else S (x:ys)


-- Costo: O(n) donde n es la longitud de la lista
belongs :: Eq a => a -> Set a -> Bool
belongs x (S xs) = pertenece x xs

-- Costo: O(n) donde n es la longitud de la lista
pertenece :: Eq a => a -> [a] -> Bool
pertenece x []     = False
pertenece x (y:ys) = x == y || pertenece x ys

-- Costo: O(n) donde n es la longitud de la lista
sizeS :: Eq a => Set a -> Int
sizeS (S xs) = length xs

-- Costo: O(n) donde n es la longitud de la lista
removeS :: Eq a => a -> Set a -> Set a
removeS x (S ys) = if pertenece x ys
                    then S (sacarElemento x ys) 
                    else S ys

-- Costo: O(n) donde n es la longitud de la lista
sacarElemento :: Eq a => a -> [a] -> [a]
sacarElemento x (y:ys) = if x == y
                         then ys
                         else y : sacarElemento x ys

-- Costo: O(N*M) siendo N la longitud de la primer lista y M la longitud de la segunda
unionS :: Eq a => Set a -> Set a -> Set a
unionS (S xs) (S ys) = S (unirSinRepetir xs ys)

-- Costo: O(N*M) siendo N la longitud de la primer lista y M la longitud de la segunda
unirSinRepetir :: Eq a => [a] -> [a] -> [a]  
unirSinRepetir []     ys = ys
unirSinRepetir (x:xs) ys = if pertenece x ys
                            then unirSinRepetir xs ys
                            else x : unirSinRepetir xs ys

-- Costo: O(1)
setToList :: Eq a => Set a -> [a]
setToList (S xs) = xs