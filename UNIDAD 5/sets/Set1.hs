-- //////////////////////////////// 2.1 Set (conjunto) \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\


module Set1
    (Set, emptyS, addS, belongs,sizeS, removeS, unionS, setToList)
    where   

data Set a = S [a]

-- pensado para una Usuario que pregunta mucho, pero hace pocos ingresos

emptyS :: Set a
emptyS = S []

addS :: Eq a => a -> Set a -> Set a
addS x (S ys) = if pertenece x ys   
                 then S ys
                 else S (x:ys)



belongs :: Eq a => a -> Set a -> Bool
belongs x (S xs) = pertenece x xs

pertenece :: Eq a => a -> [a] -> Bool
pertenece x []     = False
pertenece x (y:ys) = x == y || pertenece x ys

sizeS :: Eq a => Set a -> Int
sizeS (S xs) = length xs



removeS :: Eq a => a -> Set a -> Set a
removeS x (S ys) = if pertenece x ys
                    then S (sacarElemento x ys) 
                    else S ys

sacarElemento :: Eq a => a -> [a] -> [a]
sacarElemento x (y:ys) = if x == y
                         then ys
                         else y : sacarElemento x ys


unionS :: Eq a => Set a -> Set a -> Set a
unionS (S xs) (S ys) = S (unirSinRepetir xs ys)


unirSinRepetir :: Eq a => [a] -> [a] -> [a]  
unirSinRepetir []     ys = ys
unirSinRepetir (x:xs) ys = if pertenece x ys
                            then unirSinRepetir xs ys
                            else x : unirSinRepetir xs ys


setToList :: Eq a => Set a -> [a]
setToList (S xs) = xs