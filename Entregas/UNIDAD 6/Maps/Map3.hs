
--  3. Como dos listas, una de claves y otra de valores, donde la clave ubicada en la posición i está
--asociada al valor en la misma posición, pero de la otra lista.

module Map3 (Map, emptyM, assocM, lookupM, deleteM, domM)
    where

--Inv.Rep: Ambas listas tienen la misma longitud, y el orden en el que ingresan los datos es igual

data Map  k v = M [k] [v]  --   k de  Key y v de Value

emptyM :: Map k v --Propósito: devuelve un map vacío
assocM :: Eq k => k -> v -> Map k v -> Map k v --Propósito: agrega una asociación clave-valor al map.
lookupM :: Eq k => k -> Map k v -> Maybe v --Propósito: encuentra un valor dado una clave.
deleteM :: Eq k => k -> Map k v -> Map k v --Propósito: borra una asociación dada una clave.
domM :: Eq k => Map k v -> [k] --Propósito: devuelve las claves del map.


-- preguntar bien la diferencia del n vs N. Por algo que menciono en el teórico

--Costo: O (1)
emptyM = M [] []

--Costo: O (1)
assocM x y (M xs ys) = M (x:xs) (y:ys) 

--Costo: O (n) donde n es la longitud de la lsitas. ya que ambas miden lo mismo
lookupM x (M xs ys) = correspondencia x xs ys

--Costo: O (n) donde n es la longitud de la lsitas. ya que ambas miden lo mismo
correspondencia ::  Eq k =>  k -> [k] -> [v] -> Maybe v
correspondencia x [] _          =  Nothing
correspondencia x (y:ys) (z:zs) = if x == y 
                                    then Just z 
                                    else correspondencia x ys zs 



--Costo: O (n) donde n es la longitud de la lsitas. ya que ambas miden lo mismo
deleteM    x (M [] [])      = emptyM
deleteM x (M (k:ks) (v:vs)) = if x ==  k
                                    then deleteM x (M ks  vs)
                                    else assocM k v ( deleteM x (M ks  vs))

--Costo: O (n*n) donde n es la longitud de la lsita
domM  (M xs ys) = sinRepetidos xs

sinRepetidos :: Eq k => [k] -> [k]
sinRepetidos [] = []
sinRepetidos (x:xs) = if elem x xs
                        then sinRepetidos xs
                        else  x : sinRepetidos xs

