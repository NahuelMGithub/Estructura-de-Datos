module MultiSet1 (MultiSet, emptyMS, addMS, ocurrencesMS, unionMS, intersectionMS, multiSetToList )
    where

import Map1

{-
emptyM :: Map k v --Propósito: devuelve un map vacío
assocM :: Eq k => k -> v -> Map k v -> Map k v --Propósito: agrega una asociación clave-valor al map.
lookupM :: Eq k => k -> Map k v -> Maybe v --Propósito: encuentra un valor dado una clave.
deleteM :: Eq k => k -> Map k v -> Map k v --Propósito: borra una asociación dada una clave.
domM :: Map k v -> [k] --Propósito: devuelve las claves del map. FIDEL le dice domM (dominio del map)
-}

data MultiSet a = MS (Map a Int)

{- emptyMS :: MultiSet a
Propósito: denota un multiconjunto vacío.
addMS :: Ord a => a -> MultiSet a -> MultiSet a
Propósito: dados un elemento y un multiconjunto, agrega una ocurrencia de ese elemento al
multiconjunto.
ocurrencesMS :: Ord a => a -> MultiSet a -> Int
Propósito: dados un elemento y un multiconjunto indica la cantidad de apariciones de ese
elemento en el multiconjunto.
unionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a (opcional)
Propósito: dados dos multiconjuntos devuelve un multiconjunto con todos los elementos de
ambos multiconjuntos.
intersectionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a (opcional)
Propósito: dados dos multiconjuntos devuelve el multiconjunto de elementos que ambos
multiconjuntos tienen en común.
multiSetToList :: MultiSet a -> [(a, Int)]
Propósito: dado un multiconjunto devuelve una lista con todos los elementos del conjunto y
su cantidad de ocurrencias.
 -}
emptyMS = error "Hacer"

addMS = error "Hacer"
ocurrencesMS = error "Hacer"
unionMS = error "Hacer"
intersectionMS = error "Hacer"
multiSetToList = error "Hacer"