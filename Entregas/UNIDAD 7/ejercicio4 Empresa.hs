{-
Donde se observa que:
los empleados son un tipo abstracto.
el primer map relaciona id de sectores con los empleados que trabajan en dicho sector.
el segundo map relaciona empleados con su número de CUIL.
un empleado puede estar asignado a más de un sector
tanto Map como Set exponen una interfaz eciente con costos logarítmicos para inserción,
búsqueda y borrado, tal cual vimos en clase.
-}


{-
Dicho esto, indicar invariantes de representación adecuados para la estructura y denir la
siguiente interfaz de Empresa, respetando los costos dados y calculando los faltantes. Justicar
todos los costos dados. En los costos, S es la cantidad de sectores de la empresa, y E es la
cantidad de empleados.




agregarASector :: SectorId -> CUIL -> Empresa -> Empresa
Propósito: agrega un sector al empleado con dicho CUIL.
Costo: calcular.
borrarEmpleado :: CUIL -> Empresa -> Empresa
Propósito: elimina al empleado que posee dicho CUIL.
Costo: calcular.

-}

module Empresa (Empresa, consEmpresa )
    where

type SectorId = Int
type CUIL = Int
data Empresa = ConsE (Map SectorId (Set Empleado)) (Map CUIL Empleado)


-- import Map1

--emptyM :: Map k v --Propósito: devuelve un map vacío
--assocM :: Eq k => k -> v -> Map k v -> Map k v --Propósito: agrega una asociación clave-valor al map.
--lookupM :: Eq k => k -> Map k v -> Maybe v --Propósito: encuentra un valor dado una clave.
--deleteM :: Eq k => k -> Map k v -> Map k v --Propósito: borra una asociación dada una clave.
--domM :: Map k v -> [k] --Propósito: devuelve las claves del map.

-- import Set

--emptyS :: Set a Crea un conjunto vacío.
--addS :: Eq a => a -> Set a -> Set a Dados un elemento y un conjunto, agrega el elemento al conjunto.
--belongs :: Eq a => a -> Set a -> Bool Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.
--sizeS :: Eq a => Set a -> Int Devuelve la cantidad de elementos distintos de un conjunto.
--removeS :: Eq a => a -> Set a -> Set a Borra un elemento del conjunto.
--unionS :: Eq a => Set a -> Set a -> Set a Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos. conjuntos.
--setToList :: Eq a => Set a -> [a] Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.


consEmpresa :: Empresa
--Propósito: construye una empresa vacía.
--Costo: O(1)
consEmpresa = ConsE (emptyM) (emptyM)

buscarPorCUIL :: CUIL -> Empresa -> Empleado
--Propósito: devuelve el empleado con dicho CUIL.
--Costo: O(log E)
buscarPorCUIL c  (ConsE mIdes mCuils) = lookupM c mCuils


empleadosDelSector :: SectorId -> Empresa -> [Empleado]
--Propósito: indica los empleados que trabajan en un sector dado.
--Costo: O(logS + E)
empleadosDelSector id  (ConsE mIdes mCuils) = lookupM id mIdes

todosLosCUIL :: Empresa -> [CUIL]
--Propósito: indica todos los CUIL de empleados de la empresa.
--Costo: O(E), siendo E la cantidad de empleados.
todosLosCUIL   (ConsE mIdes mCuils) = domM mCuils

todosLosSectores :: Empresa -> [SectorId]
--Propósito: indica todos los sectores de la empresa.
--Costo: O(S) siendo S la cantidad de Sectores
todosLosSectores (ConsE mIdes mCuils) = domM mIdes


agregarSector :: SectorId -> Empresa -> Empresa
--Propósito: agrega un sector a la empresa, inicialmente sin empleados.
--Costo: O(logS)
agregarSector sect (ConsE mIdes mCuils) =  (ConsE (assocM sect  emptyS  mIdes) mCuils)


--agregarEmpleado :: [SectorId] -> CUIL -> Empresa -> Empresa 
--Propósito: agrega un empleado a la empresa, en el que trabajará en dichos sectores y tendrá el CUIL dado.
--Costo: calcular.
agregarEmpleado [] c emp       = emp
agregarEmpleado (s:secs) c emp = asocia    agregarEmpleado ssecs c emp


{-
Y sabemos que la interfaz de Empleado es:
consEmpleado :: CUIL -> Empleado
Propósito: construye un empleado con dicho CUIL.
Costo: O(1)
cuil :: Empleado -> CUIL
Propósito: indica el CUIL de un empleado.
Costo: O(1)
incorporarSector :: SectorId -> Empleado -> Empleado
Propósito: incorpora un sector al conjunto de sectores en los que trabaja un empleado.
Costo: O(log S), siendo S la cantidad de sectores que el empleado tiene asignados.
sectores :: Empleado -> SectorId
Propósito: indica los sectores en los que el empleado trabaja.
Costo: O(1)
-}