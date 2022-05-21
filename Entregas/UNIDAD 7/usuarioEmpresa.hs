{-
Ejercicio 5
Como usuario del tipo Empresa implementar las siguientes operaciones, calculando el costo obtenido al implementarlas, y justicando cada
uno adecuadamente.

-}

--import Empresa

{-
consEmpresa :: Empresa Propósito: construye una empresa vacía. Costo: O(1)
buscarPorCUIL :: CUIL -> Empresa -> Empleado Propósito: devuelve el empleado con dicho CUIL. Costo: O(log E)
empleadosDelSector :: SectorId -> Empresa -> [Empleado] Propósito: indica los empleados que trabajan en un sector dado. Costo: O(logS + E)
todosLosCUIL :: Empresa -> [CUIL] Propósito: indica todos los CUIL de empleados de la empresa. Costo: O(E)
todosLosSectores :: Empresa -> [SectorId] Propósito: indica todos los sectores de la empresa. Costo: O(S)
agregarSector :: SectorId -> Empresa -> Empresa Propósito: agrega un sector a la empresa, inicialmente sin empleados. Costo: O(logS)
agregarEmpleado :: [SectorId] -> CUIL -> Empresa -> Empresa Propósito: agrega un empleado a la empresa, en el que trabajará en dichos 
ectores y tendrá  el CUIL dado. Costo: calcular.
agregarASector :: SectorId -> CUIL -> Empresa -> Empresa Propósito: agrega un sector al empleado con dicho CUIL. Costo: calcular.
borrarEmpleado :: CUIL -> Empresa -> Empresa Propósito: elimina al empleado que posee dicho CUIL. Costo: calcular.
type SectorId = Int
type CUIL = Int
data Empresa = ConsE (Map SectorId (Set Empleado)) (Map CUIL Empleado)
-}

comenzarCon :: [SectorId] -> [CUIL] -> Empresa
--Propósito: construye una empresa con la información de empleados dada. Los sectores no tienen empleados.
--Costo: calcular.
comenzarCon = error "Hacer"


recorteDePersonal :: Empresa -> Empresa
--Propósito: dada una empresa elimina a la mitad de sus empleados (sin importar a quiénes).
--Costo: calcular.
recorteDePersonal = error "Hacer"

convertirEnComodin :: CUIL -> Empresa -> Empresa
--Propósito: dado un CUIL de empleado le asigna todos los sectores de la empresa.
--Costo: calcular.
convertirEnComodin = error "Hacer"

esComodin :: CUIL -> Empresa -> Bool
--Propósito: dado un CUIL de empleado indica si el empleado está en todos los sectores.
--Costo: calcular.
esComodin = error "Hacer"