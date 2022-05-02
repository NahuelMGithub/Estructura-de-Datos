--------------------------------------------------------------------------

------------------------ 1 Pizzas ----------------------------------------

--------------------------------------------------------------------------

data Pizza = Prepizza | Capa Ingrediente Pizza
    deriving Show

data Ingrediente = Salsa | Queso | Jamon | Aceitunas Int
    deriving Show

cantidadDeCapas :: Pizza -> Int -- Dada una pizza devuelve la cantidad de ingredientes
cantidadDeCapas Prepizza   = 0
cantidadDeCapas (Capa i p) = 1 + cantidadDeCapas p

------------- PIZZAS ejemplos
pizza0 = Prepizza
pizza1 = Capa Salsa Prepizza
pizza2 = Capa Queso (Capa Salsa Prepizza)
pizza3 = Capa (Aceitunas 8) 
              (Capa Queso (Capa Salsa Prepizza))
pizza4 = Capa Jamon (Capa Queso (Capa (Aceitunas 8) (Capa Salsa Prepizza)))              
pizza5 = Capa Queso (Capa Queso (Capa Queso (Capa Queso Prepizza)))

armarPizza :: [Ingrediente] -> Pizza --Dada una lista de ingredientes construye una pizza
armarPizza []     = Prepizza
armarPizza (i:is) = Capa i (armarPizza is)

sacarJamon :: Pizza -> Pizza -- Le saca los ingredientes que sean jamón a la pizza
sacarJamon Prepizza   = Prepizza
sacarJamon (Capa i p) = if esJamon i
                            then sacarJamon p
                            else Capa i (sacarJamon p)  
                         

esJamon :: Ingrediente -> Bool
esJamon Jamon = True
esJamon _     = False

tieneSoloSalsaYQueso :: Pizza -> Bool -- Dice si una pizza tiene salsa y queso
tieneSoloSalsaYQueso Prepizza   = True
tieneSoloSalsaYQueso (Capa i p) = (esQuesoOSalsa i) && (tieneSoloSalsaYQueso p)

esQuesoOSalsa :: Ingrediente -> Bool
esQuesoOSalsa Queso = True
esQuesoOSalsa Salsa = True
esQuesoOSalsa _     = False


duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza   = Prepizza
duplicarAceitunas (Capa i p) =   Capa (dobleDeAceitunas i) (duplicarAceitunas p) 
                           

dobleDeAceitunas :: Ingrediente -> Ingrediente -- Duplica la cantidad de aceitunas
dobleDeAceitunas (Aceitunas n) = (Aceitunas (2*n))
dobleDeAceitunas i             = i

--Dada una lista de pizzas devuelve un par donde la primera componente es la cantidad de
--ingredientes de la pizza, y la respectiva pizza como segunda componente.

cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
cantCapasPorPizza []     = []
cantCapasPorPizza (p:ps) = (cantidadDeCapas p, p)   : cantCapasPorPizza ps 


--------------------------------------------------------------------------

--------------------- 2 Mapa de tesoros (con bifurcaciones)---------------

--------------------------------------------------------------------------

data Dir = Izq | Der
    deriving Show
data Objeto = Tesoro | Chatarra
    deriving Show
data Cofre = Cofre [Objeto]
    deriving Show
data Mapa = Fin Cofre | Bifurcacion Cofre Mapa Mapa
    deriving Show


cof1 = Cofre []
cof2 = Cofre [Tesoro]
mapa0 = Fin cof1
mapa1 = Bifurcacion cof1 mapa0 mapa0
mapa2 = Bifurcacion cof1 mapa1 mapa1
mapa3 = Bifurcacion cof2 mapa1 mapa2
mapa4 = Bifurcacion cof1 mapa1 (Bifurcacion cof1 (Fin cof2) (Fin cof1))
mapa5 = Bifurcacion cof1 mapa1 (Bifurcacion cof1 (Fin cof2) mapa1)

hayTesoro :: Mapa -> Bool --Indica si hay un tesoro en alguna parte del mapa.
hayTesoro (Fin c)               = hayTesoroEnCofre c
hayTesoro (Bifurcacion c m1 m2) = hayTesoroEnCofre c || hayTesoro m1 || hayTesoro m2 

hayTesoroEnCofre:: Cofre -> Bool --- hago una subtarea para no hacer recursion sobre .... Cofre que es? Una SUMA? 
hayTesoroEnCofre (Cofre  obs) = hayTesoroEnObjs obs

hayTesoroEnObjs :: [Objeto] -> Bool
hayTesoroEnObjs []      = False
hayTesoroEnObjs (o:obs) = esTesoro o || hayTesoroEnObjs obs

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _      = False


hayTesoroEn :: [Dir] -> Mapa -> Bool ----- No me convence como puse la recursion.
--Indica si al final del camino hay un tesoro. Nota: el final de un camino se representa con una
--lista vacía de direcciones.
hayTesoroEn []     m             = hayTesoroEnCofre (cofreActualEnMapa m)
hayTesoroEn (d:ds) (Fin c)               = False
hayTesoroEn (d:ds) (Bifurcacion c m1 m2) = if esIzquierda d 
                                            then hayTesoroEn ds m1
                                            else hayTesoroEn ds m2
cofreActualEnMapa :: Mapa ->  Cofre
cofreActualEnMapa (Fin c)             = c     
cofreActualEnMapa (Bifurcacion c _ _) = c 



esIzquierda :: Dir -> Bool
esIzquierda Izq = True
esIzquierda _   = False



-- Si, es feo, pero no se como resolver si hay justo tesoro en el primer Cofre Cofre 
caminoAlTesoro :: Mapa -> [Dir] -- Indica el camino al tesoro. Precondición: existe un tesoro y es único.
caminoAlTesoro (Fin c)               = []
caminoAlTesoro (Bifurcacion c m1 m2) = if hayTesoroEnCofre c 
                                        then []
                                        else if  hayTesoro m1
                                            then Izq : caminoAlTesoro m1
                                            else Der : caminoAlTesoro m2


caminoDeLaRamaMasLarga :: Mapa -> [Dir]
caminoDeLaRamaMasLarga (Fin _)               = []
caminoDeLaRamaMasLarga (Bifurcacion _ m1 m2) = if (longCamino m1) >  (longCamino m2) 
                                                then Izq : caminoDeLaRamaMasLarga m1
                                                else Der : caminoDeLaRamaMasLarga m2



longCamino :: Mapa -> Int
longCamino (Fin _ )              = 0
longCamino (Bifurcacion _ m1 m2) = 1 + max (longCamino m1) (longCamino m2)



tesorosPorNivel :: Mapa -> [[Objeto]] -- Devuelve los tesoros separados por nivel en el árbol.
tesorosPorNivel (Fin c)               = tesosrosDelCofre c : []
tesorosPorNivel (Bifurcacion c m1 m2) = (tesosrosDelCofre c : []) ++  tesorosPorNivel m1
                                       ++ tesorosPorNivel m2
                                        

tesosrosDelCofre :: Cofre -> [Objeto]
tesosrosDelCofre (Cofre obs) = todosLosTesoros obs


todosLosTesoros :: [Objeto] -> [Objeto]
todosLosTesoros []       = []
todosLosTesoros (o:objs) = singularSi (esTesoro o) o ++ todosLosTesoros objs


singularSi :: Bool -> a -> [a]
singularSi True  x = [x]
singularSi False _ = []



todosLosCaminos :: Mapa -> [[Dir]] --Devuelve todos lo caminos en el mapa.
todosLosCaminos (Fin _)               = []
todosLosCaminos (Bifurcacion _ m1 m2) = consACada Izq (todosLosCaminos m1)
                                        ++ consACada Der (todosLosCaminos m2)


consACada :: a -> [[a]] -> [[a]]
consACada x []     = [[x]]
consACada x (ys:yss) = (x:ys) : consACada x yss

-- Devuelve caminos duplicados, porque agrega por ambas ramas lo mismo... 
--La idea es que por cada cofre haya exactamente un camino en el resultado.


--------------------------------------------------------------------------

--------------------- 3 Nave Espacial ------------------------------------

--------------------------------------------------------------------------

{-
modelaremos una Nave como un tipo algebraico, el cual nos permite construir una nave espacial,
dividida en sectores, a los cuales podemos asignar tripulantes y componentes
-}

data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]
    deriving Show
data Barril = Comida | Oxigeno | Torpedo | Combustible
    deriving Show
data Sector = S SectorId [Componente] [Tripulante]
    deriving Show
type SectorId = String
type Tripulante = String
data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
    deriving Show
data Nave = N (Tree Sector)
    deriving Show

sectores :: Nave -> [SectorId] -- Propósito: Devuelve todos los sectores de la nave.
sectores (N t) = todosLosSectores t

todosLosSectores :: Tree Sector -> [SectorId]
todosLosSectores EmptyT          = []
todosLosSectores (NodeT x t1 t2) = idDelSector x : todosLosSectores t1 ++ todosLosSectores t2

idDelSector :: Sector -> SectorId
idDelSector (S id _ _) = id


poderDePropulsion :: Nave -> Int -- Devuelve la suma de poder de propulsión de todos los motores de la nave.
--Nota: el poder de propulsión es el número que acompaña al constructor de motores.
poderDePropulsion (N t) = poderDePropulsionDeNave t

poderDePropulsionDeNave :: Tree Sector -> Int
poderDePropulsionDeNave EmptyT          = 0
poderDePropulsionDeNave (NodeT x t1 t2) = prpulsionDelSector x + poderDePropulsionDeNave t1 + poderDePropulsionDeNave t2

prpulsionDelSector :: Sector -> Int
prpulsionDelSector (S _ cs _) = propulsionDeComponenets cs

propulsionDeComponenets :: [Componente] -> Int
propulsionDeComponenets []     = 0
propulsionDeComponenets (c:cs) =  potenciaDeMotor c +  propulsionDeComponenets cs
                             
potenciaDeMotor :: Componente -> Int
potenciaDeMotor (Motor n) = n
potenciaDeMotor _         = 0


barriles :: Nave -> [Barril] --Propósito: Devuelve todos los barriles de la nave.
barriles  (N t) = barrilesEn t

barrilesEn :: Tree Sector -> [Barril]  
barrilesEn EmptyT          = []
barrilesEn (NodeT x t1 t2) = barrilesDeSector x ++ barrilesEn t1 ++ barrilesEn t2


barrilesDeSector :: Sector -> [Barril] 
barrilesDeSector (S _ cs _) = barrilesDeComponentes cs

barrilesDeComponentes :: [Componente] -> [Barril] 
barrilesDeComponentes []     = []
barrilesDeComponentes (c:cs) = barrilesDeAlmacen c ++ barrilesDeComponentes cs

barrilesDeAlmacen :: Componente -> [Barril]
barrilesDeAlmacen (Almacen bs) = bs
barrilesDeAlmacen _            = []



agregarASector :: [Componente] -> SectorId -> Nave -> Nave -- Añade una lista de componentes a un sector de la nave.
agregarASector cs s (N t) = N( agregarComponentesASector cs s t)

agregarComponentesASector :: [Componente] -> SectorId -> Tree Sector -> Tree Sector
agregarComponentesASector cs i EmptyT          = EmptyT
agregarComponentesASector cs i (NodeT x t1 t2) = if esMismoSector i x
                                                 then NodeT (sectorConComponente  x cs) t1 t2
                                                 else NodeT x (agregarComponentesASector cs i t1) (agregarComponentesASector cs i t2)    



sectorConComponente :: Sector -> [Componente] -> Sector
sectorConComponente (S id cs ts) comtes = (S id (cs ++ comtes) ts)




esMismoSector :: SectorId -> Sector -> Bool
esMismoSector i (S id _ _) = i == id 


asignarTripulanteA :: Tripulante -> [SectorId] -> Nave -> Nave
--Propósito: Incorpora un tripulante a una lista de sectores de la nave.
--Precondición: Todos los id de la lista existen en la nave.
asignarTripulanteA t i (N a) = N (asignarTripulanteANave t i a)

asignarTripulanteANave :: Tripulante -> [SectorId] -> Tree Sector -> Tree Sector
asignarTripulanteANave t []      sec             =  sec
asignarTripulanteANave t (i:ids) EmptyT          =  EmptyT  
asignarTripulanteANave t (i:ids) (NodeT x t1 t2) = if  esMismoSector i x
                                                    then NodeT (agregarTripulanteASector t x) (asignarTripulanteANave t ids t1 ) (asignarTripulanteANave t ids t2 )
                                                    else NodeT x (asignarTripulanteANave t ids t1 ) (asignarTripulanteANave t ids t2 )

agregarTripulanteASector :: Tripulante -> Sector -> Sector
agregarTripulanteASector t (S id obs trip) = (S id obs (t:trip))

            
{-
data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]
data Barril = Comida | Oxigeno | Torpedo | Combustible
data Sector = S SectorId [Componente] [Tripulante]
type SectorId = String
type Tripulante = String
data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
data Nave = N (Tree Sector)
-}

--Básicamente, solamente usás el PRIMER SectorId, puesto que NUNCA mirás en is. Esto es culpa de usar doble PM en lugar de confiar 
--en subtareas. NO SE ABRE un parámetro a menos que sea necesario...




----
sectoresAsignados :: Tripulante -> Nave -> [SectorId]
sectoresAsignados t (N a) = sectoresDeTrabajo t a

sectoresDeTrabajo :: Tripulante -> Tree Sector -> [SectorId]
sectoresDeTrabajo  t EmptyT          = [] 
sectoresDeTrabajo  t (NodeT x a1 a2) = idSiTrabaja t x ++
                                        sectoresDeTrabajo t a1 ++
                                        sectoresDeTrabajo t a2


idSiTrabaja :: Tripulante -> Sector -> [SectorId]
idSiTrabaja t (S i _ ts) = singularSi (pertenece t ts) i

pertenece :: Eq a => a -> [a] -> Bool
pertenece x []     = False
pertenece x (y:ys) = x == y || pertenece x ys


tripulantes :: Nave -> [Tripulante] -- Devuelve la lista de tripulantes, sin elementos repetidos.
tripulantes (N x) =  sinRepetirTripulantes (todosLosTripulantes x)

todosLosTripulantes :: Tree Sector ->  [Tripulante]
todosLosTripulantes EmptyT          = []
todosLosTripulantes (NodeT x t1 t2) = tripulantesDe x ++
                                      todosLosTripulantes t1 ++
                                      todosLosTripulantes t2 


tripulantesDe :: Sector ->  [Tripulante]
tripulantesDe (S _ _ ts) = ts


sinRepetirTripulantes :: [Tripulante] -> [Tripulante] 
sinRepetirTripulantes []      = []
sinRepetirTripulantes (t:ts)  = if pertenece t ts
                                   then sinRepetirTripulantes ts 
                                   else t : sinRepetirTripulantes ts
 


--------------------------------------------------------------------------

--------------------- 4 Manada de lobos ----------------------------------

--------------------------------------------------------------------------

type Presa = String -- nombre de presa
type Territorio = String -- nombre de territorio
type Nombre = String -- nombre de lobo
data Lobo = Cazador Nombre [Presa] Lobo Lobo Lobo | Explorador Nombre [Territorio] Lobo Lobo | Cria Nombre
    deriving Show
data Manada = M Lobo
    deriving Show

{-

Modelaremos una manada de lobos, como un tipo Manada, que es un simple registro compuesto
de una estructura llamada Lobo, que representa una jerarquía entre estos animales.
Los diferentes casos de lobos que forman la jerarquía son los siguientes:
Los cazadores poseen nombre, una lista de especies de presas cazadas y 3 lobos a cargo.
Los exploradores poseen nombre, una lista de nombres de territorio explorado (nombres de
bosques, ríos, etc.), y poseen 2 lobos a cargo.
Las crías poseen sólo un nombre y no poseen lobos a cargo.

-}

-- Construir un valor de tipo Manada que posea 1 cazador, 2 exploradores y que el resto sean crías.

cazador1 = Cazador "Hunter" ["Liebre"] explorador1 explorador0 cria1
cria1 = Cria "Cachorrito"
explorador1 = Explorador "Patagon" ["Bosques", "Lagos", "Rios"] cria1 explorador0
explorador0 = Explorador "Pampa" ["llanura", "Rios"] cria1 cria1

manada1 = M cazador1
manada2 = (M (Explorador "exp1" [] cazador1 cria1))

buenaCaza :: Manada -> Bool
buenaCaza (M l) = (alimentosEnManada l) >= (criasEnManada l)

alimentosEnManada :: Lobo -> Int
alimentosEnManada (Cria _)                  = 0
alimentosEnManada (Explorador _ _ l1 l2)    = alimentosEnManada l1 + alimentosEnManada l2
alimentosEnManada (Cazador _ ps l1 l2 l3)   = length ps + alimentosEnManada l1 + alimentosEnManada l2 +  alimentosEnManada l3


criasEnManada :: Lobo -> Int
criasEnManada (Cria _)                  = 1
criasEnManada (Explorador _ _ l1 l2)    = criasEnManada l1 + criasEnManada l2
criasEnManada (Cazador _ ps l1 l2 l3)   = criasEnManada l1 + criasEnManada l2 + criasEnManada l3




elAlfa :: Manada -> (Nombre, Int)
elAlfa (M l) = elLoboAlfa l


elLoboAlfa :: Lobo -> (Nombre, Int)
elLoboAlfa (Cazador n ps l1 l2 l3) = laDuplaMasGrande (n, length ps) ( laDuplaMasGrande (elLoboAlfa l1) 
                                                            (laDuplaMasGrande (elLoboAlfa l2) (elLoboAlfa l3) ))
elLoboAlfa (Explorador n _ l1 l2) = laDuplaMasGrande (n, 0) ( laDuplaMasGrande (elLoboAlfa l1) (elLoboAlfa l2))
elLoboAlfa (Cria n)               = (n, 0)

laDuplaMasGrande :: (a, Int) -> (a, Int) -> (a, Int)
laDuplaMasGrande (x, n) (y, m) = if n >= m 
                                  then (x, n)
                                  else (y, m)

-----------------------------------------


losQueExploraron :: Territorio -> Manada -> [Nombre]
losQueExploraron t (M l) = exploradoresDelTerritorio t l 

exploradoresDelTerritorio :: Territorio -> Lobo -> [Nombre]
exploradoresDelTerritorio t (Cria n)                = []
exploradoresDelTerritorio t (Explorador n ts l1 l2) = singularSi ( perteneceT t ts)   n ++  
                                                      exploradoresDelTerritorio t l1 
                                                      ++ exploradoresDelTerritorio t l2
exploradoresDelTerritorio t (Cazador _ _ l1 l2 l3)  = exploradoresDelTerritorio  t l1  
                                                      ++ exploradoresDelTerritorio t l2 
                                                      ++ exploradoresDelTerritorio t l3  

perteneceT:: Territorio -> [Territorio] -> Bool 
perteneceT x []     = False
perteneceT x (y:ys) = x == y || perteneceT x ys

---------------------------------------

-- ///////////////////////////////// exploradoresPorTerritorio nuevo intento \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

exploradoresPorTerritorio :: Manada -> [(Territorio, [Nombre])]
exploradoresPorTerritorio (M l) = lobosExploradoresPorTerritorio l

lobosExploradoresPorTerritorio :: Lobo -> [(Territorio, [Nombre])]
lobosExploradoresPorTerritorio (Cria _)                = []
lobosExploradoresPorTerritorio (Explorador n ts l1 l2) =        exploradoresDeTerritorios n ts 
                                                                (juntarTerritorios
                                                                (lobosExploradoresPorTerritorio l1 ) 
                                                                (lobosExploradoresPorTerritorio l2 ))

lobosExploradoresPorTerritorio (Cazador _ _ l1 l2 l3) =  juntarTerritorios (lobosExploradoresPorTerritorio l1) 
                                                             (juntarTerritorios (lobosExploradoresPorTerritorio l2) 
                                                                                (lobosExploradoresPorTerritorio l3)) 
                                                    

juntarTerritorios :: [(Territorio, [Nombre])] -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])] 
juntarTerritorios []     xs = xs
juntarTerritorios (y:ys) xs = juntarTerritorioYNombres y  (juntarTerritorios ys xs)

juntarTerritorioYNombres :: (Territorio, [Nombre])  -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])] 
juntarTerritorioYNombres   xs  []     = [xs]
juntarTerritorioYNombres   xs (y:ys)  = if  (territorioDe xs) == (territorioDe y) 
                                        then (territorioDe y, (unirListaDeNombres (nombresDe xs) (nombresDe y))) : ys
                                        else y : juntarTerritorioYNombres  xs ys
                                        
territorioDe :: (Territorio, [Nombre]) -> Territorio
territorioDe (t, _) = t

nombresDe :: (Territorio, [Nombre]) -> [Nombre]
nombresDe (_, ns) = ns

unirListaDeNombres :: [Nombre] -> [Nombre] -> [Nombre]
unirListaDeNombres []     xs = xs
unirListaDeNombres (n:ns) xs = if perteneceN n xs
                                then unirListaDeNombres ns xs
                                else n : unirListaDeNombres ns xs


exploradoresDeTerritorios :: Nombre -> [Territorio] -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])] 
exploradoresDeTerritorios _  []    lis  = lis
exploradoresDeTerritorios n ts []       = distribuirNombreEnTerritorios n ts 
exploradoresDeTerritorios n (t:ts) lis  = agregarNombreATerritorioExplorado n t ( exploradoresDeTerritorios n ts lis)


distribuirNombreEnTerritorios ::  Nombre -> [Territorio] -> [(Territorio, [Nombre])] 
distribuirNombreEnTerritorios n []     = []
distribuirNombreEnTerritorios n (t:ts) = (t, [n]) : distribuirNombreEnTerritorios n ts

agregarNombreATerritorioExplorado :: Nombre -> Territorio -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])] 
agregarNombreATerritorioExplorado n t []        = [(t,[n])]
agregarNombreATerritorioExplorado n t (x:xs) = if esMismoTerritorio t x
                                                          then conNombreSiFalta n x : xs
                                                          else x : agregarNombreATerritorioExplorado n t xs

esMismoTerritorio :: Territorio -> (Territorio, [Nombre]) -> Bool 
esMismoTerritorio    t1  (t2, _) = t1 == t2

conNombreSiFalta :: Nombre -> (Territorio, [Nombre]) -> (Territorio, [Nombre]) 
conNombreSiFalta n (t, ns) = (t, (agregarNombreSiFalta n ns) ) 

agregarNombreSiFalta :: Nombre -> [Nombre] -> [Nombre]
agregarNombreSiFalta n (xs) = if perteneceN n xs
                                then xs
                                else n : xs

perteneceN:: Nombre -> [Nombre] -> Bool 
perteneceN x []     = False
perteneceN x (y:ys) = x == y || perteneceN x ys


{-     charla con Fidel 
Y entonces, para el explorador n tendrías que armar algo del mismo tipo, y juntarlo
O sea, si lobosExploradoresPorTerritorio devuelve una lista...
...hacé una operación juntarTerritorios que tome dos de esas y devuelva otra con todo junto
Luego usá esa para encarar las recursiónes
Y para agregar a n, vas a precisar tener algo del mismo tipo, y volver a usar esa



CON ACUMULADOR> 
Cuando vos hiciste exploradoresDeTerritorios :: Nombre -> [Territorio] -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])] tu tercer 
argumento es algo a lo que le vas a agregar los datos necesarios
ESO se llama acumulador
O sea, le estás dando una lista para que te la "complete" y te la devuelva con lo que tenía y un poco más
O sea, acumularle los resultados 😉
En general, un acumulador en Haskell toma la forma de una función que modifica un dato
Entonces hago agregar :: Esto -> AcumuladorAntes -> AcumuladorDespués
O mejor agregar :: Esto -> BaseDeAcumulacion -> AcumuladorConEsto


Pero si lo hacés para exploradoresDeTerritorios, deberías hacerlo también para lobosExploradoresPorTerritorio
(yo las llamaría agregarExploradorA y agregarExploradoresDeManadaA...) 


-}








---------------------------------------------------------------------------------------

superioresDelCazador :: Nombre -> Manada -> [Nombre]
superioresDelCazador n (M l) = superioresDelLoboCazador n l

superioresDelLoboCazador :: Nombre -> Lobo -> [Nombre]
superioresDelLoboCazador n (Cria _)               = error "La cria no puede tener subordinados"
superioresDelLoboCazador n (Explorador n1 _ l1 l2) = n : superioresDelLoboCazador n (subordinadoDe n [l1,l2] )
superioresDelLoboCazador n (Cazador n1 _ l1 l2 l3) = n : superioresDelLoboCazador n (subordinadoDe n [l1,l2, l3] )

subordinadoDe :: Nombre -> [Lobo] -> Lobo
subordinadoDe n (l:[]) = l
subordinadoDe n (l:ls) = if esLoboSubordinado n l
                        then l
                        else subordinadoDe n ls

esLoboSubordinado :: Nombre -> Lobo -> Bool
esLoboSubordinado n (Cria  _)                = False
esLoboSubordinado n (Explorador _ _ l1 l2)  = estaElLoboSubordinado n [l1, l2]
esLoboSubordinado n (Cazador _ _ l1 l2 l3) =  estaElLoboSubordinado n [l1, l2, l3]

estaElLoboSubordinado :: Nombre -> [Lobo] -> Bool
estaElLoboSubordinado n []     = False
estaElLoboSubordinado n (l:ls) = esLoboBuscado n l || estaElLoboSubordinado n ls


esLoboBuscado :: Nombre -> Lobo -> Bool
esLoboBuscado n l =  n == (nombreDeLobo l)

nombreDeLobo :: Lobo -> Nombre
nombreDeLobo (Cazador n _ _ _ _)  = n
nombreDeLobo (Explorador n _ _ _) = n
nombreDeLobo (Cria n)             = n





--- Esta es la forma correcta de solucionar cuentaRegresiva y agregar (Esto son notas tomadas en la clase, fuera del TP)

cuentaRegr :: Int -> [Int]
cuentaRegr n = if n < 0
                then []
                else cuentaR n


cuentaR :: Int -> [Int]
cuentaR  0  = []
cuentaR  n  = n : cuentaR (n-1)


agregar :: [a] -> [a] -> [a] 
agregar []     ys = ys
agregar (x:xs) ys = x : agregar xs ys 