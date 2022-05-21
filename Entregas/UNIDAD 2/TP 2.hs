{--1. Recursión sobre listas
--}

sumatoria :: [Int] -> Int
sumatoria []     = 0
sumatoria (x:xs) = x + sumatoria xs

longitud :: [a] -> Int
longitud []     = 0
longitud (x:xs) = 1 + longitud xs

sucesores :: [Int] -> [Int]
sucesores []     = []
sucesores (n:ns) = n+1 : sucesores ns


---- DUDA!!!!!!!!!!!!!! Como hago esta con elemento vacio
--conjuncion :: [Bool] -> Bool
--conjuncion  []    = True
--conjuncion (b:bs) = b && conjuncion bs

conjuncion :: [Bool] -> Bool
conjuncion  []     = error "La lista no puede estar vacia"
conjuncion  (b:[]) = b
conjuncion  (b:bs) = b && conjuncion bs


---- DUDA!!!!!!!!!!!!!! Como hago esta con elemento vacio
disyuncionMal :: [Bool] -> Bool
disyuncionMal  []    = False
disyuncionMal (b:bs) = b || disyuncion bs 

disyuncion :: [Bool] -> Bool
disyuncion  []     = error "La lista no puede estar vacia"
disyuncion  (b:[]) = b
disyuncion (b:bs)  = b || disyuncion bs 

------------------
--- Hasta ahora solo hice un comit. Me lo permitio. cambie el aplanar y ahora quiero hacer un psuh.
aplanar :: [[a]] -> [a]
aplanar []       = []
aplanar (ls:lss)   =  ls ++ aplanar lss 


pertenece :: Eq a => a -> [a] -> Bool
pertenece y []     = False
pertenece y (x:xs) = y == x || pertenece y xs

apariciones :: Eq a => a -> [a] -> Int
apariciones a []     = 0
apariciones a (x:xs) = if a == x
                        then 1 + apariciones a xs
                        else apariciones a xs

--9
losMenoresA :: Int -> [Int] -> [Int]
losMenoresA n []     = []
losMenoresA n (x:xs) = if x < n 
                        then x : losMenoresA n xs
                        else losMenoresA n xs

lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA n []      = []
lasDeLongitudMayorA  n (l:ls) = if n < (longitud l)
                                 then l : lasDeLongitudMayorA n ls
                                 else lasDeLongitudMayorA n ls
            
agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal []     a = a : []
agregarAlFinal (x:xs) a = x : agregarAlFinal xs a


concatenar :: [a] -> [a] -> [a]
concatenar []     ys = ys
concatenar (x:xs) ys = x : concatenar xs ys


 
reversa :: [a] -> [a]
reversa  []     =  []
reversa  (x:xs) =   agregarAlFinal (reversa xs)  x



zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos []      _     = []
zipMaximos _       []    = []
zipMaximos (n:ns) (m:ms) = (maximo n m) : zipMaximos ns ms

maximo :: Int -> Int -> Int
maximo x y = if x > y 
              then x
              else y 


elMinimo :: Ord a => [a] -> a
elMinimo [] = error "La lista no tiene elementos"
elMinimo (x : []) = x
elMinimo (x:xs) = minimo x (elMinimo xs)

minimo :: Ord a => a -> a -> a
minimo x y = if x > y
            then y
            else x

------------------------------------------- 2 Recursión sobre números

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

cuentaRegresiva :: Int -> [Int]
cuentaRegresiva 0 = []
cuentaRegresiva n = n : cuentaRegresiva (n-1)

repetir :: Int -> a -> [a]
repetir 0 x = []
repetir n x = x : repetir (n-1) x 

losPrimeros :: Int -> [a] -> [a]
losPrimeros 0 _      = []
losPrimeros n []     = []
losPrimeros n (x:xs) = x : losPrimeros (n-1) xs


sinLosPrimeros :: Int -> [a] -> [a]
sinLosPrimeros 0 xs     = xs
sinLosPrimeros n []     = []
sinLosPrimeros n (x:xs)   = sinLosPrimeros (n-1) (xs)


------------------------------------- Registros

data Persona = ConsPer  String Int  
    deriving Show


mayoresA :: Int -> [Persona] -> [Persona]
mayoresA n []     = []
mayoresA n (p:ps) = if edad p > n 
                     then p : mayoresA n ps
                     else mayoresA n ps

edad :: Persona -> Int
edad (ConsPer n e) = e

listaFamilia = [ConsPer "Nahue" 33, ConsPer "Cami" 18 ]


------- yo hice SumarEdades, porque NO pude poner sumatoria dado que toma int,
--  y no se como usar edad para que me de un Int dado que le doy una lista y no una Persona
promedioEdad :: [Persona] -> Int
promedioEdad []   = error "NO hay personas"
promedioEdad xs   = div (sumarEdades xs) (longitud xs)

sumarEdades :: [Persona] -> Int
sumarEdades []     = 0
sumarEdades (p:ps) = edad p + sumarEdades ps


elMasViejo :: [Persona] -> Persona
elMasViejo (p:[]) = p
elMasViejo (p:ps) = elMayorEntre p (elMasViejo ps)

elMayorEntre :: Persona -> Persona -> Persona 
elMayorEntre (ConsPer n1 e1) (ConsPer n2 e2) = if e1 > e2
                                                then ConsPer n1 e1
                                                else ConsPer n2 e2

------------------------ 2

data TipoDePokemon = Agua | Fuego | Planta
    deriving Show

data Pokemon = ConsPokemon TipoDePokemon Int
    deriving Show

data Entrenador = ConsEntrenador String [Pokemon]
    deriving Show

cantPokemon :: Entrenador -> Int
cantPokemon  (ConsEntrenador _ ps) = longitud ps



---------------- aca creo que use muchas subtareas

cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
cantPokemonDe t (ConsEntrenador _ ps) = todosLosDeTipo t ps

todosLosDeTipo :: TipoDePokemon -> [Pokemon] -> Int
todosLosDeTipo t []     = 0
todosLosDeTipo t (p:ps) = unoSiEsmismoTipo t (tipoDePokemon p) 
                          + todosLosDeTipo t ps


unoSiEsmismoTipo :: TipoDePokemon -> TipoDePokemon -> Int
unoSiEsmismoTipo Agua   Agua   = 1
unoSiEsmismoTipo Fuego  Fuego  = 1
unoSiEsmismoTipo Planta Planta = 1
unoSiEsmismoTipo _      _      = 0

tipoDePokemon :: Pokemon -> TipoDePokemon
tipoDePokemon (ConsPokemon t _) = t

---------------------------- FIDEL! 

losQueLeGanan :: TipoDePokemon -> Entrenador -> Entrenador -> Int
losQueLeGanan tipo (ConsEntrenador _ ps1)  (ConsEntrenador _ ps2) =
             todosLosQueGanan ( losDeMismoTipo  tipo ps1 ) ps2


losDeMismoTipo :: TipoDePokemon -> [Pokemon]-> [Pokemon]
losDeMismoTipo t []     = []
losDeMismoTipo t (p:ps) = if esMismoTipo t (tipoDePokemon p)
                           then p : losDeMismoTipo t ps
                           else losDeMismoTipo t ps

esMismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
esMismoTipo Agua    Agua  = True
esMismoTipo Fuego   Fuego = True
esMismoTipo Planta  Planta = True
esMismoTipo _       _     = False
 
todosLosQueGanan :: [Pokemon]-> [Pokemon] ->  Int
todosLosQueGanan []  _     = 0
todosLosQueGanan _   []    = 0
todosLosQueGanan (p:ps) ys = unoSi(superaATodos p ys)
                            + todosLosQueGanan ps ys

superaATodos :: Pokemon -> [Pokemon] -> Bool
superaATodos p []     = True
superaATodos p (x:xs) = superaA p x && superaATodos p xs


superaA :: Pokemon -> Pokemon -> Bool
superaA (ConsPokemon t1 _) (ConsPokemon t2 _) = elementoSuperaA t1 t2

elementoSuperaA :: TipoDePokemon -> TipoDePokemon -> Bool
elementoSuperaA Agua   Fuego  = True
elementoSuperaA Fuego  Planta = True
elementoSuperaA Planta Agua   = True
elementoSuperaA _      _      = False 

-------------------------------------

ash = ConsEntrenador "Ash" [ConsPokemon Agua 2, ConsPokemon Fuego 3, ConsPokemon Planta 5]
misty = ConsEntrenador "Mysty" [ConsPokemon Agua 2, ConsPokemon Agua 3, ConsPokemon Agua 5]

esMaestroPokemon :: Entrenador -> Bool
esMaestroPokemon (ConsEntrenador _ (ps)) = poseeTodosLosTipos ps

poseeTodosLosTipos :: [Pokemon] -> Bool
poseeTodosLosTipos (pokemons) = hayAlMenosUnTipo Fuego pokemons
                               && hayAlMenosUnTipo Agua pokemons
                               && hayAlMenosUnTipo Planta pokemons

hayAlMenosUnTipo:: TipoDePokemon -> [Pokemon] -> Bool
hayAlMenosUnTipo t []     = False
hayAlMenosUnTipo t (p:ps) = esMismoTipo t (tipoDePokemon p)
                            || hayAlMenosUnTipo t ps



---------------------- 3

data Seniority = Junior | SemiSenior | Senior
data Proyecto = ConsProyecto String
data Rol = Developer Seniority Proyecto | Management Seniority Proyecto
data Empresa = ConsEmpresa [Rol]

proyectos :: Empresa -> [Proyecto]
proyectos (ConsEmpresa roles) = sinRepetir ( totalProyectos roles )

totalProyectos :: [Rol] -> [Proyecto]
totalProyectos []     = []
totalProyectos (r:rs) =  proyectoDelRol r  : totalProyectos rs

sinRepetir :: [Proyecto] -> [Proyecto] 
sinRepetir []     = []
sinRepetir (x:xs) = let xs' = sinRepetir xs
                     in if perteneceProyecto x xs' 
                         then xs'
                         else x : xs'    

perteneceProyecto :: Proyecto -> [Proyecto] -> Bool
perteneceProyecto x []     = False
perteneceProyecto x (p:ps)  = nombreProyecto x == nombreProyecto p
                                || perteneceProyecto x ps 

             

proyectoDelRol :: Rol -> Proyecto
proyectoDelRol (Developer  _ p) = p
proyectoDelRol (Management _ p) = p

--

losDevSenior :: Empresa -> [Proyecto] -> Int
losDevSenior (ConsEmpresa roles) ps = seniorEnProyectos roles ps

seniorEnProyectos :: [Rol] -> [Proyecto] -> Int
seniorEnProyectos (r:rs) ps = unoSi (seniorYParticipa r ps) + seniorEnProyectos rs ps

seniorYParticipa :: Rol -> [Proyecto] -> Bool
seniorYParticipa r (p:ps) =  esSenior (seniorityDelRol r) && perteneceProyecto (proyectoDelRol r) ps 


seniorityDelRol :: Rol -> Seniority
seniorityDelRol (Developer  s _) = s
seniorityDelRol (Management s _) = s

esSenior :: Seniority -> Bool
esSenior  Senior  = True
esSenior  _       = False

singularSi :: Bool -> a -> [a]
singularSi True  x = [x]
singularSi False _ = []


unoSi :: Bool -> Int
unoSi True  = 1
unoSi False = 0

cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int  --Indica la cantidad de empleados que trabajan en alguno de los proyectos dados.
cantQueTrabajanEn ps (ConsEmpresa rol) = totalTrbajandoEn ps rol

totalTrbajandoEn :: [Proyecto] -> [Rol] -> Int
totalTrbajandoEn []     _     = 0
totalTrbajandoEn (p:ps) roles = unoSi (proyectoAsignado p roles)
                                + totalTrbajandoEn ps roles

proyectoAsignado :: Proyecto -> [Rol] -> Bool
proyectoAsignado p []     = False
proyectoAsignado p (r:rs) = nombreProyecto p == nombreProyecto(proyectoDelRol r ) 
                                || proyectoAsignado p rs


nombreProyecto :: Proyecto -> String
nombreProyecto (ConsProyecto s) = s


asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
asignadosPorProyecto (ConsEmpresa rs) = procesarProyectosDeRoles rs

procesarProyectosDeRoles :: [Rol] -> [(Proyecto, Int)]
procesarProyectosDeRoles []      = []
procesarProyectosDeRoles (r:rs)  =  aumentarEnUnoTupla (proyectoDelRol r) (procesarProyectosDeRoles rs)

aumentarEnUnoTupla :: Proyecto -> [(Proyecto, Int)] -> [(Proyecto, Int)] 
aumentarEnUnoTupla p []     = [(p, 1)]
aumentarEnUnoTupla p (x:xs) = if esMismoProyecto p x
                                then tuplaAumentada x : xs
                                else x : aumentarEnUnoTupla p xs

esMismoProyecto :: Proyecto -> (Proyecto, Int) -> Bool
esMismoProyecto p (x, _) = nombreProyecto p == nombreProyecto x


tuplaAumentada :: (Proyecto, Int) -> (Proyecto, Int)
tuplaAumentada (p, n) = (p, (n+1))

