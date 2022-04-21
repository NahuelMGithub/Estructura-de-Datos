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
conjuncion :: [Bool] -> Bool
conjuncion  []    = True
conjuncion (b:bs) = b && conjuncion bs

---- DUDA!!!!!!!!!!!!!! Como hago esta con elemento vacio
disyuncion :: [Bool] -> Bool
disyuncion  []    = False
disyuncion (b:bs) = b || disyuncion bs 

------------------

aplanar :: [[a]] -> [a]
aplanar []       = []
aplanar (l:ls)   =  l ++ aplanar ls 

---- DUDA!!!!!!!!!!!!!! si en lugar de a uso letra y no me lo toma
pertenece :: Eq a => a -> [a] -> Bool
pertenece a []     = False
pertenece a (x:xs) = a == x || pertenece a xs

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

--- Esta es como agregar, pero en la Diapo NO pone el ejemplo de  [] []
concatenar :: [a] -> [a] -> [a]
concatenar [] []       = []
concatenar [] (ys)     = ys
concatenar (x:xs) (ys) = x : concatenar xs ys

---- 13  por que no?
 
--reversa :: [a] -> [a]
--reversa  []     =  []
--reversa  (xs:x) = x : reversa xs 

--zipMaximos :: [Int] -> [Int] -> [Int]


---- Lo intente con subtareas y me tira error 
elMinimo :: Ord a => [a] -> a
elMinimo [] = error "La lista no tiene elementos"
elMinimo (x : []) = x
elMinimo (x:xs) = if x < elMinimo xs    
                    then x 
                    else elMinimo xs

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

--------------- Esta resuelto, pero no se si esta bien planteada la recusr
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


------- yo hice SumarEdades, porque NO pude poner sumatoria dado que toma int, y no se como usar edad para que me de un Int dado que le doy una lista y no una Persona
promedioEdad :: [Persona] -> Int
promedioEdad []   = error "NO hay personas"
promedioEdad (xs) = div (sumarEdades xs) (longitud xs)

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

---

--losQueLeGanan :: TipoDePokemon -> Entrenador -> Entrenador -> Int
--losQueLeGanan tipo (ConsEntrenador _ [ps1])  (ConsEntrenador _ [ps2]) = vencedoresPorTipo tipo ps1 ps2

--vencedoresPorTipo :: TipoDePokemon -> [Pokemon] -> [Pokemon] -> Int

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


esMismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
esMismoTipo Agua   Agua   = True
esMismoTipo Fuego  Fuego  = True
esMismoTipo Planta Planta = True
esMismoTipo _      _      = False


---------------------- 3

data Seniority = Junior | SemiSenior | Senior
data Proyecto = ConsProyecto String
data Rol = Developer Seniority Proyecto | Management Seniority Proyecto
data Empresa = ConsEmpresa [Rol]

proyectos :: Empresa -> [Proyecto]
proyectos (ConsEmpresa roles) = totalProyectos roles

totalProyectos :: [Rol] -> [Proyecto]
totalProyectos []     = []
totalProyectos (r:rs) =  proyectoDelRol r  : totalProyectos rs

----- Como hago para que sea sin repetir?

proyectoDelRol :: Rol -> Proyecto
proyectoDelRol (Developer _ p)  = p
proyectoDelRol (Management _ p) = p

--
{---
losDevSenior :: Empresa -> [Proyecto] -> Int
losDevSenior (ConsEmpresa roles) ps = seniorEnProyectos roles ps

seniorEnProyectos :: [Rol] -> [Proyecto] -> Int
seniorEnProyectos []     _      = 0
seniorEnProyectos (r:rs) []     = 0
seniorEnProyectos (r:rs) ps = unoSi (pertenece (proyectoDelRol r) ps )
                                    + seniorEnProyectos rs ps
                                


esSenior :: Rol -> Bool
esSenior (Developer Senior _)  = True
esSenior (Management Senior _) = True
esSenior _                     = False

---- No me toma Pertenece xq?



-}


unoSi :: Bool -> Int
unoSi True  = 1
unoSi False = 0

cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int  --Indica la cantidad de empleados que trabajan en alguno de los proyectos dados.
cantQueTrabajanEn ps (ConsEmpresa rol) = totalTrbajandoEn ps rol

totalTrbajandoEn :: [Proyecto] -> [Rol] -> Int
totalTrbajandoEn []     _     = 0
totalTrbajandoEn (p:ps) []    = 0
totalTrbajandoEn (p:ps) roles = unoSi (proyectoAsignado p roles)
                                + totalTrbajandoEn ps roles

proyectoAsignado :: Proyecto -> [Rol] -> Bool
proyectoAsignado p []     = False
proyectoAsignado p (r:rs) = nombreProyecto p == nombreProyecto(proyectoDelRol r ) 
                                || proyectoAsignado p rs


nombreProyecto :: Proyecto -> String
nombreProyecto (ConsProyecto s) = s


--------