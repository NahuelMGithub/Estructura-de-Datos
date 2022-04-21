{-- 1. Números enteros --} 

--1 A 

sucesor :: Int -> Int
sucesor n = n + 1

--1 B

sumar :: Int -> Int -> Int
sumar    n m = n + m

--1 C 

divisionYResto :: Int -> Int -> (Int, Int)
divisionYResto n m = (div n m, mod n m)

-- 1 D

maxDelPar :: (Int,Int) -> Int
maxDelPar (n, m) = if n > m 
                    then n 
                    else m


{-- 
2. De 4 ejemplos de expresiones diferentes que denoten el número 10, 
utilizando en cada expresión a todas las funciones del punto anterior.
Ejemplo: maxDePar (divisionYResto (suma 5 5) (sucesor 0))

--}

-- sumar 6 4
--divisionEntera 20 2
--sucesor 9
--sucesor (sucesor(sumar 7 1))

-------------------------------------------------------------------

--------------- 2. Tipos enumerativos

data Dir = Norte | Este | Sur | Oeste
    deriving Show

-- 2-1 a

opuesto :: Dir -> Dir
opuesto Este  = Oeste
opuesto Oeste = Este
opuesto Sur   = Norte
opuesto Norte = Sur

-- 2-1 b

iguales :: Dir -> Dir -> Bool
iguales Norte Norte = True
iguales Oeste Oeste = True
iguales Sur Sur     = True
iguales Este Este   = True
iguales _ _         = False

--2-1 c

siguiente :: Dir -> Dir
siguiente Norte = Este
siguiente Este  = Sur
siguiente Sur   = Oeste
siguiente Oeste = Norte

--------

{--
Definir el tipo de dato DiaDeSemana, con las alternativas Lunes, Martes, Miércoles, Jueves,
Viernes, Sabado y Domingo. Supongamos que el primer día de la semana es lunes, y el último
es domingo. Luego implementar las siguientes funciones:

--}

data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo
    deriving Show

-- 2-2 a

primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
primeroYUltimoDia = (Lunes, Domingo)

--2-2-b

empiezaConM :: DiaDeSemana -> Bool
empiezaConM Martes    = True
empiezaConM Miercoles = True
empiezaConM _         = False

-- 2-2-c

vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues Domingo Domingo = False
vieneDespues Domingo _       = True
vieneDespues Sabado Domingo  = False
vieneDespues Sabado Sabado   = False
vieneDespues Sabado _        = True
vieneDespues Viernes Domingo = False
vieneDespues Viernes Sabado  = False
vieneDespues Viernes Viernes = False
vieneDespues Viernes _       = True
vieneDespues Jueves Domingo  = False
vieneDespues Jueves Sabado   = False
vieneDespues Jueves Viernes  = False
vieneDespues Jueves Jueves   = False
vieneDespues Jueves _        = True
vieneDespues Miercoles Lunes = True
vieneDespues Miercoles Martes= True
vieneDespues Miercoles _     = False
vieneDespues Martes    Lunes = True
vieneDespues _         _     = False

vieneDespues' :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues'  a b = indice a > indice b 
     
indice :: DiaDeSemana  -> Int
indice Lunes     = 0
indice Martes    = 1
indice Miercoles = 2
indice Jueves    = 3
indice Viernes   = 4
indice Sabado    = 5
indice Domingo   = 6



-- 2-2-D

estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio Lunes   = False
estaEnElMedio Domingo = False
estaEnElMedio _       = True

------ 2-3

--2 3 a

negar :: Bool -> Bool
negar True = False
negar False = True

--2 3 b

implica :: Bool -> Bool -> Bool
implica True False = False
implica _    _     = True

--2 3 c

and :: Bool -> Bool -> Bool
and True b = b
and False _    = False

--2 3 d

or :: Bool -> Bool -> Bool
or False b     = b
or True  _     = True 


{-
True || error ""
False && error ""
-}
------------------------ 3 Registros

data Persona = P String Int -- Nombre y Edad
    deriving Show

nombre :: Persona -> String
nombre (P n _)  = n

edad ::  Persona -> Int
edad (P _ e) = e 

crecer :: Persona -> Persona
crecer (P n e ) = (P n (e+1))

cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre nombre (P n e) = (P nombre e)

esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra (P _ e1) (P _ e2) = e1 > e2
                                    

laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor (P n1 e1) (P n2 e2) = if e1 >= e2 
                                    then P n1 e1
                                    else P n2 e2


{- 
Definir los tipos de datos Pokemon, como un TipoDePokemon (agua, fuego o planta) y un
porcentaje de energía; y Entrenador, como un nombre y dos Pokémon. 

-}

data TipoDePokemon = Agua | Fuego | Planta
    deriving Show

data Pokemon = ConsPokemon TipoDePokemon Int
    deriving Show

data Entrenador = E String Pokemon Pokemon
    deriving Show


charmander = ConsPokemon Fuego 76
bolbasor   = ConsPokemon Planta 69
squrtul    = ConsPokemon Agua   91

ash = E "Ash" charmander bolbasor
misty = E "Misty" squrtul squrtul

-- 3.2
{--
superaAMal :: Pokemon -> Pokemon -> Bool
superaAMal (Pokemon Fuego _)  (Pokemon Planta _) = True
superaAMal (Pokemon Planta _) (Pokemon Agua _)   = True
superaAMal (Pokemon Agua _)   (Pokemon Fuego _)  = True
superaAMal _                   _                 = False

superaA :: Pokemon -> Pokemon -> Bool
superaA (Pok t1 _) (Pok t1 _) = elementoSuperaA t1 t2
--}

elementoSuperaA :: TipoDePokemon -> TipoDePokemon -> Bool
elementoSuperaA Agua   Fuego  = True
elementoSuperaA Fuego  Planta = True
elementoSuperaA Planta Agua   = True
elementoSuperaA _      _      = False 




cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonDe t (E _ p1 p2) = cantidadDe t p1 p2

cantidadDe :: TipoDePokemon -> Pokemon -> Pokemon -> Int
cantidadDe  tipo (ConsPokemon t1 _) (ConsPokemon t2 _) = unoSiEsmismoTipo tipo t1 +
                                                         unoSiEsmismoTipo tipo t2

unoSiEsmismoTipo :: TipoDePokemon -> TipoDePokemon -> Int
unoSiEsmismoTipo Agua   Agua   = 1
unoSiEsmismoTipo Fuego  Fuego  = 1
unoSiEsmismoTipo Planta Planta = 1
unoSiEsmismoTipo _      _      = 0

unoSi :: Bool -> Int
unoSi True  = 1
unoSi False = 0

{-------------- mal-------------------------------------------------
cantidadDePokemonDe Agua (E _  (Pokemon Agua _) (Pokemon Agua _))       = 2
cantidadDePokemonDe Agua (E _  (Pokemon Agua _) (Pokemon _ _))          = 1
cantidadDePokemonDe Agua (E _  (Pokemon _ _) (Pokemon Agua _))          = 1
cantidadDePokemonDe Fuego (E _  (Pokemon Fuego _) (Pokemon Fuego _))    = 2
cantidadDePokemonDe Fuego (E _  (Pokemon Fuego _) (Pokemon _ _))        = 1
cantidadDePokemonDe Fuego (E _  (Pokemon _ _) (Pokemon Fuego _))        = 1
cantidadDePokemonDe Planta (E _  (Pokemon Planta _) (Pokemon Planta _)) = 2
cantidadDePokemonDe Planta (E _  (Pokemon Planta _) (Pokemon _ _))      = 1
cantidadDePokemonDe Planta (E _  (Pokemon _ _) (Pokemon Planta _))      = 1
cantidadDePokemonDe _       _                                           = 0
-----------------------------------------------------------------------}


juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]
juntarPokemon ((E _ p1 p2), (E _ p3 p4)) = [p1, p2, p3, p4]

---------------- EJERCICIO 4  Funciones polimórficas
{--
4. Funciones polimórficas
1. Defina las siguientes funciones polimórficas:
a) loMismo :: a -> a
Dado un elemento de algún tipo devuelve ese mismo elemento.
b) siempreSiete :: a -> Int
Dado un elemento de algún tipo devuelve el número 7.
c) swap :: (a,b) -> (b, a)
Dadas una tupla, invierte sus componentes.
¿Por qué existen dos variables de tipo diferentes?

-}

--4 a

loMismo :: a -> a
loMismo a = a

-- 4 b

siempreSiete :: a -> Int
siempreSiete a = 7

-- 4 c

swap :: (a,b) -> (b, a)
swap (a, b) = (b, a)


----------------------- 5. Pattern matching sobre listas


data Gusto = Chocolate | Limon | DDL
    deriving Show   

data Helado = Vasito Gusto | Cucurucho Gusto Gusto
    deriving Show

-- Aca vemos cono la Palabra Vasito es un Constructor de Helado, como la P era de Persona
-- Pero en este caso tiene variedades, por eso tiene mas constructores, como la palabra Cucurucho

lista1 = [1, 2, 3]
listaVacia = []

estaVacia :: [a] -> Bool
estaVacia [] = True
estaVacia _  = False

elPrimero :: [a] -> a
elPrimero (x:xs) = x

sinElPrimero :: [a] -> [a]
sinElPrimero (_:xs) = xs

splitHead :: [a] -> (a, [a])
splitHead (x:xs) = (x, xs)
