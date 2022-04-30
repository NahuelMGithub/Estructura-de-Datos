data Color = Azul | Rojo
    deriving Show

data Celda = Bolita Color Celda | CeldaVacia
    deriving Show

nroBolitas :: Color -> Celda -> Int
nroBolitas c CeldaVacia       = 0
nroBolitas c (Bolita col cel) = unoSi(esMisMoColor c col) + nroBolitas c cel

unoSi :: Bool -> Int
unoSi True  = 1
unoSi False = 0

esMisMoColor :: Color -> Color -> Bool
esMisMoColor Azul Azul = True
esMisMoColor Rojo Rojo = True
esMisMoColor _    _    = False

--- Ejemplos de Celdas para trabajar
celda2 = Bolita Rojo (Bolita Azul (Bolita Rojo (Bolita Azul CeldaVacia)))
celda3= Bolita Azul (Bolita Azul (Bolita Rojo (Bolita Azul CeldaVacia)))
celda3Rojos = Bolita Rojo (Bolita Rojo (Bolita Rojo (CeldaVacia)))

poner :: Color -> Celda -> Celda
poner c cel = Bolita c cel

sacar :: Color -> Celda -> Celda
sacar c (CeldaVacia)     = CeldaVacia
sacar c (Bolita col cel) = if esMisMoColor c col
                            then cel
                            else Bolita col (sacar c cel)


ponerN :: Int -> Color -> Celda -> Celda
ponerN 0 _ cel  = cel
ponerN n c cel  = Bolita c (ponerN (n-1) c cel  )


--------------------- 1.2  Camino hacia el tesoro

data Objeto = Cacharro | Tesoro
    deriving Show

data Camino = Fin | Cofre [Objeto] Camino | Nada Camino
    deriving Show



---- Caminos de ejemplo
camino0 = Fin
camino1 = Nada (camino0)
camino2 = Nada (Cofre [Cacharro] (Nada (Fin)))
camino3 = Nada (Cofre [Cacharro, Tesoro] (Nada (Fin)))
camino4 = Nada (Cofre [Cacharro] (Nada (Cofre [Tesoro, Tesoro, Tesoro] (Fin))))
camino5 = Nada (Cofre [Cacharro, Tesoro, Tesoro] (Nada (Cofre [Tesoro, Tesoro, Tesoro] (Fin))))

hayTesoro :: Camino -> Bool
hayTesoro Fin             = False
hayTesoro (Cofre objs  c) = hayTesoroEnCofre objs || hayTesoro c
hayTesoro (Nada c)        = hayTesoro c

hayTesoroEnCofre :: [Objeto] -> Bool
hayTesoroEnCofre []      = False
hayTesoroEnCofre (o:obs) = esTesoro o || hayTesoroEnCofre obs

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro  _     = False

pasosHastaTesoro :: Camino -> Int
pasosHastaTesoro (Nada c)      = 1 + pasosHastaTesoro c
pasosHastaTesoro (Cofre obs c) = if  hayTesoroEnCofre obs
                                then 0
                                else 1 + pasosHastaTesoro c


hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn 0 c           = hayTesoroAqui c
hayTesoroEn n (Nada c)    = hayTesoroEn (n-1) c
hayTesoroEn n (Cofre _ c) = hayTesoroEn (n-1) c

hayTesoroAqui :: Camino -> Bool
hayTesoroAqui (Cofre cofre c) = hayTesoroEnCofre cofre
hayTesoroAqui  _              = False


alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros n (Fin )         = n <= 0 
alMenosNTesoros n (Nada c )      = alMenosNTesoros c
alMenosNTesoros n (Cofre obs c ) = n <= totalDeTesoros obs ||
                                     alMenosNTesoros (n - totalDeTesoros obs  ) c


totalDeTesoros :: [Objeto] -> Int
totalDeTesoros []       = 0
totalDeTesoros (o:objs) = unoSi(esTesoro o) + totalDeTesoros objs


--- Desafio en clase


------------------------------------- 2. Tipos arbóreos

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
    deriving Show

arbolNum = NodeT 4 (NodeT 2 EmptyT EmptyT ) (EmptyT)
arbonVacio  = EmptyT
arbol1 = NodeT "base" (NodeT "lado A" EmptyT EmptyT) (NodeT "lado B" (NodeT "lado B A" EmptyT EmptyT) (NodeT "lado B B" EmptyT EmptyT))

ej :: Tree Int
ej = NodeT 1 
      (NodeT 2 
        (NodeT 4
          (NodeT  8 (NodeT 16 EmptyT EmptyT)
                    (NodeT 17 EmptyT EmptyT))
          (NodeT  9 (NodeT 18 EmptyT EmptyT)
                    (NodeT 19 EmptyT EmptyT)))
        (NodeT 5
          (NodeT 10 (NodeT 20 EmptyT EmptyT)
                    (NodeT 21 EmptyT EmptyT))
          (NodeT 11 (NodeT 22 EmptyT EmptyT)
                    (NodeT 23 EmptyT EmptyT))))
      (NodeT 3 
        (NodeT 6
          (NodeT 12 (NodeT 24 EmptyT EmptyT)
                    (NodeT 25 EmptyT EmptyT))
          (NodeT 13 (NodeT 26 EmptyT EmptyT)
                    (NodeT 27 EmptyT EmptyT)))
        (NodeT 7
          (NodeT 14 (NodeT 28 EmptyT EmptyT)
                    (NodeT 29 EmptyT EmptyT))
          (NodeT 15 (NodeT 30 EmptyT EmptyT)
                    (NodeT 31 EmptyT EmptyT))))


sumarT :: Tree Int -> Int
sumarT EmptyT            = 0
sumarT (NodeT n  a1 a2 ) = n + sumarT a1 + sumarT a2 


sizeT :: Tree a -> Int
sizeT EmptyT          = 0
sizeT (NodeT _ a1 a2) = 1 + sizeT a1 + sizeT a2

mapDobleT :: Tree Int -> Tree Int
mapDobleT EmptyT          = EmptyT
mapDobleT (NodeT n a1 a2) = NodeT (2*n) (mapDobleT a1) (mapDobleT a2)

perteneceT :: Eq a => a -> Tree a -> Bool
perteneceT e EmptyT = False
perteneceT e (NodeT elemento a1 a2) = e == elemento || perteneceT e a1 || perteneceT e a2

aparicionesT :: Eq a => a -> Tree a -> Int
aparicionesT e EmptyT                 = 0
aparicionesT e (NodeT elemento a1 a2) = unoSi ( e == elemento ) +
                                            aparicionesT e a1 +
                                            aparicionesT e a2 


    
leaves :: Tree a -> [a]
leaves EmptyT          = []
leaves (NodeT e a1 a2) = singularSi (esVacio a1 && esVacio a2) e ++  leaves a1 ++  leaves a2

esVacio :: Tree a -> Bool
esVacio EmptyT = True
esVacio _      = False


singularSi :: Bool -> a -> [a]
singularSi True  x = [x]
singularSi False _ = []


heightT :: Tree a -> Int
heightT EmptyT = 0
heightT (NodeT _ a1 a2) = 1 + mayor (heightT a1)  (heightT a2) 

mayor :: Int -> Int -> Int 
mayor a b = if a > b 
             then a
             else b



celdaC = Bolita Rojo CeldaVacia



mirrorT :: Tree a -> Tree a
mirrorT EmptyT          = EmptyT
mirrorT (NodeT e a1 a2) = NodeT e   (mirrorT  a2) (mirrorT a1)


-------------------------Aca para agregar el elemento x lo puse entre corchetes, no se si eso es feliz 
toList :: Tree a -> [a]
toList  EmptyT         = []
toList (NodeT x a1 a2) = toList a1 ++ [x] ++ toList a2

 
levelN :: Int -> Tree a -> [a]
levelN _ EmptyT              = []
levelN 0 (NodeT x a1 a2)     = [x] 
levelN n (NodeT x a1 a2)     = (levelN (n-1) a1) ++  (levelN (n-1) a2)


listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT          =  []
listPerLevel (NodeT x a1 a2) = [x] : juntarNiveles (listPerLevel a1)  (listPerLevel a2)

juntarNiveles :: [[a]] -> [[a]] -> [[a]] 
juntarNiveles    []       yss      = yss
juntarNiveles    xss      []       = xss
juntarNiveles    (xs:xss) (ys:yss) = (xs ++ ys) : (juntarNiveles xss yss)   

ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT          = []
ramaMasLarga (NodeT e a1 a2) = e :  elMasLargo (ramaMasLarga a1) (ramaMasLarga a2)

elMasLargo :: [a] -> [a] -> [a]
elMasLargo a b = if length a > length b 
                  then a 
                  else b


    
todosLosCaminos :: Tree a -> [[a]]  -- Este no lo super resolver. lo COPIE de la clase 4
todosLosCaminos EmptyT          =  []
todosLosCaminos (NodeT x t1 t2) = [x] : consACada x (todosLosCaminos t1)
                                           ++  consACada x (todosLosCaminos t2)

consACada :: a -> [[a]] -> [[a]]
consACada x []       = []
consACada x (ys:yss) = (x:ys) : consACada x yss 


----------------------------------   2.2 Expresiones Aritméticas

data ExpA = Valor Int | Sum ExpA ExpA | Prod ExpA ExpA | Neg ExpA
    deriving Show

eval :: ExpA -> Int
eval (Valor n)         = n
eval (Sum  expA expB ) = eval expA  + eval expB 
eval (Prod expA expB ) = eval expA  * eval expB 
eval (Neg expA)        = -1         * eval expA



---------------- con Fidel
simplificar :: ExpA -> ExpA
simplificar (Valor n)        = Valor n
simplificar (Sum  expA expB) = armarSumaSimplificada    (simplificar  expA) (simplificar expB)
simplificar (Prod expA expB) = armarProductoSimplificado (simplificar expA) (simplificar expB )
simplificar (Neg  expA)      = armarNegacionSimplificada (simplificar expA)


armarSumaSimplificada ::  ExpA -> ExpA -> ExpA 
armarSumaSimplificada  (Valor 0)  exp       = exp
armarSumaSimplificada  exp        (Valor 0) = exp
armarSumaSimplificada  expA       expB      = Suma expA expB

armarProductoSimplificado :: ExpA-> Exp A-> ExpA
armarProductoSimplificado (Valor 1) exp       = exp
armarProductoSimplificado exp       (Valor 1) = exp
armarProductoSimplificado (Valor 0) _         = Valor 0
armarProductoSimplificado _         (Valor 0) = Valor 0
armarProductoSimplificado expA      expB      = Prod expA expB

armarNegacionSimplificada :: ExpA -> ExpA 
armarNegacionSimplificada (Neg expA) = expA
armarNegacionSimplificada expA             = Neg expA