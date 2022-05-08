-- ////////////////////////////////////// 5. Queue con dos listas \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

{-
Implemente la interfaz de Queue pero en lugar de una lista utilice dos listas. Esto permitirá
que todas las operaciones sean constantes (aunque alguna/s de forma amortizada).
La estructura funciona de la siguiente manera. Llamemos a una de las listas fs (front stack) y
a la otra bs (back stack). Quitaremos elementos a través de fs y agregaremos a través de bs, pero
todas las operaciones deben garantizar el siguiente invariante de representación: Si fs se encuentra
vacía, entonces la cola se encuentra vacía.
¿Qué ventaja tiene esta representación de Queue con respecto a la que usa una sola lista?

-}

--Una Queue es un tipo abstracto de datos de naturaleza FIFO (first in, first out). 

module QueueDosListas
    (Queue, emptyQ, isEmptyQ, queue, firstQ, dequeue)
    where
data Queue a = Q [a] [a] -- Q [ Front Stack] [ Back Stack]

emptyQ :: Queue a --Crea una cola vacía.
isEmptyQ :: Queue a -> Bool --Dada una cola indica si la cola está vacía.
queue :: a -> Queue a -> Queue a --Dados un elemento y una cola, agrega ese elemento a la cola.
firstQ :: Queue a -> a --Dada una cola devuelve el primer elemento de la cola.
dequeue :: Queue a -> Queue a --Dada una cola la devuelve sin su primer elemento.

--INV.REP: la primer lista no puede estar vacía si la segunda lista tiene elementos.

-- Costo: O(1)
emptyQ = Q [] [] 


-- Costo: O(1)
isEmptyQ (Q xs _) = null xs  --- Si fs es null es suficiente para garantizar que la cola está vacía


-- Costo: O(1)
queue x (Q [] [] ) =  Q [x] [] 
queue x (Q xs ys ) =  Q xs (x:ys)       

{-
agregarEnFs :: a -> Queue a -> Queue a 
agregarEnFs x (Q [] []) = Q [x] []

agregarEnBs :: a -> Queue a -> Queue a  
agregarEnBs x (Q xs ys) = Q xs (x:ys)

-}
-- Costo: O(1)
firstQ (Q fs bs) = head fs
             

{-
secuencia esperada
tengo esto  [1]     [2,3] 
agrego      [1]     [2,3,4]
saco        [2,3,4] []
agrego      [2,3,4] [5]
agrego      [2,3,4] [5, 6]
saco        [3,4] [5, 6]
-} 


-- Costo: O(1) el costo es amortizado, ya que la función posee un costo 1, en caso de que fs tenga mas de un elemento.
-- y posee un costo elevado (n*2, donde n es la longitud de bs) pero SOLO en caso de que fs tenga un único elemento.
dequeue (Q fs bs) = if tieneUnSoloElemento fs
                    then (Q (reversa bs) [])
                    else (Q (tail fs) bs)
-- Costo: O(1)    
tieneUnSoloElemento :: [a] -> Bool
tieneUnSoloElemento (x: []) = True
tieneUnSoloElemento _       = False

-- Costo: O(n*2) donde n es la longitud de la lista
reversa :: [a] -> [a]
reversa []      = []
reversa  (x:xs) = reversa xs ++ [x]