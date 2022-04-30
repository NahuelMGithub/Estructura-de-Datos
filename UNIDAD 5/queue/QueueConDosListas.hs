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


emptyQ = Q [] []

isEmptyQ (Q xs _) = esVacio xs  --- Si fs es null es suficiente para garantizar que la cola está vacía

esVacio :: [a] -> Bool
esVacio []     = True
esVacio (x:xs) = False

queue x q = if isEmptyQ q 
             then agregarEnFs x q
             else agregarEnBs x q

agregarEnFs :: a -> Queue a -> Queue a 
agregarEnFs x (Q [] []) = Q [x] []

agregarEnBs :: a -> Queue a -> Queue a  
agregarEnBs x (Q xs ys) = Q xs (x:ys)

firstQ (Q fs bs) = if esVacio bs 
             then last fs
             else last bs

dequeue (Q fs bs) = Q (tail fs ++ bs) []


