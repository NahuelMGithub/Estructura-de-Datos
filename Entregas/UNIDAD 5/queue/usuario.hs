-- ////////////////////////////// 3. Queue (cola) \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

--import Queue1
--import Queue2
import QueueDosListas


lengthQ :: Queue a -> Int --Cuenta la cantidad de elementos de la cola.
queueToList :: Queue a -> [a] --Dada una cola devuelve la lista con los mismos elementos, donde el orden de la lista es el de la cola.
--Nota: chequear que los elementos queden en el orden correcto.
unionQ :: Queue a -> Queue a -> Queue a --Inserta todos los elementos de la segunda cola en la primera


-- Costo: O(n) donde n es la longitud de la cola
lengthQ q = if isEmptyQ q 
                 then 0
                 else 1 + lengthQ (dequeue q)

 -- Costo: O(n) donde n es la longitud de la cola   
queueToList q =if isEmptyQ q 
                 then []
                 else firstQ q :  queueToList (dequeue q  )




 -- Costo: O(n) donde n es la longitud de la cola  q 2
unionQ q1 q2 = if isEmptyQ q2
                then q1
                else unionQ (queue (firstQ q2) q1) (dequeue q2)



{-
data Queue a = Q [a] [a] -- Q [ Front Stack] [ Back Stack]

emptyQ :: Queue a --Crea una cola vacía.
isEmptyQ :: Queue a -> Bool --Dada una cola indica si la cola está vacía.
queue :: a -> Queue a -> Queue a --Dados un elemento y una cola, agrega ese elemento a la cola.
firstQ :: Queue a -> a --Dada una cola devuelve el primer elemento de la cola.
dequeue :: Queue a -> Queue a --Dada una cola la devuelve sin su primer elemento.
-}