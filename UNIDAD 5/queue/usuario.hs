-- ////////////////////////////// 3. Queue (cola) \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

import Queue1
--import queue2.hs


lengthQ :: Queue a -> Int --Cuenta la cantidad de elementos de la cola.
queueToList :: Queue a -> [a] --Dada una cola devuelve la lista con los mismos elementos, donde el orden de la lista es el de la cola.
--Nota: chequear que los elementos queden en el orden correcto.
unionQ :: Queue a -> Queue a -> Queue a --Inserta todos los elementos de la segunda cola en la primera



lengthQ q = if isEmptyQ q 
                 then 0
                 else 1 + lengthQ (dequeue q)

    
queueToList q =if isEmptyQ q 
                 then []
                 else firstQ q :  queueToList (dequeue q  )

unionQ q1 q2 = if isEmptyQ q2
                then q1
                else queue (firstQ q2) ( unionQ q1 (dequeue q2) )


