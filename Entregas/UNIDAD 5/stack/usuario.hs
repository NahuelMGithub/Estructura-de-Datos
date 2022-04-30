import Stack1

--data Stack a = S [a]
{-
emptyS :: Stack a --Crea una pila vacía.
isEmptyS :: Stack a -> Bool --Dada una pila indica si está vacía.
push :: a -> Stack a -> Stack a --Dados un elemento y una pila, agrega el elemento a la pila.
top :: Stack a -> a --Dada un pila devuelve el elemento del tope de la pila.
pop :: Stack a -> Stack a --Dada una pila devuelve la pila sin el primer elemento.
lenS :: Stack a -> Int --Dada una pila devuelve la longitud.
-}

apilar :: [a] -> Stack a --Dada una lista devuelve una pila sin alterar el orden de los elementos.
desapilar :: Stack a -> [a] --Dada una pila devuelve una lista sin alterar el orden de los elementos.
insertarEnPos :: Int -> a -> Stack a -> Stack a --Dada una posicion válida en la stack y un elemento, ubica dicho elemento en dicha
--posición (se desapilan elementos hasta dicha posición y se inserta en ese lugar).

apilar []     = emptyS
apilar (x:xs) = push x (apilar xs)


desapilar stack = if (isEmptyS stack)
                    then []
                    else top stack : desapilar (pop stack)


insertarEnPos 0 x s = push x s
insertarEnPos n x s = push (top s)   (insertarEnPos (n-1) x (pop s)  )