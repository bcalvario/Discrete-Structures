--Berenice Calvario Gonzalez 316119227

--Funcion disyuncion. La funcion recibe los parametros p y q. La funcion devuelve la conjuncion de p y q sin el uso de operadores logicos de haskell.
disyuncion :: Bool -> Bool -> Bool
disyuncion True _ = True
disyuncion _ True = True
--disyuncion True True = True
disyuncion _ _ = False


--Funcion conjuncion. La funcion recibe los parametros p y q. La funcion devuelve la conjuncion de p y q sin el uso de operadores logicos de haskell.
conjuncion True True = True
conjuncion _ _ = False

--funcion implicacion. La funcion recibe los parametros p y q, devuelve la implicacion de p y q
implicacion :: Bool -> Bool -> Bool
implicacion True False = False
implicacion _ _ = True

--Funcion dobleImplica. La funcion recibe los parametros p y q, devuelve la doble equivalencia de p y q.
dobleImplica :: Bool -> Bool -> Bool
dobleImplica True True = True
dobleImplica False False = True
dobleImplica _ _ = False 

--LISTAS
--Una funcion recursiva que recibe una lista de cuelaquier tipo y calcula la longitud de esta.
longitud :: [a] -> Int
longitud [] = 0
longitud (_:xs) = 1 + longitud xs

--Una funcion recursiva que recibe una lista de numeros y devuelve el resultado de la suma de todos ellos
sumaNumeros :: Num a => [a] -> a
sumaNumeros [] = 0
sumaNumeros (x:xs) = x + sumaNumeros xs

--Maximo en una lista. La funcion recibe como parametros una lista xs de valores numericos y devuelve el maximo de esta.
maximo::Ord a => [a] -> a  
maximo (y:ys) = maximoAux ys y
--maximo [x] = x
--maximo (x:xs) = if x > maximo xs
  --              then x 
    --            else maximo xs     
maximoAux:: Ord a => [a] -> a -> a 
maximoAux [ ] x = x
maximoAux (y:ys) x = if x <  y
                     then maximoAux ys y
                     else maximoAux ys x

--Lugar del elemento. La funcion recibe como parametros, un indice i y una lista xs, devuelve el elemento que esta en el lugar i en la lista xs
indiceDe :: Int -> [a] -> a
indiceDe _ [] = error "indice invalido"
indiceDe 0  (x:xs) = x
indiceDe n (x:xs) = indiceDe (n-1) xs 

--Inserta elemento. La funcion recibe los parametros, un elemento x, una lista xs y un booleano b. La funcion regresa lal lista xs mas el elemento x,
--si b = True el elemento se inserta al principio de la lista, si b = False se inserta al final (Con un solo operador de concatencaion).
insertaElemento :: a -> [a] -> Bool -> [a]
insertaElemento x l True = x:l
insertaElemento x l False = l ++ [x]

--Palindromo. Una funsion recursiva que decide si una lista es un palindromo(tambien puedes recibir una cadena).
esPalindromo :: (Eq a) => [a] -> Bool
esPalindromo l = if (l == (reversa l))
                then True
                else False

--Reversa. Funcion recursiva que regresa la reversa de una lista.
reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = reversa (xs) ++ [x]

--Convertir en conjunto. Funcion recursiva que recibe una lista y elimina las repeticiones de elementos, es decir, convierte la lista en un conjunto. Hint: utiliza listas por comprensio.
aConjunto :: (Eq a) => [a] -> [a]
aConjunto [] = []
aConjunto (x:xs) = [x] ++ aConjunto [ y | y <- xs, y /= x]

--Union de conjuntos. Una funcion recursiva que calcula la union de dos listas. No se puede usar listas por comprension.
union :: (Eq a) => [a] -> [a] -> [a]
union l1 l2 = aConjunto(l1 ++ l2)

--Interseccion de conjuntos. Funcion recursiva que calcula la interseccion de dos listas. No se puede usar listas por comprension.
interseccion :: (Eq a) => [a] -> [a] -> [a]
interseccion [] l2 = [] 
interseccion (x:xs) l = if x `elem` l then x : interseccion xs l else interseccion xs l

--Producto cartesiano. Funcion recursiva que recibe dos listas y devuelve el producto cruz de ambas listas.
productoCruz :: [a] -> [a] -> [(a,a)]
productoCruz l1 l2 = [(x,y) | x <- l1, y <- l2]

--Funcion recursiva que calcula la diferencia simetrica de dos listas.
diferenciaSimetrica :: (Eq a) => [a] -> [a] -> [a]
diferenciaSimetrica l1 l2 = [x | x <- l1, notElem x l2] ++ [y | y <- l2, notElem y l1]


--PUNTO EXTRA

--Funcion que calcule el conjunto de divisores de un numero entero. HINT: Utiliza listas por comprension.
divisores ::  Int -> [Int]
divisores n = [x | x <- [1..n], (mod n x) == 0]

--Funcion recursiva que recibe una lista l de elementos (puede incluir elementos repetidos) y calcula el conjunto potencia
--(Sin repetir pares ordenados) del conjunto que representa la lista l. HINT: Utiliza la funcion aConjuntos.
conjuntoPotencia :: (Eq a) => [a] -> [(a,a)]
conjuntoPotencia l = auxCPj 0 l

auxCPi :: a -> [a] -> [(a,a)]
auxCPi x [] = []
auxCPi x (a:as) = [(x,a)] ++ (auxCPi x as) 

auxCPj :: Int -> [a] -> [(a,a)]
auxCPj n (x:xs) = if n >= (longitud (x:xs))
                  then []
                  else (auxCPi (indiceDe n (x:xs) ) (x:xs) ) ++ (auxCPj (n+1) (x:xs))