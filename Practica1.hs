--Práctica 1
--Calvario González Berenice 316119227

--Función suma. Rescibe los parámetros a y b, devuelve la suma de estos.
suma :: Int -> Int -> Int
suma a b = a + b

--Función resta. Recibe los parámetros a y b, devuelve la resta de estos.
resta :: Int -> Int -> Int
resta a b = a - b

--Función multiplicación. Resive los párametros a y b, devuelve la multiplicación de a y b.
multiplicacion :: Float -> Float -> Float
multiplicacion a b = a * b

--Función división. Recibe los parametros a y b, devuelve la división de a entre b.
division :: Float -> Float -> Float
division a b = a / b

--Función comparador. Recibe los parámetros a y b, devuelve  0 si a es igual a b, -1 si a < b y 1 si a > b.
comparador :: Float -> Float -> Float
comparador a b = if a == b then 0 else if a < b then -1 else 1

--Función potencia. Recibe los parámetos a y b, devuelve a ** b
potencia :: Int -> Int -> Int
potencia a b = a ^ b

--Función maximo. Recibe los parámetros a, b y c, devuelve el máximo entre a, b y c.
maximo :: Float -> Float -> Float -> Float
maximo a b c = if a > b && a > c then a else if b > a && b > c then b else c

--Función distanciaPuntos. Recibe los parámetros reales x1, y1, x2, y2, devuelve la distancia entre los puntos (x1, y1) y (x2, y2).
distanciaPuntos :: Float -> Float -> Float -> Float -> Float
distanciaPuntos x1 y1 x2 y2 = sqrt((x1 - x2)^2 + (y1 - y2)^2)

--Función hipotenusa. Recibe los parámetros b y h, donde b es la base y h es la altura del triángulo rectángulo, devuelve la hipotenusa del triángulo.
hipotenusa :: Float -> Float -> Float
hipotenusa b h = sqrt (h ^ 2 + b ^ 2)

--Función pendiente. Recibe los parámetros reales x1, y1, x2, y2, devuelve la pendiente entre la recta que pasa por los puntos (x1, y2) y (x2, y2).
pendiente :: Float -> Float -> Float -> Float -> Float
pendiente y2 y1 x2 x1 = (y2 - y1) / (x2 - x1)