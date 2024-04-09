module Dibujo (figura, encimar, apilar,
          juntar, rot45, rotar, espejar,
          foldDib, mapDib, Dibujo(..)
        ) where

--nuestro lenguaje 
-- data Dibujo a = Dibujo deriving (Eq, Show) DEF TIENE Q SER ASI

data Dibujo a = Figura a 
            | Rotar (Dibujo a)
            | Espejar (Dibujo a)
            | Rot45 (Dibujo a)
            | Apilar Float Float (Dibujo a) (Dibujo a)
            | Juntar Float Float (Dibujo a) (Dibujo a)
            | Encimar (Dibujo a) (Dibujo a)
           deriving(Show, Eq) 

-- combinadores
infixr 6 ^^^

infixr 7 .-.

infixr 8 ///

comp :: Int -> (a -> a) -> a -> a
comp 0 f d = d
comp n f d = comp (n-1) f (f(d))


-- Funciones constructoras
figura :: a -> Dibujo a
figura a = Figura a

encimar :: Dibujo a -> Dibujo a -> Dibujo a
encimar d1 d2 = Encimar d1 d2

apilar :: Float -> Float -> Dibujo a -> Dibujo a -> Dibujo a
apilar n m d1 d2 = Apilar n m d1 d2

juntar  :: Float -> Float -> Dibujo a -> Dibujo a -> Dibujo a
juntar n m d1 d2 = Juntar n m d1 d2

rot45 :: Dibujo a -> Dibujo a
rot45 d = Rot45 d

rotar :: Dibujo a -> Dibujo a
rotar d = Rotar d

espejar :: Dibujo a -> Dibujo a
espejar d = Espejar d

-- Superpone un dibujo con otro.
(^^^) :: Dibujo a -> Dibujo a -> Dibujo a
(^^^) d1 d2 = encimar d1 d2

-- Pone el primer dibujo arriba del segundo, ambos ocupan el mismo espacio.
(.-.) :: Dibujo a -> Dibujo a -> Dibujo a
(.-.) d1 d2 = apilar 1 1 d1 d2

-- Pone un dibujo al lado del otro, ambos ocupan el mismo espacio.
(///) :: Dibujo a -> Dibujo a -> Dibujo a
(///) d1 d2 = juntar 1 1 d1 d2

-- rotaciones
r90 :: Dibujo a -> Dibujo a
r90 d = rotar d --entiendo q rotar es rot90

r180 :: Dibujo a -> Dibujo a
r180 d = comp 2 r90 d

r270 :: Dibujo a -> Dibujo a
r270 d = comp 3 r90 d

-- una figura repetida con las cuatro rotaciones, superimpuestas.
encimar4 :: Dibujo a -> Dibujo a
-- encimar4 d = (^^^) d (((^^^)(r90 d) ((^^^)(r180 d) (r270 d))))
encimar4 d = (^^^)((^^^) d (r90 d)) ((^^^)(r180 d) (r270 d))

-- cuatro figuras en un cuadrante.
cuarteto :: Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a
cuarteto d1 d2 d3 d4 = (.-.) ((///) d1 d2) ((///) d3 d4)

-- un cuarteto donde se repite la imagen, rotada (¡No confundir con encimar4!)
ciclar :: Dibujo a -> Dibujo a
ciclar d = (.-.) ((///) d (r90 d)) ((///) (r180 d) (r270 d)) 
-- ciclar d = cuarteto d (r90 d) (r180 d) (r270 d)

-- ACÁ TENEMOS QUE USAR LOS CONSTRUCTORES O FUNCIONES CONSTRUCTORAS??

-- map para nuestro lenguaje

mapDib :: (a -> b) -> Dibujo a -> Dibujo b
mapDib f (Figura d) = Figura (f d)
mapDib f (Rotar d) = Rotar (mapDib f d)
mapDib f (Espejar d) = Espejar (mapDib f d)
mapDib f (Rot45 d) = Rot45 (mapDib f d)
mapDib f (Apilar m n d1 d2) = Apilar m n (mapDib f d1) (mapDib f d2)
mapDib f (Juntar m n d1 d2) = Juntar m n (mapDib f d1) (mapDib f d2)
mapDib f (Encimar d1 d2) = Encimar (mapDib f d1)(mapDib f d2)
-- verificar que las operaciones satisfagan
-- 1. map figura = id
-- 2. map (g . f) = mapDib g . mapDib f


-- Cambiar todas las básicas de acuerdo a la función.
change :: (a -> Dibujo b) -> Dibujo a -> Dibujo b
change f (Figura d) = (f d)
change f (Rotar d) = Rotar (change f d)
change f (Espejar d) = Espejar (change f d)
change f (Rot45 d) = Rot45 (change f d)
change f (Apilar m n d1 d2) = Apilar m n (change f d1) (change f d2)
change f (Juntar m n d1 d2) = Juntar m n (change f d1) (change f d2)
change f (Encimar d1 d2) = Encimar (change f d1)(change f d2)

-- Principio de recursión para Dibujos.
-- Estructura general para la semántica (a no asustarse). Ayuda: 
-- pensar en foldr y las definiciones de intro a la lógica
-- foldDib aplicado a cada constructor de Dibujo debería devolver el mismo
-- dibujo

foldDib ::
  (a -> b) -> (b -> b) -> (b -> b) -> (b -> b) ->
  (Float -> Float -> b -> b -> b) ->
  (Float -> Float -> b -> b -> b) ->
  (b -> b -> b) ->
  Dibujo a -> b
foldDib fb ro90 es r45 ap j en (Figura d) = fb d -- ????
foldDib fb ro90 es r45 ap j en (Rotar d) = ro90 (foldDib fb ro90 es r45 ap j en d)
foldDib fb ro90 es r45 ap j en (Espejar d) = es (foldDib fb ro90 es r45 ap j en d)
foldDib fb ro90 es r45 ap j en (Rot45 d) = r45 (foldDib fb ro90 es r45 ap j en d)
foldDib fb ro90 es r45 ap j en (Apilar m n d1 d2) = ap m n (foldDib fb ro90 es r45 ap j en d1)(foldDib fb ro90 es r45 ap j en d2)
foldDib fb ro90 es r45 ap j en (Juntar m n d1 d2) = j m n (foldDib fb ro90 es r45 ap j en d1)(foldDib fb ro90 es r45 ap j en d2)
foldDib fb ro90 es r45 ap j en (Encimar d1 d2) = en (foldDib fb ro90 es r45 ap j en d1)(foldDib fb ro90 es r45 ap j en d2)

-- bas figura básica
-- ro90 
-- es espejar
-- r45
-- ap apilar
-- j juntar
-- en encimar

-- Extrae todas las figuras básicas de un dibujo.
--figuras :: Dibujo a -> [a]