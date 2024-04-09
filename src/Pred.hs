module Pred (
  Pred,
  cambiar, anyDib, allDib, orP, andP, falla
) where
import Dibujo (Dibujo, foldDib, mapDib)

type Pred a = a -> Bool


-- Dado un predicado sobre básicas, cambiar todas las que satisfacen
-- el predicado por la figura básica indicada por el segundo argumento.
cambiar :: Pred a -> a -> Dibujo a -> Dibujo a
cambiar p a b = mapDib (fun_cambia p a) b

fun_cambia :: Pred a -> a -> a -> a
fun_cambia p a b | p b = a
                | otherwise = b


-- Alguna básica satisface el predicado.
anyDib :: Pred a -> Dibujo a -> Bool
anyDib f x = foldDib f id id id (\i j x y -> x || y) 
            (\i j x y -> x || y) (\x y -> x || y) x

-- Todas las básicas satisfacen el predicado.
allDib :: Pred a -> Dibujo a -> Bool
allDib f x = foldDib f id id id (\i j x y -> x && y) 
            (\i j x y -> x && y) (\x y -> x && y) x

-- Los dos predicados se cumplen para el elemento recibido.
andP :: Pred a -> Pred a -> Pred a
andP p1 p2 x = p1 x && p2 x


-- Algún predicado se cumple para el elemento recibido.
orP :: Pred a -> Pred a -> Pred a
orP p1 p2 x = p1 x || p2 x

falla = True