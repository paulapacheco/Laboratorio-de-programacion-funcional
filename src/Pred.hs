{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use camelCase" #-}
module Pred (
  Pred,
  cambiar, anyDib, allDib, orP, andP
) where
import Dibujo (Dibujo, foldDib, mapDib)

type Pred a = a -> Bool

-- Dado un predicado sobre básicas, cambiar todas las que satisfacen
-- el predicado por la figura básica indicada por el segundo argumento.
cambiar :: Pred a -> a -> Dibujo a -> Dibujo a
cambiar p a b = mapDib (cambia_aux p a) b

cambia_aux :: Pred a -> a -> a -> a
cambia_aux p a b | p b = a
                | otherwise = b


-- Alguna básica satisface el predicado.
anyDib :: Pred a -> Dibujo a -> Bool
anyDib p d = foldDib p bool1 bool1 bool1 bool_any3 bool_any3 bool_any2
              bool4 bool4 d

bool1 :: Bool -> Bool  
bool1 b = b

bool_any2 :: Bool -> Bool -> Bool
bool_any2 b1 b2 = b1 || b2

bool_any3 :: Float -> Float -> Bool -> Bool -> Bool
bool_any3 _ _ b1 b2 = bool_any2 b1 b2

bool4 :: Float -> Bool -> Bool
bool4 _ b = b

-- Todas las básicas satisfacen el predicado.

allDib :: Pred a -> Dibujo a -> Bool
allDib p d = foldDib p bool1 bool1 bool1 bool_all3 bool_all3 bool_all2
              bool4 bool4 d

bool_all2 :: Bool -> Bool -> Bool
bool_all2 b1 b2 = b1 && b2

bool_all3 :: Float -> Float -> Bool -> Bool -> Bool
bool_all3 _ _ b1 b2 = bool_all2 b1 b2

-- Los dos predicados se cumplen para el elemento recibido.
andP :: Pred a -> Pred a -> Pred a
andP p1 p2 d = p1 d && p2 d


-- Algún predicado se cumple para el elemento recibido.
orP :: Pred a -> Pred a -> Pred a
orP p1 p2 d = p1 d || p2 d
