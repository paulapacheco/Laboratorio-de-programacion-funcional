{-# LANGUAGE TemplateHaskell #-}

import Test.HUnit
import Pred
import Dibujo
import Graphics.Gloss.Data.Picture

-- Prueba para la función 'cambiar'
testCambiar :: Test
testCambiar = TestList
    [ "Cambiar básica que satisface el predicado" ~:
        cambiar even 0 (Figura 0) ~?= Figura 0
    , "No cambiar básica que no satisface el predicado" ~:
        cambiar even 0 (Figura 3) ~?= Figura 3
    ]

-- Predicado para verificar si el área de una figura es mayor que 500
areaMayorQue500 :: Pred (Dibujo Picture) -- Funcion de tipo "Dibujo Picture -> Bool"
areaMayorQue500 (Figura (Circle r)) = pi * r * r > 500
areaMayorQue500 (Apilar _ _ d1 d2) = areaMayorQue500 d1 && areaMayorQue500 d2 -- al usarse con AllDib, tenemos que usar el operador &&
areaMayorQue500 (Juntar _ _ d1 d2) = areaMayorQue500 d1 && areaMayorQue500 d2
areaMayorQue500 _ = False

-- Predicado para verificar si el área de una figura es mayor que 1000
areaMayorQue1000 :: Pred (Dibujo Picture) -- Funcion de tipo "Dibujo Picture -> Bool"
areaMayorQue1000 (Figura (Circle r)) = pi * r * r > 1000
areaMayorQue1000 (Apilar _ _ d1 d2) = areaMayorQue1000 d1 || areaMayorQue1000  d2 -- al usarse con AnyDib, tenemos que usar el operador ||
areaMayorQue1000 (Juntar _ _ d1 d2) = areaMayorQue1000 d1 || areaMayorQue1000  d2
areaMayorQue1000 _ = False

-- Prueba para verificar la función anyDib con figuras compuestas
testAnyDib :: Test
testAnyDib = "anyDib" ~: do
    let dibujo1 = figura $ apilar 10 10 (figura (Circle 30)) (figura (Circle 5))
        dibujo2 = figura $ juntar 20 20 (figura (Circle 2)) (figura (Circle 10))
    assertBool "Algunas figuras tienen área mayor que 1000" (anyDib areaMayorQue1000 dibujo1)
    assertBool "Ninguna figura tiene área mayor que 1000" (not $ anyDib areaMayorQue1000 dibujo2)

-- Prueba para verificar la función allDib con figuras compuestas
testAllDib :: Test
testAllDib = "allDib" ~: do
    let dibujo1 = figura $ apilar 5 5 (figura (Circle 30)) (figura (Circle 5))
        dibujo2 = figura $ juntar 10 10 (figura (Circle 15)) (figura (Circle 25))
    assertBool "Algunas figuras no tienen área mayor que 500" (not $ allDib areaMayorQue500 dibujo1)
    assertBool "Todas las figuras tienen área mayor que 500" (allDib areaMayorQue500 dibujo2)

-- Prueba para la función 'andP'
testAndP :: Test
testAndP = "Los dos predicados se cumplen para el elemento recibido" ~:
    andP even (> 0) 2 ~?= True

-- Prueba para la función 'orP'
testOrP :: Test
testOrP = "Algún predicado se cumple para el elemento recibido" ~:
    orP even odd 3 ~?= True

-- Ejecutar todas las pruebas
main :: IO Counts
main = runTestTT $ TestList [testCambiar, testAnyDib, testAllDib, testAndP, testOrP]