import Test.HUnit
import Dibujo
import Graphics.Gloss.Data.Picture

-- Prueba para la función 'figura'
testFigura :: Test
testFigura = "Creación de figura básica" ~:
    figura 5 ~?= Figura 5

-- Prueba para la función 'encimar'
testEncimar :: Test
testEncimar = "Superposición de dos dibujos" ~:
    encimar (figura 1) (figura 2) ~?= Encimar (Figura 1) (Figura 2)

-- Prueba para la función 'apilar'
testApilar :: Test
testApilar = "Apilado de dos dibujos" ~:
    apilar 1 1 (figura 1) (figura 2) ~?= Apilar 1 1 (Figura 1) (Figura 2)

-- Prueba para la función 'juntar'
testJuntar :: Test
testJuntar = "Juntar dos dibujos" ~:
    juntar 1 1 (figura 1) (figura 2) ~?= Juntar 1 1 (Figura 1) (Figura 2)

-- Prueba para la función 'rot45'
testRot45 :: Test
testRot45 = "Rotación de un dibujo 45 grados" ~:
    rot45 (figura 1) ~?= Rot45 (Figura 1)

-- Prueba para la función 'rotar'
testRotar :: Test
testRotar = "Rotación de un dibujo" ~:
    rotar (figura 1) ~?= Rotar (Figura 1)

-- Prueba para la función 'espejar'
testEspejar :: Test
testEspejar = "Espejado de un dibujo" ~:
    espejar (figura 1) ~?= Espejar (Figura 1)

testRotAlpha :: Test
testRotAlpha = "Rotar un dibujo con ángulo alpha" ~:
    rotAlpha 30 (figura 1) ~?= RotarAlpha 30 (Figura 1)

testModDim :: Test
testModDim = "Modificar dimensión de un dibujo" ~:
    modDim 2.0 (figura 1) ~?= ModDim 2.0 (Figura 1)



-- Prueba para verificar que el tipo Dibujo se pueda mostrar correctamente
testShowDibujo :: Test
testShowDibujo = "Mostrar Dibujo" ~: do
    let dibujo1 = figura (Circle 50)
        dibujo2 = rotar (figura (Line [(0,0),(100,100)]))
        dibujo3 = apilar 50 50 (figura (rectangleSolid 100 50)) (figura (Circle 50))
    assertEqual "Mostrar dibujo 1" (show dibujo1) "Figura (Circle 50.0)"
    assertEqual "Mostrar dibujo 2" (show dibujo2) "Rotar (Figura (Line [(0.0,0.0),(100.0,100.0)]))"
    assertEqual "Mostrar dibujo 3" (show dibujo3) "Apilar 50.0 50.0 (Figura (Polygon [(-50.0,-25.0),(-50.0,25.0),(50.0,25.0),(50.0,-25.0)])) (Figura (Circle 50.0))"

-- Ejecutar todas las pruebas
main :: IO Counts
main = runTestTT $ TestList [testFigura, testEncimar, testApilar, testJuntar, testRot45, testRotar, testEspejar, testShowDibujo, testModDim, testRotAlpha]