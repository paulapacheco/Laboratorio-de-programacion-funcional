module Dibujos.Grilla where
import Dibujo (Dibujo, figura, juntar, apilar)
import FloatingPic(Conf(..), Output, half)
import qualified Graphics.Gloss.Data.Point.Arithmetic as V
import Graphics.Gloss(text, translate, scale)

tupleToString :: Int -> Int -> String
tupleToString num1 num2 = "(" ++ show num1 ++ ", " ++ show num2 ++ ")"

data Basica = Tupla Int Int

interpBasica :: Output Basica
interpBasica (Tupla coord1 coord2) x y w = translate
                    (fst $ x V.+ half (half y)) (snd $ x V.+ half (half w))
                    (scale 0.15 0.15 $ text $ tupleToString coord1 coord2)

row :: [Dibujo a] -> Dibujo a
row [] = error "row: no puede ser vacío"
row [d] = d
row (d:ds) = juntar 1 (fromIntegral $ length ds) d (row ds)

column :: [Dibujo a] -> Dibujo a
column [] = error "column: no puede ser vacío"
column [d] = d
column (d:ds) = apilar 1 (fromIntegral $ length ds) d (column ds)

grilla :: [[Dibujo a]] -> Dibujo a
grilla = column . map row

tupla :: Int -> Int -> Dibujo Basica
tupla n1 n2 = figura $ Tupla n1 n2

testGrilla :: Dibujo Basica
testGrilla = grilla [[tupla i j | j <- [0..7]] | i <- [0..7]]

grillaConf :: Conf
grillaConf = Conf {
    name = "Grilla"
    , pic = testGrilla
    , bas = interpBasica
}