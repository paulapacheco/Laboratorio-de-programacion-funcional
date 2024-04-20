module Dibujos.Caracoles where

import Dibujo (Dibujo, figura, juntar, apilar, rot45, rotar, encimar, espejar, modDim, rotAlpha) 
import FloatingPic(Conf(..), Output, half, zero)
import qualified Graphics.Gloss.Data.Point.Arithmetic as V
import Graphics.Gloss ( Picture, Picture(Text), blank, translate, scale, blue, red, color, line, pictures )


data Color = Azul | Rojo
    deriving (Show, Eq)

data BasicaSinColor = Rectangulo | Blanca 
    deriving (Show, Eq)

type Basica = (BasicaSinColor, Color)

colorear :: Color -> Picture -> Picture
colorear Azul = color blue
colorear Rojo = color red

-- Las coordenadas que usamos son:
--
--  x + y
--  |
--  x --- x + w

interpBasicaSinColor :: Output BasicaSinColor
interpBasicaSinColor Rectangulo x y w = line [x, x V.+ y, x V.+ y V.+ w, x V.+ w, x]
interpBasicaSinColor Blanca _ _ _ = blank

interpBas :: Output Basica
interpBas (b, c) x y w = colorear c $ interpBasicaSinColor b x y w


-- Funciones para colorear

figRoja :: BasicaSinColor -> Dibujo Basica
figRoja b = figura (b, Rojo)

figAzul :: BasicaSinColor -> Dibujo Basica
figAzul b = figura (b, Azul)



-- Funcion recursiva para rotaciones

funRec :: Float -> Dibujo Basica -> Dibujo Basica
funRec n d 
     | n < 2 = d
     | otherwise = encimar d (funRec (n-2) (rotAlpha n d))



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


rectR :: Dibujo Basica
rectR = figura (Rectangulo, Rojo)

rectA :: Dibujo Basica
rectA = figura (Rectangulo, Azul)

figVacia :: Dibujo Basica
figVacia = figura (Blanca, Azul)


test :: Dibujo Basica
test = grilla[
    [modDim 0.3 (encimar (rotAlpha 45 (funRec 40 rectR)) (rotAlpha 38 (funRec 40 rectA))), figVacia],
    [figVacia, encimar (rotAlpha 45 (funRec 40 rectR)) (rotAlpha 38 (funRec 40 rectA))]
    ]



caracolesConf :: Conf
caracolesConf = Conf { 
    name = "Caracoles"
    , pic = test
    , bas = interpBas
}


