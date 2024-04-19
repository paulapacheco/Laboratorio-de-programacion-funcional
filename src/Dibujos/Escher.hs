module Dibujos.Escher where

import Dibujo (Dibujo, figura, juntar, apilar, rot45, rotar, encimar, cuarteto)
import FloatingPic(FloatingPic, Conf(..), Output, zero)
import qualified Graphics.Gloss.Data.Point.Arithmetic as V
import Graphics.Gloss ( Picture, blank, line)

-- Supongamos que eligen.
type Escher = Bool

simple :: Picture -> FloatingPic
simple p _ _ _ = p

trian :: FloatingPic
trian a b c = line $ map (a V.+) [zero, c, b,zero]

interpBas :: Output Escher
interpBas True = trian
interpBas False = simple blank


-- El dibujo u.

dibujoU :: Dibujo Escher -> Dibujo Escher
dibujoU p = encimar (encimar p1 (rotar p1)) (encimar (rotar (rotar p1)) (rotar (rotar (rotar p1))) )
        where p1 = rot45 p

-- El dibujo t.
dibujoT :: Dibujo Escher -> Dibujo Escher
dibujoT p = encimar p (encimar p1 p2)  
    where p1 = (rot45 p)
          p2 = rotar (rotar (rotar p1))

-- Esquina con nivel de detalle en base a la figura p.
esquina :: Int -> Dibujo Escher -> Dibujo Escher
esquina 0 _ = figura(False)
esquina n p = cuarteto (esquina (n-1) p) (lado (n-1) p) (rotar (lado (n-1) p)) (dibujoU p)

-- Lado con nivel de detalle.
lado :: Int -> Dibujo Escher -> Dibujo Escher
lado 0 _ = figura(False)
lado n p = cuarteto(lado (n-1) p)(lado (n-1) p) (rotar (dibujoT p)) (dibujoT p)


-- Por suerte no tenemos que poner el tipo!
noneto p q r s t u v w x =  apilar 1 2 (juntar 1 2 p (juntar 1 1 q r)) (apilar 1 1 (juntar 1 2 s (juntar 1 1 t u)) (juntar 1 2 v (juntar 1 1 w x)))

-- El dibujo de Escher:
escher :: Int -> Escher -> Dibujo Escher
escher n p = noneto (esquina n (figura p))
                (lado n (figura p))
                (rotar (rotar (rotar (esquina n (figura p)))))
                (rotar (lado n (figura p)))
                (dibujoU (figura p))
                (rotar (rotar (rotar (lado n (figura p)))))
                (rotar (esquina n (figura p)))
                (rotar (rotar (lado n (figura p))))
                (rotar (rotar (esquina n (figura p))))

test :: Dibujo Escher
test = escher 2 True

escherConf :: Conf
escherConf = Conf {
    name = "Escher"
    , pic = test
    , bas = interpBas
}