module Main (main) where

import Dibujos.Feo (feoConf)
import Dibujos.Escher (escherConf)
import Dibujos.Grilla (grillaConf)
import Dibujos.Caracoles (caracolesConf)
import FloatingPic (Conf (..))
import Interp (initial)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import Control.Monad (when, )

-- Lista de configuraciones de los dibujos
configs :: [Conf]
configs = [feoConf, grillaConf, escherConf, caracolesConf]

-- Dibuja el dibujo n
initial' :: [Conf] -> String -> IO ()
initial' [] n = do
  putStrLn $ "No hay un dibujo llamado " ++ n
initial' (c : cs) n =
  if n == name c
    then
      initial c 800
    else
      initial' cs n

main :: IO ()
main = do
  args <- getArgs
  when (length args > 2 || null args) $ do
    putStrLn "Sólo puede elegir un dibujo. Para ver los dibujos use -l ."
    exitFailure
  when (head args == "-l") $ do
    putStrLn "Los dibujos disponibles son:"
    mapM_ (putStrLn . name) configs
    putStrLn "Ingrese el nombre del dibujo que desea ver:"
    exitSuccess
  initial' configs $ head args