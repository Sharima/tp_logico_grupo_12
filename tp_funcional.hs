-- TP funcional 2018 
import Text.Show.Functions
import Data.List
-- Otro Data--
{-data Microprocesador = Microprocesador {
  memoria :: [Int],
  acumulador :: (Int, Int)
  programCounter :: Int
  mensajeError:: String
  }
  xt8088 = Microprocesador [} (0,0) 0 ""
  
nop (Microprocesador memoria acumuladorA acumuladorB programCounter mensajeDeError)= (Microprocesador memoria acumuladorA acumuladorB (programCounter +1) mensajeDeError)
lodv val (Microprocesador memoria acumuladorA acumuladorB programCounter mensajeDeError)= (Microprocesador memoria val acumuladorB programCounter mensajeDeError)
swap (Microprocesador memoria acumuladorA acumuladorB programCounter mensajeDeError)= (Microprocesador memoria acumuladorB acumuladorA programCounter mensajeDeError)
add (Microprocesador memoria acumuladorA acumuladorB programCounter mensajeDeError)= (Microprocesador memoria (acumuladorA + acumuladorB) 0 programCounter mensajeDeError)

(nop.nop.nop) xt8088

cargarValorEnContador val unMicroprocesador = (nop.swap.nop.lodv val) unMicroprocesador
cargarValorYSumar val unMicroprocesador = (nop.add.nop.lodv val) unMicroprocesador

sumarAcumuladores valor1 valor2 unMicroprocesador = (cargarValorYSumar valor2.cargarValorEnContador valor1) unMicroprocesador

sumarAcumuladores2 valor1 valor2 = cargarValorYSumar valor2.cargarValorEnContador valor1

  -}


data Microprocesador = Microprocesador {cantPosicion:: Int, contA:: Int, contB:: Int, progCounter:: Int, etiqueta::String} deriving (Show)
xT8088 = Microprocesador {cantPosicion=0, contA=0, contB=0}


nop (Microprocesador cantPosicion contA contB progCounter etiqueta) = Microprocesador cantPosicion contA contB (progCounter + 1) etiqueta

add (Microprocesador cantPosicion contA contB progCounter etiqueta) = Microprocesador cantPosicion (contA + contB) 0 progCounter etiqueta

div (Microprocesador cantPosicion contA contB progCounter etiqueta) = Microprocesador cantPosicion (div contA contB) 0 progCounter etiqueta

swap (Microprocesador cantPosicion contA contB progCounter etiqueta) = Microprocesador cantPosicion contB contA progCounter etiqueta

lodAddr (Microprocesador cantPosicion contA contB progCounter etiqueta) addr = Microprocesador cantPosicion addr contB progCounter etiqueta

strAddrVal val addr= addr

lodvVal (Microprocesador cantPosicion contA contB progCounter etiqueta) val = Microprocesador cantPosicion val contB progCounter etiqueta
