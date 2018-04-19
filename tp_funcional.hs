-- TP funcional 2018 
import Text.Show.Functions
import Data.List

data Microprocesador = Microprocesador {cantPosicion:: Int, contA:: Int, contB:: Int, progCounter:: Int, etiqueta::String} deriving (Show)
xT8088 = Microprocesador {cantPosicion=0, contA=0, contB=0}


nop (Microprocesador cantPosicion contA contB progCounter etiqueta) = Microprocesador cantPosicion contA contB (progCounter + 1) etiqueta

add (Microprocesador cantPosicion contA contB progCounter etiqueta) = Microprocesador cantPosicion (contA + contB) 0 progCounter etiqueta

div (Microprocesador cantPosicion contA contB progCounter etiqueta) = Microprocesador cantPosicion (div contA contB) 0 progCounter etiqueta

swap (Microprocesador cantPosicion contA contB progCounter etiqueta) = Microprocesador cantPosicion contB contA progCounter etiqueta

lodAddr (Microprocesador cantPosicion contA contB progCounter etiqueta) addr = Microprocesador cantPosicion addr contB progCounter etiqueta

strAddrVal val addr= addr

lodvVal (Microprocesador cantPosicion contA contB progCounter etiqueta) val = Microprocesador cantPosicion val contB progCounter etiqueta
