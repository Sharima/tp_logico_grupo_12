-- TP funcional 2018 
import Text.Show.Functions
import Data.List

--3.1.1 Punto 1: Modelar micro
data Microprocesador = Microprocesador {memoria::[Int], contA:: Int, contB:: Int, progCounter:: Int, mensajeError::String} deriving (Show)

--3.1.2 Punto 1: Modelar micro
xT8088 = Microprocesador {memoria=[], contA=0, contB=0, progCounter=0, mensajeError=""}
-- xt8088 = [] 0 0 0 ""
--3.2.1 Punto 2
nop (Microprocesador memoria contA contB progCounter mensajeError) = Microprocesador memoria contA contB (progCounter + 1) mensajeError
{-3.2.1 Punto 2
(nop.nop.nop) xT8088
En este punto interviene el concepto de composicion.-}

--3.3.1 Punto 3
lodv (Microprocesador memoria contA contB progCounter mensajeError) val = Microprocesador memoria val contB progCounter mensajeError
swap (Microprocesador memoria contA contB progCounter mensajeError) = Microprocesador memoria contB contA progCounter mensajeError
add (Microprocesador memoria contA contB progCounter mensajeError) = Microprocesador memoria (contA + contB) 0 progCounter mensajeError

{-3.3.2 Punto 3
nop (add (nop (lodv (nop (swap (nop (lodv xT8088 10)))) 22)))-}

cargarValorEnContador val unMicroprocesador = (nop.swap.nop.lodv val) unMicroprocesador
cargarValorYSumar val unMicroprocesador = (nop.add.nop.lodv val) unMicroprocesador
sumarAcumuladores valor1 valor2 unMicroprocesador = (cargarValorYSumar valor2.cargarValorEnContador valor1) unMicroprocesador
-- También se lo puede realizando aplicando "Aplicación Parcial" eliminando el parámetro "unMicroProcesador"
cargarValorEnContador2 val = (nop.swap.nop.lodv val)
cargarValorYSumar2 val = (nop.add.nop.lodv val)
sumarDiezYVeintiDos valor1 valor2 = cargarValorYSumar2 valor2.cargarValorEnContador2 valor1

--3.4.1 Punto 4
diV (Microprocesador memoria contA contB progCounter mensajeError) | contB /= 0 = Microprocesador memoria (div contA contB) 0 progCounter mensajeError
																	| otherwise = Microprocesador memoria 0 0 progCounter "DIVISION BY ZERO"
																	
str (Microprocesador memoria contA contB progCounter mensajeError) addr val= (Microprocesador (agregarPosicion addr val memoria) contA contB progCounter mensajeError)

agregarPosicion addr val memoria = (take (addr-1) memoria) ++ [val] ++ drop (addr-1) memoria

lod (Microprocesador memoria contA contB progCounter mensajeError) addr = Microprocesador memoria ((!!(addr -1)) memoria) contB progCounter mensajeError

{-3.4.2 Punto 4 -}



{- Casos de Prueba
4. 1 (nop.nop.nop) xT8088


4.3.2
lod (Microprocesador (replicate 1024 0) 5 0 0 "") 2

4.3.4
xt80882 = Microprocesador (replicate 1024 0) 12 4 0 ""
diV xt80882


-}
