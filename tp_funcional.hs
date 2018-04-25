-- TP funcional 2018 
import Text.Show.Functions
import Data.List

--3.1.1 Punto 1: Modelar micro
data Microprocesador = Microprocesador {memoria::[Int], acumuladorA:: Int, acumuladorB:: Int, programCounter:: Int, mensajeDeError::String} deriving (Show)

--3.1.2 Punto 1: Modelar micro
xt8088 = Microprocesador [] 0 0 0 ""
at8086 = Microprocesador [1..20] 0 0 0 ""
fp20 = Microprocesador [] 7 24 0 ""
-- xt8088 = [] 0 0 0 ""
--3.2.1 Punto 2
nop (Microprocesador memoria acumuladorA acumuladorB programCounter mensajeDeError) = Microprocesador memoria acumuladorA acumuladorB (programCounter + 1) mensajeDeError
{-3.2.1 Punto 2
*Main> (nop.nop.nop) xT8088
Microprocesador {memoria = [], acumuladorA = 0, acumuladorB = 0, programCounter = 3, mensajeDeError = ""}

En este punto interviene el concepto de composicion.-}

--3.3.1 Punto 3
lodv val (Microprocesador memoria acumuladorA acumuladorB programCounter mensajeDeError) = Microprocesador memoria val acumuladorB programCounter mensajeDeError
swap (Microprocesador memoria acumuladorA acumuladorB programCounter mensajeDeError) = Microprocesador memoria acumuladorB acumuladorA programCounter mensajeDeError
add (Microprocesador memoria acumuladorA acumuladorB programCounter mensajeDeError) = Microprocesador memoria (acumuladorA + acumuladorB) 0 programCounter mensajeDeError

{-3.3.2 Punto 3
nop (add (nop (lodv (nop (swap (nop (lodv xT8088 10)))) 22)))-}

cargarValorEnContador val unMicroprocesador = (nop.swap.nop.lodv val) unMicroprocesador
cargarValorYSumar val unMicroprocesador = (nop.add.nop.lodv val) unMicroprocesador
sumarAcumuladores valor1 valor2 unMicroprocesador = (cargarValorYSumar valor2.cargarValorEnContador valor1) unMicroprocesador
-- También se lo puede realizando aplicando "Aplicación Parcial" eliminando el parámetro "unMicroProcesador"
cargarValorEnContador2 val = (nop.swap.nop.lodv val)
cargarValorYSumar2 val = (nop.add.nop.lodv val)
sumarDiezYVeintiDos valor1 valor2 = cargarValorYSumar2 valor2.cargarValorEnContador2 valor1
{- Llamadas individuales a las funciones
*Main> nop (lodv 10 xT8088)
Microprocesador {memoria = [], acumuladorA = 10, acumuladorB = 0, programCounter = 1, mensajeDeError = ""}
*Main> nop (swap (nop (lodv 10 xT8088)))
Microprocesador {memoria = [], acumuladorA = 0, acumuladorB = 10, programCounter = 2, mensajeDeError = ""}
*Main> nop (lodv 22(nop (swap (nop (lodv 10 xT8088)))))
Microprocesador {memoria = [], acumuladorA = 22, acumuladorB = 10, programCounter = 3, mensajeDeError = ""}
*Main> nop (add (nop (lodv 22(nop (swap (nop (lodv 10 xT8088)))))))
Microprocesador {memoria = [], acumuladorA = 32, acumuladorB = 0, programCounter = 4, mensajeDeError = ""} -}

--3.4.1 Punto 4
divide (Microprocesador memoria acumuladorA acumuladorB programCounter mensajeDeError) | acumuladorB /= 0 = Microprocesador memoria (div acumuladorA acumuladorB) 0 programCounter mensajeDeError
 | otherwise = Microprocesador memoria 0 0 programCounter "DIVISION BY ZERO"

str (Microprocesador memoria acumuladorA acumuladorB programCounter mensajeDeError) addr val= (Microprocesador (agregarPosicion addr val memoria) acumuladorA acumuladorB programCounter mensajeDeError)

agregarPosicion addr val memoria = (take (addr-1) memoria) ++ [val] ++ drop (addr-1) memoria

lod (Microprocesador memoria acumuladorA acumuladorB programCounter mensajeDeError) addr = Microprocesador memoria ((!!(addr -1)) memoria) acumuladorB programCounter mensajeDeError

{-3.4.2 Punto 4 
*Main> nop(divide(nop(lod(nop (swap (nop (lod (nop (str (nop(str xT8088 1 2))2 0)) 2))))1)))
Microprocesador {memoria = [2,0], contA = 0, contB = 0, progCounter = 6, mensajeError = "DIVISION BY ZERO"}
-}

{- Casos de Prueba
4.1 
	*Main> (nop.nop.nop) xT8088
	Microprocesador {memoria = [], contA = 0, contB = 0, progCounter = 3, mensajeError = ""}
4.2.1 
	*Main> xT8088
	Microprocesador {memoria = [], contA = 0, contB = 0, progCounter = 0, mensajeError = ""}
	*Main> lodv xT8088 5
	Microprocesador {memoria = [], contA = 5, contB = 0, progCounter = 0, mensajeError = ""}
4.2.2 
	*Main> swap fp20
	Microprocesador {memoria = [], contA = 24, contB = 7, progCounter = 0, mensajeError = ""}
	*Main> add fp20
	Microprocesador {memoria = [], contA = 31, contB = 0, progCounter = 0, mensajeError = ""}
	*Main> 
	

-}

{-Punto 5: desde 5.1.1 a 5.1.4
Tal como definimos el Data Microprocesador, cada parámetro que toma el Microprocesador se lo puede utilizar como función para poder
consultar el valor de cualquier parámetro que conforma el Microprocesador, por ejemplo:

*Main> memoria at8086
[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]

*Main> acumuladorA at8086
0
*Main> acumuladorB at8086
0
*Main> programCounter at8086
0
*Main> mensajeDeError at8086
""
*Main> show at8086
"Microprocesador {memoria = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20], acumuladorA = 0, acumuladorB = 0, programCounter = 0, mensajeDeError = \"\"}"
*Main> 
-}


