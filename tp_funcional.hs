-- TP funcional 2018 
type cantPosicion = Int
type A = Int
type B = Int
type etiqueta = String

data Microprocesador = Microprocesador CantPosicion A B etiqueta deriving (Show)

data xT8088= Microprocesador = Microprocesador (0 0 0 _)




add (Microprocesador CantPosicion A B etiqueta) = Microprocesador CantPosicion (A+B) 0 etiqueta

div (Microprocesador CantPosicion A B etiqueta) = Microprocesador CantPosicion (A div B) 0 etiqueta

swap (Microprocesador CantPosicion A B etiqueta) = Microprocesador CantPosicion B A etiqueta

lodAddr (Microprocesador CantPosicion A B etiqueta) addr = Microprocesador CantPosicion addr B etiqueta

strAddrVal val addr= addr

lodvVal (Microprocesador CantPosicion A B etiqueta) val = Microprocesador CantPosicion val B etiqueta
 
