import Text.Show.Functions

-- Piratas
jackSparrow =  ("Jack Sparrow",[("Brujula",10000),("Frasco de Arena",0)]) 
davidJones = ("David Jones",[("Cajita Musical",1)])
anneBonny = ("Anne Bonny",[("Doblones",100),("Frasco de Arena",1)])
elizabethSwann = ("Elizabeth Swann" ,[("Moneda Del Cofre Muerto", 100),("Espada de Hierro", 50)])
willTurner = ("Will Turner" , [("Cuchillo",5)])

-- Islas
islaTortuga = ("Isla Tortuga",[("Frasco de Arena",1)])
islaDelRon = ("Isla del Ron",[("Botella de ron",25)])

-- Ciudades
portRoyal = [("Botella",200),("Oro",120),("Perla",5000),("Brujula",5000),("Cofre",60000),("Espada", 500)]
carmenDePatagones = [("Cuchillo",5)]

-- Otras Funciones
nombresClave = ["Oro","Sombrero","Moneda","Cofre","Brujula","Cuchillo"]
nombreIsla (nombreDeLaIsla,tesoroAbundante) = nombreDeLaIsla
tesoroAbundante (nombreDeLaIsla,tesoroAbundante) = tesoroAbundante
botin (nombreDelPirata,botin) = botin
nombrePirata (nombreDelPirata,botin) = nombreDelPirata
nombreTesoro (nombreTesoro,valorTesoro) = nombreTesoro
valorTesoro (nombreTesoro,valorTesoro) = valorTesoro
tesoro = (nombreTesoro,valorTesoro)
condicionTesoroNoValioso (nombreTesoro,valorTesoro) = valorTesoro<=100

-- Funciones Primera Parte
--tieneTesoroConMismoNombreYDistintoValor pirata tesoro = any (tienenMismoNombreDistintoValor tesoro)  (botin pirata)
--piratasConMismoTesoroPeroDistintoValor pirata1 pirata2 = any (tieneTesoroConMismoNombreYDistintoValor  pirata2)  (botin pirata1)
   
nuevoTesoro (nombreDelPirata,botin) tesoro = (nombreDelPirata,botin ++ [tesoro])
pierdeTesorosValiosos (nombreDelPirata,botin) = filter condicionTesoroNoValioso botin
pierdeTesorosConUnNombre (nombreDelPirata,botin) x = filter (/=x) (map nombreTesoro botin)
cantidadDeTesoros (nombreDelPirata,botin) = length botin
valorDelBotin (nombreDelPirata,botin) = sum (map snd botin)
afortunado (nombreDelPirata,botin) = (valorDelBotin (nombreDelPirata,botin))>=10000
tesoroMasValioso (nombreDelPirata,botin) = maximum (map snd botin)

-- Funciones Segunda Parte
saquearTesorosValiosos tesoro = (condicionTesoroNoValioso tesoro == False)
saquearPorNombre tesoro = ([fst tesoro] == (filter (==fst tesoro) nombresClave))
saqueoCorazon tesoro = False
saqueoComplejo tesoro = ((saquearTesorosValiosos tesoro == True && saquearPorNombre tesoro == True) || (saquearTesorosValiosos tesoro == False && saquearPorNombre tesoro == True) || (saquearTesorosValiosos tesoro == False && saquearPorNombre tesoro == True))
saquear formaSaqueo (nombreDelPirata,botin) tesoro
 |formaSaqueo tesoro = nuevoTesoro (nombreDelPirata,botin) tesoro
 |otherwise = (nombreDelPirata,botin)

--Funciones Tercera Parte
perlaNegra = (saquearTesorosValiosos,[jackSparrow,anneBonny])
holandesErrante = (saquearPorNombre,[davidJones])

incorporarALaTripulacion barco (nombreDelPirata,botin) = (snd barco) ++ [(nombreDelPirata,botin)]
abandonarALaTripulacion barco (nombreDelPirata,botin) = filter (/=((nombreDelPirata,botin))) (snd barco)
anclarIsla (formaSaqueo,tripulacion) (nombreDeLaIsla,tesoroAbundante)  =  zip (map fst tripulacion) (map (++tesoroAbundante) (map snd tripulacion))
anclarCiudad (formaSaqueo,tripulacion) ciudad = (zipWith (saquear formaSaqueo) tripulacion ciudad) 
abordarBarco (formaSaqueo1,tripulacion1) (formaSaqueo2,tripulacion2)
 |(length tripulacion1) > (length tripulacion2) = zipWith (saquear formaSaqueo1) tripulacion1 (concat (map snd tripulacion2))
 | otherwise = [] -- "muertos en batalla por los saqueos" , (caso en que las dos tripulaciones son iguales--