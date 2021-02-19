import Text.Show.Functions()

-- Modelo inicial
data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Int,
  precisionJugador :: Int
} deriving (Eq, Show)

-- Jugadores de ejemplo
bart :: Jugador
bart = UnJugador "Bart" "Homero" (Habilidad 25 60)

todd :: Jugador
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)

rafa :: Jugador
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = UnTiro {
  velocidad :: Int,
  precision :: Int,
  altura :: Int
} deriving (Eq, Show)

type Puntos = Int

-- Funciones útiles---------------------
between :: (Eq a, Enum a) => a -> a -> a -> Bool
between n m x = elem x [n .. m]

maximoSegun :: Ord b => (a -> b) -> [a] -> a
maximoSegun f = foldl1 (mayorSegun f)

mayorSegun :: Ord x => (t -> x) -> (t -> t -> t)
mayorSegun f a b
  | f a > f b = a
  | otherwise = b
----------------------------------------
mapVelocidadTiro :: (Int -> Int) -> Tiro -> Tiro
mapVelocidadTiro funcionQueModifica unTiro = unTiro {velocidad = funcionQueModifica . velocidad $ unTiro}

mapPrecisionTiro :: (Int -> Int) -> Tiro -> Tiro
mapPrecisionTiro funcionQueModifica unTiro = unTiro {precision = funcionQueModifica . precision $ unTiro}

mapAlturaTiro :: (Int -> Int) -> Tiro -> Tiro
mapAlturaTiro funcionQueModifica unTiro = unTiro {altura = funcionQueModifica . altura $ unTiro}

----------------------------------------

asignarNuevaVelocidad :: Int -> Tiro -> Tiro
asignarNuevaVelocidad nuevaVelocidad unTiro = unTiro {velocidad = nuevaVelocidad }

asignarNuevaPrecision :: Int -> Tiro -> Tiro
asignarNuevaPrecision nuevaPrecision unTiro = unTiro {precision = nuevaPrecision }

asignarNuevaAltura :: Int -> Tiro -> Tiro
asignarNuevaAltura nuevaAltura unTiro = unTiro {altura = nuevaAltura }

------------------------------------------
--palos 
type NombrePalo = String
type Palo = Habilidad -> Tiro


--ESTO SERIA CON PATTERN MATCHING
--type Palo = NombrePalo -> Habilidad -> Tiro
--palo :: Palo
--palo "putter" habilidad = UnTiro {velocidad = 10,precision = precisionJugador habilidad * 2, altura = 0}


--1
----a

putter :: Palo
putter habilidadDelJugador = UnTiro {velocidad = 10,precision = precisionJugador habilidadDelJugador * 2, altura = 0}

madera :: Palo
madera habilidadDelJugador = UnTiro {velocidad = 100,precision = div (precisionJugador habilidadDelJugador) 2, altura = 5}

hierro :: Int -> Palo
hierro nroPalo habilidadDelJugador = UnTiro{velocidad = fuerzaJugador habilidadDelJugador * nroPalo,precision =  div (precisionJugador habilidadDelJugador) nroPalo,altura = max (nroPalo-3) 0 } 

habilidadTest :: Habilidad
habilidadTest = Habilidad{fuerzaJugador = 12, precisionJugador = 4}

----b

palos :: [Palo]
palos = [putter, madera ] ++ map hierro [1..10]

--2

--golpe :: Jugador -> Palo -> Tiro
--golpe unJugador unPalo = unPalo $ habilidad unJugador

golpe :: Palo -> Jugador ->  Tiro
golpe unPalo = unPalo . habilidad

--3
----a
--type Obstaculo = Tiro -> Tiro

atributoMayorAX :: Int -> Int -> Bool
atributoMayorAX numeroCondicion atributo = numeroCondicion < atributo

vaAlRasDelSuelo :: Tiro -> Bool
vaAlRasDelSuelo = (==0) . altura

detenerse :: Tiro -> Tiro
detenerse =  asignarNuevaVelocidad 0 . asignarNuevaAltura 0 . asignarNuevaPrecision 0
 
dobleValor :: Int -> Int -> Int
dobleValor valorDelJugador _ = valorDelJugador * 2

dividirPorLargo :: Int -> Int -> Int
dividirPorLargo largoLaguna alturaDelTiro = div alturaDelTiro largoLaguna

--
--obstaculoSuperableSi :: (Tiro -> Bool) -> (Tiro -> Tiro) -> Obstaculo
--obstaculoSuperableSi condicionParaSuperar efectoPostObstaculo unTiro
--  | condicionParaSuperar unTiro = efectoPostObstaculo unTiro
--  | otherwise = detenerse unTiro

intentarSuperarObstaculo :: Obstaculo -> Tiro -> Tiro
intentarSuperarObstaculo unObstaculo unTiro 
  | puedeSuperar unObstaculo unTiro = efectoLuegoDeSuperar unObstaculo unTiro
  | otherwise = detenerse unTiro 
--a
tunelConRampita :: Obstaculo
tunelConRampita = UnObstaculo cumpleCondicionParaSuperarTunel efectoDeSuperarTunel

cumpleCondicionParaSuperarTunel :: Tiro -> Bool
cumpleCondicionParaSuperarTunel unTiro = atributoMayorAX 90 (precision unTiro) && vaAlRasDelSuelo unTiro 

efectoDeSuperarTunel :: Tiro -> Tiro
efectoDeSuperarTunel unTiro = mapVelocidadTiro (dobleValor (velocidad unTiro)) $ (asignarNuevaAltura 0 . asignarNuevaPrecision 100) unTiro

--b
laguna :: Int -> Obstaculo
laguna largoDeLaLaguna = UnObstaculo cumpleCondicionParaSuperarLaguna $ efectoDeSuperarLaguna largoDeLaLaguna

cumpleCondicionParaSuperarLaguna :: Tiro -> Bool
cumpleCondicionParaSuperarLaguna unTiro = atributoMayorAX 80 (velocidad unTiro) && (between 1 5 (altura unTiro))

efectoDeSuperarLaguna :: Int -> Tiro -> Tiro
efectoDeSuperarLaguna largoDeLaLaguna unTiro = mapAlturaTiro (dividirPorLargo largoDeLaLaguna) unTiro

--c
hoyo :: Obstaculo
hoyo = UnObstaculo cumpleCondicionParaSuperarHoyo efectoDeSuperarHoyo

cumpleCondicionParaSuperarHoyo :: Tiro -> Bool
cumpleCondicionParaSuperarHoyo unTiro = between 5 20 (velocidad unTiro) && vaAlRasDelSuelo unTiro && atributoMayorAX 95 (precision unTiro)

efectoDeSuperarHoyo :: Tiro -> Tiro
efectoDeSuperarHoyo unTiro =  detenerse unTiro

--HAY QUE REFACTORIZAR EL TIPOO DE OBSTACULO PORQUE NO SRIVE PARA EL PTO 4 =>
data Obstaculo = UnObstaculo {
  puedeSuperar :: Tiro -> Bool,
  efectoLuegoDeSuperar :: Tiro -> Tiro
  }

--entonces ya no sirve delegar en la funcion: 

--obstaculoSuperableSi :: (Tiro -> Bool) -> (Tiro -> Tiro) -> Obstaculo
--obstaculoSuperableSi condicionParaSuperar efectoPostObstaculo unTiro
--  | condicionParaSuperar unTiro = efectoPostObstaculo unTiro
--  | otherwise = detenerse unTiro


--4
---a
palosUtiles :: Jugador -> Obstaculo -> [Palo]
palosUtiles unJugador unObstaculo = filter (leSirveParaSuperar unJugador unObstaculo) palos

leSirveParaSuperar :: Jugador -> Obstaculo -> Palo -> Bool
leSirveParaSuperar unJugador unObstaculo unPalo = puedeSuperar unObstaculo (golpe unPalo unJugador)

---b
cuantosObstaculosConsecutivosSupera ::  Tiro -> [Obstaculo]  -> Int
cuantosObstaculosConsecutivosSupera _ [] = 0
cuantosObstaculosConsecutivosSupera unTiro (unObstaculo:restoDeObstaculos)
  | puedeSuperar unObstaculo unTiro = 1 + cuantosObstaculosConsecutivosSupera(efectoLuegoDeSuperar unObstaculo unTiro) restoDeObstaculos
  | otherwise = 0


---c

paloMasUtil :: Jugador -> [Obstaculo] -> Palo
paloMasUtil unJugador unosObstaculos = maximoSegun (flip cuantosObstaculosConsecutivosSupera unosObstaculos . flip golpe unJugador ) palos

--5
jugadorDeTorneo :: (a, b) -> a
jugadorDeTorneo = fst

puntosGanados :: (a, b) -> b
puntosGanados = snd

pierdenLaApuesta :: [(Jugador,Puntos)] -> [String]
pierdenLaApuesta puntosDeTorneo = (map(padre.jugadorDeTorneo) . filter(not . gano puntosDeTorneo)) puntosDeTorneo

gano :: [(Jugador,Puntos)] -> (Jugador,Puntos) -> Bool
gano puntosDeTorneo puntosDeUnJugador = (all ((< puntosGanados puntosDeUnJugador).puntosGanados) . filter (/= puntosDeUnJugador)) puntosDeTorneo