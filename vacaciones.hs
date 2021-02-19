import Text.Show.Functions()

type NivelDeCansancio = Int
type Stress = Int
type Idiomas = String

data Turista = Turista {
  nivelDeCansancio :: NivelDeCansancio,
  stress :: Stress,
  viajaSolo :: Bool,
  idiomasQueHabla :: [Idiomas]
} deriving (Eq, Show)


ana :: Turista
ana = Turista{
    nivelDeCansancio = 0,
    stress = 21,
    viajaSolo = False,
    idiomasQueHabla = ["espaniol"]
}

beto :: Turista
beto = Turista{
    nivelDeCansancio = 15,
    stress = 15,
    viajaSolo = False,
    idiomasQueHabla = ["aleman"]
}

cathi :: Turista
cathi = Turista{
    nivelDeCansancio = 15,
    stress = 15,
    viajaSolo = False,
    idiomasQueHabla = ["aleman","catalan"]
}

nico :: Turista
nico = Turista{
    nivelDeCansancio = 15,
    stress = 15,
    viajaSolo = True,
    idiomasQueHabla = ["aleman","catalan"]
}

-----------------------------------
mapNivelDeCansancio :: (NivelDeCansancio -> NivelDeCansancio) -> Turista -> Turista
mapNivelDeCansancio funcionQueModifica unTurista = unTurista {nivelDeCansancio = funcionQueModifica . nivelDeCansancio $ unTurista}

mapStress :: (Stress -> Stress) -> Turista -> Turista
mapStress funcionQueModifica unTurista = unTurista {stress = funcionQueModifica . stress $ unTurista}

mapIdiomas :: ([Idiomas] -> [Idiomas]) -> Turista -> Turista
mapIdiomas funcionQueModifica unTurista = unTurista {idiomasQueHabla = funcionQueModifica . idiomasQueHabla $ unTurista}

mapEstaAcompaniado :: Turista -> Turista
mapEstaAcompaniado unTurista = unTurista {viajaSolo = False}
------------------------------------
--2
type Excursion = Turista -> Turista
type Elemento = String
type Minutos = Int
type Intensidad = String

estaViajandoAcompaniado :: Turista -> Turista
estaViajandoAcompaniado unTurista = unTurista{viajaSolo = False}

irALaPlaya :: Excursion
irALaPlaya unTurista
    | viajaSolo unTurista = mapNivelDeCansancio  (max 0 . ((flip (-))) 5) unTurista 
    | otherwise = mapStress ((max 0 . ((flip (-))) 1)) unTurista

apreciarElementoDelPaisaje :: Elemento -> Excursion
apreciarElementoDelPaisaje unElemento = mapStress (max 0 . ((flip (-)))(length unElemento)) 

salirAHablarUnIdioma :: Idiomas -> Excursion
salirAHablarUnIdioma unIdioma = mapEstaAcompaniado . mapIdiomas (aprenderIdioma unIdioma) 

aprenderIdioma :: Idiomas -> [Idiomas] -> [Idiomas]
aprenderIdioma unIdioma listaIdiomas = unIdioma : listaIdiomas

caminar :: Minutos -> Excursion
caminar cantDeMinutos  = (mapNivelDeCansancio ((+) (nivelDeIntensidad cantDeMinutos))) . (mapStress (max 0 . ((flip (-)))(nivelDeIntensidad cantDeMinutos)))  

nivelDeIntensidad :: Int -> Int
nivelDeIntensidad mins = div mins 4

paseoEnBarco :: Intensidad -> Excursion
paseoEnBarco "fuerte" = (mapNivelDeCansancio ((+) 10)) . (mapStress ((+) 5))
paseoEnBarco "moderada" = id 
paseoEnBarco "tranquila" = caminar 10 . apreciarElementoDelPaisaje "mar" . salirAHablarUnIdioma "aleman"

--UNA MEJOR FORMA DE HACERLO: -----------
data Marea
  = Tranquila
  | Moderada
  | Fuerte

paseoEnBarco1 :: Marea -> Excursion
paseoEnBarco1 Fuerte = (mapNivelDeCansancio ((+) 10)) . (mapStress ((+) 5))
paseoEnBarco1 Moderada = id 
paseoEnBarco1 Tranquila = caminar 10 . apreciarElementoDelPaisaje "mar" . salirAHablarUnIdioma "aleman"

------------------------
--2a

realizarUnaExcursion :: Excursion -> Turista -> Turista 
realizarUnaExcursion unaExcursion  = reducir10Destress . unaExcursion

reducir10Destress :: Turista -> Turista
reducir10Destress unTurista = mapStress (max 0 . ((flip (-))) (calcular10Porciento $ stress unTurista)) unTurista

calcular10Porciento :: Int -> Int
calcular10Porciento unNro = div (unNro * 10) 100

--2b

deltaSegun :: (a -> Int) -> a -> a -> Int
deltaSegun f algo1 algo2 = f algo1 - f algo2

deltaExcursionSegun :: (Turista -> Int) -> Turista -> Excursion -> Int
deltaExcursionSegun f unTurista unaExcursion = deltaSegun f (realizarUnaExcursion unaExcursion unTurista) unTurista

--c
esEducativa :: Turista -> Excursion -> Bool
esEducativa unTurista  = (>0) . deltaExcursionSegun (length . idiomasQueHabla) unTurista

esDesestresante :: Turista -> Excursion -> Bool
esDesestresante unTurista = (<= -3) . deltaExcursionSegun stress unTurista

excursionesDesestresantes :: Turista -> [Excursion] -> [Excursion]
excursionesDesestresantes unTurista  = filter (esDesestresante unTurista) 

--3

type Tour = [Excursion]

tourCompleto :: Tour
tourCompleto = [caminar 20 , apreciarElementoDelPaisaje "cascada", caminar 40, irALaPlaya, salirAHablarUnIdioma "malmacquiano"]

tourLadoB :: Excursion -> Tour
tourLadoB unaExcursion = [paseoEnBarco "tranquila", unaExcursion , caminar 120]

tourIslaVecina :: String -> Tour
tourIslaVecina tipoMarea = [paseoEnBarco tipoMarea,excursionEnIslaVecina tipoMarea , paseoEnBarco tipoMarea]

excursionEnIslaVecina :: String -> Excursion
excursionEnIslaVecina tipoMarea 
    | tipoMarea == "fuerte" = apreciarElementoDelPaisaje "lago"
    | otherwise = irALaPlaya

---a
hacerUnTour :: Turista -> Tour -> Turista
hacerUnTour unTurista unTour = mapStress ((+) $ length unTour) $ foldl (flip realizarUnaExcursion) unTurista unTour

