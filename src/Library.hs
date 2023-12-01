module Library where
import PdePreludat

-- Defino mis alias
type Edad = Number
type Nombre = String
type Energia = Number
type Habilidad = String
type Habilidades = [Habilidad]
type Planeta = String
type Material = String
type Gema = Personaje -> Personaje
type Gemas = [Gema]
type Habitante = Personaje
type Habitantes = [Habitante]
type CondicionGuantelete = Guantelete -> Bool
type CondicionesGuantelete = [CondicionGuantelete]
type DesaparecerUniverso = Guantelete -> Universo -> Universo
type NuevoUniverso = Universo -> Universo
type Poder = Personaje -> Personaje

-- Defino mis tipos
data Personaje = Personaje {
    edad :: Edad,
    nombre :: Nombre,
    planeta :: Planeta,
    habilidades :: Habilidades,
    energia :: Energia
} deriving Show

data Guantelete = Guantelete {
    material :: Material,
    gemas :: Gemas
} deriving Show

data Universo = Universo {
    habitantes :: Habitantes
} deriving Show

-- Defino todas las funciones necesarias para el chasquido
esDeUru :: CondicionGuantelete
esDeUru guantelete = material guantelete == "uru"

tieneLasGemasNecesarias :: CondicionGuantelete
tieneLasGemasNecesarias guantelete = length (gemas guantelete) >= 6

condiciones :: CondicionesGuantelete
condiciones = [esDeUru, tieneLasGemasNecesarias]

cumpleCondicion :: Guantelete -> CondicionGuantelete -> Bool
cumpleCondicion guantelete condicion = condicion guantelete

guanteleteCumpleCondiciones :: CondicionGuantelete
guanteleteCumpleCondiciones guantelete = all (cumpleCondicion guantelete) condiciones 

cantidadAEliminar :: Habitantes -> Number
cantidadAEliminar habitantes = div (length habitantes) 2

desaparecerPersonajes :: NuevoUniverso
desaparecerPersonajes universo = universo {habitantes = take (cantidadAEliminar (habitantes universo)) (habitantes universo)}

desaparecerUniverso :: DesaparecerUniverso
desaparecerUniverso guantelete universo 
    | guanteleteCumpleCondiciones guantelete = desaparecerPersonajes universo
    | otherwise = universo


-- Defino el chaquido
ironMan, drStrange, capitanAmerica, hulk :: Personaje
ironMan = Personaje 30 "Tony" "Tierra" ["Lanza rayitos"] 100
drStrange = Personaje 35 "Stephen" "Tierra" ["Lanza hechizos"] 200
capitanAmerica = Personaje 120 "Steve" "Tierra" ["Lanza Escudo"] 50
hulk = Personaje 32 "Bruce" "Tierra" ["Pega fuerte"] 10000

universo616 :: Universo
universo616 = Universo [ironMan, drStrange, capitanAmerica, hulk]

chasquidoDeUniverso :: DesaparecerUniverso
chasquidoDeUniverso guantelete universo = desaparecerUniverso guantelete universo

-- Defino si el universo el apto para péndex
menosDe45 :: Habitante -> Bool
menosDe45 habitante = edad habitante < 45

esAptoParaPendex :: Universo -> Bool
esAptoParaPendex universo = any menosDe45 (habitantes universo)

-- Defino la energia total de un universo
energiaHabitante :: Habitante -> Energia
energiaHabitante habitante = energia habitante

energiaUniverso :: Universo -> Energia
energiaUniverso universo = sum (map energiaHabitante (habitantes universo))

-- Defino las funciones necesarias para las gemas
noEsLaHabilidad :: Habilidad -> Habilidad -> Bool
noEsLaHabilidad habilidad1 habilidad2 = habilidad1 /= habilidad2

eliminarHabilidad :: Habilidad -> Habilidades -> Habilidades
eliminarHabilidad habilidadAEliminar habilidades = filter (noEsLaHabilidad habilidadAEliminar) habilidades

seLasSacaTodas :: Habilidades -> Habilidades
seLasSacaTodas habilidades 
    | length habilidades <= 2 = []
    | otherwise = habilidades

reducirALaMitadEdad :: Edad -> Edad
reducirALaMitadEdad edad 
    | div edad 2 > 18 = div edad 2
    | otherwise = 18

-- Defino las gemas
gemaMente :: Number -> Gema
gemaMente valor personaje = personaje {energia = energia personaje - valor}

gemaAlma :: Habilidad -> Gema
gemaAlma habilidad personaje = personaje {habilidades = eliminarHabilidad habilidad (habilidades personaje), energia = energia personaje - 10}

gemaEspacio :: Planeta -> Gema
gemaEspacio planetaNuevo personaje = personaje {planeta = planetaNuevo, energia = energia personaje - 20}

gemaPoder :: Gema
gemaPoder personaje = personaje {energia = 0, habilidades = seLasSacaTodas (habilidades personaje)}

gemaTiempo :: Gema
gemaTiempo personaje = personaje {energia = energia personaje - 50, edad = reducirALaMitadEdad (edad personaje)}

gemaLoca :: Gema -> Gema
gemaLoca gema personaje = (gema.gema) personaje

-- Ejemplo guantelete de goma
guanteleteDeGoma :: Guantelete
guanteleteDeGoma = Guantelete "goma" [gemaTiempo, (gemaAlma "usar Mjolnir"), (gemaLoca (gemaAlma "programación en Haskell"))]

-- Defino la función "utilizar"
utilizar :: Gemas -> Gema
utilizar gemas personaje = foldr ($) personaje $ gemas -- Aplicar una lista de funciones sobre algo

-- Punto 7

infinitasGemas :: Gema -> [Gema]
infinitasGemas gema = gema:(infinitasGemas gema)

guanteleteDeLocos :: Guantelete
guanteleteDeLocos = Guantelete "vesconite" (infinitasGemas gemaTiempo)

usoLasTresPrimerasGemas :: Guantelete -> Personaje -> Personaje
usoLasTresPrimerasGemas guantelete = (utilizar . take 3. gemas) guantelete
