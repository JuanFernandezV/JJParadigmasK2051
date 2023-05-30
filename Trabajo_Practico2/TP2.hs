-- 1. Responder (agregar un comentario con la respuesta en el archivo con el
-- código).
-- i. ¿Qué pasaría al consultar si una línea está en peligro si hubiera
-- una cantidad infinita de zombies bases en la misma?
-- ii. ¿Qué pasaría al consultar si una línea con una cantidad infinita
-- de PeaShooters necesita ser defendida? ¿Y con una cantidad
-- infinita de Sunflowers?
-- iii. Justificar las respuestas conceptualmente.

-- 1. si intentamos verificar si una línea está en peligro y hay una cantidad infinita de zombies bases en la misma, la evaluación se volverá infinita y el programa no se detendrá.

-- Esto se debe a la naturaleza perezosa (lazy) de Haskell. Cuando se realiza una evaluación perezosa, Haskell evita realizar cálculos innecesarios hasta que sea absolutamente necesario. En el caso de tener una lista infinita de zombies bases en la línea de defensa, la función totalMordiscos intentará calcular la suma de los daños de todos los zombies en la lista. Sin embargo, nunca se llegará a un resultado porque la lista es infinita.

-- Si intentas ejecutar una función como estaEnPeligro con una línea de defensa que contenga una lista infinita de zombies bases, el programa se quedará en un bucle infinito o nunca terminará de ejecutarse.


-- 2. En el caso específico de una cantidad infinita de PeaShooters, la función necesitaSerDefendida intentará verificar si todos los PeaShooters son "Proveedores". Dado que la lista es infinita, la evaluación nunca se completará.

-- Lo mismo ocurre con una cantidad infinita de Sunflowers. La función necesitaSerDefendida intentará verificar si todos los Sunflowers son "Proveedores", lo cual nunca se puede determinar en una lista infinita.

-- En resumen, al trabajar con cantidades infinitas de plantas en Haskell, debes tener en cuenta que la evaluación puede no terminar y el programa puede quedarse en un bucle infinito. Para evitar esto, es necesario encontrar formas de limitar la evaluación o utilizar técnicas como el concepto de listas infinitas controladas mediante generadores o funciones de generación perezosa.
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

data Planta = Planta{
    especie :: String,
    vida :: Int,
    cantSoles :: Int,
    pAtaque :: Int
} deriving(Eq,Show)

peaShoter :: Planta
peaShoter = Planta "Peashoter" 5 0 2
repeater :: Planta
repeater = Planta "Repeater" 5 0 4
sunflower :: Planta
sunflower = Planta "Sunflower" 7 1 0
nut :: Planta
nut = Planta "Nut" 100 0 0
planta1 :: Planta
planta1 = Planta "PeaShoterDouble" 5 0 5
planta2 :: Planta
planta2 = Planta "Bomb" 2 0 10

-- 2) Se agrega al modelo esta nueva variedad de planta: Cactus

cactus :: Planta
cactus = Planta "Cactus" 9 0 0

data Zombie = Zombie{
    nombre :: String,
    accesorios :: [String],
    danio :: Int
} deriving(Eq,Show)

zombieBase:: Zombie
zombieBase = Zombie "Zombie Base" [] 1
ballonzombie:: Zombie
ballonzombie = Zombie "Ballon zombie" ["globo"] 1
newszombie:: Zombie
newszombie = Zombie "Newspaper Zombie" ["diario"] 2
gargantuar:: Zombie
gargantuar = Zombie "Gargantuar Hulk Smash Puny God" ["poste electrico","zombie enano"] 30

-- 3) Modelar la información y agregar una horda de zombies a un jardín (una lista de línea de defensa).

data LineaDeDefensa = LineaDeDefensa {
    plantas :: [Planta],
    zombies :: [Zombie]
} deriving(Eq,Show)

data Jardin = Jardin {
    lineas :: [LineaDeDefensa]
} deriving (Show)

linea1 :: LineaDeDefensa
linea1 = LineaDeDefensa [planta1,planta2] [zombieBase,gargantuar]
linea2 :: LineaDeDefensa
linea2 = LineaDeDefensa [peaShoter,repeater,peaShoter,sunflower] [newszombie,ballonzombie,zombieBase,zombieBase]
linea3 :: LineaDeDefensa
linea3 = LineaDeDefensa [repeater,planta2,nut] [newszombie,newszombie,zombieBase]
jardin :: Jardin
jardin =Jardin [linea1, linea2, linea3]

-- 4) Saber cómo sería el resultado de una ronda de ataque para una planta y un zombie.

rondaAtaque :: Planta -> Zombie -> (Planta, Zombie)
rondaAtaque planta zombie = (ataquePlanta planta zombie, ataqueZombie zombie planta)

ataquePlanta :: Planta -> Zombie -> Planta
ataquePlanta planta zombie = planta { vida = vida planta - danio zombie }

ataqueZombie :: Zombie -> Planta -> Zombie
ataqueZombie zombie planta = zombie { nombre = drop (pAtaque planta) (nombre zombie) }

-- 5 Definir una función que permita saber luego de un fuego cruzado:
-- a. Si una planta murió.

plantaMurio :: Planta -> Bool
plantaMurio planta = vida planta <= 0

-- b. Si un zombie determinado murió

zombieMurio :: Zombie -> Bool
zombieMurio zombie = nivelDeMuerte zombie <= 0

nivelDeMuerte :: Zombie -> Int
nivelDeMuerte zombie = length (nombre zombie)

-- 6 Se quiere conocer el resultado de una lista de plantas atacadas sistemáticamente por un solo zombie. Realizarlo por listas por comprensión.

ataqueSistematico :: [Planta] -> Zombie -> [(Planta, Planta)]
ataqueSistematico plantas zombie = [(planta, ataquePlanta planta zombie) | planta <- plantas]

-- 7  Se desea conocer dada una línea de defensa y una horda de zombies incluir dicha horda en la línea y luego mostrar cómo quedaría la línea de defensa luego de que se produzca una serie de ataques tomando solo a los zombies que no son peligrosos.

esPeligroso :: Zombie -> Bool
esPeligroso zombie = length (accesorios zombie) > 1 || danio zombie > 10


-- serieDeAtaques :: LineaDeDefensa -> [Zombie] -> LineaDeDefensa
-- serieDeAtaques = realizarAtaques

-- realizarAtaques :: LineaDeDefensa -> [Zombie] -> LineaDeDefensa
-- realizarAtaques linea [] = linea
-- realizarAtaques (LineaDeDefensa [] _) _ = LineaDeDefensa [] []
-- realizarAtaques (LineaDeDefensa _ []) _ = LineaDeDefensa [] []
-- realizarAtaques (LineaDeDefensa (planta:plantasRestantes) zombies)(zombie:zombiesRestantes) | esPeligroso zombie = realizarAtaques (LineaDeDefensa (planta:plantasRestantes) zombies) zombiesRestantes
--                                                                                             | otherwise = realizarAtaques (LineaDeDefensa (nuevasPlantas plantasRestantes) nuevosZombies) zombiesRestantes 

-- nuevosZombies = tail
-- nuevasPlantas [] = []
-- nuevasPlantas (p:ps) = ataquePlanta p zombie : p : ps

-- 8) Desarrollar la función theZombiesAteYourBrains, que nos dice, si para un jardín y una horda de zombies se incorporan y atacan con un mordisco, luego de un ataque en serie de los zombies, nos quedamos sin plantas en todo el jardín.


data Legion = Legion {
    zombiesLegion :: [Zombie]
} deriving (Show)

theZombiesAteYourBrains :: Jardin -> Legion -> Bool
theZombiesAteYourBrains jardin legion = null (lineasSinPlantas jardin)

lineasSinPlantas :: Jardin -> [LineaDeDefensa]
lineasSinPlantas jardin = filter (null . plantas) (lineas jardin)

-- 9) Dado un zombie y una linea decir si todos los zombies que están incluidos 
-- tienen menos cantidad de letras en sus nombres que el zombie dado.

todosZombiesConMenosLetras :: Zombie -> LineaDeDefensa -> Bool
todosZombiesConMenosLetras zombie linea = all (\z -> length (nombre z) < length (nombre zombie)) (zombies linea)

-- 10) Explicar qué hace la función y dar los tipos de la función:
-- la funcion f recibe cuatro argumentos que determinan diferentes salidas:
-- si valor de h matchea con la lista, la funcion filtra la lista usando la funcion m con h de argumento devolviendo los elementos que vuelvan a matchear con el formato de h
-- despues, toma el primer elemento de la lista que cumple con la condicion m  h y devuelve el primer elemento obtenido
-- si h no hace match con los elementos de la lista, sale por otherwise devolviendo el primer elemento p. ya que fst p es una tupla 

-- 11) Se quiere saber el nivel de supervivencia de una línea. El mismo se expresa 
-- como la diferencia entre suma de vida de las plantas y suma de muerte de los
-- zombies de una línea de plantas. Realizar la función utilizando expresiones
-- Lambda.

nivelSupervivencia :: LineaDeDefensa -> Int
nivelSupervivencia linea = sum (map (\p -> vida p) (plantas linea)) - sum (map (\z -> danio z) (zombies linea))