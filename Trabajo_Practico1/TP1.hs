-- 1) Modelar las plantas y zombies definidos a continuación mediante funciones
-- constantes
-- a) --
data Planta = Planta{
    especie :: String,
    vida :: Int,
    cantSoles :: Int,
    pAtaque :: Int
}
double :: Planta(_,_,_,Int) -> Int
double x = x + x
peaShoter :: Planta
peaShoter = Planta "Peashoter" 5 0 2
repeater :: Planta
repeater = Planta "Repeater" 5 0 double(Planta (_,_,_,2))
sunflower :: Planta
sunflower = Planta "Sunflower" 7 1 0 
nut :: Planta
nut = Planta "Nut" 100 0 0
planta1 :: Planta
planta1 = Planta "PeaShoterDouble" 5 0 5
planta2 :: Planta
planta2 = Planta "Bomb" 2 0 10

-- b) --

data Zombie = Zombie{
    nombre :: String,
    accesorios :: [String],
    danio :: Int
}
zombieBase:: Zombie
zombieBase = "Zombie Base" [] 1
ballonzombie:: Zombie
ballonzombie = "Ballon zombie" ["globo"] 1
newszombie:: Zombie
newszombie = "Newspaper Zombie" ["diario"] 2
gargantuar:: Zombie
gargantuar = "Gargantuar Hulk Smash Puny God" ["poste electrico","zombie enano"] 30

-- determinar nivel de muerte --

niveldeMuerte :: (String->Bool) -> Int  
niveldeMuerte (head:tail) = [] == []
niveldeMuerte (head:tail) = 1 + niveldeMuerte tail
-- 2) a) determinar especialidad planta --

especialidadPlanta :: Planta -> String
especialidadPlanta planta   | cantSoles plantas > 0 = "Proveedora" 
                            | pAtaque planta > vida planta = "Atacante"
                            | otherwise = "Defensiva"

-- 2) b) Determinar si un zombie es peligroso --

esPeligroso :: Zombie -> Bool
esPeligroso zombie = length(accesorios zombie) > 1 || danio > 10

--3) a) --
data LineaDeDefensa = LineaDeDefensa{
    plantas :: [Planta],
    zombies :: [Zombie]
}

--3) a) i) Agregar una planta a una línea (se agrega al final)--

agregarPlantaLinea :: LineaDeDefensa -> Planta -> LineaDeDefensa
agregarPlantaLinea linea planta = plantas linea ++ planta

--3) a) ii) Agregar un zombie a una línea (se agrega al final)--

agregarZombieLinea :: LineaDeDefensa -> Zombie -> LineaDeDefensa
agregarZombieLinea linea zombie = linea { zombies = nuevoZombie }
    where nuevoZombie = zombies linea ++ [zombie]

-- 3) b) Saber si una línea está en peligro, que es cuando el total de ataque de todas
--las plantas es inferior al total de mordiscos de todos los zombies, o bien
--todos los zombies de esa línea son peligrosos y hay al menos un zombie

--funcion para buscar cantidad de plantas en la linea

totalAtaque :: LineaDeDefensa -> Int
totalAtaque linea = sum(map pAtaque(plantas linea))

-- funcion para buscar cantidad de zombies en la linea (igual a plantas)

totalMordiscos :: LineaDeDefensa -> Int
totalMordiscos linea = sum(map danio (zombies linea))

-- ver si los zombies son peligrosos o no en la linea

todosPeligrosos :: LineaDeDefensa -> Bool
todosPeligrosos linea = all esPeligroso(zombies linea)

-- Ver si la linea esta en peligro 

estaEnPeligro :: LineaDeDefensa -> Bool
estaEnPeligro linea = totalAtaque linea < totalMordiscos linea || todosPeligrosos linea

-- 3) c) Poder determinar si una línea necesita ser defendida, esto pasa cuando
-- todas las plantas de esa línea son proveedoras

--verificar si una planta es proveedora

esProveedora :: Planta -> Bool
esProveedora planta = especialidadPlanta(planta) == "Proveedora"

-- ver si necesita ser defendida

necesitaSerDefendida :: LineaDeDefensa -> Bool
necesitaSerDefendida linea = all esProveedora(plantas linea)

-- 4)Saber si una línea es mixta, que es cuando ninguna de las plantas de la línea tiene
-- la misma especialidad que su inmediata siguiente. Además, la línea debe tener al
-- menos dos plantas.
-- Nota: No usar length (ni ninguna función que tenga el mismo propósito).

lineaMixta :: LineaDeDefensa -> Bool
lineaMixta linea (especialidad) | head(plantas linea) != [] && null(tail plantas linea)--verifica si hay mas de una planta en la linea, si no hay manda true
                                | head(plantas linea) == especialidad && tail(plantas linea, especialidad) = True

-- 5) a)
-- Una planta a un zombie: Cuando una planta ataca a un zombie, lo daña
-- según su potencia de ataque, pero ese daño consiste en quitarle al nombre
-- del zombie las primeras n letras, siendo n la potencia de ataque de la planta.
-- Cuando el nivel de muerte del zombie llega a cero, es derrotado, pero eso
-- no se refleja acá.

ataquePlanta :: Planta -> Zombie -> Zombie
ataquePlanta planta zombie = zombie {nombre = drop(pAtaque planta) (nombre zombie)}


-- 5) b) Un zombie a una planta: El zombie muerde a la planta causándole un daño
--a la vida equivalente a la fuerza de su mordida.

ataqueZombie :: Zombie -> Planta -> Planta
ataqueZombie zombie planta = planta {vida = vida planta - danio zombie}

