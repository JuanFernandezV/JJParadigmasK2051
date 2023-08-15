%comio(Personaje, Bicho) 
comio(pumba, vaquitaSanAntonio(gervasia,3)). 
comio(pumba, hormiga(federica)). 

comio(pumba, hormiga(tuNoEresLaReina)). 
comio(pumba, cucaracha(ginger,15,6)). 
comio(pumba, cucaracha(erikElRojo,25,70)). 

comio(timon, vaquitaSanAntonio(romualda,4)). 
comio(timon, cucaracha(gimeno,12,8)). 
comio(timon, cucaracha(cucurucha,12,5)). 

comio(simba, vaquitaSanAntonio(remeditos,4)). 
comio(simba, hormiga(schwartzenegger)). 
comio(simba, hormiga(niato)). 
comio(simba, hormiga(lula)). 

pesoHormiga(2). 

%peso(Personaje, Peso) 
peso(pumba, 100). 
peso(timon, 50). 
peso(simba, 200). 

% 1) a.

jugosita(Cucaracha) :-
    comio(_, cucaracha(Cucaracha, Tamanio, Peso)),
    otraCucarachaMismoTamanio(Cucaracha, Tamaño, Peso).

otraCucarachaMismoTamanio(Cucaracha, Tamanio, Peso) :-
    comio(_, cucaracha(OtraCucaracha, Tamanio, PesoMayor)),
    OtraCucaracha \= Cucaracha,
    PesoMayor > Peso.

% 1) b.

hormigofílico(Personaje) :-
    comio(Personaje, hormiga(_)),
    findall(_, comio(Personaje, hormiga(_)), Hormigas), % findall -> orden superior
    length(Hormigas, Cantidad),                         % length -> orden superior
    Cantidad >= 2. 

% 1) c.

cucarachofibico(Personaje) :-
    not(comio(Personaje, cucaracha(_, _, _))).

% 1) d.

picaron(pumba). % pumba es picaron de por si
picaron(Personaje) :-                                   % Inversibilidad, para acotar el conjunto consultado
    comio(Personaje, cucaracha(_, _, _)),
    jugosita(cucaracha(_, _, _)).
picaron(Personaje) :-
    comio(Personaje, vaquitaSanAntonio(remeditos, _)).


% 2) 

persigue(scar, timon).
persigue(scar, pumba).
persigue(shenzi, simba).
persigue(shenzi, scar).
persigue(banzai, timon).

comio(shenzi,hormiga(conCaraDeSimba)). 

peso(scar, 300). 
peso(shenzi, 400). 
peso(banzai, 500). 

% 2) a.

cuantoEngorda(Personaje, PesoTotal) :-
    findall(Peso, (comio(Personaje, Bicho), pesoBicho(Bicho, Peso)), Pesos),
    sumlist(Pesos, PesoTotal).

pesoBicho(vaquitaSanAntonio(_, Peso), Peso).
pesoBicho(cucaracha(_, _, Peso), Peso).
pesoBicho(hormiga(_), Peso) :-
    pesoHormiga(Peso).

% 2) b.

cuantoEngorda(Personaje, PesoTotal) :-
    findall(Peso, comio(Personaje, Bicho), Pesos),
    sumlist(Pesos, PesoComida),
    peso(Personaje, PesoPersonaje),
    PesoTotal is PesoPersonaje + PesoComida,
    persigue(_, Personaje),
    cuantoEngorda(_, PesoPerseguido),
    PesoTotal is PesoTotal + PesoPerseguido.
    
cuantoEngorda(Personaje, PesoTotal) :-
    findall(Peso, comio(Personaje, Bicho), Pesos),
    sumlist(Pesos, PesoComida),
    peso(Personaje, PesoPersonaje),
    PesoTotal is PesoPersonaje + PesoComida.

% 2) c.

cuantoEngorda(Personaje, PesoTotal) :-
    findall(Peso, comio(Personaje, Bicho), Pesos),
    sumlist(Pesos, PesoComida),
    peso(Personaje, PesoPersonaje),
    pesoVictimas(Personaje, PesoVictimas),
    PesoTotal = PesoPersonaje + PesoComida + PesoVictimas.          % Unificacion

pesoVictimas(Personaje, PesoTotal) :-
    findall(Victima, persigue(Personaje, Victima), Victimas),
    cuantoEngorda(Victimas, PesoTotal).

cuantoEngorda([], 0).                           % Recursividad para calcular cuanto aumentaron de peso las victimas para luego sumarla
cuantoEngorda([Victima | Resto], PesoTotal) :-  
    cuantoEngorda(Victima, PesoVictima),
    cuantoEngorda(Resto, PesoResto),
    PesoTotal = PesoVictima + PesoResto.


