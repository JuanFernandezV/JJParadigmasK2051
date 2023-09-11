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
    comio(Personaje, Bicho),
    not(esCucaracha(Bicho)).

% 1) d.

picarones([pumba]). % Lista de picarones iniciales

picaron(pumba, _). % Pumba es picarón de por sí

picaron(Personaje, Picarones) :-
    comio(Personaje, cucaracha(_, _, _)),
    jugosita(cucaracha(_, _, _)),
    \+ member(Personaje, Picarones).    % funcion de orden superior member

picaron(Personaje, Picarones) :-
    comio(Personaje, vaquitaSanAntonio(remeditos, _)),
    \+ member(Personaje, Picarones).

picarones(Picarones) :-
    findall(Personaje, picaron(Personaje, Picarones), Picarones).

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
    cuantoEngorda(_, PesoPerseguido),           % recursividad para obtener el peso si persigue a mas de una victima
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
    PesoTotal is PesoPersonaje + PesoComida + PesoVictimas.          % Unificacion

pesoVictimas(Personaje, PesoTotal) :-
    findall(Victima, persigue(Personaje, Victima), Victimas),
    cuantoEngorda(Victimas, PesoTotal).

cuantoEngorda([], 0).                           % Recursividad para calcular cuanto aumentaron de peso las victimas para luego sumarla
cuantoEngorda([Victima | Resto], PesoTotal) :-  
    cuantoEngorda(Victima, PesoVictima),
    cuantoEngorda(Resto, PesoResto),
    PesoTotal is PesoVictima + PesoResto.

% 3
% Utilizamos recursividad en este caso para generar todas las combinaciones posibles con respecto a la comida
listaComidas(Personaje, Lista1):-
comio(Personaje, _),
not(persigue(Personaje, _)),
findall(Bicho, comio(Personaje, Bicho), Lista1).


listaComidas(Personaje, Lista1):-
persigue(Personaje, _),
not(comio(Personaje, _)),
findall(Personje2, persigue(Personaje, Personaje2), Lista1).


listaComidas(Personaje, ListaT):-
persigue(Personaje, _),
comio(Personaje, _),
findall(Bicho, comio(Personaje, Bicho), Lista1),
findall(Personje2, persigue(Personaje, Personaje2), Lista2),
append(Lista1, Lista2, ListaT).

subconjunto([], []).
subconjunto([X|Resto], [X|Sub]) :- subconjunto(Resto, Sub).
subconjunto([_|Resto], Sub) :- subconjunto(Resto, Sub).

combinaComidas(Personaje, ListaComidas):-
listaComidas(Personaje, Lis),
subconjunto(Lis, ListaComidas).

% 4
rey(Rey) :-
    persigue(_, Rey),                      
    not(persigue(Rey, _)),                 
    not(comio(Rey, _)),                    
    forall((persigue(Animal, _), Animal \= Rey), adora(Animal, Rey)), 
    soloUnPerseguidor(Rey).                

adora(Animal, Rey) :-
    Animal \= Rey,
    not(persigue(Animal, Rey)),
    not(comio(Animal, Rey)).

soloUnPerseguidor(Rey) :-
    findall(Perseguidor, persigue(Perseguidor, Rey), Perseguidores),
    length(Perseguidores, 1).


% a. Polimorfismo: El polimorfismo se utiliza en la definición del predicado cuantoEngorda y su versión recursiva. Se manejan varios tipos de bichos (hormigas, vaquitas de San Antonio, cucarachas) y el predicado se adapta para funcionar con cualquiera de estos tipos, lo que refleja el concepto de polimorfismo.

% b. Recursividad: La recursividad se usa en el predicado cuantoEngordaRecursivo para calcular el peso total de un personaje considerando lo que comió y lo que comieron sus víctimas, teniendo en cuenta que cada víctima también puede haber comido a otras víctimas.

% c. Inversibilidad: En los predicados jugosita, hormigofilico, cucarachofobico, picarones, combinaComidas y rey, se hacen consultas en ambas direcciones, es decir, se pueden hacer preguntas tanto sobre variables sin instanciar como sobre variables ya instanciadas. Esto es posible gracias a la inversibilidad de Prolog, que permite tanto buscar soluciones como generar posibles valores para las variables.