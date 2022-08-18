sangre(harry,mestiza).
sangre(draco,pura).
sangre(hermione,impura).

caracter(harry,coraje).
caracter(harry,amistoso).
caracter(harry,orgulloso).
caracter(harry,inteligente).
caracter(draco,inteligente).
caracter(draco,orgulloso).
caracter(hermione,inteligente).
caracter(hermione,orgulloso).
caracter(hermione,responsable).

odiaria(draco,hufflepuff).
odiaria(harry,slytherin).

seleccionSombrero(gryffindor,coraje).
seleccionSombrero(slytherin,orgulloso).
seleccionSombrero(slytherin,inteligente).
seleccionSombrero(ravenclaw,inteligente).
seleccionSombrero(ravenclaw,responsable).
seleccionSombrero(hufflepuff,amistoso).

% --Abstraccion
mago(Mago):- 
    sangre(Mago,_).

% Punto1)
% 1. Saber si una casa permite entrar a un mago, lo cual se cumple para cualquier mago y 
% cualquier casa excepto en el caso de Slytherin, que no permite entrar a magos de sangre 
% impura. 

laCasaPermiteEntrar(slytherin,Mago):- %especifico el que no cumple la condicion
    mago(Mago),
    not(sangre(Mago,impura)). % restriccion

laCasaPermiteEntrar(Casa,Mago):- %se cumple para todos
    mago(Mago),
    seleccionSombrero(Casa,_),
    not(Casa = slytherin). % menos para slytherin

% 2. Saber si un mago tiene el carácter apropiado para una casa, lo cual se cumple para cualquier 
% mago si sus características incluyen todo lo que se busca para los integrantes de esa casa, 
% independientemente de si la casa le permite la entrada.

tieneElCaracterApropiado(Mago,Casa):-
    seleccionSombrero(Casa,_),
    mago(Mago),
    forall(seleccionSombrero(Casa,Caracter),caracter(Mago,Caracter)).


% 3. Determinar en qué casa podría quedar seleccionado un mago sabiendo que tiene que tener 
% el carácter adecuado para la casa, la casa permite su entrada y además el mago no odiaría 
% que lo manden a esa casa. Además Hermione puede quedar seleccionada en Gryffindor, 
% porque al parecer encontró una forma de hackear al sombrero.

puedeQuedarSeleccionado(Casa,Mago):-
    tieneElCaracterApropiado(Mago,Casa),
    laCasaPermiteEntrar(Casa,Mago),
    not(odiaria(Mago,Casa)). % tanto Mago como Casa llegan ligados al not

puedeQuedarSeleccionado(gryffindor,hermione).

% 4. Definir un predicado cadenaDeAmistades/1 que se cumple para una lista de magos si todos 
% ellos se caracterizan por ser amistosos y cada uno podría estar en la misma casa que el 
% siguiente. No hace falta que sea inversible, se consultará de forma individual.

% cadenaDeAmistades(Lista_Magos):-
%     member(Mago,Lista_Magos),
%     forall(member(Mago,_),(sonTodosAmistosos(Mago),estaEnLaMismaCasaQueElSiguiente(Mago))).

% sonTodosAmistosos(Mago):-
%     mago(Mago),
%     forall(caracter(Mago,_),(Mago,amistoso))

% estaEnLaMismaCasaQueElSiguiente(Mago):-


% Parte 2 - La copa de las casas 

% accion(harry,mala(fuera_de_la_cama,-50)).
% accion(harry,mala(fue_al_bosque,-50)).
% accion(harry,mala(fue_al_tercer_piso,-75)).
% accion(hermione,mala(fue_al_tercer_piso,-75)).
% accion(hermione,mala(fue_seccion_restringida_biblioteca,-10)).
% accion(draco,neutral(fue_a_las_mazmorras)).
% accion(ron,buena(gano_partida_ajedrez_magico,50)).
% accion(hermione,buena(salvo_a_sus_amigos,50)).
% accion(harry,buena(vencio_voldemort,60)).

% puntajeTotalDeUnMago(Mago,Puntaje):-
%     accionesyPuntajes(Accion,Puntaje),
%     accion(Mago,Accion).

% esDe(hermione, gryffindor). 
% esDe(ron, gryffindor). 
% esDe(harry, gryffindor). 
% esDe(draco, slytherin). 
% esDe(luna, ravenclaw).

% Punto 2.1
% Saber si un mago es buen alumno, que se cumple si hizo alguna acción y ninguna de las cosas 
% que hizo se considera una mala acción (que son aquellas que provocan un puntaje negativo)

% esBuenAlumno(Mago):-
%     accion(Mago,_),
%     not(accion(Mago,mala(_,_,_))).

% Punto 2.2
% Saber si una acción es recurrente, que se cumple si más de un mago hizo esa misma acción. 

% laAccionEsRecurrente(Accion):-
%    conjuntoDeAcciones(Mago,Accion),
%    conjuntoDeAcciones(OtroMago,Accion),
%    OtroMago \= Mago.
 
% Saber cuál es el puntaje total de una casa, que es la suma de los puntos obtenidos por sus 
% miembros.

% sumaDelPuntajeTotal(Casa):-
%     esDe(Mago,Casa),


% accionesyPuntajes(Accion,Puntaje):-
%     accion(_,buena(Accion,Puntaje)).

% accionesyPuntajes(Accion,Puntaje):-
%     accion(_,mala(Accion,Puntaje)).




% conjuntoDeAcciones(Mago,Accion):-
%     accion(Mago,mala(Accion,_,_)).
% conjuntoDeAcciones(Mago,Accion):-
%    accion(Mago,neutral(Accion)).
% conjuntoDeAcciones(Mago,Accion):-
%     accion(Mago,buena(Accion,_,_)).


:- begin_tests(parcial).

test(parte1a_laCasaPermiteEntrar_a_un_mago,nondet):-
    \+laCasaPermiteEntrar(slytherin,hermione),
    laCasaPermiteEntrar(_,draco),
    laCasaPermiteEntrar(_,harry),
    laCasaPermiteEntrar(gryffindor,hermione),
    laCasaPermiteEntrar(hufflepuff,hermione),
    laCasaPermiteEntrar(ravenclaw,hermione).

test(parte1b_tiene_el_caracter_apropiado_para_la_casa,nondet):-
    tieneElCaracterApropiado(draco,slytherin),
    tieneElCaracterApropiado(harry,slytherin),
    tieneElCaracterApropiado(harry,gryffindor),
    tieneElCaracterApropiado(hermione,ravenclaw),
    tieneElCaracterApropiado(harry,hufflepuff).

% test(parte1c_puede_quedar_seleccionado,nondet):-
%     puedeQuedarSeleccionado(slytherin,draco),
%     puedeQuedarSeleccionado(gryffindor,hermione).

% test(parte2_a_No_Hizo_niguna_mala_accion,nondet):-
%     esBuenAlumno(draco),
%     esBuenAlumno(ron).


:- end_tests(parcial).
