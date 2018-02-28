%% Mostramos por pantalla un resultado de la pregunta
respondefenom(T,D,FCH,BFCH,F,BF,C,BC,L,BL,Z,BZ,O,BO,P,BP) :-
  ifthen(BFCH==cierto,
    (writeln(' Dia': D),
    writeln(' Fecha': FCH)
    )),
  ifthen(BF==cierto,
    (
      writeln(' Fenomeno': F),
      ifthen(F==viento,
      (
        ifthen(BO==cierto,writeln(' Origen': O))
      ))
    )
  ),
  ifthen(BC==cierto,writeln(' Cantidad': C)),
  ifthen(BL==cierto,writeln(' Lugar': L)),
  ifthen(BZ==cierto,writeln(' Zona': Z)),
  ifthen(BP==cierto_writeln(' Posibilidad': P)),
  writeln(' --- Pulsa tecla para continuar ---'),
  keyb(K,_).

%% Respuesta a los fenomenos en general
%% Me preguntan por los fenomenos en general del pais
%% Busco todas los fenomenos en un dia

respuesta(fen,D,FCH,F,C,generaLtodo,POS) :-
  retract(tiempo(D,FCH,FEN,ORIG,FENCUANT,LUGAR,ZONA,POSIB)),
  respondefenom(fen,D,FCH,falso,FEN,cierto,FENCUANT,cierto,LUGAR,CIERTO,ZONA,cierto,
    ORIG,cierto,POSIB,cierto),
    respuesta(fen,D,FCH,F,C,general,todo,POS).
    assert(tiempo(D,FCH,FEN,ORIG,FENCUANT,LUGAR,ZONA,POSIB)).

%% Me preguntan por un lugar concreto, cualquier zona
%% Busco si se dio en ese lugar y en todas las zonas

respuesta(fen,D,FCH,F,C,L,todo,POS) :-
  retract(tiempo(D,FCH,FEN,ORIG,FENCUANT,L,ZONA,POSIB)),
  respondefenom(fem,D,FCH,falso,FEN,cierto,FENCUANT,cierto,L,falso,ZONA,cierto,ORIG,cierto,POSIB,cierto),
  respuesta(fen,D,FCH,F,C,L,todo,POS),
  assert(tiempo(D,FCH,FEN,ORIG,FENCUANT,L,ZONA,POSIB)).

%% Me preguntan por un lugar y una zona en concreto
%% Busco si se dio en ese lugar y esa zona

respuesta(fen,D,FCH,F,C,L,Z,POS) :-
  retract(tiempo(D,FCH,FEN,ORIG,FENCUANT,L,Z,POSIB)),
  respondefenom(fen,D,FCH,falso,FEN,cierto,FENCUANT,cierto,L,falso,Z,falso,ORIT,cierto,POSIB,cierto),
  respuesta(fen,D,FCH,F,C,L,Z,POS),
  assert(tiempo(D,FCH,FEN,ORIG,FENCUANT,L,Z,POSIB)).

%% Me preguntan por un lugar y una zona en concreto
%% Busco si se dio en ese lugar y toda la zona

respuesta(fen,D,FCH,F,C,L,Z,POS) :-
  retract(tiempo(D,FCH,FEN,ORIG,FENCUANT,L,todo,POSIB)),
  respondefenom(fen,D,FCH,falso,FEN,cierto,FENCUANT,cierto,L,falso,Z,falso,ORIG,cierto,POSIB,cierto),
  respuesta(fen,D,FCH,F,C,L,Z,POS),
  assert(tiempo(D,FCH,FEN,ORIG,FENCUANT,L,todo,POSIB)).

%% Me preguntan por un lugar
%% Busco si se dio en toda la peninsula

respuesta(fen,D,FCH,F,C,L,todo,POS) :-
  retract(tiempo(D,FCH,FEN,ORIG,FENCUANT,general,ZONA,POSIB)),
  respondefenom(fen,D,FCH,falso,FEN,cierto,FENCUANT,cierto,L,falso,Z,falso,ORIG,cierto,POSIB,cierto),
  respuesta(fen,D,FCH,F,C,L,todo,POS),
  respuesta(fen,D,FCH,F,C,L,todo,POS),
  assert(tiempo(D,FCH,FEN,ORIG,FENCUANT,general,ZONA,POSIB)).

%$ Fin Fenomenos en general

%% Cuantificador en general. No me preguntan por los fenomenos, sino por
%% como eran estos fenomenos
%% Me preguntan por los fenomenos en general del pais
%% Busco todas los fenomenos en un dia

respuesta(cuant,D,FCH,F,C,general,todo,POS) :-
  retract(tiempo(D,FCH,F,ORIG,FENCUANT,LUGAR,ZONA,POSIB)),
  respondefenom(cuant,D,FCH,falso,F,falso,FENCUANT,cierto,LUGAR,cierto,ZONA,cierto,
    ORIG,cierto,POSIB,cierto),
    respuesta(cuant,D,FCH,F,C,general,todo,POS),
    assert(tiempo(D,FCH,F,ORIG,FENCUANT,LUGAR,ZONA,POSIB)).

%% Me preguntan por un lugar concreto, cualquier zona
%% Busco si se dio en ese lugar y en todas las zonas

respuesta(cuant,D,FCH,F,C,L,todo,POS) :-
  retract(tiempo(D,FCH,F,ORIG,FENCUANT,L,ZONA,POSIB)),
  respondefenom(cuant,D,FCH,falso,F,cierto,FENCUANT,cierto,L,falso,ZONA,cierto,
    ORIG,cierto,POSIB,cierto),
  respuesta(cuant,D,FCH,F,C,L,todo,POS),
  assert(tiempo(D,FCH,F,ORIG,FENCUANT,L,ZONA,POSIB)).

%% Me preguntan por un lugar y una zona en concreto
%% Busco si se dio en ese lugar y esa zona

respuesta(cuant,D,FCH,F,C,L,Z,POS) :-
  retract(tiempo(D,FCH,F,ORIG,FENCUANT,L,Z,POSIB)),
  respondefenom(cuant,D,FCH,falso,F,cierto,FENCUANT,cierto,L,falso,Z,falso,ORIG,
    cierto,POSIB,cierto),
  respuesta(cuant,D,FCH,F,C,L,Z,POS),
  assert(tiempo(D,FCH,F,ORIG,FENCUANT,L,Z,POSIB)).

%% Me preguntan por un lugar y una zona en concreto
%% Busco si se dio en ese lugar y toda la zona

respuesta(cuant,D,FCH,F,C,L,Z,POS) :-
  retract(tiempo(D,FCH,F,ORIG,FENCUANT,L,todo,POSIB)),
  respondefenom(cuant,D,FCH,falso,F,cierto,FENCUANT,cierto,L,falso,Z,falso,ORIG,
    cierto,POSIB,cierto),
  respuesta(cuant,D,FCH,F,C,L,Z,POS),
  assert(tiempo(D,FCH,F,ORIG,FENCUANT,L,todo,POSIB)).

%% Me preguntan por un lugar
%% busco si se dio en toda la peninsula

respuesta(cuant,D,FCH,F,L,todo,POS) :-
  retract(tiempo(D,FCH,F,ORIG,FENCUANT,general,ZONA,POSIB)),
  respondefenom(cuant,D,FCH,falso,F,cierto,FENCUANT,cierto,L,falso,Z,falso,
    ORIG,cierto,POSIB,cierto),
  respuesta(cuant,D,FCH,F,C,L,todo,POS),
  assert(tiempo(D,FCH,F,ORIG,FENCUANT,general,ZONA,POSIB)).

%% Fin cuantificadores en general
%% Respuesta dias
%% Nos estan preguntando cuando se dio un fenomeno en un sitio
respuesta(cuan,D,FCH,F,C,L,todo,POS) :-
  retract(tiempo(DIA,FECHA,F,ORIG,FENCUANT,L,ZONA,POSIB)),
  respondefenom(cuan,DIA,FECHA,cierto,F,cierto,FENCUANT,cierto,L,falso,ZONA,
    cierto,ORIG,cierto,POSIB,cierto),
  respuesta(cuan,D,FCH,F,C,L,todo,POS),
  assert(tiempo(DIA,FECHA,F,ORIG,FENCUANT,L,ZONA,POSIB)).

%% Nos estan preguntando cuando se dio un fenomeno en un sitio, en una zona
respuesta(cuan,D,FCH,F,C,L,Z,POS) :-
  retract(tiempo(DIA,ECHA,F,ORIG,FENCUANT,L,Z,POSIB)),
  respondefenom(cuan,DIA,FECHA,cierto,F,cierto,FENCUANT,cierto,L,falso,Z,falso,
    ORIG,cierto,POSIB,cierto),
  respuesta(cuan,D,FCH,F,C,L,Z,POS),
  assert(tiempo(DIA,FECHA,F,ORIG,FENCUANT,L,Z,POSIB)).

%% Fin respuesta dias
%% Respuesta lugares

%% Nos estan preguntando donde se dio un fenomeno
respuesta(don,D,FCH,F,C,L,Z,POS) :-
  retract(tiempo(D,FCH,F,ORIG,FENCUANT,LUGAR,ZONA,POSIB)),
  respondefenom(don,D,FCH,cierto,F,cierto,FENCUANT,cierto,LUGAR,cierto,ZONA,
    cierto,ORIG,cierto,POSIB,cierto),
  respuesta(don,D,FCH,F,C,L,Z,POS),
  assert(tiempo(D,FCH,F,ORIG,FENCUANT,LUGAR,ZONA,POSIB)).

%% Fin respuesta lugares
%% Si no se puede encontrar un fenomeno en un sitio, se busca en un sitio padre

%% respuesta(fen,D,FCH,F,C,L,Z,POS) :-
%%  padre(L2,L,Z2),
%%  respuesta(fen,D,FCH,F,C,L2,Z2,POS).


%% respuesta(fen,D,FCH,F,C,L,Z,POS) :-
%%  padre(L2,L,22).
%% respuesta(cuant,D,FCH,F,C,L2,Z2,POS).

%% respuesta(fen,D,FCH,F,C,L,Z,POS) :-
%% padre(L2,L,Z2),
%% respuesta(temp,D,FCH,F,C,L2,Z2,POS).

%% Respuesta para cuando no queda información
respuesta(T,D,FCH,F,C,L,Z,POS) :- write('No queda mas informacion'),keyb(K,_).
%% %% asi obligamos a backtracking
