%% Obtiene una palabra
obtiene_palabra(T) :-
  write($Frase --> $), read(T).

% Dada una palabra, y el carácter que la sigue, leer el resto de la frase
restofrase(Fichero,P,_,[]) :-
  ultimapalabra(P), !.

restofrase(Fichem,P,C,[P1|Ps]) :-
  leepalabra(Fichero,C,P1,C1),restofrase(Fichero,P1,C1,Ps).

% leer una sola palabra, dado un carácter inicial, recordando el carácter
% que venía detrás de la palabra
leepalabra(Fichero,C,P,C1) :-
  caracter_unico(C),!,name(P,[C]),get0(Fichero,C1).

leepalabra(Fichero,C,P,C2) :-
  en_palabra(C,NuevoC),!,get0(Fichero,C1),
  restopalabra(Fichero,C1,Cs,C2),name(P,[NuevoC|Cs]).

leepalabra(Fichero.C,P,C2) :-
  get0(Fichero,C1),leepalabra(Fichero,C1,P,C2).

restopalabra(Fichero,C,[NuevoC|Cs],C2) :-
  en_palabra(C,NuevoC),!,get0(Fichero,C1),
  restopalabra(Fichero,C1,Cs,C2).

restopalabra(_,C,[],C).

% los siguientes caracteres forman palabra por si mismos

caracter_unico(44). /* , */
caracter_unico(59). /* ; */
caracter_unico(58). /* : */
caracter_unico(63). /* ? */
caracter_unico(33). /* ! */
caracter_unico(46). /* . */
caracter_unico(13). /* retorno de carro */

% los siguientes caracteres pueden aparecer dentro de una palabra
% la segunda cláusula de en_palabra convierte mayúsculas a minúsculas

en_palabra(C,C) :- C>96, C<123. /* ab ... z */
en_palabra(C,L) :- C>64, C<91, L is C+32. /* AB ... Z */
en_palabra(C,C) :- C>47, C<58. /* 1 2 ... 9 */
en_palabra(39,39). /* '' */
en_palabra(45,45). /* - */

%% las siguientes palabras terminan una frase
ultimapalabra('.').
ultimapalabra('!').
ultimapalabra('?').
ultimapalabra(13).

leer_frase(Y,Fichero) :- write('> '),
  read_line(Fichero,Z1),anayade_punto(Z1,Z2),list_text(Z,Z2),
  convierte_a_lista(Z,Y).

leer_frasejp(Y,Fichero) :- write('> '),
  read_string(Fichero,5000,Z1),
  anayade_puntojp(Z1,Z2),
  list_text(Z,Z2),
  convierte_a_lista(Z,Y).

convierte_a_lista(Zin, [Pal1Atom|X]) :-
  saltarblancos(Zin,Zin1),
  separa_comas(Zin1,Zin2),
  primera_palabra(Zin2,Pal1 ,Zout1),
  name(Pal1Atom,Pal1),
  resto_frase(Zout1,X).

% Inserta un espacio delante de cada coma
separa_comas([Car|Zin],Zout) :- Car=47,
  separa_comas(Zin,Zin2),
  concatena([32,47,32],Zin2,Zout).

separa_comas([Car|Zin],Zout) :- Car=44,
  separa_comas(Zin,Zin2),
  concatena([32,44,32],Zin2,Zout).

separa_comas([Car|Zin],Zout) :- Car=46,
  separa_comas(Zin,Zin2),
  concatena([32,46,32],Zin2,Zout).

separa_comas([Car|Zin],Zout) :- Car=63,
  separa_comas(Zin,Zin2),
  concatena([32,63,32],Zin2,Zout).

separa_comas([Car|Zin],Zout) :- Car=168,
  separa_comas(Zin,Zin2),
  concatena([168,32].Zin2,Zout).

separa_comas([Car|Zin],Zout) :- finlinea(Car),
  separa_comas(Zin,Zin2),
  concatena([32],Zin2,Zout).

separa_comas([Car],[Car]) :- punto(Car),!.
separa_comas([Car],[Car]) :- intercerrar(Car),!.
separa_comas([Car],[Car]) :- finlinea(Car),!.
separa_comas([Cad|Zin],[Car|Zout]) :- Car\=44,
  separa_comas(Zin,Zout).

% Concatenacion de dos listas
concatena([],L,L).
concatena([X|L1],L2,[X|L3]) :- concatena(L1,L2,L3).

contraccion(del, [de,el]).
contraccion(al,[a,el]).

descontraer([],[]).
descontraer([X|Y],R) :- contraccion(X,LX), descontraer(Y,RR), concatena(LX,RR,R).
descontraer([X|Y],[X|R]) :- not(contraccion(X,_)), descontraer(Y,R).

anayade_punto(Z1,Z2) :- string_length(Z1,Long),
  ifthenelse(Long==0, concat(Z1,$Frasevacia.$,Z3),concat(Z1,$$,Z3)),
  string_length(Z3, Long),
  dec(Long,L),
  nth_char(L,23,C),
  ifthenelse((C\==46,
    concat(Z3,$.$,Z2),concat(Z3,$$,Z2)).

anayade_puntojp(Z1,Z2) :- string_length(Z1, Long),
  ifthenelse(Long==0, concat(Z1,$Frase vacía.$,Z3),concat(Z1,$$,Z3)),
  string_length(Z3,Long),
  dec(Long,L),
  nth_char(L,Z3,C),
  ifthenelse((C\==46),
    concat(Z3,$.$,Z2),concat(Z3,$$.Z2)).

saltarblancos([Caracter|Zin],Zout) :- Caracter=32, saltarblancos(Zin,Zout).
%%saltarblancos([Caracter|Zin],[Caracter|Zin]) :- Caracter\=32,!.

saltarblancos([Caracter|Zin],Zout) :- Caracter=10. saltaxblanoos(szout).
saltarblancos([Caracter|Zin],Zout) :- Caracter= 1 3. saltarblancos(Zin,lout).
saltarblancos([Caracter|Zin],[Caracter|Zin]) :- Caracter\=32,
  Caracter\=10,
  Caracter\=13.!.

primera_palabra([Car],[],[Car]) :- punto(Car),!.
primera_palabra([Car|Zin],[],Zin) :- espacio(Car),!.
primera_palabra([Car|Zin],[Car|X],Zout) :- primera_palabra(Zin,X,Zout).

resto_frase([Car],[P]) :- punto(Car),name(P,[46]),!.

resto_frase(Zin,[PalAtom|X]) :- saltarblancos(Zin,Zin1),
  primera_palabra(Zin1,Pal,Zout1),
  name(PalAtom,Pal),
  resto_frase(Zout1,X).

espacio(32).
punto(46).
finlinea(10).
finlinea(13).
intercerrar(63).
coma(44).
