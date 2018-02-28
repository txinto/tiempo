:- public main/0,restart/0.

%%% Nada mas comenzar cargamos el fichero de predicados extra
main :-
  consult('texto.pl'),
  consult('respues.pl'),
  mainaux.

%%% Llamamos a la presentación y ejecutamos la opción que el usuario haya escogido
mainaux :- presentacion(OPCION_1_1),
  ejecutar(OPCION_1_1).

%% Redirigimos funciones del prolog antiguo al SWIPL, o hacemos stub
cls :- write('\e[H\e[2J').
define_window(A1,A2,A3,A4,A5).
current_window(A1,A2).
delete_window(A1).

keyb(A1,A2) :- get_single_char(A1).

%%% Borra todas las ocurrencias de un aserto
retract_all(X) :- retract(X), fail.
retract_all(X) :- retract((X :- Y)),fail.
retract_all(_).

%% Leer juego de pruebas
ejecutar(OPCION1) :- OPCION1=48, cls,
  define_window(ventana,'',(4,4),(21,76),(30,27)),
  current_window(_,ventana),
  leertextojp(L),
  descontraer(L,LR),
  analizarjp(LR),nl,
  delete_window(ventana),
  mainaux.

%% Lee una predicción por el teclado
ejecutar(OPCION1) :- OPCION1=49, cls,
  define_window(ventana,'',(4,4),(21,76),(30,27)),
  current_window(_,ventana),
  leertexto(L),
  descontraer(L,LR),
  analizar(LR),nl,
  delete_window(ventana),
  mainaux.

%% Lee una pregunta por el teclado
ejecutar(OPCION1) :- segundaopcion(OPCION1),
  define_window(ventana,'',(4,4),(21,76),(30,27)),
  current_window(_,ventana),
  leerpregunta(PREG),
  descontraer(PREG,PREGR),
  analizarpreg(PREGR),nl,
  delete_window(ventana),
  mainaux.

%% Salimos del programa
ejecutar(OPCION1) :- salir(OPCION1).

%% Opción equivoacada, volvemos al menú
ejecutar(OPCION1) :- pordefecto(OPCION1), mainaux.

%% Función auxiliar, escribe una línea completa con retorno
writeln(X) :- write(X), nl.

%% Muestra por pantalla una presentación y toma la opción
presentacion(OPCION1) :-
  cls,
  define_window(ventana,'',(4,4),(21,76),(30,27)),
  current_window(_,ventana),
  write('        Predicciones meteorológicas                          '), nl,
  write('                                                             '), nl,
  write('Este programa permite introducir textos meteorológicos en la '), nl,
  write('base de datos climatológica y realizar preguntas referentes a'), nl,
  write('esos datos.                                                  '), nl,
  write('                                                             '), nl,
  write('                                                             '), nl,
  write('   0. Leer juego de pruebas                                  '), nl,
  write('   1. Insertar textos                                        '), nl,
  write('   2. Realizar preguntas                                     '), nl,
  write('   3. Salir                                                  '), nl,
  keyb(OPCION1,_),
  delete_window(ventana).

% Si escogemos Salir
salir(OPCION1) :- OPCION1=51, cls,
  define_window(ventana,'',(4,4),(21,76),(30,27)),
  current_window(_,ventana), writeln('Gracias por utilizar nuestro programa'), keyb(X,_),
  delete_window(ventana).

% Si escogemos la segunda opcion
segundaopcion(OPCION1) :- OPCION1=50, cls,
  define_window(ventana,'',(4,4),(21,76),(30,27)),
  current_window(_,ventana),
  delete_window(ventana).

% Opción no valida
pordefecto(OPCION1) :- OPCION1 \=49,  OPCION1 \=50,  OPCION1 \=51, cls,
  define_window(ventana,'',(4,4),(21,76),(30,27)),
  current_window(_,ventana), writeln('Opción no válida, vuelva a intentarlo'), keyb(X,_),
  delete_window(ventana).

%% En caso de que las cosas vayan mal
restart :- cls, write('Programa abortado durante ejecución'),keyb(K,_), break.

%% Leemos el texto por teclado
leertexto(L) :- write('Introduzca la predicción -'), leer_frase(L,0).

%% Leemos el texto del fichero de predicciones
leertextojp(L) :- p_open(X,'jpt.txt',r),
  write('Cargamos de fichero -'),
  leer_frasejp(L,X),
  close(X).

%% Hemos fallado al intentar abrir el fichero
leertextojp(L) :- writeln('ups!!! Parece que el fichero no está!'), keyb(K,_).

%% Leemos el texto de una pregunta
leerpregunta(LP) :-
  writeln('Pregunta -'), leer_frase(LP,0).

%% Analizamos la prediccion entrada por teclado
analizar(LR) :- analisis(F,L,R,[]),
  !.

analizar(LR) :- writeln('No entiendo la frase.  Pulsa una tecla.'), keyb(K,_).

%% Para analizar la frase, hay que llamar a la gramatica de predicado.
analisis(P,X,[]) :- predicjp(P,X,[]).

%% Analizamos las preguntas
analizarpreg(APR) :- analisispreg(P,APR,[]),!.

%% Cuando falla el analisis de preguntas (hemos acabado), salvamos el contenido
%% de tiempo2 en tiempo
analizarpreg(APR) :- volcar.

%% Para analizar las preguntas, invocamos una gramatica de preguntas
analisispreg(P,APR,[]) :- pregunta(T,F,C,L,Z,D,FCH,POS,APR,[]),
  tratarpreg(T,F,L,Z,D,FCH,POS).

%% Una vez resuelta la pregunta, la tratamos
tratarpreg(T,F,L,Z,D,FCH,POS) :- respuesta(T,D,FCH,F,C,L,Z,POS).

%% Esto vuelca los predicados tiempo2 sobre tiempo
volcar :- retract(tiempo2(DFCH,F,ORIG,C,L,Z,P)),
  assert(tiempo(DFCH,F,ORIG,C,L,Z,P)),
  volcar.

volcar :- true.

%
%              LÉXICO
%

fech('Lunes','Lunes').
fech('Martes','Martes').
fech('Miercoles','Miercoles').
fech('Jueves','Jueves').
fech('Viernes','Viernes').
fech('Sabado','Sabado').
fech('Domingo','Domingo').
fech(lunes,'Lunes').
fech(martes,'Martes').
fech(miercoles,'Miercoles').
fech(jueves,'Jueves').
fech(viernes,'Viernes').
fech(sabado,'Sabado').
fech(domingo,'Domingo').
fech('Lunes','Lunes').
fech('Martes','Martes').
fech('Miercoles','Miercoles').
fech('Jueves','Jueves').
fech('Viernes','Viernes').
fech('Sabado','Sabado').
fech('Domingo','Domingo').

lug('Orense','Orense',gind,sng).
lug('Corunya','Corunya',fem,sng).
lug('Coruña','Corunya',fem,sng).
lug('Lugo','Lugo',gind,sng).
lug('Pontevedra','Pontevedra',gind,sng).

lug('Barcelona','Barcelona',gind,sng).
lug('Tarragona','Tarragona',gind,sng).
lug('Lerida','Lerida',gind,sng).
lug('Lleida','Lerida',gind,sng).
lug('Gerona','Gerona',gind,sng).
lug('Girona','Gerona',gind,sng).

lug('Viladecans','Viladecans',gind,sng).
lug('Alella','Alella',gind,sng).
lug('Espluges','Espluges',gind,sng).

lug('Canarias','Canarias',fem,pl).
lug('Asturias','Asturias',gind,sng).
lug('Murcia','Murcia',gind,sng).
lug('Extremadura','Extremadura',gind,sng).
lug('Malaga','Malaga',gind,sng).
lug('Madrid','Madrid',gind,sng).
lug('Sevilla','Sevilla',gind,sng).
lug('Galicia','Galicia',gind,sng).
lug('Castilla','Castilla',gind,sng).
lug('Leon','Leon',gind,sng).
lug('Rioja','Rioja',gind,sng).
lug('Navarra','Navarra',gind,sng).
lug('Aragon','Aragon',gind,sng).
lug('Catalunya','Catalunya',gind,sng).
lug('Cataluña','Catalunya',gind,sng).
lug('Cantabrico','Cantabrico',gind,sng).
lug('Andalucia','Andalucia',gind,sng).
lug('Baleares','Baleares',gind,sng).
lug('Pirineos','Pirineos',msc,pl).
lug('Estrecho','Estrecho',msc,sng).
lug('Cantabria','Cantabria',gind,sng).
lug('Peninsula','Peninsula',fem,sng).
lug('Espanya','Espanya',fem,sng).
lug('España','Espanya',fem,sng).

zon(litoral,litoral,msc,sng).
zon(norte,norte,msc,sng).
zon(sur,sur,msc,sng).
zon(este,este,msc,sng).
zon(oeste,oeste,msc,sng).
zon(nordeste,nordeste,msc,sng).
zon(sureste,sureste,msc,sng).
zon(noroeste,noroeste,msc,sng).
zon(suroeste,suroeste,msc,sng).
zon(area,todo,msc,sng).
zon(zona,todo,fem,sng).
zon(comunidades,todo,fem,pl).

orig(norte,norte,msc,sng).
orig(sur,sur,msc,sng).
orig(este,este,msc,sng).
orig(oeste,oeste,msc,sng).
orig(nordeste,nordeste,msc,sng).
orig(sureste,sureste,msc,sng).
orig(noroeste,noroeste,msc,sng).
orig(suroeste,suroeste,msc,sng).

cuant(ocasional,ocasional,msc,sng).
cuant(ocasional,ocasional,fem,sng).
cuant(abundante,abundante,msc,sng).
cuant(abundante,abundante,fem,sng).
cuant(moderado,moderado,msc,sng).
cuant(moderada,moderada,fem,sng).
cuant(frecuente,frecuente,msc,sng).
cuant(frecuente,frecuente,fem,sng).
cuant(fuerte,fuerte,msc,sng).
cuant(fuerte,fuerte,fem,sng).
cuant(debil,debil,msc,sng).
cuant(debil,debil,fem,sng).
cuant(ocasionales,ocasional,msc,pl).
cuant(ocasionales,ocasional,fem,pl).
cuant(abundantes,abundante,msc,pl).
cuant(abundantes,abundante,fem,pl).
cuant(moderados,moderado,msc,pl).
cuant(moderadas,moderada,fem,pl).
cuant(frecuentes,frecuente,msc,pl).
cuant(frecuentes,frecuente,fem,pl).
cuant(fuertes,fuerte,msc,pl).
cuant(fuertes,fuerte,fem,pl).
cuant(debiles,debil,msc,pl).
cuant(debiles,debil,fem,pl).

fenom('Viento',viento,msc,sng).
fenom('viento',viento,msc,sng).
fenom('Vientos',viento,msc,sng).
fenom('vientos',viento,msc,sng).
fenom('Nuboso',nubes,msc,sng).
fenom('Nubes',nubes,fem,pl).
fenom(nubes,nubes,fem,pl).
fenom('Claros',claros,msc,pl).
fenom(claros,claros,msc,pl).
fenom('Nieblas',niebla,fem,pl).
fenom(nieblas,niebla,fem,pl).
fenom('Niebla',niebla,fem,sng).
fenom(niebla,niebla,fem,sng).
fenom('Brumas',bruma,fem,pl).
fenom(brumas,bruma,fem,pl).
fenom('Bruma',bruma,fem,sng).
fenom(bruma,bruma,fem,sng).
fenom('Lluvias',lluvia,fem,pl).
fenom(lluvias,lluvia,fem,pl).
fenom('Lluvia',lluvia,fem,sng).
fenom(lluvia,lluvia,fem,sng).
fenom('Lloviznas',lluvia,fem,pl).
fenom(lloviznas,lluvia,fem,pl).
fenom('Llovizna',lluvia,fem,sng).
fenom(llovizna,lluvia,fem,sng).
fenom('Chubascos',lluvia,msc,pl).
fenom(chubascos,lluvia,msc,pl).
fenom(precipitaciones,lluvia,fem,pl).
fenom('Precipitaciones',lluvia,fem,pl).
fenom('Sol',sol,msc,sng).
fenom('Soleado',sol,msc,sng).
fenom(sol,sol,msc,sng).
fenom(soleado,sol,msc,sng).
fenom('Nieve',nieve,fem,sng).
fenom(nieve,nieve,fem,sng).
fenom('Nevadas',nieve,fem,pl).
fenom(nevadas,nieve,fem,pl).

adj(catalan,'Catalunya'.msc,sug,lugarb).
adj(andaluz,'Andalucia'.msc,sng,lugarb).
adj(peninsular,'Peninsula',msc,sng,lugarb).
adj(gallego,'Galicia',msc,sng,lugarb).
adj(mediterraneo,'Mediterraneo',msc,sng,lugarb).

adj(occidental,occidental,gind,sng,lugar).
adj(oriental,oriental,gind,sng,lugar).
adj(septentrional,septentrional,gind,sng,lugar).
adj(meridional,meridional,gind,sng,lugar).
adj(ligera,ligera,fem,sng,calif).
adj(ligeras,ligera,fem,pl,calif).
adj(ligeros,ligero,msc,pl,calif).
adj(ligero,ligero,msc,sng,calif).
adj(moderada,moderada,fem,sng,calif).
adj(moderadas,moderada,fem,pl,calif).
adj(moderado,moderado,msc,sng,calif).
adj(moderados,moderado,msc,pl,calif).
adi(notable,notable,fem,sng,calif).
adj(notables,notable,fem,pl,calif).
adj(notables,notable,msc,pl,calif).
adj(notable,notable,msc,sng,calif).

adj('Poco',poco,msc,sng,calif).
adj(poco,poco,msc,sng,calif).
adj('Pocas',poca,fem,pl,calif).
adj('Poca',poca,fem,sng,calif).
adj(pocas,poca,fem,pl,calif).
adj(poca,poca,fem,sng,calif).
adj('Muy',mucho,msc,sng,calif).
adj(muy,mucho,msc,sng,calif).
adj('Muy',mucho,fem,sng,calif).
adj(muy,mucho,fem,sng,calif).
adj('Bastante',bastante,msc,sng,calif).
adj(bastante,bastante,msc,sng,calif).
adj('Bastante',bastante,fem,sng,calif).
adj(bastante,bastante,fem,sng,calif).

camb(ascenso,ascenso).
camb(descenso,descenso).

sust(cambios,cambios).
sust(variaciones,cambios).

temp('Temperaturas').
temp('Temperatura').
temp(temperaturas).
temp(temperatura).

prep(en,plug).
prep(cabe,pindef).
prep(a,a).
prep(sin,sin).
prep(con,con).
prep(de,de).
prep('En',plug).
prep('Cabe',pindef).
prep('A',a).
prep('in'.sin).
prep('Con',con).
prep('De',de).

articul(el,msc,sng).
articul(la,fem,sng).
articul(los,msc,pl).
articul(las,fem,pl).
articul('El',msc,sng).
articul('La',fem,sng).
articul('Los',msc,pl).
articul('Las',fem,pl).

prob('Probabilidad').
prob('Posibilidad').
prob(probabilidad).
prob(posibilidad).
prob('Predominio').
prob(predominio).

conj(y).
conj(e).
punt(.).
dospunt(':').

verb(era,vnorm,ser,indef).
verb(fue,vnorm, ser,indef).
verb(hizo,vnorm,hacer,indef).
verb(estaba,vnorm,estar,indef).
verb(hubo,vnorm,haber,indef).
verb(llovio,vfen,llcver,lluvia).
verb(nevo,vfen,nevar,nieve).
verb(granizo,vfen,granizar,granizo).
verb(nublo,vfen,nublar,nubes).
verb('Hizo',vnorm,hacer,indef).
verb('Estaba',vnorm,estar,indef).
verb('Hubo',vnorm,haber,indef).
verb('Llovio',vfen,llover,lluvia).
verb('Nevo',vfen,nevar,nieve).
verb('Granizo',vfen,granizar,granizo).

parentes_ab('(').
parentes_ce(')').
interrab('¿').
interrce('?').
qu(que).
qu('Que').
com(como).
com('Como').
cuand('Cuando').
cuand(cuando).
dond(donde).
dond('Donde').

todofen(tiempo).

ex_obj_f(cielo,cielo,sng).
ex_obj_f(cielos,cielo,pl).


%
% GRAMATICA
%

predic(X) --> l_pred_diaria(X).
predicjp(X) --> l_pred_diaria(x).

%% Preguntas tipo ¿Que temperatura hizo en Barcelona el lunes 1/1/1?
pregunta(cuant,temperatura,C,L,Z,D,FCH,POS) -->
  interabrir,que,expr_temp,verbo(vnorm,hacer,_),expr_lugar(L,Z),articulo(msc,sng),fecha(D),fechanames(FCH),intercerrar,punto.

pregunta(cuant,temperatura,C,L,Z,D,FCH,POS) -->
  interabrir,que,expr_temp,verbo(vnorm,hacer,_),articulo(msc,sng),fecha(D),fechanames(FCH),expr_lugar(L,Z),intercerrar,punto.

pregunta(cuant,temperatura,C,L,Z,D,FCH,POS) -->
  interabrir,que,expr_temp,verbo(vnorm,haber,_),expr_lugar(L,Z),articulo(msc,sng),fecha(D),fechanames(FCH),intercerrar,punto.

pregunta(cuant,temperatura,C,L,Z,D,FCH,POS) -->
  interabrir,que,expr_temp,verbo(vnorm,haber,_),articulo(msc,sng),fecha(D),fechanames(FCH),expr_lugar(L,Z),intercerrar,punto.


pregunta(cuant,temperatura,C,L,Z,D,FCH,POS) -->
  interabrir,que,expr_temp,verbo(vnorm,hacer,_),expr_lugar(L,Z),articulo(msc.sng),fechnames(FCH), intercerrar, punto.

pregunta(cuant,temperatura,C,L,Z,D,FCH,POS) -->
  interabrir,que,expr_temp,verbo(vnorm,hacer,_),articulo(msc,sng),fechnames(FCH),expr_lugar(L,Z), intercerrar, punto.

pregunta(cuant,temperatura,C,L,Z,D,FCH,POS) -->
  interabrir,que,expr_temp,verbo(vnorm,haber,_),expr_lugar(L,Z),articulo(msc.sng),fechnames(FCH), intercerrar, punto.

pregunta(cuant,temperatura,C,L,Z,D,FCH,POS) -->
  interabrir,que,expr_temp,verbo(vnorm,haber,_),articulo(msc,sng),fechnames(FCH),expr_lugar(L,Z), intercerrar, punto.


%% Preguntas tipo ¿Que tiempo hizo en Barcelona el lunes 1/1/1?

pregunta(fen,F,C,L,Z,D,FCH,POS) -->
interabrir,que,todofenom,verbo(vnorm,hacer,_),expr_lugar(L,Z),articulo(msc,sng),fecha(D),fechames(F,CH), intercerrar,punto.

pregunta(fen,F,C,L,Z,D,FCH,POS) -->
interabrir,que,todofenom,verbo(vnorm,hacer,_),articulo(msc,sng),fecha(D),fechames(F,CH),expr_lugar(L,Z),intercerrar,punto.

pregunta(fen,F,C,L,Z,D,FCH,POS) -->
interabrir,que,todofenom,verbo(vnorm,haber,_),expr_lugar(L,Z),articulo(msc,sng),fecha(D),fechames(F,CH), intercerrar,punto.

pregunta(fen,F,C,L,Z,D,FCH,POS) -->
interabrir,que,todofenom,verbo(vnorm,haber,_),articulo(msc,sng),fecha(D),fechames(F,CH),expr_lugar(L,Z),intercerrar,punto.

pregunta(fen,F,C,L,Z,D,FCH,POS) -->
interabrir,que,todofenom,verbo(vnorm,hacer,_),expr_lugar(L,Z),articulo(msc,sng),fechames(FCH),intercerrar,punto.

pregunta(fen,F,C,L,Z,D,FCH,POS) -->
interabrir,que,todofenom,verbo(vnorm,hacer,_),articulo(msc,sng),fechames(FCH),expr_lugar(L,Z),intercerrar,punto.

pregunta(fen,F,C,L,Z,D,FCH,POS) -->
interabrir,que,todofenom,verbo(vnorm,haber,_),expr_lugar(L,Z),articulo(msc,sng),fechames(FCH),intercerrar,punto.

pregunta(fen,F,C,L,Z,D,FCH,POS) -->
interabrir,que,todofenom,verbo(vnorm,haber,_),articulo(msc,sng),fechames(FCH),expr_lugar(L,Z),intercerrar,punto.

%% Preguntas tipo ¿Como estaba el cielo el lunes 1/1/1 en Barcelona ?
pregunta(fen,cielo,C,L,Z,D,FCH,POS) -->
interabrir,como,verbo(vnorm,estar,_),expr_obj_fenom(OBJ,N),expr_lugar(L,Z),articulo(msc,sng),fechames(FCH), intecerrar, punto.

pregunta(fen,cielo,C,L,Z,D,FCH,POS) -->
interabrir,como,verbo(vnorm,estar,_),expr_obj_fenom(OBJ,N),articulo(msc,sng),fechames(FCH),expr_lugar(L,Z), intecerrar, punto.

pregunta(fen,cielo,C,L,Z,D,FCH,POS) -->
interabrir,como,verbo(vnorm,estar,_),expr_obj_fenom(OBJ,N),expr_lugar(L,Z),articulo(msc,sng),fecha(D), fechames(FCH), intecerrar, punto.

pregunta(fen,cielo,C,L,Z,D,FCH,POS) -->
interabrir,como,verbo(vnorm,estar,_),expr_obj_fenom(OBJ,N),articulo(msc,sng),fecha(D),fechames(FCH),expr_lugar(L,Z), intecerrar, punto.

%% Preguntas tipo ¿Como llovio el lunes 1/1/1 en Barcelona ?

pregunta(cuant,F,C,L,Z,D,FCH,POS) -->
interabrir,como,verbo(vfen,_,F),expr_lugar(L,Z),articulo(msc,sug),fecha(D),fechames(FCH),intercerrar,punto.
pregunta(cuant,F,C,L,Z,D,FCH,POS) -->
interabrir,como,verbo(vfen,_,F),articulo(msc,sug),fecha(D),fechames(FCH),expr_lugar(L,Z),intercerrar,punto.
pregunta(cuant,F,C,L,Z,D,FCH,POS) -->
interabrir,como,verbo(vfen,_,F),expr_lugar(L,Z),articulo(msc,sug),fechames(FCH),intercerrar,punto.
pregunta(cuant,F,C,L,Z,D,FCH,POS) -->
interabrir,como,verbo(vfen,_,F),articulo(msc,sug),fechames(FCH),expr_lugar(L,Z),intercerrar,punto.

%º o Preguntas tipo ¿Como era la lluvia del lunes 1/ l/ 1 en Barcelona ?
pregunta(cuant,F,C,L,Z,D,FCH,POS) -->
inteabrir,como,verbo(vnorm,ser,_),articulo(G,N),fenomeno(F,G,N),expr_lugar(L,Z),articulo(msc,sng),fecha(D),fechames(FCH),intercerrar,punto.
pregunta(cuant,F,C,L,Z,D,FCH,POS) -->
inteabrir,como,verbo(vnorm,ser,_),articulo(G,N),fenomeno(F,G,N),articulo(msc,sng),fecha(D),fechames(FCH),expr_lugar(L,Z),intercerrar,punto.
pregunta(cuant,F,C,L,Z,D,FCH,POS) -->
inteabrir,como,verbo(vnorm,ser,_),articulo(G,N),fenomeno(F,G,N),expr_lugar(L,Z),articulo(msc,sng),fechames(FCH),intercerrar,punto.
pregunta(cuant,F,C,L,Z,D,FCH,POS) -->
inteabrir,como,verbo(vnorm,ser,_),articulo(G,N),fenomeno(F,G,N),articulo(msc,sng),fechames(FCH),expr_lugar(L,Z),intercerrar,punto.

%% Preguntas tipo ¿Que lluvia hubo el lunes UU] en Barcelona ?
pregunta(cuant,F,C,L,Z,D,FCH,POS) -->
interabrir,que,fenomeno(F,_,_),verbo(vnorm,hacer,_),expr_lugar(L,Z),articulo(msc,sng),fecha(D),fechames(FCH),intercerrar,punto.
pregunta(cuant,F,C,L,Z,D,FCH,POS) -->
interabrir,que,fenomeno(F,_,_),verbo(vnorm,hacer,_),articulo(msc,sng),fecha(D),fechames(FCH),expr_lugar(L,Z),intercerrar,punto.
pregunta(cuant,F,C,L,Z,D,FCH,POS) -->
interabrir,que,fenomeno(F,_,_),verbo(vnorm,hacer,_),expr_lugar(L,Z),articulo(msc,sng),fechames(FCH),intercerrar,punto.
pregunta(cuant,F,C,L,Z,D,FCH,POS) -->
interabrir,que,fenomeno(F,_,_),verbo(vnorm,hacer,_),articulo(msc,sng),fechames(FCH),expr_lugar(L,Z),intercerrar,punto.
pregunta(cuant,F,C,L,Z,D,FCH,POS) -->
interabrir,que,fenomeno(F,_,_),verbo(vnorm,haber,_),expr_lugar(L,Z),articulo(msc,sng),fecha(D),fechames(FCH),intercerrar,punto.
pregunta(cuant,F,C,L,Z,D,FCH,POS) -->
interabrir,que,fenomeno(F,_,_),verbo(vnorm,haber,_),articulo(msc,sng),fecha(D),fechames(FCH),expr_lugar(L,Z),intercerrar,punto.
pregunta(cuant,F,C,L,Z,D,FCH,POS) -->
interabrir,que,fenomeno(F,_,_),verbo(vnorm,haber,_),expr_lugar(L,Z),articulo(msc,sng),fechames(FCH),intercerrar,punto.
pregunta(cuant,F,C,L,Z,D,FCH,POS) -->
interabrir,que,fenomeno(F,_,_),verbo(vnorm,haber,_),articulo(msc,sng),fechames(FCH),expr_lugar(L,Z),intercerrar,punto.

%% Preguntas tipo ¿Cuando llovio en Barcelona?
pregunta(cuant,F,C,L,Z,D,FCH,POS) -->
interabrir,cuando,verbo(vfen,_,VF),expr_lugar(L,Z),intercerrar,punto.

%% Preguntas tipo ¿Donde nevo el lunes 1/1/ ]?
pregunta(don,F,C,L,Z,D,FCH,POS) -->
interabrir,donde,verbo(vfen,_,VF),articulo(msc.sng),fecha(D),fechames(FCH),intercerrar,punto.
pregunta(don,F,C,L,Z,D,FCH,POS) -->
interabrir,donde,verbo(vfen,_,VF),articulo(msc.sng),fecha(D),fechames(FCH),intercerrar,punto.

expr_obj_fenom(OBJ,N) --> articulo(msc,N),expr_obj_fen(OBJ,N).

l_pred_diaria([X|Y]) --> pred_diaria(x),l_pred_diaria(Y).
l_pred_diaria(X) --> [].

pred_diaria(D) --> expr_fecha(D,FC),dospuntos,l_frases(D,FC).
expr_fecha(D,FC) --> dia,fechames(FC),fecha(D).

fecha(D) --> [W],{fech(W,D)}.

l_frases(D,FC) --> frase(D,FC),l_frases(D,FC).
l_frases(D,FC) --> [].

%º () Fenomeno, lista de cuanliñwdom y lugares
frase(D,FC) --> expr_fenomeno(F,O,C,G,N,POS),l_cuantif_lugar(D,FC,F,O,G,N,POS),['.'].
frase(D,FC) --> [''],['.'].

%% Adj fenomeno lista lugares
frase(D,FC) --> expr_fenomeno(F,O,C,G,N,POS),l_expr_lugar(D,FC,F,O,C,POS).['.'].

frase(D,FC) --> expr_temp,l_cuantif_lugart(D,FC),['.'].

%% Lista de cuantificadores y lugares
%% cuantif lugar y cuantif lugar
l_cuantif_lugar(D,FC,F,O,G,N,POS) --> expr_cuantif(C1,G,N),l_expr_lugar(D,FC,F,O,C1,POS),
  conjun,expr_cuantif(C2,G,N),l_expr_lugar(D,FC,F,O,C2,POS).
l_cuantif_lugar(D,FC,F,O,G,N,POS) --> expr_cuantif(C,G,N),l_expr_lugar(D,FC,F,O,C,POS).
l_cuantif_lugar(D,FC,F,O,G,N,POS) --> expr_cuantif(C,G,N),l_expr_lugar(D,FC,F,O,C,POS),
  ['.'],l_cuantif_lugar(D,FC,F,O,G,N,POS).

%%%%%%%%%%%%%%%%%%%% VAMOS POR AQUÍ %%%%%%%%%%%%%%%%%%%%%


l_cuantif_lugart(D,FC) --> expr_cuantift(C),{tratar(D,FC,temperatura,O,C,general,todo,cierta)}.
l_cuantif_lugart(D,FC) --> expr_cuantift(C),l_expr_lugar(D,FC,temperatura,O,C,cierta).
l_cuantif_lugart(D,FC) -->
  expr_cuantift(C),l_expr_lugar(D,FC,temperatura,O,C,cierta),[','],l_cuantif_lugart(D,FC).
l_cuantif_lugarl(D,FC) -->
  expr_cuantift(C1),l_expr_lugar(D,FC,temperatura,O,C1,cierta),conjun,expr_cuantif(C2),
  l_expr_lugar(D,FC,temperatura,O,C2,cierta).

%% Lista de lugares

l_expr_lugar(D,FC,F,O,C,POS) --> expr_lugar(L1,Z1),conjun, expr_lugar(L2,Z2),
  {tratar(D,FC,F,O,C,L1,Z1,POS)},{tratar(D,FC,F,O,C,L2,Z2,POS)}.
l_expr_lugar(D,FC,F,O,C,POS) --> expr_lugar(L,Z),['.'],l_expr_lugar(D,FC,F,O,C,POS),
  {tratar(D,FC,F,O,C,L,Z,POS)}.
l_expr_lugar(D,FC,F,O,C,POS) --> expr_lugar(L,Z),{tratar(D,FC,F,O,C,L,Z,POS)}.
l_expr_lugar(D.FC,F,O,C,POS) --> [].

tratar(D,FC,F,O,C,L,Z,POS) :-
  retractall(tiempo(D,FC,F,O,C,L,Z,POS)),
  assert(tiempo(D,FC,F,O,C,L,Z,POS)),
  write('.').

expr_lugar(L,Z) --> prepos(plug),lugar(L,_,_),adjet(Z,_,_,lugar). %% p-e- Andalucia ºccidental
expr_lugar(L,todo) --> prepos(plug),lugar(L,_,_).

expr_lugar(L,Z) --> prepos(plug),articulo(G,N),zona(Z,G,N),
  adjet(L,G,N,lugarb). %% el nordeste peninsular

expr_lugar(L,Z) --> prepos(plug),articulo(G,N),lugar(L,G,N),adjet(Z,_,_,lugar).

expr_lugar(L,todo) --> prepos(plug),articulo(G,N),lugar(L,G,N).

expr_lugar(L,Z) --> prepos(plug),articulo(G,N),zona(Z,G,N),prepos(de),lugar(L,_,_).

expr_lugar(L,Z) --> prepos(plug),articulo(G1,N1),zona(Z,G1,N1),prepos(de),articulo(G2,N2),lugar(L,_,_).

expr_cuantif(C,G,N)--> cuantif(C1,G,N),prepos(a),cuantif(C2,G,N),{concat([C1,' a ',C2],C)}.
expr_cuantif(C,G,N) --> cuantif(C,G,N).

expr_cuantift(C) --> prepos(sin),sustant(C1),{concat(['sin ',C1],C)}.

expr_cuantif(c)--> prepos(plug),adjet(A,G,N,calif),cambio(C1),{concat([A,' ',C1],C)}.

expr_cuantift(C) --> prepos(plug),adjet(A1,G,N,calif),
  prepos(a),adjet(A2,G,N,calif),cambio(C1),{concat([A1,' a ',A2,' ',C1],C)}.

%% Fenomenos tipo brumas y nieblas, nubes y claros

expr_fenomeno(F,nulo,normal,G1,N1,cierta) -->
fenomeno(F1,G1,N1),conjun,fenomeno(F2,G2,N2),{concat([F1,' y ',F2],F)}.

expr_fenomeno(F,nulo,normal,G,N,cierta) --> fenomeno(F,G,N).

expr_fenomeno(F,nulo,normal,G,N,probable)-->
  probab,prepos(de),fenomeno(F1,G1,N1),conjunfenomeno(F2,G2,N2),{concat([F1, ' y ',F2],F)}.

expr_fenomeno(F,nulo,normal,G,N,probable) --> probab,prepos(de),fenomeno(F,G,N).

expr_fenomeno(F,nulo,C,G,N,cierta) --> adjet(C,G,N,calif),fenomeno(F,G,N).

expr_fenomeno(viento,O,C,G,N,cierta) --> fenomeno(viento,G,N),prepos(de),
  articulo(G,N),origen(O1,G,N),{concat(['del ',O1],O)}.

expr_temp --> temperat.

di('Dia').
di(dia).
barra('/').
dia--> [W].{di(W)}.

fechames(F) -->[I1],{integer(I1),int_text(I1,W1)},[W2],{barra(W2)},
  [I3],(integer(I3),int_text(I3,W3)),[I4],{barra(W4)},
  [I5],{integer(I5),int_text(I5,W5)},
  {concat([W1,W2,W3,W4,W5],F)}.

lugar(L,G,N) --> [W],{lug(W,L,G,N)}.

cuantif(C,G,N) --> [W],{cuant(W,C,G,N)}.

fenomeno(F,G,N) --> [W],{fenom(W,F,G,N)}.

todofenom --> [W],{todofen(W)}.

temperat --> [W],{temp(W)}.

conjun --> [W],{conj(w)}.

adjet(A,G,N,T) --> [W],{adj(W,A,G,N,T)}.

probab --> [W],{prob(W)}.

sustant(S) --> [W],{sust(W,S)}.

cambio(C) --> [W],{camb(W,C)}.

punto --> [W],{punt(W)}.

despuntos-->[W],{dospunt(W)}.

puntazo-->[W],{punt(W)}.
prepos(T)-->[W],{prep(W,T)}.
articulo(G,N)-->[W],{articul(W,G,N)}.
zona(Z,G,N)-->[W],{zon(W,Z,G,N)}.
origen(O,G,N) --> [W],{orig(W,O,G,N)}.

interabrir --> [W],{interrab(W)}.
intercerrar --> [W],{interrce(W)}.

que --> [W],{qu(W)}.

cuando --> [W],{cuand(W)}.

donde --> [W],{dond(W)}.

como --> [W],{com(W)}.
verb(hizo,vnorm,hacer,indef).

verbo(T,I,F) --> [W],{verb(W,T,I,F)}.
parentesisabrir --> [W],{parentes_ab(W)}.
parentesiscerrar --> [W],{parentes_ce(W)}.

expr_obj_fen(OBJ,N) --> [W],{ex_obj_f(W,OBJ,N)}.
