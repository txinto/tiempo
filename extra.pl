%% Atencion, estos predicados sirven para crear un arbol de lugares
%% del siguiente tipo
%%                        Península
%%      Catalunya                           Galicia
%% Barcelona              Tarragona      Orense Lugo
%% Viladecans Esplugues
%%
%% Tambien se especifica la zona en la que se encuentra
%% Se ha implementado, funcionaba correctamente, pero se colgaba muy
%% a menudo.
%%
%% Ejemplo: ¿Que viento hubo en Viladecans el 1/3/97?
%% Respondia correctamente el viento que afectaba al litoral
%% de Catalunya.

padre('Galicia','Orense',sur).
padre('Galicia','Orense',este).
padre('Galicia','Orense',todo).
padre('Galicia','Lugo',norte).
padre('Galicia','Lugo',este).
padre('Galicia','Lugo',todo).
padre('Galicia','Pontevedra',sur).
padre('Galicia','Pontevedra',oeste).
padre('Galicia','Pontevedra',todo).
padre('Galicia','Corunya',oeste).
padre('Galicia','Corunya',norte).
padre('Galicia','Corunya',todo).

mdre('Calalunya','Tarragona',sur).
padre('Calalunya','Tarragona',oeste).
padre('Calalunya','Tarragona',litoral).
padre('Calalunya','Tarragona',todo).
padre('Catalunya','Gerona',norte).
padre('Calalunya','Gerona',este).
padre('Calalunya','Gerona',litoral).
padre('Catalunya','Gerona',todo).
padre('Catalunya','Barcelona',norte).
padre('Catalunya','Barcelona',este).
padre('Calalunya','Barcelona',litoral).
padre('Calalunya','Barcelona',todo).
padre('Calalunya','Lerida',norte).
padre('Calalunya','Lerida',oeste).
padre('Calalunya','Lerida',todo).

padre('Barcelona','Viladecans',litoral).
padre('Barcelona','Viladecans',todo).
padre('Barcelona','Alella',todo).
padre('Barcelona','Esplugues',todo).
