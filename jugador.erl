%%%_______________________________________________________________________________
%%%
%%%	ARQUITECTURA SOFTWARE - PRACTICA GRUPAL
%%%	CURSO 2016-17 - GRUPO 1.1
%%%
%%%	SISTEMA DE JUEGO DEL SIETE Y MEDIO - JUGADOR
%%%
%%%	Autores:
%%%		-Chas Alvarez, Lucia <lucia.chas@udc.es>
%%%		-Corton de Blas, Diego <diego.cortonde.blas@udc.es>
%%%		-Costa Garcia, Sergio <sergio.costag@udc.es>
%%%		-Freire Paz, Adrian <adrian.freire.paz@udc.es>
%%%		-Toledano Franco, Sainza <sainza.toledano@udc.es>
%%%		-Vidal Garcia, Sara <sara.vidal@udc.es>
%%%_______________________________________________________________________________


-module(jugador).
-export([empezar/1, apostar/1, carta/1, plantarse/0, abandonar/0, on/0]). %API
-export([init/1, handle_info/3, terminate/3]).
-export([espera/2,jugada/2,apuesta/2]).

-define(JUGADOR, jugador).


%%%_______________________________________________________________________________
%%%__________________________INICIO Y FIN DEL JUGADOR_____________________________

%Generacion de la maquina de estados del jugador
on() ->
    gen_fsm:start_link({local, ?JUGADOR}, ?MODULE, 0, []).

%%%_________________________________________________________________________________

%El jugador se inicia con el estado de espera
init(0) ->
   {ok, espera, {0, 0}}.

%%%_________________________________________________________________________________

%Fin del jugador
terminate(normal, _Estado, _NewStateData) ->
    ok.


%%%_________________________________________________________________________________
%%%_____________________________ACCIONES DEL JUGADOR________________________________

empezar(Nodo) ->
   {_, Pid} = on(),
   {balanceador, Nodo} ! {jugar, Pid}.

%___________________________________________________________________________________

apostar(Apuesta) ->
   gen_fsm:send_event(?JUGADOR, {apuesta, Apuesta}).

%___________________________________________________________________________________

%Pedir una carta (TJugada = tipo de jugada, puede ser cubierta o descubierta)
carta(TJugada) ->
   gen_fsm:send_event(?JUGADOR, {carta, TJugada}).
%___________________________________________________________________________________

plantarse() ->
   gen_fsm:send_event(?JUGADOR, {plantarse}).
%___________________________________________________________________________________

abandonar() ->
   gen_fsm:send_event(?JUGADOR, {abandonar}).


%%%_________________________________________________________________________________
%%%_____________________________ESTADOS DEL JUGADOR_________________________________

espera(_,{Sala,Fichas}) ->
	io:format("Esperando turno~n"),
	{next_state, espera, {Sala,Fichas}}.
%___________________________________________________________________________________

apuesta({apuesta, Apuesta},{Sala, Fichas}) ->
	Sala ! {{apuesta, Apuesta}, self()},
	{next_state, espera, {Sala,Fichas}};

apuesta({abandonar},{Sala, _Fichas}) ->
	Sala ! {abandonar, self()},
	{stop, normal, []};

apuesta(_,{Sala,Fichas}) ->
	io:format("Haga su apuesta para continuar [fichas actuales: ~w]~n", [Fichas]),
	{next_state,apuesta,{Sala,Fichas}}.
%___________________________________________________________________________________

jugada({plantarse},{Sala,Fichas}) ->
	Sala ! {plantarse, self()},
	{next_state, espera, {Sala,Fichas}};

jugada({carta, TJugada},{Sala,Fichas}) ->
	Sala ! {{carta, TJugada}, self()},
	{next_state, espera, {Sala,Fichas}};

jugada({abandonar},{Sala,_}) ->
	Sala ! {abandonar,self()},
	{stop,normal,[]};

jugada(_,{Sala,Fichas}) ->
	io:format("Indique su jugada para continuar~n"),
	{next_state,jugada,{Sala,Fichas}}.


%_______________________________________________________________________________
%______________________________MENSAJES DE LA SALA______________________________

handle_info({apostar,Sala,Fichas}, espera, _) ->
	io:format("Es tu turno para apostar, dispones de ~w fichas. Tienes 1 minuto para apostar.~n", [Fichas]),
	{next_state, apuesta, {Sala, Fichas}};
%_______________________________________________________________________________

handle_info({errorApostar, Sala, Fichas}, espera, _) ->
	io:format("La cantidad apostada es incorrecta. Dispones de ~w fichas.~n", [Fichas]),
	{next_state, apuesta, {Sala, Fichas}};
%_______________________________________________________________________________

handle_info({mensaje, {Cadena, Variables}}, espera, {Sala, Fichas}) ->
	io:format(Cadena, Variables),
	{next_state, espera, {Sala, Fichas}};
%_______________________________________________________________________________

handle_info({jugar}, espera, {Sala, Fichas}) ->
	io:format("Es tu turno para jugar. Tienes 1 minuto para indicar tu jugada.~n"),
   	{next_state,jugada,{Sala, Fichas}};
%_______________________________________________________________________________

handle_info({cubierta, {Valor, _Nombre, Palo}}, espera, {_Sala,_Fichas}) ->
  io:format("La carta recibida es el ~w de ~w~n", [Valor],[Palo]).
