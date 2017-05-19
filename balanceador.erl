%%%_______________________________________________________________________________
%%%
%%%	ARQUITECTURA SOFTWARE - PRACTICA GRUPAL
%%%	CURSO 2016-17 - GRUPO 1.1
%%%
%%%	SISTEMA DE JUEGO DEL SIETE Y MEDIO - BALANCEADOR DE CARGA
%%%
%%%	Autores:
%%%		-Chas Alvarez, Lucia <lucia.chas@udc.es>
%%%		-Corton de Blas, Diego <diego.cortonde.blas@udc.es>
%%%		-Costa Garcia, Sergio <sergio.costag@udc.es>
%%%		-Freire Paz, Adrian <adrian.freire.paz@udc.es>
%%%		-Toledano Franco, Sainza <sainza.toledano@udc.es>
%%%		-Vidal Garcia, Sara <sara.vidal@udc.es>
%%%_______________________________________________________________________________

-module(balanceador).
-export([start/0,stop/1]). %API
-export([loop/1]).

-define(MAX_TIEMPO, 30000).
-define(MAX_JUGADORES, 2).

%_______________________________________________________________________________
%______________________________FUNCIONES DEL API________________________________

start() ->
	register(?MODULE,spawn(?MODULE,loop,[[]])),
	io:format("Balanceador iniciado. ~w~n",[whereis(?MODULE)]),
	ok.
%_______________________________________________________________________________
	
stop(Pid)->
	Pid ! {stop},
	io:format("Balanceador detenido. ~w~n",[Pid]),
	ok.

%_______________________________________________________________________________
%_______________________________FUNCIONES INTERNAS______________________________

loop([]) ->
	io:format("[Balanceador (~w)] Esperando jugadores~n",[self()]),
  	receive
  		{stop} ->
  			ok;
    		{jugar, Jugador} ->
       		io:format("[Balanceador (~w)] Jugador ~w se ha unido a la sala~n",[self(),Jugador]),
    		loop([Jugador])
 	end;
%_______________________________________________________________________________

loop(Jugadores) when length(Jugadores) < ?MAX_JUGADORES ->
	io:format("[Balanceador (~w)] Esperando jugadores~n",[self()]),
  	receive
  		{stop} ->
  			ok;
    		{jugar, Jugador} ->
       		io:format("[Balanceador (~w)] Jugador ~w se ha unido a la sala~n",[self(),Jugador]),
    		loop([Jugador|Jugadores])
     	after
     		?MAX_TIEMPO ->
     		io:format("[Balanceador (~w)] Tiempo maximo superado~n~nIniciando sala...~n~n",[self()]),
       		spawn(sala,on,[Jugadores]),
        	loop([])
 	end;
%________________________________________________________________________________

loop(Jugadores) when length(Jugadores) =:= ?MAX_JUGADORES ->
	io:format("[Balanceador (~w)] Aforo completo. Iniciando sala...~n~n",[self()]),
	spawn(sala, on, [Jugadores]),
	loop([]).


