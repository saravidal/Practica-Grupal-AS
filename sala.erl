%%%_______________________________________________________________________________
%%%
%%%	ARQUITECTURA SOFTWARE - PRACTICA GRUPAL
%%%	CURSO 2016-17 - GRUPO 1.1
%%%
%%%	SISTEMA DE JUEGO DEL SIETE Y MEDIO - SALA
%%%
%%%	Autores:
%%%		-Chas Alvarez, Lucia <lucia.chas@udc.es>
%%%		-Corton de Blas, Diego <diego.cortonde.blas@udc.es>
%%%		-Costa Garcia, Sergio <sergio.costag@udc.es>
%%%		-Freire Paz, Adrian <adrian.freire.paz@udc.es>
%%%		-Toledano Franco, Sainza <sainza.toledano@udc.es>
%%%		-Vidal Garcia, Sara <sara.vidal@udc.es>
%%%_______________________________________________________________________________

-module(sala).

-compile(export_all).
-export([esperarApuesta/2]).
-export([buscarJugador/2]).

-define(SALA, sala).


%%%_______________________________________________________________________________
%%%__________________________INICIO Y FIN DE LA SALA______________________________

%Generacion de la maquina de estados de la sala
on(Clientes) ->
	ListaJ = crearListaJugadores(Clientes),
	io:format("[Sala] Sala iniciada.~n"),
	gen_fsm:start_link({local, ?SALA}, ?MODULE, ListaJ, []).
%_________________________________________________________________________________

%Inicializacion de la sala
init(ListaJ) ->
	io:format("[Sala] Empezamos la partida~n"),
	StateData = { 0, {nueva_baraja(), [], 0}, {ListaJ, ListaJ}},
	self() ! {apuesta},
	{ok, pedirApuesta, StateData}.
%_________________________________________________________________________________

%Finalizacion de la sala
terminate(normal, _Estado, _DatosEstado) ->
	ok.
%_________________________________________________________________________________

handle_event(stop, _Estado, _DatosEstado) ->
	{stop, normal, []}.


%_________________________________________________________________________________
%________________________________ESTADOS DE LA SALA_______________________________

esperarApuesta ({timeout, _From, JAux}, {Temp, Juego, {ListaJ, []}}) ->
	{_Pid, NombreJ, _Fichas, _ApuestaJ, _Mano, _Estado} = buscarJugador(ListaJ, JAux),
	io:format("[Sala] Eliminamos a ~s~n", [NombreJ]),
	ListaJMod = borrarJugador(JAux, ListaJ),
	io:format("[Sala] Proximo estado REPARTIR CARTAS~n"),
	exit(JAux, kill),
	if
		(length(ListaJMod) == 0) ->
		   	io:format("[Sala] Sala cerrada~n"),
   			{stop, normal, []};
		(true) ->
			self() ! {repartir},
			{next_state, repartirCartas, {Temp, Juego, {ListaJMod, ListaJMod}}}
	end;

esperarApuesta ({timeout, _From, JAux}, {Temp, Juego, {ListaJ, ListaJAux}}) ->
	{_Pid, NombreJ, _Fichas, _ApuestaJ, _Mano, _Estado} = buscarJugador(ListaJ, JAux),
	io:format("[Sala] Eliminamos a ~s~n", [NombreJ]),
	ListaJMod = borrarJugador(JAux, ListaJ),
	exit(JAux, kill),
	if
		(length(ListaJMod) == 0) ->
			io:format("[Sala] Sala cerrada~n"),
   			{stop, normal, []};
		(true) ->
			self() ! {apuesta},
			{next_state, pedirApuesta, {Temp, Juego, {ListaJMod, ListaJAux}}}
	end.
%_________________________________________________________________________________________________

esperarJugada ({timeout, _From, JAux}, {Temp, Juego, {ListaJ, []}}) ->
	{_Pid, NombreJ, _Fichas, _ApuestaJ, _Mano, _Estado} = buscarJugador(ListaJ, JAux),
	io:format("[Sala] Eliminamos a ~s~n", [NombreJ]),
	ListaJMod = borrarJugador(JAux, ListaJ),
	exit(JAux, kill),
	ListaJP = jugadoresJuego(ListaJMod),
	if
		(length(ListaJMod) == 0) ->
			io:format("[Sala] Sala cerrada~n"),
 			{stop, normal, []};
		(length (ListaJP) > 0) ->
			self() ! {jugada},
			{next_state, estadoPedirJugada, {Temp, Juego, {ListaJMod, ListaJP}}};
		(true) ->
			io:format("[Sala] Proximo estado JUGADA BANCA~n"),
			self() ! {banca},
			{next_state, jugadaBanca, {Temp, Juego, {ListaJMod, ListaJMod}}}
	end;


esperarJugada ({timeout, _From, JAux}, {Temp, Juego, {ListaJ, ListaJAux}}) ->
	{_Pid, NombreJ, _Fichas, _ApuestaJ, _Mano, _Estado} = buscarJugador(ListaJ, JAux),
	io:format("[Sala] Eliminamos a ~s~n", [NombreJ]),
	ListaJMod = borrarJugador(JAux, ListaJ),
	exit(JAux, kill),
	if
		(length(ListaJMod) == 0) ->
			io:format("[Sala] Sala cerrada~n"),
   			{stop, normal, []};
		(true) ->
			self() ! {jugada},
   			{next_state, pedirJugada, {Temp, Juego, {ListaJMod, ListaJAux}}}
	end.


%_________________________________________________________________________________
%______________________________MENSAJES DE LOS JUGADORES__________________________

handle_info ({apuesta}, pedirApuesta, {Temp, Juego, {ListaJ, []}}) ->
	io:format("[Sala] Proximo estado REPARTIR CARTAS~n"),
	self() ! {repartir},
	{next_state, repartirCartas, {Temp, Juego, {ListaJ, ListaJ}}};

handle_info ({apuesta}, pedirApuesta, {_Temp, Juego, { ListaJ,
		[{Jugador, NombreJ, Fichas, _Apuesta, _Mano, _Estado}|RestoJ]}}) ->
	io:format("[Sala] Pedimos apuesta a ~s~n", [NombreJ]),
	TempAux = gen_fsm:start_timer(60000,  Jugador),
	Jugador ! {apostar, self(), Fichas},
	{next_state, esperarApuesta, {TempAux, Juego, {ListaJ, RestoJ}}};
%_________________________________________________________________________________

handle_info ({{apuesta, Apuesta}, Jugador}, esperarApuesta, {Temp, Partida, {ListaJ, ListaJAux}}) ->
	gen_fsm:cancel_timer(Temp),
	{_Pid, NombreJ, Fichas, _ApuestaJ, _Mano, _Estado} = buscarJugador(ListaJ, Jugador),
	if
		(Fichas >= Apuesta) ->
			io:format("[Sala] ~s ha apostado ~w~n", [NombreJ, Apuesta]),
			ListaJAp = apuestaJugador(ListaJ, Apuesta, Jugador),
			broadCast(ListaJAp, {"~s ha apostado ~w fichas.~n", [NombreJ, Apuesta]}),
			self() ! {apuesta},
			{next_state, pedirApuesta, {Temp, Partida, {ListaJAp, ListaJAux}}};
		(true) ->
			TempAux = gen_fsm:start_timer(60000,  Jugador),
			Jugador ! {errorApostar, self(), Fichas},
			{next_state, esperarApuesta, {TempAux, Partida, {ListaJ, ListaJAux}}}
	end;
%_________________________________________________________________________________

handle_info ({repartir}, repartirCartas, {Temp, {Baraja, _CBanca, PBanca}, {ListaJ, ListaJ}}) ->
	io:format("[Sala] Repartiendo cartas.~n"),
	{ListaJAux, RestoB} = repartirCartasJ({ListaJ, Baraja}),
	{{Valor, Nombre, Palo, _EstadoC}, RestoB2} = obtenerCarta(RestoB),
	broadCast(ListaJ, {"Banca recibe: ~w de ~w .~n", [[Valor], [Palo]]}),
	io:format("[Sala] Proximo estado PEDIR JUGADA~n"),
	self() ! {jugada},
	{next_state, pedirJugada, {Temp, {RestoB2, [{Valor, Nombre, Palo, descubierta}], PBanca}, {ListaJAux, ListaJAux}}};
%_________________________________________________________________________________

handle_info ({jugada}, pedirJugada, {Temp, Juego, {ListaJ, []}}) ->
	ListaJP = jugadoresJuego(ListaJ),
	TamListaJ = length (ListaJP),
	if
		(TamListaJ > 0) ->
			io:format("[Sala] Jugadores en juego ~w~n", [ListaJP]),
			self() ! {jugada},
			{next_state, pedirJugada, {Temp, Juego, {ListaJ, ListaJP}}};
		(true) ->
			io:format("[Sala] Proximo estado JUGADA BANCA~n"),
			self() ! {banca},
			{next_state, jugadaBanca, {Temp, Juego, {ListaJ, ListaJ}}}
	end;

handle_info ({jugada}, pedirJugada, { _Temp, Juego,
		{ ListaJ, [{Jugador, NombreJ, _Fichas, _Apuesta, _Mano, _Estado}|RestoJ]}}) ->
	{_Pid, NombreJ, _Fichas, _ApuestaJ, _Mano, _Estado} = buscarJugador(ListaJ, Jugador),
	io:format("[Sala] ESTADO pedirJugada a ~s~n", [NombreJ]),
	TempAux = gen_fsm:start_timer(60000, Jugador),
	Jugador ! {jugar},
	{next_state, esperarJugada, {TempAux, Juego, {ListaJ, RestoJ}}};
%_________________________________________________________________________________

handle_info({banca}, jugadaBanca, {Temp, {Baraja, CBanca, _PBanca}, {ListaJ, _ListaJAux}}) ->
	{CBancaN, MazoN} = cartasBanca(CBanca, Baraja),
	PBancaAux = sumarCartas(CBancaN),
	broadCast(ListaJ, {"Cartas Banca: ~w~n", [CBancaN]}),
	broadCast(ListaJ, {"La Banca tiene una puntuacion de ~w puntos~n", [PBancaAux]}),
	io:format("[Sala] Proximo estado CALCULAR GANADORES~n"),
	self() ! {calcularGanadores},
	{next_state, calcularGanadores, {Temp,{MazoN, CBanca, PBancaAux} , {ListaJ, ListaJ}}};
%_________________________________________________________________________________

 handle_info({calcularGanadores}, calcularGanadores, {Temp, {_Baraja, _CBanca, PBanca}, {ListaJ, _ListaJAux}}) ->
 	ListaJN = sumarFichas(PBanca, ListaJ),
 	io:format("[Sala] Proximo estado PEDIR APUESTAS~n"),
	self() ! {apuesta},
 	{next_state, pedirApuesta, {Temp, {nueva_baraja(), [], 0}, {ListaJN, ListaJN}}};
%_________________________________________________________________________________


handle_info({plantarse, Jugador}, esperarJugada, {Temp, Partida, {ListaJ, ListaJAux}}) ->
	gen_fsm:cancel_timer(Temp),
	{_Pid, NombreJ, _Fichas, _ApuestaJ, _Mano, _Estado} = buscarJugador(ListaJ, Jugador),
	io:format("[Sala] ~s se ha plantado~n", [NombreJ]),
	ListaJN = cambiarEstado(ListaJ, Jugador, 1),
	broadCast(ListaJN, {"~s se ha plantado.~n", [NombreJ]}),
	self() ! {jugada},
	{next_state, pedirJugada, {Temp, Partida, {ListaJN, ListaJAux}}};
%_________________________________________________________________________________

handle_info({{carta, TJugada}, Jugador}, esperarJugada, {Temp, {Baraja, CBanca, PBanca}, {ListaJ, ListaJAux}}) ->
	gen_fsm:cancel_timer(Temp),
	{_Pid, NombreJ, _Fichas, _ApuestaJ, _Mano, _Estado} = buscarJugador(ListaJ, Jugador),
	{{Valor, Nombre, Palo, EstadoC}, RestoB} = obtenerCarta(Baraja),

	if
		(TJugada == "cubierta") ->
			{_Pid, _NombreJ, _Fichas, _ApuestaJ, ManoN, _Estado} = buscarJugador(ListaJ, Jugador),
			{ValorR, NombreR, PaloR, EstadoC} = lists:keyfind(cubierta, 4, ManoN),
			lists:keyreplace(cubierta, 4, ManoN, {ValorR, NombreR, PaloR, descubierta}),
			{_Pid, _NombreJ, _Fichas, _ApuestaJ, ManoN, _Estado} = buscarJugador(ListaJ, Jugador),
			ListaJN = entregarCarta(ListaJ, Jugador, {Valor, Nombre, Palo, EstadoC}),
			io:format("[Sala] El jugador ~s pide carta cubierta. Revela ~w de ~w~n", [NombreJ, [ValorR],[PaloR]]),
			PJugador = sumarCartas(ManoN) + Valor,
			broadCast(ListaJN, {"~s ha pedido carta cubierta. Carta Revelada: ~w de ~w   Puntuacion: ~w~n", [[NombreJ], [ValorR], [PaloR], [PJugador]]}),
			Jugador ! {mensaje, {"PRIVADO: Recibes ~w de ~w.~n", [[Valor],[Palo]]}};

		(TJugada == "descubierta") ->
			ListaJN = entregarCarta(ListaJ, Jugador, {Valor, Nombre, Palo, descubierta}),
			io:format("[Sala] El jugador ~s pide carta descubierta. Recibe ~w de ~w~n", [NombreJ, [Valor],[Palo]]),
			{_Pid, _NombreJ, _Fichas, _ApuestaJ, ManoN, _Estado} = buscarJugador(ListaJN, Jugador),
			PJugador = sumarCartas(ManoN),
			broadCast(ListaJN, {"~s ha pedido carta descubierta. Carta nueva: ~w de ~w   Puntuacion: ~w~n", [[NombreJ], [Valor], [Palo], [PJugador]]})
	end,


	if
		(PJugador > 75) ->
			broadCast(ListaJ, {"~s se ha pasado.~n", [NombreJ]}),
			ListaJNAux = cambiarEstado(ListaJN, Jugador, 2),
			self() ! {jugada},
			{next_state, pedirJugada, {Temp, {RestoB, CBanca, PBanca}, {ListaJNAux, ListaJAux}}};
		(true) ->
			self() ! {jugada},
			{next_state, pedirJugada, {Temp, {RestoB, CBanca, PBanca}, {ListaJN, ListaJAux}}}
	end;
%_________________________________________________________________________________

handle_info({abandonar, Jugador}, esperarApuesta, {Temp, Partida, {ListaJ, ListaJAux}} ) ->
	gen_fsm:cancel_timer(Temp),
	{_Pid, NombreJ, _Fichas, _ApuestaJ, _Mano, _Estado} = buscarJugador(ListaJ, Jugador),
	io:format("[Sala] ~s ha abandonado la partida.~n", [NombreJ]),
	ListaJMod = borrarJugador(Jugador, ListaJ),
	broadCast(ListaJMod, {"~s ha abandonado la partida.~n", [NombreJ]}),
	if
		(length(ListaJMod) == 0) ->
			io:format("[Sala] Sala cerrada~n"),
   			{stop, normal, []};
		(true) ->
			self() ! {apuesta},
			{next_state, pedirApuesta, {Temp, Partida, {ListaJMod, ListaJAux}}}
	end;

handle_info({abandonar, Jugador}, esperarJugada, {Temp, Partida, {ListaJ, ListaJAux}} ) ->
	gen_fsm:cancel_timer(Temp),
	{_Pid, NombreJ, _Fichas, _ApuestaJ, _Mano, _Estado} = buscarJugador(ListaJ, Jugador),
	io:format("[Sala] ~s ha abandonado la partida.~n", [NombreJ]),
	ListaJMod = borrarJugador(Jugador, ListaJ),
	broadCast(ListaJMod, {"~s ha abandonado la partida.~n", [NombreJ]}),
	if
		(length(ListaJMod) == 0) ->
			io:format("[Sala] Sala cerrada~n"),
   			{stop, normal, []};
		(true) ->
			self() ! {jugada},
			{next_state, pedirJugada, {Temp, Partida, {ListaJMod, ListaJAux}}}
	end.


%_________________________________________________________________________________
%________________________________FUNCIONES AUXILIARES_____________________________

auxiliarLista([], ListaJ, _Contador) ->
	ListaJ;

auxiliarLista([PrimerJugador|Jugadores], ListaJ, Contador) ->
	NombreJ = string:concat("Jugador", integer_to_list(Contador)),
	auxiliarLista(Jugadores,  ListaJ++[{PrimerJugador, NombreJ , 100, 0, [], 0}] , Contador+1).

crearListaJugadores(Jugadores) ->
	auxiliarLista(Jugadores, [], 1).
%_________________________________________________________________________________

nueva_baraja()->
	 [{10, espadas1, espadas, cubierta},{10, bastos1, bastos, cubierta},
	 {10, oros1, oros, cubierta},{10, copas1, copas, cubierta},
	 {20, espadas2, espadas, cubierta},{20, bastos2, bastos, cubierta},
	 {20, oros2, oros, cubierta},{20, copas2, copas, cubierta},
	 {30, espadas3, espadas, cubierta},{30, bastos3, bastos, cubierta},
	 {30, oros3, oros, cubierta},{30, copas3, copas, cubierta},
	 {40, espadas4, espadas, cubierta},{40, bastos4, bastos, cubierta},
	 {40, oros4, oros, cubierta},{40, copas4, copas, cubierta},
	 {50, espadas5, espadas, cubierta},{50, bastos5, bastos, cubierta},
	 {50, oros5, oros, cubierta},{50, copas5, copas, cubierta},
	 {60, espadas6, espadas, cubierta},{60, bastos6, bastos, cubierta},
	 {60, oros6, oros, cubierta},{60, copas6, copas, cubierta},
	 {70, espadas7, espadas, cubierta},{70, bastos7, bastos, cubierta},
	 {70, oros7, oros, cubierta},{70, copas7, copas, cubierta},
	 {5, espadas8, espadas, cubierta}, {5, bastos8, bastos, cubierta},
	 {5, oros8, oros, cubierta}, {5, copas8, copas, cubierta},
	 {5, espadas9, espadas, cubierta}, {5, bastos9, bastos, cubierta},
	 {5, oros9, oros, cubierta}, {5, copas9, copas, cubierta},
	 {5,espadas10,espadas, cubierta},  {5,bastos10,bastos, cubierta},
	 {5,oros10,oros, cubierta},  {5,copas10,copas, cubierta}].

randomSeed() ->
	case random:seed(now()) of
		undefined ->
			randomSeed();
		Else ->
			Else
	end.

random(Num) ->
	{_H,_M,S} = randomSeed(),
	(S rem Num) + 1 .
%_________________________________________________________________________________

borrarJugador(_,[]) ->
	[];

borrarJugador(Jugador,[{Jugador,_,_,_,_,_}|ListaJO]) ->
	ListaJO;

borrarJugador(Jugador, [{_JugadorAux, NombreJ, Fichas, Apuesta, Mano, Estado} | ListaJO]) ->
	[{Jugador, NombreJ, Fichas, Apuesta, Mano, Estado} | borrarJugador(Jugador, ListaJO)].
%_________________________________________________________________________________

apuestaJugador([], _, _) ->
	[];

apuestaJugador([{Jugador, NombreJ, Fichas, _ApuestaAntigua, Mano, Estado}|ListaJO], Apuesta, Jugador) ->
	ListaJO++[{Jugador, NombreJ, Fichas-Apuesta, Apuesta, Mano, Estado}];

apuestaJugador([{JugadorAux, NombreJ, Fichas, Apuesta, Mano, Estado}|ListaJO], ApuestaNueva, Jugador) ->
	 apuestaJugador(ListaJO, ApuestaNueva, Jugador)++[{JugadorAux, NombreJ, Fichas, Apuesta, Mano, Estado}].
%_________________________________________________________________________________

repartirCartasJ({ListaJ, Baraja})->
	repartirCartasJAux({ListaJ, Baraja},[], ListaJ).

repartirCartasJAux({[], Baraja}, ListaJAux, _ListaJ) ->
	{ListaJAux, Baraja};

repartirCartasJAux({[{Jugador, NombreJ, Fichas, Apuesta, _Mano, Estado}|T], Baraja}, ListaJAux, ListaJ) ->
	{{Valor, Nombre, Palo, EstadoC}, RestoB} = obtenerCarta(Baraja),
	broadCast(ListaJ, {"~s recibe una carta cubierta~n",  [NombreJ]}),
	Jugador ! {mensaje, {"PRIVADO: Recibes ~w de ~w.~n", [[Valor],[Palo]]}},
	repartirCartasJAux({T, RestoB}, ListaJAux++[{Jugador, NombreJ, Fichas, Apuesta, [{Valor, Nombre, Palo, EstadoC}], Estado}], ListaJ).
%_________________________________________________________________________________

%_________________________________________________________________________________

broadCast([], {Cadena, Variables}) ->
	io:format(Cadena, Variables);
broadCast([{Jugador, _NombreJ, _Fichas, _Apuesta, _Mano, _Estado}|T], {Cadena, Variables}) ->
	Jugador ! {mensaje, {Cadena, Variables}},
	broadCast(T, {Cadena, Variables}).
%_________________________________________________________________________________

obtenerCarta(Baraja) ->
	{Valor, Nombre, Palo, EstadoC} = lists:nth(random(length(Baraja)), Baraja),
	RestoB = lists:keydelete(Nombre,2,Baraja),
	{{Valor, Nombre, Palo, EstadoC}, RestoB}.
%_________________________________________________________________________________

entregarCarta([], _, _) ->
	[];

entregarCarta([{Jugador, NombreJ, Fichas, Apuesta, Mano, Estado}|ListaJO], Jugador, {Valor, Nombre, Palo, EstadoC}) ->
	ListaJO++[{Jugador, NombreJ, Fichas, Apuesta, [{Valor, Nombre, Palo, EstadoC} | Mano], Estado}];

entregarCarta([{JugadorAux, NombreJ, Fichas, Apuesta, Mano, Estado}|ListaJO], Jugador, {Valor, Nombre, Palo, EstadoC}) ->
	entregarCarta(ListaJO, Jugador, {Valor, Nombre, Palo, EstadoC})++[{JugadorAux, NombreJ, Fichas, Apuesta, Mano, Estado}].
%_________________________________________________________________________________

cambiarEstado([], _, _Estado) ->
	[];

cambiarEstado([{Jugador, NombreJ, Fichas, Apuesta, Mano, _Estado}|ListaJO], Jugador, Estado) ->
	ListaJO++[{Jugador, NombreJ, Fichas, Apuesta, Mano, Estado}];

cambiarEstado([{JugadorAux, NombreJ, Fichas, Apuesta, Mano, Estado}|ListaJO], Jugador, Carta) ->
	cambiarEstado(ListaJO, Jugador, Carta)++[{JugadorAux, NombreJ, Fichas, Apuesta, Mano, Estado}].
%_________________________________________________________________________________

buscarJugador([], _) ->
	{};

buscarJugador([{Jugador, NombreJ, Fichas, Apuesta, Mano, Estado}|_ListaJO], Jugador) ->
	{Jugador, NombreJ, Fichas, Apuesta, Mano, Estado};

buscarJugador([{_JugadorAux, _NombreJ, _Fichas, _Apuesta, _Mano, _Estado}|ListaJO], Jugador) ->
	 buscarJugador (ListaJO, Jugador).
%_________________________________________________________________________________

cartasBanca(CBanca, Baraja) ->
	io:format("[Sala] A la baraja le quedan ~w cartas ~n",[length(Baraja)]),
	{{Valor, Nombre, Palo, EstadoC}, RestoB} = obtenerCarta(Baraja),
	TotalCartas = CBanca++[{Valor, Nombre, Palo, EstadoC}],
	Puntuacion = sumarCartas(TotalCartas),
	if
		Puntuacion < 60  ->
			cartasBanca(TotalCartas, RestoB);
		true ->
			{TotalCartas, RestoB}
	end.
%_________________________________________________________________________________

sumarCartas([]) -> 0;

sumarCartas([{Valor, _Nombre, _Palo, _EstadoC}|ListaCartas]) ->
		Valor + sumarCartas(ListaCartas).
%_________________________________________________________________________________

sumarFichas(_PBanca, [] ) ->
	[];

sumarFichas(PBanca, ListaJ) ->
	sumarFichas(PBanca, ListaJ, []).

sumarFichas(PBanca, [{Jugador,NombreJ,Fichas,Apuesta,Mano,Estado}|ListaJO], ListaJ) ->
	PuntuacionJ = sumarCartas(Mano),
	io:format("PuntuacionBanca: ~w PuntuacionJugador: ~w~n", [[PBanca],[PuntuacionJ]]),
	if
		(PuntuacionJ > 75) ->
			Jugador ! {mensaje, {"Has perdido con ~w, te has pasado de 75.~n", [PuntuacionJ]}},
			sumarFichas(PBanca, ListaJO, [{Jugador,NombreJ, Fichas, 0,[], Estado}]++ListaJ);
		(PBanca < PuntuacionJ) ->
			Jugador ! {mensaje, {"Has ganado a la banca(~w) con ~w puntos, has ganado ~w fichas.~n", [[PBanca],[PuntuacionJ], [Apuesta*2]]}},
			sumarFichas(PBanca, ListaJO, [{Jugador,NombreJ,Fichas+(Apuesta*2),0,[], Estado}]++ListaJ);
		(PBanca == PuntuacionJ) ->
			Jugador ! {mensaje, {"Has empatado con la banca con ~w puntos, has ganado ~w fichas.~n", [[PuntuacionJ], [Apuesta]]}},
			sumarFichas(PBanca, ListaJO, [{Jugador, NombreJ, Fichas+Apuesta,0,[], Estado}]++ListaJ);
		(PBanca > PuntuacionJ) ->
			Jugador ! {mensaje, {"Has perdido con ~w puntos, te ha ganado la banca.~n", [PuntuacionJ]}},
			sumarFichas(PBanca, ListaJO, [{Jugador,NombreJ, Fichas,0,[],Estado}]++ListaJ)
	end;

sumarFichas(_PBanca, [], ListaJ) ->
	ListaJ.
%_________________________________________________________________________________

jugadoresJuego(ListaJ) ->
	jugadoresJuegoAux(ListaJ, []).

jugadoresJuegoAux([], ListaJJ) ->
	ListaJJ;

jugadoresJuegoAux([{Jugador, NombreJ, Fichas, Apuesta, Cartas, Estado}|Cola], ListaJJ) ->
	if
		(Estado == 0) ->
			jugadoresJuegoAux (Cola, ListaJJ++[{Jugador, NombreJ, Fichas, Apuesta, Cartas, Estado}]);
		(true) ->
			jugadoresJuegoAux(Cola, ListaJJ)
	end.
