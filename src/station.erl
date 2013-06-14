%% @author abd447
%% @doc @todo Add description to start.


-module(station).
-behaviour(gen_server).
-import('werkzeug', [logging/2]).
-import('tools',[get_socket/3,randomElem/1,getElemFromDatapackage/2,getElemFromDatapackage/3,createDataPackage/5]).
-import('datensenke',[datensenke_start/0]).
-import('datenquelle',[datenquelle_start/0]).
%% ====================================================================
%% API functions
%% ====================================================================
-export([start/3]).


%% ====================================================================
%% Start
%% ====================================================================

%% Startet Sender und Listener
start(MultiIP, IP, Port) ->
	gen_server:start_link({local, station}, station, [MultiIP, IP, Port], []).

init([MultiIP, IP, Port]) ->
	DatensenkePID = spawn(fun() -> datensenke_start() end),
	DatenquellePID = spawn(fun() -> datenquelle_start() end),
	spawn(fun() -> listener_start(IP,Port,MultiIP,DatensenkePID, DatenquellePID) end),	
	spawn(fun() -> sender_start(MultiIP,Port,DatensenkePID, DatenquellePID) end),
	StateDict = dict:append(station_class, "", dict:append(next_slot, 0, dict:append(free_slots, [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24], dict:new()))),
	{ok,StateDict}.

%% ====================================================================
%% Empfänger
%% ====================================================================

%% Startmethode des Listeners, öffnet das UDP-Socket
listener_start(IP,Port,MultiIP,DatensenkePID, DatenquellePID) ->
	logging("listener_log.log", io:format("Listener gestartet" ++ "~n",[])),		
	%Socket = get_socket(receiver,Port,IPAddr,{225,10,1,2}),
	Socket = get_socket(sender,Port,IP),
	listener_loop(Socket,DatensenkePID, DatenquellePID),
	ok.

%% Lauschen auf dem Socket
listener_loop(Socket,DatensenkePID, DatenquellePID) ->
	
	% empfange neues Paket
	case gen_udp:recv(Socket, 0) of
		{ok, {_Address, _Port, Packet}} ->
			logging("listener_log.log", io:format("Nachricht von Kanal: ~s", [Packet])),
			
			% StationsKlasse extrahieren
			StationString = getElemFromDatapackage(24, Packet),
			<<Station/bitstring>> = StationString,
			
			% reservierte Slotnummer extrahieren
			SlotString = getElemFromDatapackage(25, Packet),
			<<Slot/integer>> = SlotString,
			
			% Zeit extrahieren
			ZeitString = getElemFromDatapackage(26, 33, Packet),
			<<Zeit/integer>> = ZeitString,
			
			%Zeitsynchronisation
			case Station == "A" of
				true ->
					%Zeit Synchronisieren
					ok;
				false ->
					ok
			end,
			
			% Liegt die Zeit im aktuellen Frame?
			case 13 > 37 of
				true ->
					% Slot aus der Liste der freien Slots entfernen
					gen_server:call(station, {remove_slot, Slot}),	
					
					% ggf. neuen Slot wählen (zum Reservieren)
					case gen_server:call(station, {get_next_slot}) == Slot of
					  	true ->
							% suche den Slot zufällig aus
						  	RandomSlot = randomElem(gen_server:call(station, {get_slotlist})),
					 		gen_server:call(station, {set_next_slot, RandomSlot});
					  	false ->
						  	ok
				  	end;				
				false ->
					gen_server:call(station, {reset_slots})
			end;
		{error, Reason} ->
			logging("listener_log.log", io:format("Fehler: ~p", [Reason])),
			error
	end,
	listener_loop(Socket, DatensenkePID, DatenquellePID).

%% ====================================================================
%% Sender
%% ====================================================================

%% Startmethode des Senders, öffnet das UDP-Socket
sender_start(MultiIP,Port,DatensenkePID, DatenquellePID) ->
	Socket = get_socket(sender,Port,MultiIP),
	logging("listener_log.log", io:format("Der Sender ist gestartet!", [])),			
	sender_loop(MultiIP,Port,Socket, DatensenkePID, DatenquellePID),
	ok.

%% Senden in den Zeitslot
sender_loop(MultiIP,Port,Socket,DatensenkePID, DatenquellePID) ->
	timer:send_after(51515151, aufstehen),
	receive
		aufstehen ->
			% hole den reservierten Slot aus dem Dictionary
			Slot = gen_server:call(station, {get_next_slot}),
	
			% erzeuge ein Datenpaket
			Datapackage = createDatapackage(Slot, DatenquellePID),

			%Sende die Nachricht
			gen_udp:send(Socket, MultiIP, Port, Datapackage),
			sender_loop(MultiIP,Port,Socket, DatensenkePID, DatenquellePID)
	end.

%% ====================================================================
%% Erstellt ein Datenpaket
%%
%%	Nutzdaten(1)	=>	Byte 0-9: Nutzdaten: Name der sendenden Station
%%	Nutzdaten(2)  	=>	Byte 10-23: Reserviert für weitere Nutzdaten
%%	Stationsklasse 	=>	Byte 24: Stationsklasse (A oder B)
%%	SlotNummer		=>	Byte 25: Nummer des Slots, in dem die Station im nächsten Frame senden wird
%%	Timestamp		=>	Zeitpunkt, zu dem dieses Paket gesendet wurde. Einheit: Millisekunden seit dem 1.1.1970 als 8-Byte Integer, Big Endian
%% ====================================================================
createDatapackage(Slotnummer, DatenquellePID) ->
	Nutzdaten = DatenquellePID ! {sendeNutzdaten, self()},
	Stationsklasse = gen_server:call(station, {get_station_class}),
	Timestamp = current_time(),
	io_lib:format("~p~p~p~p", [Nutzdaten,Stationsklasse,Slotnummer,Timestamp]).

current_time() ->
	ok.
	

%% ====================================================================
%% gen_server
%% ====================================================================

% Entfernt einen freien Slot
handle_call({remove_slot,Number},_From,Dict) ->
    NewDict = dict:store(slot, dict:fetch(free_slots, Dict) -- [Number], Dict),
	{reply, ok, NewDict};

% Setzt die Liste der freien Slots auf seinen Standard zurück
handle_call({reset_slots},_From,Dict) ->
    NewDict = dict:store(free_slots, [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24], Dict),
	{reply, ok, NewDict};

% Holt die Liste der freien Slots
handle_call({get_slotlist},_From,Dict) ->
    Slots = dict:fetch(free_slots, Dict),
	{reply, Slots, Dict};

% Setze den reservierten Slot
handle_call({set_next_slot,Number},_From,Dict) ->
    NewDict = dict:store(next_slot, Number, Dict),
	{reply, ok, NewDict};

% Hole den reservierten Slot
handle_call({get_next_slot},_From,Dict) ->
    Slot = dict:fetch(next_slot, Dict),
	{reply, Slot, Dict};

% Setze die Stationsklasse
handle_call({set_station_class,Stationsklasse},_From,Dict) ->
    NewDict = dict:store(station_class, Stationsklasse, Dict),
	{reply, ok, NewDict};

% Hole die Stationsklasse
handle_call({get_station_class},_From,Dict) ->
    Stationsklasse = dict:fetch(station_class, Dict),
	{reply, Stationsklasse, Dict}.

%% ====================================================================
%% Beispiele aus der Mail
%% ====================================================================

% now_to_universal_time(erlang:timestamp())

% Socket=tools:get_socket(sender,Port,Ip),
% gen_udp:controlling_process(Socket,self()),

% Socket=tools:get_socket(receiver,Port,Ip,MultIp),
% gen_udp:controlling_process(Socket,self()),

% gen_udp:close(Socket),