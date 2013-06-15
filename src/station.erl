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
-export([start/5]).


%% ====================================================================
%% Start
%% ====================================================================

%% Startet Sender und Listener
start(MultiIP, IP, Port, Stationsklasse, Zeitverschiebung) ->
	gen_server:start_link({local, station}, station, [MultiIP, IP, Port, Stationsklasse, Zeitverschiebung], []).

init([MultiIP, IP, Port, Stationsklasse, Zeitverschiebung]) ->
	StateDict = dict:append(current_frame, trunc(current_millis()/1000), dict:append(time_deviation, 0, dict:append(time_shift, Zeitverschiebung, dict:append(station_class, Stationsklasse, dict:append(next_slot, 0, dict:append(free_slots, [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24], dict:new())))))),
	DatensenkePID = spawn(fun() -> datensenke_start() end),
	DatenquellePID = spawn(fun() -> datenquelle_start() end),
	spawn(fun() -> listener_start(IP,Port,MultiIP,DatensenkePID) end),	
	spawn(fun() -> sender_start(MultiIP,Port,DatensenkePID, DatenquellePID) end),
	{ok,StateDict}.

%% ====================================================================
%% Empfänger
%% ====================================================================

%% Startmethode des Listeners, öffnet das UDP-Socket
listener_start(IP,Port,MultiIP,DatensenkePID) ->
	logging("listener_log.log", io:format("Listener gestartet" ++ "~n",[])),		
	%Socket = get_socket(receiver,Port,IPAddr,{225,10,1,2}),
	Socket = get_socket(sender,Port,IP),
	gen_udp:controlling_process(Socket, self()),
	listener_loop(Socket,DatensenkePID),
	ok.

%% ====================================================================
%%	Beispiel: Wie erkennen wir den nächsten Frame?
%% 
%%  Startframe: 1025 <--- Zeitangabe "Sekunde" 1025 
%% 	Aktueller Frame = Startframe
%% 	bekommen X Nachrichten mit Timestamp: 1025
%%  bekommen eine Nachricht mit Timestamp: 1026
%%  Aktueller Frame = Startframe + 1
%% ====================================================================

%% Lauschen auf dem Socket
listener_loop(Socket,DatensenkePID) ->
	% empfange neues Paket
	case gen_udp:recv(Socket, 0) of
		{ok, {_Address, _Port, Packet}} ->
			Ankunftszeit = current_millis(),
			%%old Debugging: logging("listener_log.log", io:format("Nachricht von Kanal: ~s", [Packet])),
			
			%%Datensenke speichere die Nachricht
			DatensenkePID ! {log, Packet},

			% StationsKlasse extrahieren
			StationString = getElemFromDatapackage(24, Packet),
			<<Station/bitstring>> = StationString,
			
			% reservierte Slotnummer extrahieren
			SlotString = getElemFromDatapackage(25, Packet),
			<<Slot/integer>> = SlotString,
			
			% Zeit extrahieren
			ZeitString = getElemFromDatapackage(26, 33, Packet),
			<<Sendezeit/integer>> = ZeitString,
			
			Stationsklasse = gen_server:call(station, {get_station_class}),
			
			%Zeitsynchronisation
			case Station == "A" of
				true ->
					sync_time(Stationsklasse, Sendezeit, Ankunftszeit);
				false -> ok
			end,
			
			Frame = gen_server:call(station, {get_current_frame}),
			SendezeitFrame = trunc(Sendezeit/1000),
			
			% Wurde das Paket im nächsten Frame gesendet?
			case SendezeitFrame > Frame of
				true ->
				  	gen_server:call(station, {set_current_frame, SendezeitFrame}),
					gen_server:call(station, {reset_slots});	
				false ->					
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
				  	end
			end;
		{error, Reason} ->
			logging("listener_log.log", io:format("Fehler: ~p", [Reason])),
			error
	end,
	listener_loop(Socket, DatensenkePID).

%% ====================================================================
%% Beispiel: Zeitsynchronisation
%% 
%% Beispiel der Synchronisation von Station B auf Station A: 
%%
%% Paket:			1		2		3		4		5
%% Sendezeit:		0		45		80		120		160
%% Ankunftszeit:	10		50		90		130		170
%% Ankunft neu:				40		85		120		160
%% 				---------------------------------------		
%% Abweichung:		-10		-5		-10		-10		-10 
%% 
%% Beispiel der Synchronisation von Station A auf Station A: 
%% 
%% Startzeit: 5 
%% Zeitachse:			---------------------------------------------------------->
%% Erhalten von A's:		5		6		7		2		5		1		5
%% Korrektur:				0		+		+		-		-		-		+
%% Aktuelle Zeit:			5+0		5+1		6+1		7-1		6-1		5-1		4+1
%% ====================================================================
sync_time(Stationsklasse, Sendezeit, Ankunftszeit) ->
	Zeitabweichung = gen_server:call(station, {get_time_deviation}),
	case Stationsklasse == "A" of
		true ->
			% A -> A Sync
			case (Ankunftszeit - Sendezeit) > 0 of
				true ->
					gen_server:call(station, {set_time_deviation, Zeitabweichung-1});
				false ->
					gen_server:call(station, {set_time_deviation, Zeitabweichung+1})
			end;
		false ->
			% B -> A Sync
			Zeitabweichung_neu = Sendezeit - Ankunftszeit + Zeitabweichung,
			gen_server:call(station, {set_time_deviation, Zeitabweichung_neu})
	end.

%% ====================================================================
%% Sender
%% ====================================================================

%% Startmethode des Senders, öffnet das UDP-Socket
sender_start(MultiIP,Port,DatensenkePID, DatenquellePID) ->
	Socket = get_socket(sender,Port,MultiIP),
	gen_udp:controlling_process(Socket, self()),
	logging("listener_log.log", io:format("Der Sender ist gestartet!", [])),			
	sender_loop(MultiIP,Port,Socket, DatensenkePID, DatenquellePID),
	ok.

%% ====================================================================
%% Beispiel: Wie viele Millisekunden sind es bis zum nächsten Frame?
%% 
%% Aktuelle Zeit: 1025465
%% Aktueller Frame: 1000 * trunc(1025465/1000) = 1000 * 1025 = 1025000 
%% 
%% Zeit		 %		Frame
%% 1025465 modulo 1025000 = 465
%% 
%% 1000 - 465 = 535
%% ====================================================================

%% Senden in den Zeitslot
sender_loop(MultiIP,Port,Socket,DatensenkePID, DatenquellePID) ->
	AktuellerFrame = 1000 * (trunc(current_millis()/1000)),
	Ergebnis = 1000 - (current_millis() rem AktuellerFrame),
	% lege Sender schlafen, bis der neue Frame begonnen hat
	timer:send_after(Ergebnis, aufstehen),
	receive
		aufstehen ->
			% hole den reservierten Slot aus dem Dictionary
			Slot = gen_server:call(station, {get_next_slot}),
	
			% erzeuge ein Datenpaket
			Datapackage = createDatapackage(Slot, DatenquellePID),
			
			% lege Sender schlafen bis zum korrekten Zeitslot
			timer:send_after(Slot * 40 + 10, senden),
			receive
				senden ->					
					%Sende die Nachricht
					gen_udp:send(Socket, MultiIP, Port, Datapackage)
			end,
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
	Timestamp = current_millis(),
	io_lib:format("~p~p~p~p", [Nutzdaten,Stationsklasse,Slotnummer,Timestamp]).

%% Aktuelle "Uhrzeit" in Millisekunden (mit Zeitverschiebung und -abweichung)
current_millis() ->
	Zeitverschiebung = gen_server:call(station, {get_time_shift}),
	Zeitabweichung = gen_server:call(station, {get_time_deviation}),
	{Mega,Seconds,Micro} = now(),
	trunc(((Mega * 1000000 + Seconds) * 1000000 + Micro) / 1000) + Zeitverschiebung + Zeitabweichung. 

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
	{reply, Stationsklasse, Dict};

% Setze die Zeitabweichung
handle_call({set_time_deviation,Zeitabweichung},_From,Dict) ->
    NewDict = dict:store(time_deviation, Zeitabweichung, Dict),
	{reply, ok, NewDict};

% Hole die Zeitabweichung
handle_call({get_time_deviation},_From,Dict) ->
    Zeitabweichung = dict:fetch(time_deviation, Dict),
	{reply, Zeitabweichung, Dict};

% Setze die Zeitverschiebung
handle_call({set_time_shift,Zeitverschiebung},_From,Dict) ->
    NewDict = dict:store(time_shift, Zeitverschiebung, Dict),
	{reply, ok, NewDict};

% Hole die Zeitverschiebung
handle_call({get_time_shift},_From,Dict) ->
    Zeitverschiebung = dict:fetch(time_shift, Dict),
	{reply, Zeitverschiebung, Dict};

% Setze den aktuellen Frame
handle_call({set_current_frame,AktuellerFrame},_From,Dict) ->
    NewDict = dict:store(current_frame, AktuellerFrame, Dict),
	{reply, ok, NewDict};

% Hole den aktuellen Frame
handle_call({get_current_frame},_From,Dict) ->
    AktuellerFrame = dict:fetch(current_frame, Dict),
	{reply, AktuellerFrame, Dict}.
