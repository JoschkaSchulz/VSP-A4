%% ./startStations.sh 172.16.1.12 225.10.1.2 16000 1 1 A 0  

%% @author abd447
%% @doc @todo Add description to start.


-module(station).
-behaviour(gen_server).
-import('werkzeug', [logging/2,type_is/1]).
-import('tools',[get_socket/3,get_socket/4,randomElem/1,getElemFromDatapackage/2,getElemFromDatapackage/3,createDataPackage/5]).
-import('datensenke',[datensenke_start/0]).
-import('datenquelle',[datenquelle_start/0]).
%% ====================================================================
%% API functions
%% ====================================================================
-export([start/1,init/1,handle_call/3,terminate/2,handle_cast/2,code_change/3,handle_info/2]).


%% ====================================================================
%% Start
%% ====================================================================

%% Startet Sender und Listener
start([InterfaceName, MulticastIPAtom, PortAtom, StationsklasseAtom, ZeitverschiebungAtom]) ->
	{ok,MultiIP} = inet_parse:address(atom_to_list(MulticastIPAtom)),
	{ok,Interface} = inet_parse:address(atom_to_list(InterfaceName)),
	{Port,_Unused} = string:to_integer(atom_to_list(PortAtom)),
	Stationsklasse = atom_to_list(StationsklasseAtom),
	{Zeitverschiebung,_Unused} = string:to_integer(atom_to_list(ZeitverschiebungAtom)),
	io:format("~n++--------------------------------------------------~n", []),
	io:format("++ Starte Station mit Parameter:~n++~n", []),
	io:format("++ Multicast IP    : ~p~n", [MultiIP]),
	io:format("++ Interface		  : ~p~n", [Interface]),
	io:format("++ Listen Port     : ~p~n", [Port]),
	io:format("++ Stationsklasse  : ~p~n", [Stationsklasse]),
	io:format("++ Zeitverschiebung: ~p~n", [Zeitverschiebung]),
	io:format("++----------------------------------------------------~n", []),
	
	gen_server:start_link({local, ?MODULE}, ?MODULE, [MultiIP, Interface, Port, Stationsklasse, Zeitverschiebung], []).

init([MultiIP, Interface, Port, Stationsklasse, Zeitverschiebung]) ->
	StateDict = dict:store(current_frame, 0, dict:store(time_deviation, 0, dict:store(time_shift, Zeitverschiebung, dict:store(station_class, Stationsklasse, dict:store(next_slot, 0, dict:store(free_slots, [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24], dict:new())))))),
	DatensenkePID = spawn(fun() -> datensenke_start() end),
 	DatenquellePID = spawn(fun() -> datenquelle_start() end),
 	spawn(fun() -> listener_start(Interface,Port,MultiIP,DatensenkePID) end),	
 	spawn(fun() -> sender_start(Interface,MultiIP,Port,DatensenkePID, DatenquellePID) end),
	{ok,StateDict}.

%% ====================================================================
%% Empfï¿½nger
%% ====================================================================

%% Startmethode des Listeners, ï¿½ffnet das UDP-Socket
listener_start(IP,Port,MultiIP,DatensenkePID) ->
	logging("listener_log.log", io:format("Listener gestartet" ++ "~n",[])),		
	Socket = get_socket(receiver,Port,IP,MultiIP),
	%Socket = get_socket(sender,Port,IP),
	gen_udp:controlling_process(Socket, self()),
	listener_loop(Socket,DatensenkePID, -1, -1),
	ok.

%% ====================================================================
%%	Beispiel: Wie erkennen wir den nï¿½chsten Frame?
%% 
%%  Startframe: 1025 <--- Zeitangabe "Sekunde" 1025 
%% 	Aktueller Frame = Startframe
%% 	bekommen X Nachrichten mit Timestamp: 1025
%%  bekommen eine Nachricht mit Timestamp: 1026
%%  Aktueller Frame = Startframe + 1
%% ====================================================================

%% Lauschen auf dem Socket
listener_loop(Socket,DatensenkePID, LastArrivedSlot, LastReservedSlot) ->
	% empfange neues Paket
	case gen_udp:recv(Socket, 0) of
		{ok, {_Address, _Port, Packet}} ->
			 <<_StationName:10/binary,_Data:14/binary,
			   Station:8/bitstring,Slot:8/integer,Sendezeit:64 / integer - big>> = Packet,	

			Ankunftszeit = current_millis(),
			Ankunftsslot = trunc((Ankunftszeit rem 1000) / 40),
%% 			
%% 			%%Datensenke speichere die Nachricht
			DatensenkePID ! {log, Packet, io_lib:format("datensenke.log",[])},
			
			Stationsklasse = gen_server:call(?MODULE, {get_station_class}),
			
			%Zeitsynchronisation
			case Station == "A" of
				true ->
					sync_time(Stationsklasse, Sendezeit, Ankunftszeit);
				false -> ok
			end,
			
			Frame = gen_server:call(?MODULE, {get_current_frame}),
			SendezeitFrame = trunc(Sendezeit/1000),
			
			% Wurde das Paket im nï¿½chsten Frame gesendet?
			case SendezeitFrame > Frame of
				true ->
				  	gen_server:call(?MODULE, {set_current_frame, SendezeitFrame}),
					gen_server:call(?MODULE, {reset_slots});
				false ->					
					ok
			end,
			
			% Kollisionskontrolle
			case LastArrivedSlot == Ankunftsslot of
			true ->
				% Bei Kollision Slots wieder freigeben
				gen_server:call(?MODULE, {add_slot, LastReservedSlot});
			false ->
				% Slot aus der Liste der freien Slots entfernen
				gen_server:call(?MODULE, {remove_slot, Slot}),	

				ReservedSlot = gen_server:call(?MODULE, {get_next_slot}),
					
				% ggf. neuen Slot wï¿½hlen (zum Reservieren)
				case ReservedSlot == Slot of
					true ->
						% suche den Slot zufï¿½llig aus
				  		RandomSlot = randomElem(gen_server:call(?MODULE, {get_slotlist})),
						gen_server:call(?MODULE, {set_next_slot, RandomSlot});
					false ->
				  		ok
				end
			end,

			listener_loop(Socket, DatensenkePID, Ankunftsslot, Slot);
			
		{error, Reason} ->
			logging("listener_log.log", io:format("Fehler: ~p", [Reason])),
			error
	end.

%% ====================================================================
%% Beispiel: Zeitsynchronisation X ! {tellmi
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
	Zeitabweichung = gen_server:call(?MODULE, {get_time_deviation}),
	case Stationsklasse == "A" of
		true ->
			% A -> A Sync
			case (Ankunftszeit - Sendezeit) > 0 of
				true ->
					gen_server:call(?MODULE, {set_time_deviation, Zeitabweichung-1});
				false ->
					gen_server:call(?MODULE, {set_time_deviation, Zeitabweichung+1})
			end;
		false ->
			% B -> A Sync
			Zeitabweichung_neu = Sendezeit - Ankunftszeit + Zeitabweichung,
			gen_server:call(?MODULE, {set_time_deviation, Zeitabweichung_neu})
	end.

%% ====================================================================
%% Sender
%% ====================================================================

%% Startmethode des Senders, ï¿½ffnet das UDP-Socket
sender_start(Interface,MultiIP,Port,DatensenkePID, DatenquellePID) ->
	Socket = get_socket(sender,Port,Interface),
	gen_udp:controlling_process(Socket, self()),
	logging("listener_log.log", io:format("Der Sender ist gestartet!~n", [])),			
	sender_loop(MultiIP,Port,Socket, DatensenkePID, DatenquellePID,-1),
	ok.

%% ====================================================================
%% Beispiel: Wie viele Millisekunden sind es bis zum nï¿½chsten Frame?
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
sender_loop(MultiIP,Port,Socket,DatensenkePID, DatenquellePID,OldSlot) ->
	AktuellerFrame = 1000 * (trunc(current_millis()/1000)),
	Ergebnis = 1000 - (current_millis() rem AktuellerFrame),
	% lege Sender schlafen, bis der neue Frame begonnen hat
	timer:send_after(Ergebnis, aufstehen),
	receive
		aufstehen ->
			
			% hole den reservierten Slot aus dem Dictionary
			case OldSlot == -1 of
				true ->	
					Slot = gen_server:call(?MODULE, {get_next_slot}),
					random:seed(now()),
					RandomSlot = random:uniform(25)-1,
					gen_server:call(?MODULE, {set_next_slot, RandomSlot});
				false ->
					Slot = OldSlot
			end,
			
			% lege Sender schlafen bis zum korrekten Zeitslot
			timer:send_after(Slot * 40 +10, senden),
			receive
				senden ->				
					% sind wir noch im korrekten Zeitslot oder bereits im nächsten?
					case (current_millis() rem 1000) < (Slot*40 + 40) of
						true ->
						  	% hole den reservierten Slot aus dem Dictionary
							NextSlot = gen_server:call(?MODULE, {get_next_slot}),
					
							% erzeuge ein Datenpaket
							Datapackage = createDatapackage(NextSlot, DatenquellePID),
					
							%Sende die Nachricht
							gen_udp:send(Socket, MultiIP, Port, Datapackage),
					  
					  		sender_loop(MultiIP,Port,Socket, DatensenkePID, DatenquellePID,NextSlot);
					  	% wenn Zeitslot überschritten, nichts senden
						false ->
							sender_loop(MultiIP,Port,Socket, DatensenkePID, DatenquellePID,-1)
					end		
			end	
	end.

%% ====================================================================
%% Erstellt ein Datenpaket
%%
%%	Nutzdaten(1)	=>	Byte 0-9: Nutzdaten: Name der sendenden Station
%%	Nutzdaten(2)  	=>	Byte 10-23: Reserviert fï¿½r weitere Nutzdaten
%%	Stationsklasse 	=>	Byte 24: Stationsklasse (A oder B)
%%	SlotNummer		=>	Byte 25: Nummer des Slots, in dem die Station im nï¿½chsten Frame senden wird
%%	Timestamp		=>	Zeitpunkt, zu dem dieses Paket gesendet wurde. Einheit: Millisekunden seit dem 1.1.1970 als 8-Byte Integer, Big Endian
%% ====================================================================
createDatapackage(Slotnummer, DatenquellePID) ->
%% 	DatenquellePID ! {sendeNutzdaten, self()},
%% 	receive
%% 		{nutzdaten, Nutzdaten} ->
%% 			io:format("Nutzdaten angekommen")
%% 	end,
	Nutzdaten = list_to_binary(io:get_chars("", 24)),
	case Nutzdaten == "" of
		true ->
			createDatapackage(Slotnummer, DatenquellePID);
		false ->
			StationsklassenByte = list_to_bitstring(gen_server:call(?MODULE, {get_station_class})),
			Timestamp = current_millis(),
			<<Nutzdaten:24/binary, StationsklassenByte:8/bitstring, Slotnummer:8/integer, Timestamp:64 / integer - big>>
	end.

%% Aktuelle "Uhrzeit" in Millisekunden (mit Zeitverschiebung und -abweichung)
current_millis() ->
	Zeitverschiebung = gen_server:call(?MODULE, {get_time_shift}),
	Zeitabweichung = gen_server:call(?MODULE, {get_time_deviation}),
	{Mega,Seconds,Micro} = now(),
	%%io:format("~n~nZeitverschiebung ist Typ: ~p ~p~n",[type_is(Zeitverschiebung),Zeitverschiebung]),
	%%io:format("~n~nZeitabweichung ist Typ: ~p~n",[type_is(Zeitabweichung)]),
	trunc(((Mega * 1000000 + Seconds) * 1000000 + Micro) / 1000) + Zeitverschiebung + Zeitabweichung. 

%% ====================================================================
%% gen_server
%% ====================================================================

% Entfernt einen freien Slot
handle_call({remove_slot,Number},_From,Dict) ->
	{reply, ok, dict:store(free_slots, dict:fetch(free_slots, Dict) -- [Number], dict:erase(free_slots,Dict))};

% fÃ¼gt einen freien Slot hinzu
handle_call({add_slot,Number},_From,Dict) ->
	Liste = dict:fetch(free_slots, Dict),
	case lists:member(Number, Liste) of
		true ->
			{reply, ok, Dict};
		false ->
			{reply, ok, dict:store(free_slots, Liste ++ [Number], Dict)}
	end;

% Setzt die Liste der freien Slots auf seinen Standard zurï¿½ck
handle_call({reset_slots},_From,Dict) ->
	{reply, ok, dict:store(free_slots, [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24], dict:erase(free_slots,Dict))};

% Hole die Liste der freien Slots
handle_call({get_slotlist},_From,Dict) ->
	{reply, dict:fetch(free_slots, Dict), Dict};

% Setze den reservierten Slot
handle_call({set_next_slot,Number},_From,Dict) ->
	{reply, ok, dict:store(next_slot, Number, dict:erase(next_slot,Dict))};

% Hole den reservierten Slot
handle_call({get_next_slot},_From,Dict) ->
	{reply, dict:fetch(next_slot, Dict), Dict};

% Setze die Stationsklasse
handle_call({set_station_class,Stationsklasse},_From,Dict) ->
	{reply, ok, dict:store(station_class, Stationsklasse, dict:erase(station_class,Dict))};

% Hole die Stationsklasse
handle_call({get_station_class},_From,Dict) ->
	{reply, dict:fetch(station_class, Dict), Dict};

% Setze die Zeitabweichung
handle_call({set_time_deviation,Zeitabweichung},_From,Dict) ->
	{reply, ok, dict:store(time_deviation, Zeitabweichung, dict:erase(time_deviation,Dict))};

% Hole die Zeitabweichung
handle_call({get_time_deviation},_From,Dict) ->
	{reply, dict:fetch(time_deviation, Dict), Dict};

% Hole die Zeitverschiebung
handle_call({get_time_shift},_From,Dict) ->
	{reply, dict:fetch(time_shift, Dict), Dict};

% Setze den aktuellen Frame
handle_call({set_current_frame,AktuellerFrame},_From,Dict) ->
	{reply, ok, dict:store(current_frame, AktuellerFrame, dict:erase(current_frame,Dict))};

% Hole den aktuellen Frame
handle_call({get_current_frame},_From,Dict) ->
	{reply, dict:fetch(current_frame, Dict), Dict}.

%% unused
terminate(normal, _State) -> ok.
handle_cast(stop, State) -> {stop, normal, State};
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Msg, State) -> {noreply, State}.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
