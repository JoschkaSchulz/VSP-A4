%% @author joschka
%% @doc @todo Add description to tools.


-module(tools).

%% ====================================================================
%% API functions
%% ====================================================================
-export([get_socket/3,get_socket/4,randomSlot/1,createDataPackage/5,getElemFromDatapackage/2,getElemFromDatapackage/3]).



%% ====================================================================
%% Internal functions
%% ====================================================================

%% Öffnet ein UDP-Socket mit IP + Port
get_socket(sender,Port,IP)->
    {ok,Socket} = gen_udp:open(Port, [binary, {active, false}, {reuseaddr, true}, {ip, IP}, inet, {multicast_loop, false}, {multicast_if, IP}]),
    Socket.

%% Öffnet ein UDP-Socket mit MultiIP + Port und trägt IP in MultiIP ein (Multicast)
get_socket(receiver,Port,IP,MultIP)->
    {ok,Socket} = gen_udp:open(Port, [binary, {active, false}, {reuseaddr, true}, {multicast_if, IP}, inet, {multicast_loop, false}, {add_membership, {MultIP,IP}}]),
    Socket.

%% ====================================================================
%% Erstellt ein Datenpaket
%%
%%	StationName		=>	Byte 0-9: Nutzdaten: Name der sendenden Station
%%	ExtendedData  	=>	Byte 10-23: Reserviert für weitere Nutzdaten
%%	StationClass  	=>	Byte 24: Stationsklasse (A oder B)
%%	SlotNumber		=>	Byte 25: Nummer des Slots, in dem die Station im nächsten Frame senden wird
%%	Timestamp		=>	Zeitpunkt, zu dem dieses Paket gesendet wurde. Einheit: Millisekunden seit dem 1.1.1970 als 8-Byte Integer, Big Endian
%% ====================================================================
createDataPackage(StationName,ExtendedData,StationClass,SlotNumber,TimeStamp) -> 
	io_lib:format("~p~p~p~p~p", [StationName,ExtendedData,StationClass,SlotNumber,TimeStamp]).

%% Wählt aus einer Liste von Zeitslots einen zufälligen Slot aus
randomSlot(SlotList) -> 
	lists:nth(random:uniform(length(SlotList)), SlotList).

%% Gibt das Byte an Stelle 'Index' im Datapackage zurück
getElemFromDatapackage(Index,Datapackage) ->
	getElemFromDatapackage(Index, Index, Datapackage).

%% Gibt die Bytes von IndexStart bis IndexEnde im Datapackage zurück
getElemFromDatapackage(IndexStart, IndexEnd, Datapackage) ->
	lists:sublist(Datapackage,IndexStart+1,(IndexEnd+1)-(IndexStart)).
