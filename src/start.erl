%% @author abd447
%% @doc @todo Add description to start.


-module(start).
-import('listener',[listenerStart/2]).
%% ====================================================================
%% API functions
%% ====================================================================
-export([start/2]).



%% ====================================================================
%% Internal functions
%% ====================================================================


start(IPAddr, Port) ->
	PID = spawn(fun() -> loop() end),
	register('Pikachu', PID),
	spawn(fun() -> listenerStart(IPAddr,Port) end),
	PID.

loop() ->
	%ZeitSlot = randomSlot(SlotList),
	loop().

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
	io_lib:format("~p~p~p~p~p", [tationName,ExtendedData,StationClass,SlotNumber,TimeStamp]).

randomSlot(SlotList) -> 
	lists:nth(random:uniform(length(SlotList)), SlotList).

get_socket(sender,Port,IP)->
    {ok,Socket} = gen_udp:open(Port, [binary, {active, false}, {reuseaddr, true}, {ip, IP}, inet, {multicast_loop, false}, {multicast_if, IP}]),
    Socket.
get_socket(receiver,Port,IP,MultIP)->
    {ok,Socket} = gen_udp:open(Port, [binary, {active, false}, {reuseaddr, true}, {multicast_if, IP}, inet, {multicast_loop, false}, {add_membership, {MultIP,IP}}]),
    Socket.

listen(Socket) ->
        receive
                {udp,Socket,Host,Port,Bin} = Message ->
                io:format("server received:~p~n",[Message]),
                listen(Socket)
        end.

getElemFromDatapackage(Index,Datapackage) ->
	getElemFromDatapackage(Index, Index, Datapackage).

getElemFromDatapackage(IndexStart, IndexEnd, Datapackage) ->
	lists:sublist(Datapackage,IndexStart+1,(IndexEnd+1)-(IndexStart)).

% now_to_universal_time(erlang:timestamp())

% Socket=tools:get_socket(sender,Port,Ip),
% gen_udp:controlling_process(Socket,self()),

% Socket=tools:get_socket(receiver,Port,Ip,MultIp),
% gen_udp:controlling_process(Socket,self()),

% gen_udp:close(Socket),