%% @author joschka
%% @doc @todo Add description to tools.


-module(tools).

%% ====================================================================
%% API functions
%% ====================================================================
-export([get_socket/3,get_socket/4,randomElem/1,getElemFromDatapackage/2,getElemFromDatapackage/3]).

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

%% Wählt aus einer Liste von Zeitslots einen zufälligen Slot aus
randomElem(List) -> 
	lists:nth(random:uniform(length(List)), List).

%% Gibt das Byte an Stelle 'Index' im Datapackage zurück
getElemFromDatapackage(Index,Datapackage) ->
	getElemFromDatapackage(Index, Index, Datapackage).

%% Gibt die Bytes von IndexStart bis IndexEnde im Datapackage zurück
getElemFromDatapackage(IndexStart, IndexEnd, Datapackage) ->
	lists:sublist(Datapackage,IndexStart+1,(IndexEnd+1)-(IndexStart)).
