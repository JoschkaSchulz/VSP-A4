%% @author abd447
%% @doc @todo Add description to Listener.


-module(listener).
-import(tools,[get_socket/3,get_socket/4]).
-import(werkzeug, [logging/2]).
%% ====================================================================
%% API functions
%% ====================================================================
-export([listenerStart/2]).



%% ====================================================================
%% Internal functions
%% ====================================================================

log(String, Liste) ->
	logging("listener_log.log", io_lib:format(String ++ "~n", Liste)).

%% Startmethode des Listeners, öffnet das UDP-Socket
listenerStart(IPAddr,Port) ->
	register('listener', self()),
	log("Der Listener ist gestartet!", []),			
	%Socket = get_socket(receiver,Port,IPAddr,{225,10,1,2}),
	Socket = get_socket(sender,Port,IPAddr),
	listenerLoop(Socket),
	ok.

%% Lauschen auf dem Socket
listenerLoop(Socket) ->
	case gen_udp:recv(Socket, 0) of
		{ok, {Address, Port, Packet}} ->
			log("Eine Nachricht von ~p~p mit dem Inhalt ~s", [Address, Port, Packet]),
			ok;
		{error, Reason} ->
			log("Fehler: ~p", [Reason]),
			error
	end.
		