%% @author joschka
%% @doc @todo Add description to sender.


-module(sender).
-import(tools,[get_socket/3]).
-import(werkzeug,[logging/2]).
%% ====================================================================
%% API functions
%% ====================================================================
-export([senderStart/2]).

%% ====================================================================
%% Internal functions
%% ====================================================================

log(String, Liste) ->
	logging("sender_log.log", io_lib:format(String ++ "~n", Liste)).

%% Startmethode des Senders, öffnet das UDP-Socket
senderStart(MultiIP,Port) ->
	register('sender', self()),
	Socket = get_socket(sender,Port,MultiIP),
	log("Der Sender ist gestartet!", []),			
	senderLoop(MultiIP,Port,Socket),
	ok.

senderLoop(MultiIP,Port,Socket) ->
	gen_udp:send(Socket, MultiIP, Port, "Hallo welt"),
	senderLoop(MultiIP,Port,Socket).


%%	gen_Server Beispiel 
%%
%% -module(ch3).
%% -behaviour(gen_server).
%% 
%% -export([start_link/0]).
%% -export([alloc/0, free/1]).
%% -export([init/1, handle_call/3, handle_cast/2]).
%% 
%% start_link() ->
%%     gen_server:start_link({local, ch3}, ch3, [], []).
%% 
%% alloc() ->
%%     gen_server:call(ch3, alloc).
%% 
%% free(Ch) ->
%%     gen_server:cast(ch3, {free, Ch}).
%% 
%% init(_Args) ->
%%     {ok, channels()}.
%% 
%% handle_call(alloc, _From, Chs) ->
%%     {Ch, Chs2} = alloc(Chs),
%%     {reply, Ch, Chs2}.
%% 
%% handle_cast({free, Ch}, Chs) ->
%%     Chs2 = free(Ch, Chs),
%%     {noreply, Chs2}.