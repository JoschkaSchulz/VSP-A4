%% @author abd447
%% @doc @todo Add description to start.


-module(starter).
-behaviour(gen_server).
-import('listener',[listenerStart/2]).
-import('sender',[senderStart/2]).
%% ====================================================================
%% API functions
%% ====================================================================
-export([start/2]).



%% ====================================================================
%% Internal functions
%% ====================================================================

% Startet Sender und Listener
start(IPAddr, Port) ->
	spawn(fun() -> senderStart(IPAddr,Port) end),
	spawn(fun() -> listenerStart(IPAddr,Port) end),
	start_link().

start_link() ->
    gen_server:start_link({local, station}, station, [], []).

alloc() ->
    gen_server:call(station, alloc).

free(Ch) ->
    gen_server:cast(ch3, {free, Ch}).

init(_Args) ->
    {ok, channels()}.

handle_call(alloc, _From, Chs) ->
    {Ch, Chs2} = alloc(Chs),
    {reply, Ch, Chs2}.

handle_cast({free, Ch}, Chs) ->
    Chs2 = free(Ch, Chs),
    {noreply, Chs2}.


% now_to_universal_time(erlang:timestamp())

% Socket=tools:get_socket(sender,Port,Ip),
% gen_udp:controlling_process(Socket,self()),

% Socket=tools:get_socket(receiver,Port,Ip,MultIp),
% gen_udp:controlling_process(Socket,self()),

% gen_udp:close(Socket),