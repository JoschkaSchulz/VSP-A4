%% @author thathalas
%% @doc @todo Add description to datensenke.


-module(datensenke).
-import(werkzeug,[logging/2]).
%% ====================================================================
%% API functions
%% ====================================================================
-export([datensenke_start/0]).



%% ====================================================================
%% Datensenke
%% ====================================================================

datensenke_start() ->
	datensenke_loop(),
	ok.

datensenke_loop() ->
	receive
		{log, Log} ->
			%%logging("datensenke.log",io_lib:format("~s~n",[Log])),
			file:write_file("datensenke.log",io_lib:format("~s~n",[Log]),[append]),
			datensenke_loop()
	end.