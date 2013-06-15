%% @author Kazura
%% @doc @todo Add description to datenquelle.


-module(datenquelle).

%% ====================================================================
%% API functions
%% ====================================================================
-export([datenquelle_start/0]).



%% ====================================================================
%% Internal functions
%% ====================================================================

datenquelle_start() ->
	datenquelle_loop(),
	ok.

datenquelle_loop() ->
	receive
		{sendeNutzdaten, Station} ->
			Station ! "012345678901234567890123"
	end.