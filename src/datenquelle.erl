%% @author Kazura
%% @doc @todo Add description to datenquelle.


-module(datenquelle).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).



%% ====================================================================
%% Internal functions
%% ====================================================================

datenquelle_start() ->
	datenquelle_loop(),
	ok.

datenquelle_loop() ->
	receive
		{sendeNutzdaten, Station} ->
			ok
	end.