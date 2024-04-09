%%%-------------------------------------------------------------------
%% @doc `Higrow App' Application public API and callbacks.
%% @end
%%%-------------------------------------------------------------------

-module(higrow_app).

-behaviour(application).

-export([start/2, stop/1]).

%% application callbacks

%% @doc Start the `higrow_app' application
-spec start(StartType :: atom(), StartArgs :: []) -> application:start_type().
start(_StartType, _StartArgs) ->
    higrow_sup:start_link().

%% @doc Stop the `higrow_app' application
-spec stop(State :: term()) -> ok.
stop(_State) ->
    ok.