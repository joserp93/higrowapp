%%%-------------------------------------------------------------------
%% @doc `Higrow App' top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(higrow_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

-define(INTENSITY, 10).
-define(PERIOD, 1_000).

%% API

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% supervisor callbacks

%% @private
init([]) ->
    Config = higrow_config:read_config(),

    ChildSpecs =
        [
            worker(higrow_mqtt, Config, [])
        ],

    SupFlags = {one_for_one, ?INTENSITY, ?PERIOD},
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions

worker(Mod, Config, Opts) ->
    Args = proplists:get_value(Mod, Config),
    {Mod, {Mod, start_link, [Args ++ Opts]},
     permanent, brutal_kill, worker, [Mod]}.