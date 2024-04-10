%%
%% Copyright (c) 2024 <jose.rodriguez@intuitivo.com>
%% All rights reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
-module(higrow).

-include_lib("kernel/include/logger.hrl").

-export([start/0]).

%% AtomVM main function

%% @doc Main AtomVM entrypoint.
%%
%% This function will prepare configuration, logging and wireless
%% networking facilities before starting the application, and after
%% that will loop forever taking the device readings and dispatching
%% them to the gateway.
start() ->
    Config = higrow_config:read_config(),
    WiFiConfig = proplists:get_value(higrow_wifi, Config),
    MqttConfig = proplists:get_value(higrow_mqtt, Config),

    {ok, _} = logger_manager:start_link(#{}),
    {ok, {IP, _, _}} = higrow_wifi:start(WiFiConfig),
    {ok, _} = higrow_mqtt:start(MqttConfig),
    {ok, _} = higrow_app:start(normal, []),

    ?LOG_NOTICE("entering loop..."),
    loop(#{sleep_ms => 50}).

%% Internal functions

%% @hidden
loop(State) ->
    #{sleep_ms := SleepMs} = State,
    _ = timer:sleep(SleepMs),
    loop(State).
