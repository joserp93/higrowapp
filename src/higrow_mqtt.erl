%%%-------------------------------------------------------------------
%% @doc Mqtt public API and configuration.
%%
%% The current implementation only allows for the `higrow_app' to connect
%% to an specific mqtt server.
%% @end
%%%-------------------------------------------------------------------

-module(higrow_mqtt).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2]).

-define(SERVER, ?MODULE).

-type mqtt_option() ::
        {url, Url :: string()}
      | {user, User :: string()}
      | {psk, Psk :: string()}
      | {topic, Topic :: binary()}.
%% Mqtt configuration option

-type config() :: [mqtt_option()].
%% Default configuration for `higrow_mqtt'

%% @doc Start and link the Mqtt server
-spec start_link(Config :: config()) -> gen_server:start_ret().
start_link(Config) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Config, []).

%% @private
init(Config) ->
    %% Start the MQTT client.

    Url = proplists:get_value(url, Config),
    User = proplists:get_value(user, Config),
    Psk = proplists:get_value(psk, Config),
    Topic = proplists:get_value(topic, Config),

    FormattedUrl = io_lib:format("mqtt://~s:~s@~s", [User, Psk, Url]),

    MQTTConfig = #{
        url => FormattedUrl,
        connected_handler => fun handle_connected/1,
        disconnected_handler => fun handle_disconnected/1,
        error_handler => fun handle_error/2
    },

    ?LOG_NOTICE("MQTT started ~p", [FormattedUrl]),
    {ok, MQTT} = mqtt_client:start(MQTTConfig),

    State = #{
        mqtt => MQTT,
        topic => Topic
    },

    {ok, State}.

%% @private
handle_info(pub_data, #{mqtt := Mqtt, topic := Topic} = State) ->
    PublishOptions = #{
    qos => exactly_once,
    published_handler => fun handle_publish/3
    },
    Message = <<"{temperature: 25}">>,
    ok = mqtt_client:publish(Mqtt, Topic, Message, PublishOptions),
    _Timer = timer_manager:send_after(10000, self(), pub_data),
    {noreply, State};
handle_info(_Msg, State) ->
    {noreply, State}.

%% @private
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_connected(MQTT) ->
    Config = mqtt_client:get_config(MQTT),
    ?LOG_NOTICE("Connected to ~p", [maps:get(url, Config)]),
    _Timer = timer_manager:send_after(10000, self(), pub_data).
    
handle_disconnected(_MQTT) ->
    ?LOG_NOTICE("Disconnected from broker", []).

handle_error(_MQTT, Error) ->
    ?LOG_NOTICE("Disconnected from broker ~p", [Error]).

handle_publish(_MQTT, Topic, MsgId) ->
    ?LOG_NOTICE("Message ~p was published to topic ~p", [MsgId, Topic]).



