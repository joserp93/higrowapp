%%%-------------------------------------------------------------------
%% @doc Mqtt public API and configuration.
%%
%% The current implementation only allows for the `higrow_app' to connect
%% to an specific mqtt server.
%% @end
%%%-------------------------------------------------------------------

-module(higrow_mqtt).

-include_lib("kernel/include/logger.hrl").

-export([start/1]).

-type mqtt_option() ::
        {url, Url :: binary()}
      | {user, User :: binary()}
      | {client, Client :: binary()}
      | {topic, Topic :: binary()}.
%% Mqtt configuration option

-type config() :: [mqtt_option()].
%% Default configuration for `higrow_mqtt'

-export_type([config/0]).

-spec start(Config :: config()) -> {ok, MQTT :: pid()} | {error, Reason :: term()}.
start(Config) ->
    %% Start the MQTT client.

    Url = proplists:get_value(url, Config),
    User = proplists:get_value(user, Config),
    Psk = proplists:get_value(psk, Config),
    Client = proplists:get_value(client, Config),

    MQTTConfig = #{
        url => Url,
        connected_handler => fun handle_connected/1,
        disconnected_handler => fun handle_disconnected/1,
        error_handler => fun handle_error/2,
        username => User,
        password => Psk,
        client_id => Client
    },

    case mqtt_client:start(MQTTConfig) of
        {ok, _Pid} ->
            ?LOG_NOTICE("MQTT started", []),
            {ok, _Pid};
        {error, reason} ->
            ?LOG_ERROR("an error happened: ~p", [reason]),
            {error, reason}
    end.

handle_connected(MQTT) ->
    Config = mqtt_client:get_config(MQTT),
    ?LOG_NOTICE("Connected to ~p", [maps:get(url, Config)]),
    
    AppConfig = higrow_config:read_config(),
    MqttConfig = proplists:get_value(higrow_mqtt, AppConfig),
    Topic = proplists:get_value(topic, MqttConfig),
    spawn(fun() -> publish_loop(MQTT, Topic) end).
    
handle_disconnected(_MQTT) ->
    ?LOG_NOTICE("Disconnected from broker", []).

handle_error(_MQTT, _Error) ->
    ?LOG_NOTICE("Disconnected from broker", []).

publish_loop(MQTT, Topic) ->
    ?LOG_NOTICE("Publishing data on topic ~p", [Topic]),
    try
        Self = self(),
        HandlePublished = fun(MQTT2, Topic2, MsgId) ->
            Self ! published,
            handle_published(MQTT2, Topic2, MsgId)
        end,
        PublishOptions = #{qos => at_least_once, published_handler => HandlePublished},
        Message = "{temperature: 25}",
        _ = mqtt_client:publish(MQTT, Topic, Message, PublishOptions),
        receive
            published ->
                ok
        after 10000 ->
            ?LOG_WARNING("Timed out waiting for publish ack")
        end
    catch
        C:E:S ->
            ?LOG_ERROR("Error in publish: ~p:~p~p", [C, E, S])
    end,
    timer:sleep(5000),
    publish_loop(MQTT, Topic).

handle_published(MQTT, Topic, MsgId) ->
    ?LOG_NOTICE("MQTT ~p published to topic ~p msg_id=~p", [MQTT, Topic, MsgId]).



