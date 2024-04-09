%%%-------------------------------------------------------------------
%% @doc Mqtt public API and configuration.
%%
%% The current implementation only allows for the `higrow_app' to connect
%% to an specific mqtt server.
%% @end
%%%-------------------------------------------------------------------

-module(higrow_mqtt).

-include_lib("kernel/include/logger.hrl").

-export([start/0]).

-spec start_link(Config :: config()) -> gen_server:start_ret().
start_link(Config) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Config, []).

%% @private
init(Config) ->
    %%
    %% Start the MQTT client.
    %%

    Url = proplists:get_value(url, Config),
    User = proplists:get_value(user, Config),
    Psk = proplists:get_value(psk, Config),
    Client = proplists:get_value(client, Config),

    Config = #{
        url => Url,
        username => User,
        password => Psk,
        client_id => Client,
        connected_handler => fun handle_connected/1
    },
    io:format("MQTT started.~n"),
    mqtt_client:start(Config).

handle_connected(MQTT) ->
    Config = mqtt_client:get_config(MQTT),
    Topic = <<"atomvm/qos0">>,
    io:format("Connected to ~p~n", [maps:get(url, Config)]),
    io:format("Subscribing to ~p...~n", [Topic]),
    ok = mqtt_client:subscribe(MQTT, Topic, #{
        subscribed_handler => fun handle_subscribed/2,
        data_handler => fun handle_data/3
    }).

handle_subscribed(MQTT, Topic) ->
    io:format("Subscribed to ~p.~n", [Topic]),
    io:format("Spawning publish loop on topic ~p~n", [Topic]),
    spawn(fun() -> publish_loop(MQTT, Topic, 1) end).

handle_data(_MQTT, Topic, Data) ->
    io:format("Received data on topic ~p: ~p ~n", [Topic, Data]),
    ok.

publish_loop(MQTT, Topic, Seq) ->
    io:format("Publishing data on topic ~p~n", [Topic]),
    try
        Self = self(),
        HandlePublished = fun(MQTT2, Topic2, MsgId) ->
            Self ! published,
            handle_published(MQTT2, Topic2, MsgId)
        end,
        PublishOptions = #{qos => at_least_once, published_handler => HandlePublished},
        Msg = list_to_binary("echo" ++ integer_to_list(Seq)),
        _ = mqtt_client:publish(MQTT, Topic, Msg, PublishOptions),
        receive
            published ->
                ok
        after 10000 ->
            io:format("Timed out waiting for publish ack~n")
        end
    catch
        C:E:S ->
            io:format("Error in publish: ~p:~p~p~n", [C, E, S])
    end,
    timer:sleep(5000),
    publish_loop(MQTT, Topic, Seq + 1).

handle_published(MQTT, Topic, MsgId) ->
    io:format("MQTT ~p published to topic ~p msg_id=~p~n", [MQTT, Topic, MsgId]).