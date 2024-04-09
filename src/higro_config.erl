%%%-------------------------------------------------------------------
%% @doc `higrow_app' configuration facilities and public API.
%%
%% The `higrow_app' configuration is stored in the card's NVS storage
%% encoded as a binary. The configuration holds all the diferent
%% options and parameters that the different application components
%% needs by default.
%% @end
%%%-------------------------------------------------------------------

-module(higrow_config).

-include("config.hrl").

-export([read_config/0,
         store_config/1,
         reset_config/0]).

-type config_option() ::
        {higrow_mqtt, higrow_mqtt:config()}
      | {higrow_wifi, higrow_wifi:config()}.
%% `higrow' configuration option

-type config() :: [config_option()].
%% Default configuration proplist for the `higrow' application

%% API

%% @doc Read the `higrow' configuration settings.
%%
%% If the configuration is not found in the NVS, it will return the
%% default configuration based on the compilation settings.
%% @end
-spec read_config() -> config().
read_config() ->
    case esp:nvs_get_binary(higrow, config) of
        undefined ->
            default_config();
        Config ->
            erlang:binary_to_term(Config)
        end.

%% @doc Store the configuration in the NVS
-spec store_config(config()) -> ok | {error, Reason :: term()}.
store_config(Config) ->
    Encoded = erlang:term_to_binary(Config),
    esp:nvs_set_binary(higrow, config, Encoded).

%% @doc Restore the configuration to the default settings
-spec reset_config() -> config().
reset_config() ->
    ok = esp:nvs_reformat(),
    Config = default_config(),
    store_config(Config),
    Config.

%% internal functions

default_config() ->
    [
     {higrow_wifi, [{ssid, ?DEFAULT_STA_SSID},
                    {psk, ?DEFAULT_STA_PSK},
                    {ntp, ?DEFAULT_NTP_HOST}]},
     {higrow_mqtt, [{url, ?DEFAULT_MQTT_URL},
                    {user, ?DEFAULT_MQTT_USER},
                    {psk, ?DEFAULT_MQTT_PSK},
                    {client, ?DEFAULT_MQTT_CLIENT}]}
    ].