%% @private
-module(usb_example_hotplug).
-export([
    start_link/0,
    stop/1
]).

-behaviour(gen_statem).
-export([
    init/1,
    callback_mode/0,
    code_change/4,
    handle_event/4,
    terminate/3
]).

-record(data, {
    hotplug_handle :: usb:hotplug_monitor()
}).

% -spec start_link() -> {ok, pid()}.
start_link() ->
    gen_statem:start_link(?MODULE, [], []).


% -spec stop(pid()) -> ok.
stop(Connection) ->
    gen_statem:stop(Connection).

%%=================================================
%% gen_statem callbacks
%%=================================================

%% @hidden
init([]) ->
    error_logger:info_msg("[usb-sensor-discovery] client started ~n"),
    case usb:monitor_hotplug([enumerate]) of
        {ok, HotplugHnd} ->
            error_logger:info_msg("[usb-sensor-discovery] Registered for hotplug event. ~n"),
            {ok, listening, #data{
                hotplug_handle = HotplugHnd
            }};
        {error, Reason} ->
            error_logger:error_msg("[usb-sensor-discovery] Error registering for hotplug events. ~n"),
            {stop, Reason}
    end.


%% @hidden
handle_event(_EventType, {usb, device_arrived,Device}, _State, _Data) ->
    error_logger:info_msg("[usb-sensor-discovery] Device arrived"),
    {ok, DeviceDesp} = usb:get_device_descriptor(Device),
    {ok, ConfigDesp} = usb:get_configuration_descriptor(Device, 0),
    VendorID = maps:get(vendor_id, DeviceDesp),
    ProductID = maps:get(product_id, DeviceDesp),
    case get_sensor_collector(VendorID, ProductID) of
        {ok, usb_bed_occupancy} ->
            error_logger:info_msg("[usb-sensor-discovery] Device supported ~n");
        {error, not_supported} ->
            error_logger:info_msg("[usb-sensor-discovery] Device not supported ~n")
        end,
    error_logger:info_msg("[usb-sensor-discovery] VID:~p PID:~p~n", [VendorID, ProductID]),
    error_logger:info_msg("[usb-sensor-discovery] Device Desp: ~p~n", [DeviceDesp]),
    error_logger:info_msg("[usb-sensor-discovery] Config Desp: ~p~n", [ConfigDesp]),
    keep_state_and_data;

handle_event(_EventType, {usb, device_left,Device}, _State, _Data) ->
    {ok, DeviceDesp} = usb:get_device_descriptor(Device),
    VendorID = maps:get(vendor_id, DeviceDesp),
    ProductID = maps:get(product_id, DeviceDesp),    
    error_logger:info_msg("[usb-sensor-discovery] Device left VID:~p PID:~p~n", [VendorID, ProductID]),
    keep_state_and_data;

handle_event(EventType, EventContent, State, _StateData) ->
    error_logger:info_msg("[usb-sensor-discovery] Unhandled event ~p(~p) in state ~p", [EventType, EventContent, State]),
    keep_state_and_data.

%% @hidden
callback_mode() ->
    handle_event_function.

%% @hidden
terminate(_Reason, _State, _StateData) ->
    error_logger:info_msg("[usb-sensor-discovery] client stopping~n"),
    ok.

%% @hidden
code_change(_Vsn, State, Data, _Extra) ->
    {ok, State, Data}.

%% @helper
get_sensor_collector(16#10c4, 16#0002) ->
    {ok, usb_bed_occupancy};

get_sensor_collector(_VendorID, _ProductID) ->
    {error, not_supported}.
