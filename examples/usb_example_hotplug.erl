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
    monitor     :: usb:hotplug_monitor(),
    devices     :: [usb:device()]
}).


-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_statem:start_link(?MODULE, [], []).


-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_statem:stop(Pid).

%%=================================================
%% gen_statem callbacks
%%=================================================

%% @hidden
init([]) ->
    error_logger:info_msg("client started ~n"),
    case usb:monitor_hotplug([enumerate]) of
        {ok, Monitor} ->
            error_logger:info_msg("Registered for hotplug event. ~n"),
            {ok, listening, #data{
                monitor = Monitor,
                devices = []
            }};
        {error, Reason} ->
            error_logger:error_msg("Error registering for hotplug events. ~n"),
            {stop, Reason}
    end.


%% @hidden
handle_event(_EventType, {usb, device_arrived, Device}, _State, Data) ->
    {ok, DeviceDesc} = usb:get_device_descriptor(Device),
    VendorID = maps:get(vendor_id, DeviceDesc),
    ProductID = maps:get(product_id, DeviceDesc),
    error_logger:info_msg("Device arrived: ~p VID:~p PID:~p~n", [Device, VendorID, ProductID]),
    {keep_state, Data#data{
        devices = [Device | Data#data.devices]
    }};

handle_event(_EventType, {usb, device_left, Device}, _State, Data) ->
    {ok, DeviceDesc} = usb:get_device_descriptor(Device),
    VendorID = maps:get(vendor_id, DeviceDesc),
    ProductID = maps:get(product_id, DeviceDesc),
    error_logger:info_msg("Device left: ~p VID:~p PID:~p~n", [Device, VendorID, ProductID]),
    {keep_state, Data#data{
        devices = lists:delete(Device, Data#data.devices)
    }};

handle_event(EventType, EventContent, State, _StateData) ->
    error_logger:info_msg("Unhandled event ~p(~p) in state ~p", [EventType, EventContent, State]),
    keep_state_and_data.

%% @hidden
callback_mode() ->
    handle_event_function.

%% @hidden
terminate(_Reason, _State, _StateData) ->
    ok.

%% @hidden
code_change(_Vsn, State, Data, _Extra) ->
    {ok, State, Data}.