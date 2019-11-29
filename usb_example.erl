%% @private
-module(usb_example).
-export([
]).
-export([
    start_link/0
]).

-behaviour(gen_server).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {
    devhnd :: term(),
    tref :: reference()
}).

-define(VENDOR_ID, 16#10c4).
-define(PRODUCT_ID, 16#0002).
-define(INTERVAL, 1000).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% @hidden
init([]) ->
    {ok, Devices} = usb:get_device_list(),
    DeviceInfo = [{Device, usb:get_device_descriptor(Device)} || Device <- Devices],
    InterestedDevices = [{Device, Info} || {Device, {ok, #{vendor_id := ?VENDOR_ID, product_id := ?PRODUCT_ID}=Info}} <- DeviceInfo],
    [Device | _] = InterestedDevices,
    DevRef = element(1, Device),
    usb:get_configuration_descriptor(DevRef, 0),
    {ok, DeviceHnd} = usb:open_device(DevRef),
    usb:detach_kernel_driver(DeviceHnd, 0),
    usb:set_configuration(DeviceHnd, 1),
    usb:claim_interface(DeviceHnd, 0),
    {ok, _} = usb:write_control(DeviceHnd, 33, 16#0a, 0, 0, <<>>, 0),
    TRef = erlang:start_timer(?INTERVAL, self(), trigger),
    {ok, #state{
        devhnd = DeviceHnd,
        tref = TRef
    }}.
    % usb:monitor_hotplug(),
    % {_ok, _DeviceHandle} = usb:open_device(Device1),


%% @hidden
handle_call(Request, _From, State) ->
    {reply, {error, {unhandled_call, Request}}, State}.


%% @hidden
handle_cast(_Request, State) ->
    io:format("_Request: ~p~n", [_Request]),
    {noreply, State}.


%% @hidden

handle_info({timeout, _Ref, trigger}, State) ->
    case usb:read_interrupt(State#state.devhnd, 16#81, 8, 2000) of
        {ok, BedStatus} ->
            io:fwrite("Bed Status~p~n", [BedStatus]);
        {error, timeout, <<>>} ->
            io:fwrite("Timeout error ~n");
        {error, timeout, _Data} ->
            io:fwrite("Timeout error Data~n");
        {error, no_device} ->
            io:fwrite("Device removed ")
    end,
    TRef = erlang:start_timer(?INTERVAL, self(), trigger),
    NewState = State#state{tref = TRef},
    {noreply, NewState};

handle_info({usb, device_left, Device}, State) ->
    io:format("USB_DEVICE_LEFT: ~p~n", [Device]),
    {noreply, State};

handle_info({usb, device_arrived, Device}, State) ->
    io:format("USB_DEVICE_ARRIVED: ~p~n", [Device]),
    DeviceInfo = usb:get_device_descriptor(Device),
    io:format("Device Descriptor: ~p~n", [DeviceInfo]),
    {ok, _DeviceHandle} = usb:open_device(Device),
    {noreply, State};
        
handle_info(_Info, State) ->
    {noreply, State}.


%% @hidden
terminate(_Reason, #state{}) ->
    ok.


%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% Helpers
