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

}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% @hidden
init([]) ->
    {ok, Devices} = usb:get_device_list(),
    % DeviceInfo = [{Device, usb:get_device_descriptor(Device)} || Device <- Devices],
    % io:format("DeviceInfo: ~p~n", [DeviceInfo]),
    usb:monitor_hotplug(),
    % {_ok, _DeviceHandle} = usb:open_device(Device1),
    {ok, #state{
    }}.


%% @hidden
handle_call(Request, _From, State) ->
    {reply, {error, {unhandled_call, Request}}, State}.


%% @hidden
handle_cast(_Request, State) ->
    io:format("_Request: ~p~n", [_Request]),
    {noreply, State}.


%% @hidden
handle_info({usb, device_left, Device}, State) ->
    io:format("USB_DEVICE_LEFT: ~p~n", [Device]),
    {noreply, State};

handle_info({usb, device_arrived, Device}, State) ->
    io:format("USB_DEVICE_ARRIVED: ~p~n", [Device]),
    DeviceInfo = usb:get_device_descriptor(Device),
    io:format("Device Descriptor: ~p~n", [DeviceInfo]),
    {ok, DeviceHandle} = usb:open_device(Device),
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
