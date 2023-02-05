%% @doc Erlang interface for interacting with USB devices.
-module(usb).
-export([
    get_device_list/0
]).
-export([
    get_bus_number/1,
    get_port_number/1,
    get_port_numbers/1,
    get_device_address/1,
    get_device_speed/1,
    get_device_descriptor/1,
    get_config_descriptor/2
]).
-export([
    open_device/1,
    close_device/1,
    claim_interface/2,
    release_interface/2,
    clear_halt/2,
    set_configuration/2,
    read_bulk/4,
    write_bulk/4,
    read_interrupt/4,
    write_interrupt/4,
    read_control/7,
    write_control/7,
    attach_kernel_driver/2,
    detach_kernel_driver/2
]).
-export([
    monitor_hotplug/0,
    monitor_hotplug/1,
    demonitor_hotplug/1
]).
-export_type([
    device/0,
    device_handle/0,
    hotplug_monitor/0
]).
-on_load(init/0).

%
% Types
%

-opaque device() :: reference(). 
%% Reference to a USB device attached to the host system.

-type device_speed() :: unknown | low | full | high | super | super_plus.
%% Indicates the speed at which the device is operating.
%% 
%% <dl>
%%  <dt>`unknown'</dt>
%%  <dd>The OS doesn't report or know the device speed.</dd>
%%  <dt>`low'</dt>
%%  <dd>The device is operating at low speed (1.5MBit/s).</dd>
%%  <dt>`full'</dt>
%%  <dd>The device is operating at full speed (12MBit/s).</dd>
%%  <dt>`high'</dt>
%%  <dd>The device is operating at high speed (480MBit/s).</dd>
%%  <dt>`super'</dt>
%%  <dd>The device is operating at super speed (5000MBit/s).</dd>
%%  <dt>`super_plus'</dt>
%%  <dd>The device is operating at super speed plus (10000MBit/s).</dd>
%% </dl>

-type device_descriptor() :: #{
    usb_version := 0..16#ffff,
    class_code := byte(),
    sub_class_code := byte(),
    protocol_code := byte(),
    max_packet_size0 := byte(),
    vendor_id := 0..16#ffff,
    product_id := 0..16#ffff,
    device_version := 0..16#ffff,
    manufacturer_string_index := byte(),
    product_string_index := byte(),
    serial_number_string_index := byte(),
    num_configurations := byte()
}.
%% A map representing the standard USB device descriptor. This descriptor is
%% documented in section 9.6.1 of the USB 3.0 specification.

-opaque device_handle() :: reference().
%% A referenced to an open USB device. This handle is used to perform I/O and
%% other operations. When finished with a device handle, you should call
%% {@link close_device/1}.

%
% Device List API
%

%% @doc Returns a list of USB devices currently attached to the system.
-spec get_device_list() -> {ok, [device()]} | {error, term()}.
get_device_list() ->
    get_device_list_nif().

%% nif
get_device_list_nif() ->
    erlang:nif_error(not_loaded).

%% @doc Get the number of the bus that a device is connected to.
-spec get_bus_number(device()) -> {ok, 0..255} | {error, term()}.
get_bus_number(Device) ->
    get_bus_number_nif(Device).

%% nif
get_bus_number_nif(_Device) ->
    erlang:nif_error(not_loaded).

%% @doc Get the number of the port that a device is connected to.
-spec get_port_number(device()) -> {ok, 0..255} | {error, term()}.
get_port_number(Device) ->
    get_port_number_nif(Device).

%%nif
get_port_number_nif(_Device) ->
    erlang:nif_error(not_loaded).

%% @doc Get the list of all port numbers from root for the specified device.
-spec get_port_numbers(device()) -> {ok, [0..255]} | {error, term()}.
get_port_numbers(Device) ->
    get_port_numbers_nif(Device).

%% nif
get_port_numbers_nif(_Device) ->
    erlang:nif_error(not_loaded).

%% @doc Get the address of the device on the bus it is connected to.
-spec get_device_address(device()) -> {ok, 0..255} | {error, term()}.
get_device_address(Device) ->
    get_device_address_nif(Device).

%% nif
get_device_address_nif(_Device) ->
    erlang:nif_error(not_loaded).

%% @doc Get the negotiated connection speed for a device.
-spec get_device_speed(device()) -> {ok, device_speed()} | {error, term()}.
get_device_speed(Device) ->
    get_device_speed_nif(Device).

%% nif
get_device_speed_nif(_Device) ->
    erlang:nif_error(not_loaded).

%
% Device API
%

%% @doc Get the USB device descriptor for a given device.
-spec get_device_descriptor(device()) -> {ok, device_descriptor()} | {error, term()}.
get_device_descriptor(Device) ->
    get_device_descriptor_nif(Device).

%% nif
get_device_descriptor_nif(_Device) ->
    erlang:nif_error(not_loaded).

%% @doc Get a USB configuration descriptor based on its index.
-spec get_config_descriptor(device(), byte()) -> {ok, config_descriptor()} | {error, term()}.
-type config_descriptor() :: #{
    configuration_number := byte(),
    num_interfaces := byte(),
    description_string_index := byte(),
    attributes := byte(),
    max_power := byte(),
    interfaces := [interface_descriptor()],
    extra := binary()
}.
-type interface_descriptor() :: #{
    alt_settings := [alt_setting()]
}.
-type alt_setting() :: #{
    interface_number := non_neg_integer(),
    setting_number := non_neg_integer(),
    class_code := non_neg_integer(),
    sub_class_code := non_neg_integer(),
    protocol_code := non_neg_integer(),
    description_string_index := non_neg_integer(),
    endpoints := [endpoint()],
    extra := binary()
}.
-type endpoint() :: #{
    address := non_neg_integer(),
    attributes := non_neg_integer(),
    max_packet_size := non_neg_integer(),
    interval := non_neg_integer(),
    refresh := non_neg_integer(),
    synch_address := non_neg_integer(),
    extra := binary()
}.
get_config_descriptor(Device, ConfigIndex) ->
    get_config_descriptor_nif(Device, ConfigIndex).

%% nif
get_config_descriptor_nif(_Device, _ConfigIndex) ->
    erlang:nif_error(not_loaded).

%% @doc Open a device and obtain a device handle. A handle allows you to perform
%% I/O on the device in question.
-spec open_device(device()) -> {ok, device_handle()} | {error, term()}.
open_device(Device) ->
    open_device_nif(Device).

open_device_nif(_Device) ->
    erlang:nif_error(not_loaded).

%% @doc Close a device handle.
-spec close_device(device_handle()) -> ok | {error, term()}.
close_device(DeviceHandle) ->
    close_device_nif(DeviceHandle).

close_device_nif(_DeviceHandle) ->
    erlang:nif_error(not_loaded).

%% @doc Claim an interface on a given device handle. You must claim the
%% interface you wish to use before you can perform I/O on any of its endpoints.
%%
%% It is legal to attempt to claim an already-claimed interface, in which case
%% this function just returns `ok' without doing anything.
%% 
%% Claiming of interfaces is a purely logical operation; it does not cause any
%% requests to be sent over the bus. Interface claiming is used to instruct the
%% underlying operating system that your application wishes to take ownership of
%% the interface.
-spec claim_interface(device_handle(), non_neg_integer()) -> ok | {error, term()}.
claim_interface(DeviceHandle, InterfaceNumber) ->
    claim_interface_nif(DeviceHandle, InterfaceNumber).

claim_interface_nif(_DeviceHandle, _InterfaceNumber) ->
    erlang:nif_error(not_loaded).

%% @doc Release an interface previously claimed with {@link claim_interface/2}.
%% 
%% You should release all claimed interfaces before closing a device handle.
-spec release_interface(device_handle(), non_neg_integer()) -> ok | {error, term()}.
release_interface(DeviceHandle, InterfaceNumber) ->
    release_interface_nif(DeviceHandle, InterfaceNumber).

release_interface_nif(_DeviceHandle, _InterfaceNumber) ->
    erlang:nif_error(not_loaded).

%% @doc Clear the halt/stall condition for an endpoint.
%% 
%% Endpoints with halt status are unable to receive or transmit data until the
%% halt condition is stalled.
%% 
%% You should cancel all pending transfers before attempting to clear the halt
%% condition.
-spec clear_halt(device_handle(), byte()) -> ok | {error, term()}.
clear_halt(DeviceHandle, Endpoint) ->
    clear_halt_nif(DeviceHandle, Endpoint).

clear_halt_nif(_DeviceHandle, _Endpoint) ->
    erlang:nif_error(not_loaded).


-spec set_configuration(device_handle(), integer()) -> ok | {error, term()}.
set_configuration(DeviceHandle, Configuration) ->
    set_configuration_nif(DeviceHandle, Configuration).

set_configuration_nif(_DeviceHandle, _Configuration) ->
    erlang:nif_error(not_loaded).

-spec attach_kernel_driver(device_handle(), integer()) -> ok | {error, term()}.
attach_kernel_driver(DeviceHandle, InterfaceNumber) ->
    attach_kernel_driver_nif(DeviceHandle, InterfaceNumber).

attach_kernel_driver_nif(_DeviceHandle, _InterfaceNumber) ->
    erlang:nif_error(not_loaded).

-spec detach_kernel_driver(device_handle(), integer()) -> ok | {error, term()}.
detach_kernel_driver(DeviceHandle, InterfaceNumber) ->
    detach_kernel_driver_nif(DeviceHandle, InterfaceNumber).

detach_kernel_driver_nif(_DeviceHandle, _InterfaceNumber) ->
    erlang:nif_error(not_loaded).


-spec read_bulk(device_handle(), byte(), integer(), timeout()) -> {ok, binary()} | {error, timeout, binary()} | {error, term()}.
read_bulk(DeviceHandle, Endpoint, DataLen, Timeout) ->
    read_bulk_nif(DeviceHandle, Endpoint, DataLen, nif_timeout(Timeout)).

%% nif
read_bulk_nif(_DeviceHandle, _Endpoint, _DataLen, _Timeout) ->
    erlang:nif_error(not_loaded).

-spec write_bulk(device_handle(), byte(), binary(), timeout()) -> {ok, integer()} | {error, timeout, non_neg_integer()} | {error, term()}.
write_bulk(DeviceHandle, Endpoint, Data, Timeout) ->
    write_bulk_nif(DeviceHandle, Endpoint, Data, nif_timeout(Timeout)).

%% nif
write_bulk_nif(_DeviceHandle, _Endpoint, _Data, _Timeout) ->
    erlang:nif_error(not_loaded).

-spec read_interrupt(device_handle(), byte(), integer(), timeout()) -> {ok, binary()} | {error, timeout, binary()} | {error, term()}.
read_interrupt(DeviceHandle, Endpoint, DataLen, Timeout) ->
    read_interrupt_nif(DeviceHandle, Endpoint, DataLen, nif_timeout(Timeout)).

%% nif
read_interrupt_nif(_DeviceHandle, _Endpoint, _DataLen, _Timeout) ->
    erlang:nif_error(not_loaded).

-spec write_interrupt(device_handle(), byte(), binary(), timeout()) -> {ok, integer()} | {error, timeout, non_neg_integer()} | {error, term()}.
write_interrupt(DeviceHandle, Endpoint, Data, Timeout) ->
    write_interrupt_nif(DeviceHandle, Endpoint, Data, nif_timeout(Timeout)).

%% nif
write_interrupt_nif(_DeviceHandle, _Endpoint, _Data, _Timeout) ->
    erlang:nif_error(not_loaded).

-spec read_control(device_handle(), byte(), byte(), non_neg_integer(), non_neg_integer(), non_neg_integer(), timeout()) -> {ok, binary()} | {error, term()}.
read_control(DeviceHandle, RequestType, Request, Value, Index, ReadLen, Timeout) ->
    read_control_nif(DeviceHandle, RequestType, Request, Value, Index, ReadLen, nif_timeout(Timeout)).

%% nif
read_control_nif(_DeviceHandle,_RequestType, _Request, _Value, _Index, _ReadLen, _Timeout) ->
    erlang:nif_error(not_loaded).

-spec write_control(device_handle(), byte(), byte(), non_neg_integer(), non_neg_integer(), binary(), timeout()) -> {ok, integer()} | {error, term()}.
write_control(DeviceHandle, RequestType, Request, Value, Index, Data, Timeout) ->
    write_control_nif(DeviceHandle, RequestType, Request, Value, Index, Data, nif_timeout(Timeout)).

%% nif
write_control_nif(_DeviceHandle,_RequestType, _Request, _Value, _Index, _Data, _Timeout) ->
    erlang:nif_error(not_loaded).

%
% Hotplug API
%

%% @doc start listening for hotplug events.
%%
%% @equiv monitor_hotplug([])
-spec monitor_hotplug() -> {ok, hotplug_monitor()} | {error, term()}.
-opaque hotplug_monitor() :: reference().
monitor_hotplug() ->
    monitor_hotplug([]).

%% @doc start listening for hotplug events. The owner process will start
%% receiving the following hotplug events:
%% 
%% `{usb, device_arrived, Device}' and
%% `{usb, device_left, Device}'.
%% 
%% If the `enumerate' flag is provided, the owner process will received
%% `device_arrived' events for all devices already plugged into the host.
-spec monitor_hotplug([monitor_hotplug_flag()]) -> {ok, hotplug_monitor()} | {error, term()}.
-type monitor_hotplug_flag() :: enumerate.
monitor_hotplug(Flags) ->
    monitor_hotplug_nif(lists:foldl(fun
        (enumerate, Acc) ->
            Acc bor (1 bsl 0)
    end, 0, Flags)).

%% nif
monitor_hotplug_nif(_Flags) ->
    erlang:nif_error(not_loaded).


%% @doc Stop listening for hotplug events.
-spec demonitor_hotplug(hotplug_monitor()) -> ok | {error, term()}.
demonitor_hotplug(HotplugEvents) ->
    demonitor_hotplug_nif(HotplugEvents).

%% nif
demonitor_hotplug_nif(_HotplugEvents) ->
    erlang:nif_error(not_loaded).

%
% Helpers
%

nif_timeout(infinity) -> 0;
nif_timeout(0) -> 1;
nif_timeout(N) -> N.

%
% Initialization
%

init() ->
    case nif_path() of
        undefined ->
            ok;
        Path ->
            ok = erlang:load_nif(Path, 0)
    end.

-spec nif_path() -> string() | binary() | undefined.
nif_path() ->
    Priv = case code:priv_dir(usb) of
        {error, bad_name} ->
            case code:which(?MODULE) of
                File when is_list(File) ->
                    filename:join([filename:dirname(File), "../priv"]);
                _ ->
                    "../priv"
            end;
        Dir ->
            Dir
    end,
    nif_path(os:type(), Priv).


nif_path(_, Dir) ->
    filename:join([Dir, "usb_nif"]).
