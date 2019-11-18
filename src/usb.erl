-module(usb).
-export([
    get_device_list/0
]).
-export([
    get_device_descriptor/1,
    get_configuration_descriptor/2
]).
-export([
    open_device/1,
    close_device/1,
    claim_interface/2,
    release_interface/2
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
% Device List API
%

-spec get_device_list() -> {ok, [device()]} | {error, term()}.
-opaque device() :: reference().
get_device_list() ->
    get_device_list_nif().

%% nif
get_device_list_nif() ->
    erlang:nif_error(not_loaded).


%
% Device API
%

-spec get_device_descriptor(device()) -> {ok, device_descriptor()} | {error, term()}.
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
get_device_descriptor(Device) ->
    get_device_descriptor_nif(Device).

%% nif
get_device_descriptor_nif(_Device) ->
    erlang:nif_error(not_loaded).


-spec get_configuration_descriptor(device(), byte()) -> {ok, configuration_descriptor()} | {error, term()}.
-type configuration_descriptor() :: #{
    configuration_number := byte(),
    num_interfaces := byte(),
    description_string_index := byte(),
    attributes := byte(),
    max_power := byte(),
    interfaces := [interface_descriptor()],
    extra := binary()
}.
-type interface_descriptor() :: #{
    % bLength := non_neg_integer(),
    % bDescriptorType := non_neg_integer(),
    % bInterfaceNumber := non_neg_integer(),
    % bAlternateSetting := non_neg_integer(),
    % bNumEndpoints := non_neg_integer(),
    % bInterfaceClass := non_neg_integer(),
    % bInterfaceSubClass := non_neg_integer(),
    % bInterfaceProtocol := non_neg_integer(),
    % iInterface := non_neg_integer(),
    % endpoints := [endpoint_descriptor()],
    % extra := non_neg_integer(),
    % extra_length := non_neg_integer()
}.
% -type endpoint_descriptor() :: #{
%     bLength := non_neg_integer(),
%     bDescriptorType := non_neg_integer(),
%     bEndpointAddress := non_neg_integer(),
%     bmAttributes := non_neg_integer(),
%     wMaxPacketSize := non_neg_integer(),
%     binterval := non_neg_integer(),
%     bRefresh := non_neg_integer(),
%     bSynchAddress := non_neg_integer(),
%     extra := non_neg_integer(),
%     extra_length := non_neg_integer()
% }.
get_configuration_descriptor(Device, ConfigIndex) ->
    get_configuration_descriptor_nif(Device, ConfigIndex).

%% nif
get_configuration_descriptor_nif(_Device, _ConfigIndex) ->
    erlang:nif_error(not_loaded).


-spec open_device(device()) -> {ok, device_handle()} | {error, term()}.
-opaque device_handle() :: reference().
open_device(Device) ->
    open_device_nif(Device).

open_device_nif(_Device) ->
    erlang:nif_error(not_loaded).


-spec close_device(device_handle()) -> ok | {error, term()}.
close_device(DeviceHandle) ->
    close_device_nif(DeviceHandle).

close_device_nif(_DeviceHandle) ->
    erlang:nif_error(not_loaded).


-spec claim_interface(device_handle(), non_neg_integer()) -> ok | {error, term()}.
claim_interface(DeviceHandle, InterfaceNumber) ->
    claim_interface_nif(DeviceHandle, InterfaceNumber).

claim_interface_nif(_DeviceHandle, _InterfaceNumber) ->
    erlang:nif_error(not_loaded).



-spec release_interface(device_handle(), non_neg_integer()) -> ok | {error, term()}.
release_interface(DeviceHandle, InterfaceNumber) ->
    release_interface_nif(DeviceHandle, InterfaceNumber).

release_interface_nif(_DeviceHandle, _InterfaceNumber) ->
    erlang:nif_error(not_loaded).


%
% Hotplug API
%

%% @doc start listening for hotplug events. The owner process will
%%      start receiving hotplug events such as:
%%      `{usb, Context, {hotplug, device_arrived, Device}}` and
%%      `{usb, Context, {hotplug, device_left, Device}}`.
-spec monitor_hotplug() -> {ok, hotplug_monitor()} | {error, term()}.
-opaque hotplug_monitor() :: reference().
monitor_hotplug() ->
    monitor_hotplug([]).

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


-spec demonitor_hotplug(hotplug_monitor()) -> ok | {error, term()}.
demonitor_hotplug(HotplugEvents) ->
    demonitor_hotplug_nif(HotplugEvents).

%% nif
demonitor_hotplug_nif(_HotplugEvents) ->
    erlang:nif_error(not_loaded).


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
