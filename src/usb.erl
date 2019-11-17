-module(usb).
-export([
    get_device_list/0
]).
-export([
    get_device_descriptor/1,
    get_configuration_descriptor/2
]).
-export_type([
    device/0
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
