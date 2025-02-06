# usb

An Erlang application for interfacing with USB peripherals using libusb.

This application allows for discovering and communicating with USB peripherals by making use of libusb through a NIF interface.

# Dependencies

This Erlang application library depends on:

* `pkg-config` (build only)
* `libusb` ≥ 1.0.16

# Setup

You need to add `usb` as a dependency to your project. If you are using `rebar3`, you can add the following to your `rebar.config`:

```erlang
{deps, [
    {usb, "0.2.1"}
]}.
```

Also ensure that `usb` is added as a dependency to your application, by
updating your `.app.src` file:

```erlang
{application, my_app, [

    {applications, [
        kernel,
        stdlib,

        usb  % <- You need this in your applications list
    ]}
]}.
```

# Usage

## Listing and querying devices

You can list currently connected USB devices, you can use:

```erlang
> {ok, [Device|_]} = usb:get_device_list().
{ok,[#Ref<0.1223016592.607256583.114769>]}.
```

You can then query the device:

```erlang
> usb:get_device_speed(Device).
{ok, high}
> usb:get_device_descriptor(Device).
{ok, #{
    class_code => 0,
    device_version => 532,
    manufacturer_string_index => 1,
    max_packet_size0 => 64,
    num_configurations => 1,
    product_id => 33028,
    product_string_index => 0,
    protocol_code => 0,
    serial_number_string_index => 2,
    sub_class_code => 0,
    usb_version => 512,
    vendor_id => 1452
}}
> usb:get_config_descriptor(Device, 0).
{ok, #{
    attributes => 224,
    configuration_number => 1,
    description_string_index => 0,
    extra => <<>>,
    interfaces => [
        #{alt_settings => [
            #{
                class_code => 3,
                description_string_index => 4,
                endpoints => [
                    #{
                        address => 129,
                        attributes => 3,
                        extra => <<>>,
                        interval => 1,
                        max_packet_size => 8,
                        refresh => 0,
                        synch_address => 0
                    }
                ],
                extra => <<9,33,1,1,0,1,34,179,0>>,
                interface_number => 0,
                protocol_code => 0,
                setting_number => 0,
                sub_class_code => 0
            }
        ]}
    ],
    max_power => 500
}}
```

## Monitoring USB device hotplug

The following will start monotoring for USB device arrival and departure:

```erlang
> {ok, Monitor} = usb:monitor_hotplug([enumerate]).
{ok, #Ref<0.731181930.70909953.42752>}
```

The `enumerate` option will create hotplug notifications about already plugged
in devices. Without the option, only newly connected devices will be reported.

From this point on, the process monitoring USB hotplug will received one of the following messages:

* `{usb, device_arrived, Device}`
* `{usb, device_left, Device}`

where `Device` is an opaque reference representing a specific device. these references can be used to interact with the device.

