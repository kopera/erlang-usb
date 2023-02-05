#include <stdbool.h>
#include <stdint.h>
#include <string.h>
#include <stdatomic.h>

#include <assert.h>

#include <erl_nif.h>
#include <libusb.h>

#include "khash.h"


#define ARRAY_LENGTH(x) \
    ((sizeof(x)/sizeof(0[x])) / ((size_t)(!(sizeof(x) % sizeof(0[x])))))
#define CHECK_MAP_ARRAYS(keys, values) \
    static_assert(((ARRAY_LENGTH((keys))) == (ARRAY_LENGTH((values)))), "key/value size mismatch")

// As per the USB 3.0 specs, the current maximum limit for the depth is 7.
#define MAX_DEV_DEPTH (7)

/* Types */
static inline uint32_t uintptr_hash_func(uintptr_t key)
{
#if UINTPTR_MAX == UINT64_MAX
    key = (~key) + (key << 18);
    key = key ^ (key >> 31);
    key = (key + (key << 2)) + (key << 4);
    key = key ^ (key >> 11);
    key = key + (key << 6);
    key = key ^ (key >> 22);
    return (uint32_t)key;
#else
    key = ~key + (key << 15);
    key = key ^ (key >> 12);
    key = key + (key << 2);
    key = key ^ (key >> 4);
    key = (key + (key << 3)) + (key << 11);
    key = key ^ (key >> 16);
    return key;
#endif
}

#define uintptr_hash_equal(a, b) ((a) == (b))

struct usb_nif_device;
typedef struct usb_nif_device usb_nif_device_t;

KHASH_INIT(devices, uintptr_t, usb_nif_device_t*, 1, uintptr_hash_func, uintptr_hash_equal);

typedef struct {
    ErlNifMutex    *lock;

    libusb_context *context;

    khash_t(devices)*devices;
    ErlNifMutex    *devices_lock;

    ErlNifTid       handle_events_thread;
    bool            handle_events_thread_running;
} usb_nif_t;

/* Atoms */

static ERL_NIF_TERM am_ok = 0;
static ERL_NIF_TERM am_error = 0;

static ERL_NIF_TERM am_usb = 0;
static ERL_NIF_TERM am_device_arrived = 0;
static ERL_NIF_TERM am_device_left = 0;

static ERL_NIF_TERM am_closed = 0;

static ERL_NIF_TERM am_io = 0;
static ERL_NIF_TERM am_invalid_param = 0;
static ERL_NIF_TERM am_access = 0;
static ERL_NIF_TERM am_no_device = 0;
static ERL_NIF_TERM am_not_found = 0;
static ERL_NIF_TERM am_busy = 0;
static ERL_NIF_TERM am_timeout = 0;
static ERL_NIF_TERM am_overflow = 0;
static ERL_NIF_TERM am_pipe = 0;
static ERL_NIF_TERM am_interrupted = 0;
static ERL_NIF_TERM am_no_mem = 0;
static ERL_NIF_TERM am_not_supported = 0;
static ERL_NIF_TERM am_other = 0;

/* Resources */

/* Resource: device */
typedef struct usb_nif_device {
    libusb_device  *device;
} usb_nif_device_t;

static ErlNifResourceType* usb_nif_device_resource_type;

static usb_nif_device_t* usb_nif_device_resource_get(usb_nif_t *usb_nif, libusb_device *device)
{
    int kh_put_ret;
    khiter_t iter;
    usb_nif_device_t* resource = NULL;

    enif_mutex_lock(usb_nif->devices_lock);

    iter = kh_put(devices, usb_nif->devices, (uintptr_t)device, &kh_put_ret);
    if (kh_put_ret == 0) { // entry already exists
        resource = kh_value(usb_nif->devices, iter);
        enif_keep_resource(resource);
    } else if (kh_put_ret > 0) {
        resource = (usb_nif_device_t*) enif_alloc_resource(
            usb_nif_device_resource_type,
            sizeof(usb_nif_device_t));
        resource->device = libusb_ref_device(device);

        kh_value(usb_nif->devices, iter) = resource;
    }

    enif_mutex_unlock(usb_nif->devices_lock);

    return resource;
}

static void usb_nif_device_resource_dtor(ErlNifEnv *env, void *obj)
{
    usb_nif_t *usb_nif = (usb_nif_t *) enif_priv_data(env);
    usb_nif_device_t *resource = (usb_nif_device_t *) obj;

    enif_mutex_lock(usb_nif->devices_lock);
    khiter_t iter = kh_get(devices, usb_nif->devices, (uintptr_t)resource->device);
    assert (iter != kh_end(usb_nif->devices));
    kh_del(devices, usb_nif->devices, iter);
    enif_mutex_unlock(usb_nif->devices_lock);

    libusb_unref_device(resource->device);
}

static ErlNifResourceTypeInit usb_nif_device_resource_callbacks = {
    .dtor = usb_nif_device_resource_dtor,
    .stop = NULL,
    .down = NULL,
};

/* Resource: device_handle */
typedef struct usb_nif_device_handle {
    ErlNifPid               owner;
    ErlNifMonitor           owner_monitor;

    libusb_device_handle   *device_handle;
    atomic_flag             device_handle_closed;
} usb_nif_device_handle_t;

static ErlNifResourceType* usb_nif_device_handle_resource_type;

static void usb_nif_device_handle_resource_dtor(ErlNifEnv *env, void *obj)
{
    // nothing
}

static void usb_nif_device_handle_resource_owner_down(ErlNifEnv *env, void *obj, ErlNifPid* pid, ErlNifMonitor* monitor)
{
    usb_nif_device_handle_t *usb_nif_device_handle = (usb_nif_device_handle_t *) obj;

    if (!atomic_flag_test_and_set(&usb_nif_device_handle->device_handle_closed)) {
        libusb_close(usb_nif_device_handle->device_handle);
        usb_nif_device_handle->device_handle = NULL;
    }
}

static ErlNifResourceTypeInit usb_nif_device_handle_resource_callbacks = {
    .dtor = usb_nif_device_handle_resource_dtor,
    .stop = NULL,
    .down = usb_nif_device_handle_resource_owner_down,
};


/* Resource: hotplug_monitor */
typedef struct usb_nif_hotplug_monitor {
    usb_nif_t                      *usb_nif;

    ErlNifPid                       owner;
    ErlNifMonitor                   owner_monitor;

    libusb_hotplug_callback_handle  callback_handle;
    atomic_flag                     closed;
} usb_nif_hotplug_monitor_t;

static ErlNifResourceType* usb_nif_hotplug_monitor_resource_type;

static void usb_nif_hotplug_monitor_resource_dtor(ErlNifEnv *env, void *obj)
{
    // nothing
}

static void usb_nif_hotplug_monitor_resource_owner_down(ErlNifEnv *env, void *obj, ErlNifPid* pid, ErlNifMonitor* monitor)
{
    usb_nif_t *usb_nif = (usb_nif_t *) enif_priv_data(env);
    usb_nif_hotplug_monitor_t *usb_nif_hotplug_monitor = (usb_nif_hotplug_monitor_t *) obj;

    if (!atomic_flag_test_and_set(&usb_nif_hotplug_monitor->closed)) {
        libusb_hotplug_deregister_callback(usb_nif->context, usb_nif_hotplug_monitor->callback_handle);

        enif_release_resource(usb_nif_hotplug_monitor);
    }
}

static ErlNifResourceTypeInit usb_nif_hotplug_monitor_resource_callbacks = {
    .dtor = usb_nif_hotplug_monitor_resource_dtor,
    .stop = NULL,
    .down = usb_nif_hotplug_monitor_resource_owner_down,
};

/* Helpers */

static ERL_NIF_TERM libusb_error_to_atom(enum libusb_error error)
{
    switch (error) {
        case LIBUSB_ERROR_IO: return am_io;
        case LIBUSB_ERROR_INVALID_PARAM: return am_invalid_param;
        case LIBUSB_ERROR_ACCESS: return am_access;
        case LIBUSB_ERROR_NO_DEVICE: return am_no_device;
        case LIBUSB_ERROR_NOT_FOUND: return am_not_found;
        case LIBUSB_ERROR_BUSY: return am_busy;
        case LIBUSB_ERROR_TIMEOUT: return am_timeout;
        case LIBUSB_ERROR_OVERFLOW: return am_overflow;
        case LIBUSB_ERROR_PIPE: return am_pipe;
        case LIBUSB_ERROR_INTERRUPTED: return am_interrupted;
        case LIBUSB_ERROR_NO_MEM: return am_no_mem;
        case LIBUSB_ERROR_NOT_SUPPORTED: return am_not_supported;
        default: return am_other;
    }
}

static ERL_NIF_TERM speed_to_atom(ErlNifEnv* env, int speed) {
    switch (speed) {
        case LIBUSB_SPEED_LOW: return enif_make_atom(env, "low");
        case LIBUSB_SPEED_FULL: return enif_make_atom(env, "full");
        case LIBUSB_SPEED_HIGH: return enif_make_atom(env, "high");
        case LIBUSB_SPEED_SUPER: return enif_make_atom(env, "super");
        default: return enif_make_atom(env, "unknown");
    }
}

static ERL_NIF_TERM libusb_endpoints_export(ErlNifEnv *env, const struct libusb_endpoint_descriptor endpoint_descriptors[], uint8_t num_endpoint_descriptors)
{
    ERL_NIF_TERM keys[] = {
        enif_make_atom(env, "address"),
        enif_make_atom(env, "attributes"),
        enif_make_atom(env, "max_packet_size"),
        enif_make_atom(env, "interval"),
        enif_make_atom(env, "refresh"),
        enif_make_atom(env, "synch_address"),
        enif_make_atom(env, "extra")
    };

    ERL_NIF_TERM result = enif_make_list(env, 0);
    for (size_t i = num_endpoint_descriptors; i > 0; i--) {
        const struct libusb_endpoint_descriptor *endpoint_descriptor = &endpoint_descriptors[i - 1];

        ERL_NIF_TERM value_extra;
        memcpy(enif_make_new_binary(env, (size_t) endpoint_descriptor->extra_length, &value_extra),
            endpoint_descriptor->extra, endpoint_descriptor->extra_length);

        ERL_NIF_TERM values[] = {
            enif_make_uint(env, endpoint_descriptor->bEndpointAddress),
            enif_make_uint(env, endpoint_descriptor->bmAttributes),
            enif_make_uint(env, endpoint_descriptor->wMaxPacketSize),
            enif_make_uint(env, endpoint_descriptor->bInterval),
            enif_make_uint(env, endpoint_descriptor->bRefresh),
            enif_make_uint(env, endpoint_descriptor->bSynchAddress),
            value_extra,
        };

        CHECK_MAP_ARRAYS(keys, values);

        ERL_NIF_TERM result_endpoint;
        enif_make_map_from_arrays(env,
            keys,
            values,
            ARRAY_LENGTH(keys),
            &result_endpoint);

        result = enif_make_list_cell(
            env,
            result_endpoint,
            result);
    }

    return result;
}


static ERL_NIF_TERM libusb_interfaces_export(ErlNifEnv *env, const struct libusb_interface interfaces[], uint8_t num_interfaces)
{
    ERL_NIF_TERM interface_keys[] = {
        enif_make_atom(env, "alt_settings"),
    };
    
    ERL_NIF_TERM alt_setting_keys[] = {
        enif_make_atom(env, "interface_number"),
        enif_make_atom(env, "setting_number"),
        enif_make_atom(env, "class_code"),
        enif_make_atom(env, "sub_class_code"),
        enif_make_atom(env, "protocol_code"),
        enif_make_atom(env, "description_string_index"),
        enif_make_atom(env, "endpoints"),
        enif_make_atom(env, "extra"),
    };

    ERL_NIF_TERM result = enif_make_list(env, 0);
    for (size_t i = num_interfaces; i > 0; i--) {
        const struct libusb_interface *interface = &interfaces[i - 1];

        ERL_NIF_TERM result_alt_settings = enif_make_list(env, 0);
        for (size_t j = interface->num_altsetting; j > 0; j--) {
            const struct libusb_interface_descriptor *interface_descriptor = &interface->altsetting[j - 1];

            ERL_NIF_TERM value_extra;
            memcpy(enif_make_new_binary(env, (size_t) interface_descriptor->extra_length, &value_extra),
                interface_descriptor->extra, interface_descriptor->extra_length);

            ERL_NIF_TERM alt_setting_values[] = {
                enif_make_uint(env, interface_descriptor->bInterfaceNumber),
                enif_make_uint(env, interface_descriptor->bAlternateSetting),
                enif_make_uint(env, interface_descriptor->bInterfaceClass),
                enif_make_uint(env, interface_descriptor->bInterfaceSubClass),
                enif_make_uint(env, interface_descriptor->bInterfaceProtocol),
                enif_make_uint(env, interface_descriptor->iInterface),
                libusb_endpoints_export(env, interface_descriptor->endpoint, interface_descriptor->bNumEndpoints),
                value_extra,
            };

            CHECK_MAP_ARRAYS(alt_setting_keys, alt_setting_values);

            ERL_NIF_TERM result_alt_setting;
            enif_make_map_from_arrays(env,
                alt_setting_keys,
                alt_setting_values,
                ARRAY_LENGTH(alt_setting_keys),
                &result_alt_setting);

            result_alt_settings = enif_make_list_cell(
                env,
                result_alt_setting,
                result_alt_settings);
        }

        ERL_NIF_TERM interface_values[] = {
            result_alt_settings
        };

        CHECK_MAP_ARRAYS(interface_keys, interface_values);

        ERL_NIF_TERM result_interface;
        enif_make_map_from_arrays(env,
                interface_keys,
                interface_values,
                ARRAY_LENGTH(interface_keys),
                &result_interface);

        result = enif_make_list_cell(
            env,
            result_interface,
            result);
    }

    return result;
}

static int libusb_hotplug_callback(libusb_context *context, libusb_device *device, libusb_hotplug_event event, void *user_data)
{
    usb_nif_hotplug_monitor_t *usb_nif_hotplug_monitor = (usb_nif_hotplug_monitor_t *)user_data;

    switch (event) {
        case LIBUSB_HOTPLUG_EVENT_DEVICE_ARRIVED: {
            ErlNifEnv *env = enif_alloc_env();
            usb_nif_device_t *usb_nif_device = usb_nif_device_resource_get(usb_nif_hotplug_monitor->usb_nif, device);

            ERL_NIF_TERM payload = enif_make_resource(env, usb_nif_device);
            ERL_NIF_TERM message = enif_make_tuple3(env, am_usb, am_device_arrived, payload);
            enif_send(NULL, &(usb_nif_hotplug_monitor->owner), env, message);

            enif_release_resource(usb_nif_device);
            enif_free_env(env);

            return 0;
        }
        case LIBUSB_HOTPLUG_EVENT_DEVICE_LEFT: {
            ErlNifEnv *env = enif_alloc_env();
            usb_nif_device_t *usb_nif_device = usb_nif_device_resource_get(usb_nif_hotplug_monitor->usb_nif, device);

            ERL_NIF_TERM payload = enif_make_resource(env, usb_nif_device);
            ERL_NIF_TERM message = enif_make_tuple3(env, am_usb, am_device_left, payload);
            enif_send(NULL, &(usb_nif_hotplug_monitor->owner), env, message);

            enif_release_resource(usb_nif_device);
            enif_free_env(env);

            return 0;
        }
        default:
            return 0;
    }
}


/* API */

static ERL_NIF_TERM usb_nif_get_device_list(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    usb_nif_t *usb_nif = (usb_nif_t *) enif_priv_data(env);
    libusb_context *context = usb_nif->context;

    libusb_device **devices;
    ssize_t ret = libusb_get_device_list(context, &devices);
    if (ret < 0) {
        return enif_make_tuple2(env, am_error, libusb_error_to_atom(ret));
    }

    ERL_NIF_TERM result = enif_make_list(env, 0);
    for (size_t i = ret; i > 0; i--) {
        usb_nif_device_t *usb_nif_device = usb_nif_device_resource_get(usb_nif, devices[i - 1]);

        result = enif_make_list_cell(
            env,
            enif_make_resource(env, usb_nif_device),
            result);
        enif_release_resource(usb_nif_device);
    }
    libusb_free_device_list(devices, true);

    return enif_make_tuple2(env, am_ok, result);
}

static ERL_NIF_TERM usb_nif_get_bus_number(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
    usb_nif_device_t *usb_device;
    if (!enif_get_resource(env, argv[0], usb_nif_device_resource_type, (void**) &usb_device)) {
        return enif_make_badarg(env);
    }

    uint8_t bus_no;
    bus_no = libusb_get_bus_number(usb_device->device);
    return enif_make_tuple2(env, am_ok, enif_make_uint(env, bus_no));
}

static ERL_NIF_TERM usb_nif_get_port_number(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
    usb_nif_device_t *usb_device;
    if (!enif_get_resource(env, argv[0], usb_nif_device_resource_type, (void**) &usb_device)) {
        return enif_make_badarg(env);
    }

    uint8_t port_no;
    port_no = libusb_get_port_number(usb_device->device);
    return enif_make_tuple2(env, am_ok, enif_make_uint(env, port_no));
}

static ERL_NIF_TERM usb_nif_get_port_numbers(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
    usb_nif_device_t *usb_device;
    if (!enif_get_resource(env, argv[0], usb_nif_device_resource_type, (void**) &usb_device)) {
        return enif_make_badarg(env);
    }

    int result;
    uint8_t port_nos[MAX_DEV_DEPTH];
    ERL_NIF_TERM result_arr[MAX_DEV_DEPTH];
    result = libusb_get_port_numbers(usb_device->device, port_nos, ARRAY_LENGTH(port_nos));

    for (unsigned int i=0; i<result; i++)
    {
        result_arr[i] = enif_make_uint(env, port_nos[i]);
    }

    return enif_make_tuple2(env, am_ok, enif_make_list_from_array(env, result_arr, result));
}

static ERL_NIF_TERM usb_nif_get_device_address(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
    usb_nif_device_t *usb_device;
    if (!enif_get_resource(env, argv[0], usb_nif_device_resource_type, (void**) &usb_device)) {
        return enif_make_badarg(env);
    }

    uint8_t addr;
    addr = libusb_get_device_address(usb_device->device);
    return enif_make_tuple2(env, am_ok, enif_make_uint(env, addr));
}

static ERL_NIF_TERM usb_nif_get_device_speed(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
    usb_nif_device_t *usb_device;
    if (!enif_get_resource(env, argv[0], usb_nif_device_resource_type, (void**) &usb_device)) {
        return enif_make_badarg(env);
    }

    int speed;
    speed = libusb_get_device_speed(usb_device->device);
    return enif_make_tuple2(env, am_ok, speed_to_atom(env, speed));
}

static ERL_NIF_TERM usb_nif_get_device_descriptor(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    usb_nif_device_t *usb_device;

    if (!enif_get_resource(env, argv[0], usb_nif_device_resource_type, (void**) &usb_device)) {
        return enif_make_badarg(env);
    }

    struct libusb_device_descriptor device_descriptor;
    int ret = libusb_get_device_descriptor(usb_device->device, &device_descriptor);
    if (ret != LIBUSB_SUCCESS) {
        return enif_make_tuple2(env, am_error, libusb_error_to_atom(ret));
    }

    ERL_NIF_TERM keys[] = {
        enif_make_atom(env, "usb_version"),
        enif_make_atom(env, "class_code"),
        enif_make_atom(env, "sub_class_code"),
        enif_make_atom(env, "protocol_code"),
        enif_make_atom(env, "max_packet_size0"),
        enif_make_atom(env, "vendor_id"),
        enif_make_atom(env, "product_id"),
        enif_make_atom(env, "device_version"),
        enif_make_atom(env, "manufacturer_string_index"),
        enif_make_atom(env, "product_string_index"),
        enif_make_atom(env, "serial_number_string_index"),
        enif_make_atom(env, "num_configurations"),
    };
    ERL_NIF_TERM values[] = {
        enif_make_uint(env, device_descriptor.bcdUSB),
        enif_make_uint(env, device_descriptor.bDeviceClass),
        enif_make_uint(env, device_descriptor.bDeviceSubClass),
        enif_make_uint(env, device_descriptor.bDeviceProtocol),
        enif_make_uint(env, device_descriptor.bMaxPacketSize0),
        enif_make_uint(env, device_descriptor.idVendor),
        enif_make_uint(env, device_descriptor.idProduct),
        enif_make_uint(env, device_descriptor.bcdDevice),
        enif_make_uint(env, device_descriptor.iManufacturer),
        enif_make_uint(env, device_descriptor.iProduct),
        enif_make_uint(env, device_descriptor.iSerialNumber),
        enif_make_uint(env, device_descriptor.bNumConfigurations),
    };

    CHECK_MAP_ARRAYS(keys, values);

    ERL_NIF_TERM result;
    enif_make_map_from_arrays(env,
        keys,
        values,
        ARRAY_LENGTH(keys),
        &result);

    return enif_make_tuple2(env, am_ok, result);
}


static ERL_NIF_TERM usb_nif_get_config_descriptor(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    usb_nif_device_t *usb_device;

    if (!enif_get_resource(env, argv[0], usb_nif_device_resource_type, (void**) &usb_device)) {
        return enif_make_badarg(env);
    }

    unsigned int config_index;
    if (!enif_get_uint(env, argv[1], &config_index) || config_index > 255) {
        return enif_make_badarg(env);
    }

    struct libusb_config_descriptor *config_descriptor;
    int ret = libusb_get_config_descriptor(usb_device->device, (uint8_t) config_index, &config_descriptor);
    if (ret != LIBUSB_SUCCESS) {
        return enif_make_tuple2(env, am_error, libusb_error_to_atom(ret));
    }

    ERL_NIF_TERM keys[] = {
        enif_make_atom(env, "configuration_number"),
        enif_make_atom(env, "description_string_index"),
        enif_make_atom(env, "attributes"),
        enif_make_atom(env, "max_power"),
        enif_make_atom(env, "interfaces"),
        enif_make_atom(env, "extra"),
    };

    ERL_NIF_TERM value_extra;
    memcpy(enif_make_new_binary(env, (size_t) config_descriptor->extra_length, &value_extra),
        config_descriptor->extra, config_descriptor->extra_length);

    enum libusb_speed speed = libusb_get_device_speed(usb_device->device);
    ERL_NIF_TERM value_max_power = enif_make_uint(env,
        (speed >= LIBUSB_SPEED_SUPER ? 8 : 2) * config_descriptor->MaxPower);

    ERL_NIF_TERM values[] = {
        enif_make_uint(env, config_descriptor->bConfigurationValue),
        enif_make_uint(env, config_descriptor->iConfiguration),
        enif_make_uint(env, config_descriptor->bmAttributes),
        value_max_power,
        libusb_interfaces_export(env, config_descriptor->interface, config_descriptor->bNumInterfaces),
        value_extra,
    };

    libusb_free_config_descriptor(config_descriptor);

    CHECK_MAP_ARRAYS(keys, values);

    ERL_NIF_TERM result;
    enif_make_map_from_arrays(env,
        keys,
        values,
        ARRAY_LENGTH(keys),
        &result);

    return enif_make_tuple2(env, am_ok, result);
}


static ERL_NIF_TERM usb_nif_open_device(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    usb_nif_device_t *usb_device;

    if (!enif_get_resource(env, argv[0], usb_nif_device_resource_type, (void**) &usb_device)) {
        return enif_make_badarg(env);
    }

    libusb_device_handle *device_handle;
    int ret = libusb_open(usb_device->device, &device_handle);
    if (ret != LIBUSB_SUCCESS){
        return enif_make_tuple2(env, am_error, libusb_error_to_atom(ret));
    }

    ErlNifPid owner;
    enif_self(env, &owner);

    usb_nif_device_handle_t *usb_nif_device_handle = (usb_nif_device_handle_t*) enif_alloc_resource(
        usb_nif_device_handle_resource_type,
        sizeof(usb_nif_device_handle_t));
    usb_nif_device_handle->owner = owner;
    usb_nif_device_handle->device_handle = device_handle;
    atomic_flag_clear(&(usb_nif_device_handle->device_handle_closed));

    if (enif_monitor_process(env, usb_nif_device_handle, &owner, &usb_nif_device_handle->owner_monitor)) {
        enif_release_resource(usb_nif_device_handle);
        libusb_close(device_handle);
        return enif_make_tuple2(env, am_error, am_other);
    }

    ERL_NIF_TERM result = enif_make_resource(env, usb_nif_device_handle);
    enif_release_resource(usb_nif_device_handle);

    return enif_make_tuple2(env, am_ok, result);
}


static ERL_NIF_TERM usb_nif_close_device(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    usb_nif_device_handle_t *usb_nif_device_handle;
    if (!enif_get_resource(env, argv[0], usb_nif_device_handle_resource_type, (void**) &usb_nif_device_handle)) {
        return enif_make_badarg(env);
    }

    if (!atomic_flag_test_and_set(&usb_nif_device_handle->device_handle_closed)) {
        libusb_close(usb_nif_device_handle->device_handle);
        usb_nif_device_handle->device_handle = NULL;

        enif_demonitor_process(env, usb_nif_device_handle, &usb_nif_device_handle->owner_monitor);

        return am_ok;
    } else {
        return enif_make_tuple2(env, am_error, am_closed);
    }
}


static ERL_NIF_TERM usb_nif_monitor_hotplug(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    usb_nif_t *usb_nif = (usb_nif_t *) enif_priv_data(env);

    unsigned int flags;
    if (!enif_get_uint(env, argv[0], &flags)) {
        return enif_make_badarg(env);
    }

    ErlNifPid owner;
    enif_self(env, &owner);

    usb_nif_hotplug_monitor_t *usb_nif_hotplug_monitor = (usb_nif_hotplug_monitor_t*) enif_alloc_resource(
        usb_nif_hotplug_monitor_resource_type,
        sizeof(usb_nif_hotplug_monitor_t));
    usb_nif_hotplug_monitor->usb_nif = usb_nif;
    usb_nif_hotplug_monitor->owner = owner;
    atomic_flag_clear(&(usb_nif_hotplug_monitor->closed));

    int libusb_flags = LIBUSB_HOTPLUG_NO_FLAGS;
    if (flags & (1U << 0)) {
        libusb_flags |= LIBUSB_HOTPLUG_ENUMERATE;
    }

    int ret = libusb_hotplug_register_callback(usb_nif->context,
        LIBUSB_HOTPLUG_EVENT_DEVICE_ARRIVED | LIBUSB_HOTPLUG_EVENT_DEVICE_LEFT,
        libusb_flags,
        LIBUSB_HOTPLUG_MATCH_ANY,
        LIBUSB_HOTPLUG_MATCH_ANY,
        LIBUSB_HOTPLUG_MATCH_ANY,
        libusb_hotplug_callback,
        usb_nif_hotplug_monitor,
        &(usb_nif_hotplug_monitor->callback_handle));
    if (ret != LIBUSB_SUCCESS) {
        enif_release_resource(usb_nif_hotplug_monitor);
        return enif_make_tuple2(env, am_error, libusb_error_to_atom(ret));
    }

    if (enif_monitor_process(env, usb_nif_hotplug_monitor, &owner, &usb_nif_hotplug_monitor->owner_monitor)) {
        libusb_hotplug_deregister_callback(usb_nif->context,
            usb_nif_hotplug_monitor->callback_handle);
        // The resource must NOT be released here because we give
        // a pointer to it to libusb. Which means this resource
        // is still reachable from the `C` world. The resource
        // is release when demonitoring the hotplug either explicitely
        // through the API call or implicitely when the owner process
        // terminates.
        return enif_make_tuple2(env, am_error, am_other);
    }

    ERL_NIF_TERM result = enif_make_resource(env, usb_nif_hotplug_monitor);
    enif_release_resource(usb_nif_hotplug_monitor);

    return enif_make_tuple2(env, am_ok, result);
}


static ERL_NIF_TERM usb_nif_demonitor_hotplug(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    usb_nif_t *usb_nif = (usb_nif_t *) enif_priv_data(env);

    usb_nif_hotplug_monitor_t *usb_nif_hotplug_monitor;
    if (!enif_get_resource(env, argv[0], usb_nif_hotplug_monitor_resource_type, (void**) &usb_nif_hotplug_monitor)) {
        return enif_make_badarg(env);
    }

    if (!atomic_flag_test_and_set(&usb_nif_hotplug_monitor->closed)) {
        libusb_hotplug_deregister_callback(usb_nif->context,
            usb_nif_hotplug_monitor->callback_handle);

        enif_demonitor_process(env, usb_nif_hotplug_monitor,
            &usb_nif_hotplug_monitor->owner_monitor);

        enif_release_resource(usb_nif_hotplug_monitor);
    }

    return am_ok;
}


static ERL_NIF_TERM usb_nif_claim_interface(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    usb_nif_device_handle_t *usb_nif_device_handle;
    if (!enif_get_resource(env, argv[0], usb_nif_device_handle_resource_type, (void**) &usb_nif_device_handle)) {
        return enif_make_badarg(env);
    }

    unsigned int interface_number;
    if(!enif_get_uint(env, argv[1], &interface_number)){
        return enif_make_badarg(env);
    }

    int ret = libusb_claim_interface(usb_nif_device_handle->device_handle, (int) interface_number);
    if(ret != LIBUSB_SUCCESS) {
        return enif_make_tuple2(env, am_error, libusb_error_to_atom(ret));
    }

    return am_ok;
}

static ERL_NIF_TERM usb_nif_release_interface(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    usb_nif_device_handle_t *usb_nif_device_handle;
    if (!enif_get_resource(env, argv[0], usb_nif_device_handle_resource_type, (void**) &usb_nif_device_handle)) {
        return enif_make_badarg(env);
    }

    unsigned int interface_number;
    if(!enif_get_uint(env, argv[1], &interface_number)){
        return enif_make_badarg(env);
    }

    int ret = libusb_release_interface(usb_nif_device_handle->device_handle, (int) interface_number);
    if(ret != LIBUSB_SUCCESS) {
        return enif_make_tuple2(env, am_error, libusb_error_to_atom(ret));
    }

    return am_ok;
}

static ERL_NIF_TERM usb_nif_clear_halt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    usb_nif_device_handle_t *usb_nif_device_handle;
    if (!enif_get_resource(env, argv[0], usb_nif_device_handle_resource_type, (void**) &usb_nif_device_handle)) {
        return enif_make_badarg(env);
    }

    unsigned int endpoint;
    if (!enif_get_uint(env, argv[1], &endpoint)) {
        return enif_make_badarg(env);
    }

    int ret = libusb_clear_halt(usb_nif_device_handle->device_handle, (unsigned char)endpoint);
    if(ret != LIBUSB_SUCCESS) {
        return enif_make_tuple2(env, am_error, libusb_error_to_atom(ret));
    }

    return am_ok;
}

static ERL_NIF_TERM usb_nif_set_configuration(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    usb_nif_device_handle_t *usb_nif_device_handle;
    if (!enif_get_resource(env, argv[0], usb_nif_device_handle_resource_type, (void**) &usb_nif_device_handle)) {
        return enif_make_badarg(env);
    }

    int configuration;
    if(!enif_get_int(env, argv[1], &configuration)) {
        return enif_make_badarg(env);
    }

    int ret = libusb_set_configuration(usb_nif_device_handle->device_handle, configuration);
    if(ret != LIBUSB_SUCCESS) {
        return enif_make_tuple2(env, am_error, libusb_error_to_atom(ret));
    }

    return am_ok;
}

static ERL_NIF_TERM usb_nif_read_bulk(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    usb_nif_device_handle_t *usb_nif_device_handle;
    if (!enif_get_resource(env, argv[0], usb_nif_device_handle_resource_type, (void**) &usb_nif_device_handle)) {
        return enif_make_badarg(env);
    }

    unsigned int endpoint;
    if (!enif_get_uint(env, argv[1], &endpoint)) {
        return enif_make_badarg(env);
    }

    int data_len;
    if (!enif_get_int(env, argv[2], &data_len)) {
        return enif_make_badarg(env);
    }

    unsigned int timeout;
    if (!enif_get_uint(env, argv[3], &timeout)) {
        return enif_make_badarg(env);
    }

    unsigned char *p_data;
    if (!(p_data = (unsigned char *)enif_alloc(data_len))) {
        return enif_make_tuple2(env, am_error, libusb_error_to_atom(LIBUSB_ERROR_NO_MEM));
    }

    int ret;
    int transferred;

    ret = libusb_bulk_transfer(usb_nif_device_handle->device_handle, (unsigned char)endpoint, p_data, data_len, &transferred, timeout);

    if (ret != LIBUSB_SUCCESS) {
        if (ret == LIBUSB_ERROR_TIMEOUT) {
            ERL_NIF_TERM data;
            memcpy((void*)enif_make_new_binary(env, (size_t) transferred, &data), (void*)p_data, (size_t)transferred);
            enif_free(p_data);
            return enif_make_tuple3(env, am_error, libusb_error_to_atom(ret), data);
        }
        enif_free(p_data);
        return enif_make_tuple2(env, am_error, libusb_error_to_atom(ret));
    }

    ERL_NIF_TERM data;
    memcpy((void*)enif_make_new_binary(env, (size_t) transferred, &data), (void*)p_data, (size_t)transferred);
    enif_free(p_data);

    return enif_make_tuple2(env, am_ok, data);
}

static ERL_NIF_TERM usb_nif_write_bulk(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    usb_nif_device_handle_t *usb_nif_device_handle;
    if (!enif_get_resource(env, argv[0], usb_nif_device_handle_resource_type, (void**) &usb_nif_device_handle)) {
        return enif_make_badarg(env);
    }

    unsigned int endpoint;
    if(!enif_get_uint(env, argv[1], &endpoint)){
        return enif_make_badarg(env);
    }

    ErlNifBinary tx;
    if (!enif_inspect_binary(env, argv[2], &tx)) {
        return enif_make_badarg(env);
    }

    unsigned int timeout;
    if(!enif_get_uint(env, argv[3], &timeout)){
        return enif_make_badarg(env);
    }

    int ret;
    int transferred = 0;
    ret = libusb_bulk_transfer(usb_nif_device_handle->device_handle, (unsigned char)endpoint, tx.data, tx.size, &transferred, timeout);

    if (ret != LIBUSB_SUCCESS) {
        if (ret == LIBUSB_ERROR_TIMEOUT) {
            return enif_make_tuple3(env, am_error, libusb_error_to_atom(ret), enif_make_int(env, transferred));
        }
        return enif_make_tuple2(env, am_error, libusb_error_to_atom(ret));
    }

    return enif_make_tuple2(env, am_ok, enif_make_int(env, transferred));
}

static ERL_NIF_TERM usb_nif_read_interrupt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    usb_nif_device_handle_t *usb_nif_device_handle;
    if (!enif_get_resource(env, argv[0], usb_nif_device_handle_resource_type, (void**) &usb_nif_device_handle)) {
        return enif_make_badarg(env);
    }

    unsigned int endpoint;
    if (!enif_get_uint(env, argv[1], &endpoint)) {
        return enif_make_badarg(env);
    }

    int data_len;
    if (!enif_get_int(env, argv[2], &data_len)) {
        return enif_make_badarg(env);
    }

    unsigned int timeout;
    if (!enif_get_uint(env, argv[3], &timeout)) {
        return enif_make_badarg(env);
    }

    unsigned char *p_data;
    if (!(p_data = (unsigned char *)enif_alloc(data_len))) {
        return enif_make_tuple2(env, am_error, libusb_error_to_atom(LIBUSB_ERROR_NO_MEM));
    }

    int ret;
    int transferred;

    ret = libusb_interrupt_transfer(usb_nif_device_handle->device_handle, (unsigned char)endpoint, p_data, data_len, &transferred, timeout);
    if (ret != LIBUSB_SUCCESS) {
        if (ret == LIBUSB_ERROR_TIMEOUT) {
            ERL_NIF_TERM data;
            memcpy((void*)enif_make_new_binary(env, (size_t) transferred, &data), (void*)p_data, (size_t)transferred);
            enif_free(p_data);
            return enif_make_tuple3(env, am_error, libusb_error_to_atom(ret), data);
        }
        enif_free(p_data);
        return enif_make_tuple2(env, am_error, libusb_error_to_atom(ret));
    }

    ERL_NIF_TERM data;
    memcpy((void*)enif_make_new_binary(env, (size_t) transferred, &data), (void*)p_data, (size_t)transferred);
    enif_free(p_data);

    return enif_make_tuple2(env, am_ok, data);
}

static ERL_NIF_TERM usb_nif_write_interrupt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    usb_nif_device_handle_t *usb_nif_device_handle;
    if (!enif_get_resource(env, argv[0], usb_nif_device_handle_resource_type, (void**) &usb_nif_device_handle)) {
        return enif_make_badarg(env);
    }

    unsigned int endpoint;
    if(!enif_get_uint(env, argv[1], &endpoint)){
        return enif_make_badarg(env);
    }

    ErlNifBinary tx;
    if (!enif_inspect_binary(env, argv[2], &tx)) {
        return enif_make_badarg(env);
    }

    unsigned int timeout;
    if(!enif_get_uint(env, argv[3], &timeout)){
        return enif_make_badarg(env);
    }

    int ret;
    int transferred = 0;
    ret = libusb_interrupt_transfer(usb_nif_device_handle->device_handle, (unsigned char)endpoint, tx.data, tx.size, &transferred, timeout);

    if (ret != LIBUSB_SUCCESS) {
        if (ret == LIBUSB_ERROR_TIMEOUT) {
            return enif_make_tuple3(env, am_error, libusb_error_to_atom(ret), enif_make_int(env, transferred));
        }
        return enif_make_tuple2(env, am_error, libusb_error_to_atom(ret));
    }

    return enif_make_tuple2(env, am_ok, enif_make_int(env, transferred));
}

static ERL_NIF_TERM usb_nif_read_control(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    usb_nif_device_handle_t *usb_nif_device_handle;
    if (!enif_get_resource(env, argv[0], usb_nif_device_handle_resource_type, (void**) &usb_nif_device_handle)) {
        return enif_make_badarg(env);
    }

    unsigned int request_type;
    if(!enif_get_uint(env, argv[1], &request_type)){
        return enif_make_badarg(env);
    }

    unsigned int request;
    if(!enif_get_uint(env, argv[2], &request)){
        return enif_make_badarg(env);
    }

    unsigned int value;
    if(!enif_get_uint(env, argv[3], &value)){
        return enif_make_badarg(env);
    }

    unsigned int index;
    if(!enif_get_uint(env, argv[4], &index)){
        return enif_make_badarg(env);
    }

    unsigned int read_len;
    if(!enif_get_uint(env, argv[5], &read_len)){
        return enif_make_badarg(env);
    }

    unsigned int timeout;
    if(!enif_get_uint(env, argv[6], &timeout)){
        return enif_make_badarg(env);
    }

    unsigned char *p_data;
    if (!(p_data = (unsigned char *)enif_alloc(read_len))) {
        return enif_make_tuple2(env, am_error, libusb_error_to_atom(LIBUSB_ERROR_NO_MEM));
    }

    int ret;
    ret = libusb_control_transfer(usb_nif_device_handle->device_handle, (uint8_t) request_type, (uint8_t) request, (uint16_t) value, (uint16_t) index, p_data, (uint16_t) read_len, timeout);

    if (ret < 0) {
        enif_free(p_data);
        return enif_make_tuple2(env, am_error, libusb_error_to_atom(ret));
    }

    ERL_NIF_TERM data;
    memcpy((void*)enif_make_new_binary(env, (size_t)ret, &data), (void*)p_data, (size_t)ret);
    enif_free(p_data);

    return enif_make_tuple2(env, am_ok, data);
}

static ERL_NIF_TERM usb_nif_write_control(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    usb_nif_device_handle_t *usb_nif_device_handle;
    if (!enif_get_resource(env, argv[0], usb_nif_device_handle_resource_type, (void**) &usb_nif_device_handle)) {
        return enif_make_badarg(env);
    }

    unsigned int request_type;
    if(!enif_get_uint(env, argv[1], &request_type)){
        return enif_make_badarg(env);
    }

    unsigned int request;
    if(!enif_get_uint(env, argv[2], &request)){
        return enif_make_badarg(env);
    }

    unsigned int value;
    if(!enif_get_uint(env, argv[3], &value)){
        return enif_make_badarg(env);
    }

    unsigned int index;
    if(!enif_get_uint(env, argv[4], &index)){
        return enif_make_badarg(env);
    }

    ErlNifBinary tx;
    if (!enif_inspect_binary(env, argv[5], &tx)) {
        return enif_make_badarg(env);
    }

    unsigned int timeout;
    if(!enif_get_uint(env, argv[6], &timeout)){
        return enif_make_badarg(env);
    }

    int ret;
    ret = libusb_control_transfer(usb_nif_device_handle->device_handle, (uint8_t) request_type, (uint8_t) request, (uint16_t) value, (uint16_t) index, tx.data, (uint16_t)tx.size, timeout);

    if (ret < 0) {
        return enif_make_tuple2(env, am_error, libusb_error_to_atom(ret));
    }

    return enif_make_tuple2(env, am_ok, enif_make_int(env, ret));
}

static ERL_NIF_TERM usb_nif_detach_kernel_driver(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    usb_nif_device_handle_t *usb_nif_device_handle;
    if (!enif_get_resource(env, argv[0], usb_nif_device_handle_resource_type, (void**) &usb_nif_device_handle)) {
        return enif_make_badarg(env);
    }

    int interface_number;
    if (!enif_get_int(env, argv[1], &interface_number)) {
        return enif_make_badarg(env);
    }

    if (libusb_has_capability(LIBUSB_CAP_SUPPORTS_DETACH_KERNEL_DRIVER)) {
        int ret;
        ret = libusb_kernel_driver_active(usb_nif_device_handle->device_handle, interface_number);
        if (ret) {
            if (ret == 1) {
                if ((ret = libusb_detach_kernel_driver(usb_nif_device_handle->device_handle, interface_number))) {
                    return enif_make_tuple2(env, am_error, libusb_error_to_atom(ret));
                }
                return am_ok;
            }

            return enif_make_tuple2(env, am_error, libusb_error_to_atom(ret));
        }
        else
            return am_ok;
    }

    return enif_make_tuple2(env, am_error, am_not_supported);
}

static ERL_NIF_TERM usb_nif_attach_kernel_driver(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    usb_nif_device_handle_t *usb_nif_device_handle;
    if (!enif_get_resource(env, argv[0], usb_nif_device_handle_resource_type, (void**) &usb_nif_device_handle)) {
        return enif_make_badarg(env);
    }

    int interface_number;
    if (!enif_get_int(env, argv[1], &interface_number)) {
        return enif_make_badarg(env);
    }

    int ret;
    if ((ret = libusb_attach_kernel_driver(usb_nif_device_handle->device_handle, interface_number))) {
        return enif_make_tuple2(env, am_error, libusb_error_to_atom(ret));
    }
    return am_ok;
}

/* Initialization */

static void* usb_nif_handle_events(void *user_data)
{
    usb_nif_t *usb_nif = (usb_nif_t *) user_data;
    bool running;
    struct timeval timeout = {
        .tv_sec = 1,
        .tv_usec = 0
    };

    enif_mutex_lock(usb_nif->lock);
    running = usb_nif->handle_events_thread_running;
    enif_mutex_unlock(usb_nif->lock);

    while (running) {
        libusb_handle_events_timeout_completed(usb_nif->context, &timeout, NULL);

        enif_mutex_lock(usb_nif->lock);
        running = usb_nif->handle_events_thread_running;
        enif_mutex_unlock(usb_nif->lock);
    }

    return NULL;
}


static int usb_nif_on_load(ErlNifEnv *env, void** priv_data, ERL_NIF_TERM load_info)
{
    usb_nif_t *usb_nif = enif_alloc(sizeof(usb_nif_t));
    if (!usb_nif) {
        return -1;
    }

    usb_nif->lock = enif_mutex_create("usb_nif_lock");

    // Create libusb context
    int ret = libusb_init(&usb_nif->context);
    if (ret != LIBUSB_SUCCESS) {
        enif_mutex_destroy(usb_nif->lock);
        enif_free(usb_nif);
        return ret;
    }

    // Initialize device hash table
    usb_nif->devices = kh_init(devices);
    usb_nif->devices_lock = enif_mutex_create("usb_nif_devices_lock");

    // Start polling thread
    usb_nif->handle_events_thread_running = true;
    ret = enif_thread_create("usb_nif_handle_events",
        &usb_nif->handle_events_thread,
        usb_nif_handle_events,
        usb_nif,
        NULL);
    if (ret != 0) {
        libusb_exit(usb_nif->context);
        enif_mutex_destroy(usb_nif->lock);
        enif_free(usb_nif);
        return ret;
    }

    // Atoms
    am_ok = enif_make_atom(env, "ok");
    am_error = enif_make_atom(env, "error");

    am_usb = enif_make_atom(env, "usb");
    am_device_arrived = enif_make_atom(env, "device_arrived");
    am_device_left = enif_make_atom(env, "device_left");

    am_closed = enif_make_atom(env, "closed");

    am_io = enif_make_atom(env, "io");
    am_invalid_param = enif_make_atom(env, "invalid_param");
    am_access = enif_make_atom(env, "access");
    am_no_device = enif_make_atom(env, "no_device");
    am_not_found = enif_make_atom(env, "not_found");
    am_busy = enif_make_atom(env, "busy");
    am_timeout = enif_make_atom(env, "timeout");
    am_overflow = enif_make_atom(env, "overflow");
    am_pipe = enif_make_atom(env, "pipe");
    am_interrupted = enif_make_atom(env, "interrupted");
    am_no_mem = enif_make_atom(env, "no_mem");
    am_not_supported = enif_make_atom(env, "not_supported");
    am_other = enif_make_atom(env, "other");

    // Resources
    usb_nif_device_resource_type = enif_open_resource_type_x(env,
        "usb_device",
        &usb_nif_device_resource_callbacks,
        ERL_NIF_RT_CREATE,
        NULL);

    usb_nif_device_handle_resource_type = enif_open_resource_type_x(env,
        "usb_device_handle",
        &usb_nif_device_handle_resource_callbacks,
        ERL_NIF_RT_CREATE,
        NULL);

    usb_nif_hotplug_monitor_resource_type = enif_open_resource_type_x(env,
        "usb_hotplug_monitor",
        &usb_nif_hotplug_monitor_resource_callbacks,
        ERL_NIF_RT_CREATE,
        NULL);

    *priv_data = usb_nif;

    return 0;
}

static void usb_nif_on_unload(ErlNifEnv *env, void* priv_data)
{
    usb_nif_t *usb_nif = (usb_nif_t *) priv_data;

    // stop thread
    enif_mutex_lock(usb_nif->lock);
    usb_nif->handle_events_thread_running = false;
    enif_mutex_unlock(usb_nif->lock);
    enif_thread_join(usb_nif->handle_events_thread, NULL);

    // Deinitilize device hash table
    enif_mutex_lock(usb_nif->devices_lock);
    kh_destroy(devices, usb_nif->devices);
    enif_mutex_unlock(usb_nif->devices_lock);
    enif_mutex_destroy(usb_nif->devices_lock);

    libusb_exit(usb_nif->context);

    enif_mutex_destroy(usb_nif->lock);

    enif_free(usb_nif);
}


static int usb_nif_on_upgrade(ErlNifEnv *env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info)
{
    // if (*old_priv_data != NULL) {
    //     return -1; /* Don't know how to do that */
    // }
    // if (*priv_data != NULL) {
    //     return -1; /* Don't know how to do that */
    // }
    if (usb_nif_on_load(env, priv_data, load_info)) {
        return -1;
    }
    return 0;
}


static ErlNifFunc nif_funcs[] = {
    {"get_device_list_nif", 0, usb_nif_get_device_list, ERL_NIF_DIRTY_JOB_IO_BOUND},

    {"get_bus_number_nif", 1, usb_nif_get_bus_number},
    {"get_port_number_nif", 1, usb_nif_get_port_number},
    {"get_port_numbers_nif", 1, usb_nif_get_port_numbers},
    {"get_device_address_nif", 1, usb_nif_get_device_address},
    {"get_device_speed_nif", 1, usb_nif_get_device_speed},
    {"get_device_descriptor_nif", 1, usb_nif_get_device_descriptor},
    {"get_config_descriptor_nif", 2, usb_nif_get_config_descriptor},

    {"open_device_nif", 1, usb_nif_open_device},
    {"close_device_nif", 1, usb_nif_close_device},

    {"monitor_hotplug_nif", 1, usb_nif_monitor_hotplug},
    {"demonitor_hotplug_nif", 1, usb_nif_demonitor_hotplug},

    {"claim_interface_nif", 2, usb_nif_claim_interface},
    {"release_interface_nif", 2, usb_nif_release_interface},

    {"clear_halt_nif", 2, usb_nif_clear_halt, ERL_NIF_DIRTY_JOB_IO_BOUND},

    {"set_configuration_nif", 2, usb_nif_set_configuration, ERL_NIF_DIRTY_JOB_IO_BOUND},

    {"read_bulk_nif", 4, usb_nif_read_bulk, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"write_bulk_nif", 4, usb_nif_write_bulk, ERL_NIF_DIRTY_JOB_IO_BOUND},

    {"read_interrupt_nif", 4, usb_nif_read_interrupt, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"write_interrupt_nif", 4, usb_nif_write_interrupt, ERL_NIF_DIRTY_JOB_IO_BOUND},

    {"read_control_nif", 7, usb_nif_read_control, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"write_control_nif", 7, usb_nif_write_control, ERL_NIF_DIRTY_JOB_IO_BOUND},

    {"attach_kernel_driver", 2, usb_nif_attach_kernel_driver},
    {"detach_kernel_driver", 2, usb_nif_detach_kernel_driver}
};


ERL_NIF_INIT(usb, nif_funcs, usb_nif_on_load, NULL, usb_nif_on_upgrade, usb_nif_on_unload);
