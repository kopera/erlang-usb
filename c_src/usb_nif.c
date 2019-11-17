#include <stdbool.h>
#include <stdint.h>
#include <string.h>
#include <assert.h>

#include <string.h>

#include <erl_nif.h>
#include <libusb.h>


#define ARRAY_LENGTH(x) \
    ((sizeof(x)/sizeof(0[x])) / ((size_t)(!(sizeof(x) % sizeof(0[x])))))
#define CHECK_MAP_ARRAYS(keys, values) \
    static_assert(((ARRAY_LENGTH((keys))) == (ARRAY_LENGTH((values)))), "key/value size mismatch")


/* Types */

typedef struct {
    ErlNifMutex    *lock;

    libusb_context *context;

    ErlNifTid       handle_events_thread;
    bool            handle_events_thread_running;
} usb_nif_t;


/* Atoms */

static ERL_NIF_TERM am_ok = 0;
static ERL_NIF_TERM am_error = 0;

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

typedef struct {
    libusb_device  *device;
} usb_nif_device_t;

static ErlNifResourceType* usb_nif_device_resource_type;

static void usb_nif_device_resource_dtor(ErlNifEnv* env, void *obj)
{
    usb_nif_device_t *usb_nif_device = (usb_nif_device_t *) obj;
    libusb_unref_device(usb_nif_device->device);
}


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

static ERL_NIF_TERM libusb_endpoints_export(ErlNifEnv* env, const struct libusb_endpoint_descriptor endpoint_descriptors[], uint8_t num_endpoint_descriptors)
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
        enif_make_map_from_arrays(
            env,
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


static ERL_NIF_TERM libusb_interfaces_export(ErlNifEnv* env, const struct libusb_interface interfaces[], uint8_t num_interfaces)
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
            enif_make_map_from_arrays(
                env,
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
        enif_make_map_from_arrays(
                env,
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

/* API */

static ERL_NIF_TERM usb_nif_get_device_list(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
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
        usb_nif_device_t *usb_device = (usb_nif_device_t*) enif_alloc_resource(
            usb_nif_device_resource_type,
            sizeof(usb_nif_device_t));
        usb_device->device = devices[i - 1];

        result = enif_make_list_cell(
            env,
            enif_make_resource(env, usb_device),
            result);
        enif_release_resource(usb_device);
    }
    libusb_free_device_list(devices, false);

    return enif_make_tuple2(env, am_ok, result);
}

static ERL_NIF_TERM usb_nif_get_device_descriptor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
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
    enif_make_map_from_arrays(
        env,
        keys,
        values,
        ARRAY_LENGTH(keys),
        &result);

    return enif_make_tuple2(env, am_ok, result);
}

static ERL_NIF_TERM usb_nif_get_configuration_descriptor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
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
    enif_make_map_from_arrays(
        env,
        keys,
        values,
        ARRAY_LENGTH(keys),
        &result);

    return enif_make_tuple2(env, am_ok, result);
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
    usb_nif_device_resource_type = enif_open_resource_type(env,
        NULL,
        "usb_device",
        usb_nif_device_resource_dtor,
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

    {"get_device_descriptor_nif", 1, usb_nif_get_device_descriptor},
    {"get_configuration_descriptor_nif", 2, usb_nif_get_configuration_descriptor},
};


ERL_NIF_INIT(usb, nif_funcs, usb_nif_on_load, NULL, usb_nif_on_upgrade, usb_nif_on_unload);