/*

  uim-sni.c: show the uim status as StatusNotifierItems

  Copyright (c) 2026 uim Project https://github.com/uim/uim

  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions
  are met:

  1. Redistributions of source code must retain the above copyright
     notice, this list of conditions and the following disclaimer.
  2. Redistributions in binary form must reproduce the above copyright
     notice, this list of conditions and the following disclaimer in the
     documentation and/or other materials provided with the distribution.
  3. Neither the name of authors nor the names of its contributors
     may be used to endorse or promote products derived from this software
     without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS IS''
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR CONTRIBUTORS BE
  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
  POSSIBILITY OF SUCH DAMAGE.

*/

/* uim-sni shows each branch of prop_list_update (the input method, the
 * input mode and so on) as a StatusNotifierItem as
 * uim-toolbar-gtk3-systray shows each branch as a button. Panels such as
 * Waybar and KDE Plasma show StatusNotifierItems in their trays. */

#include <config.h>

#include <ctype.h>
#include <locale.h>
#include <signal.h>
#include <stdlib.h>
#include <string.h>

#include <gio/gio.h>
#include <glib-unix.h>

#include <uim/gettext.h>
#include <uim/uim.h>
#include <uim/uim-helper.h>

/* The freedesktop.org draft names them org.freedesktop.*, but the
 * draft has never been finished and the implementations use the
 * org.kde.* names: KDE Plasma, libappindicator, Waybar and so on.
 * Waybar doesn't provide org.freedesktop.StatusNotifierWatcher. */
#define WATCHER_NAME      "org.kde.StatusNotifierWatcher"
#define WATCHER_PATH      "/StatusNotifierWatcher"
#define WATCHER_INTERFACE "org.kde.StatusNotifierWatcher"
#define ITEM_PATH         "/StatusNotifierItem"
#define ITEM_INTERFACE    "org.kde.StatusNotifierItem"
/* Owned to run only one uim-sni in a session. */
#define UIM_SNI_NAME "org.uim.Sni"

/* Seconds to wait before connecting to uim-helper-server again. */
#define HELPER_RECONNECT_INTERVAL 1

static const gchar item_xml[] =
  "<node>"
  "  <interface name='" ITEM_INTERFACE "'>"
  "    <property name='Category' type='s' access='read'/>"
  "    <property name='Id' type='s' access='read'/>"
  "    <property name='Title' type='s' access='read'/>"
  "    <property name='Status' type='s' access='read'/>"
  "    <property name='IconName' type='s' access='read'/>"
  "    <property name='IconThemePath' type='s' access='read'/>"
  "    <property name='IconPixmap' type='a(iiay)' access='read'/>"
  "    <property name='AttentionIconName' type='s' access='read'/>"
  "    <property name='AttentionIconPixmap' type='a(iiay)' access='read'/>"
  "    <property name='ToolTip' type='(sa(iiay)ss)' access='read'/>"
  "    <property name='ItemIsMenu' type='b' access='read'/>"
  "    <method name='Activate'>"
  "      <arg name='x' type='i' direction='in'/>"
  "      <arg name='y' type='i' direction='in'/>"
  "    </method>"
  "    <method name='SecondaryActivate'>"
  "      <arg name='x' type='i' direction='in'/>"
  "      <arg name='y' type='i' direction='in'/>"
  "    </method>"
  "    <method name='ContextMenu'>"
  "      <arg name='x' type='i' direction='in'/>"
  "      <arg name='y' type='i' direction='in'/>"
  "    </method>"
  "    <method name='Scroll'>"
  "      <arg name='delta' type='i' direction='in'/>"
  "      <arg name='orientation' type='s' direction='in'/>"
  "    </method>"
  "    <signal name='NewTitle'/>"
  "    <signal name='NewIcon'/>"
  "    <signal name='NewToolTip'/>"
  "  </interface>"
  "</node>";

/* Each item has its own connection because StatusNotifierWatcher
 * forgets an item only when its connection is gone. */
typedef struct
{
  GDBusConnection *connection;
  guint registration_id;
  gchar *id;
  gchar *icon_name;
  gchar *title;
  /* The watcher_generation when the item was sent to the watcher. */
  guint generation;
} Item;

static GDBusNodeInfo *item_node_info;
/* Item *: the branches in the order of prop_list_update. */
static GPtrArray *items;
static gboolean watcher_found;
/* Incremented whenever a watcher appears. The items whose generation
 * differs from it are sent to the current watcher. 0 means no watcher
 * has appeared yet. */
static guint watcher_generation;
/* Whether RegisterStatusNotifierItem is in flight. The items are
 * registered one by one to keep their order in the tray. */
static gboolean registering;
static int helper_fd = -1;
static guint helper_source_id;
static guint helper_reconnect_id;
static GMainLoop *loop;
static gboolean name_acquired;
static guint watcher_id;
static int exit_status = EXIT_SUCCESS;

static void
item_handle_method_call(GDBusConnection *connection,
                        const gchar *sender,
                        const gchar *object_path,
                        const gchar *interface_name,
                        const gchar *method_name,
                        GVariant *parameters,
                        GDBusMethodInvocation *invocation,
                        gpointer user_data)
{
  /* Clicks do nothing yet. */
  g_dbus_method_invocation_return_value(invocation, NULL);
}

/* The icons are given by name. */
static GVariant *
empty_pixmaps(void)
{
  return g_variant_new_array(G_VARIANT_TYPE("(iiay)"), NULL, 0);
}

static GVariant *
item_get_property(GDBusConnection *connection,
                  const gchar *sender,
                  const gchar *object_path,
                  const gchar *interface_name,
                  const gchar *property_name,
                  GError **error,
                  gpointer user_data)
{
  Item *item = user_data;

  if (g_str_equal(property_name, "Category"))
    return g_variant_new_string("SystemServices");
  if (g_str_equal(property_name, "Id"))
    return g_variant_new_string(item->id);
  if (g_str_equal(property_name, "Title"))
    return g_variant_new_string(item->title);
  if (g_str_equal(property_name, "Status"))
    return g_variant_new_string("Active");
  if (g_str_equal(property_name, "IconName"))
    return g_variant_new_string(item->icon_name);
  if (g_str_equal(property_name, "IconThemePath"))
    return g_variant_new_string(UIM_PIXMAPSDIR);
  /* Some panels such as swaybar complain about missing properties. */
  if (g_str_equal(property_name, "IconPixmap"))
    return empty_pixmaps();
  if (g_str_equal(property_name, "AttentionIconName"))
    return g_variant_new_string("");
  if (g_str_equal(property_name, "AttentionIconPixmap"))
    return empty_pixmaps();
  if (g_str_equal(property_name, "ToolTip"))
    return g_variant_new("(s@a(iiay)ss)",
                         item->icon_name,
                         empty_pixmaps(),
                         item->title,
                         "");
  if (g_str_equal(property_name, "ItemIsMenu"))
    return g_variant_new_boolean(FALSE);

  g_set_error(error,
              G_DBUS_ERROR,
              G_DBUS_ERROR_UNKNOWN_PROPERTY,
              "Unknown property: %s",
              property_name);
  return NULL;
}

static const GDBusInterfaceVTable item_vtable = {
  item_handle_method_call,
  item_get_property,
  NULL,
  {NULL},
};

static void
item_free(gpointer data)
{
  Item *item = data;

  if (item->connection) {
    if (item->registration_id > 0)
      g_dbus_connection_unregister_object(item->connection,
                                          item->registration_id);
    /* The watcher removes the item when the connection is closed. */
    g_dbus_connection_close(item->connection, NULL, NULL, NULL);
    g_object_unref(item->connection);
  }
  g_free(item->id);
  g_free(item->icon_name);
  g_free(item->title);
  g_free(item);
}

static Item *
item_new(guint index)
{
  Item *item;
  gchar *address;
  GError *error = NULL;

  address = g_dbus_address_get_for_bus_sync(G_BUS_TYPE_SESSION, NULL, &error);
  if (!address) {
    g_warning("failed to find the session bus: %s", error->message);
    g_error_free(error);
    return NULL;
  }

  item = g_new0(Item, 1);
  item->id = g_strdup_printf("uim-%u", index);
  item->icon_name = g_strdup("");
  item->title = g_strdup("");
  item->connection = g_dbus_connection_new_for_address_sync(
    address,
    G_DBUS_CONNECTION_FLAGS_AUTHENTICATION_CLIENT |
      G_DBUS_CONNECTION_FLAGS_MESSAGE_BUS_CONNECTION,
    NULL,
    NULL,
    &error);
  g_free(address);
  if (!item->connection) {
    g_warning("failed to connect to the session bus: %s", error->message);
    g_error_free(error);
    item_free(item);
    return NULL;
  }

  item->registration_id =
    g_dbus_connection_register_object(item->connection,
                                      ITEM_PATH,
                                      item_node_info->interfaces[0],
                                      &item_vtable,
                                      item,
                                      NULL,
                                      &error);
  if (item->registration_id == 0) {
    g_warning("failed to register an item: %s", error->message);
    g_error_free(error);
    item_free(item);
    return NULL;
  }

  return item;
}

static void
item_emit(Item *item, const gchar *signal_name)
{
  g_dbus_connection_emit_signal(item->connection,
                                NULL,
                                ITEM_PATH,
                                ITEM_INTERFACE,
                                signal_name,
                                NULL,
                                NULL);
}

static void
item_update(Item *item, const gchar *icon_name, const gchar *title)
{
  gboolean icon_changed = !g_str_equal(item->icon_name, icon_name);
  gboolean title_changed = !g_str_equal(item->title, title);

  if (icon_changed) {
    g_free(item->icon_name);
    item->icon_name = g_strdup(icon_name);
    item_emit(item, "NewIcon");
  }
  if (title_changed) {
    g_free(item->title);
    item->title = g_strdup(title);
    item_emit(item, "NewTitle");
  }
  /* The tool tip shows both of them. */
  if (icon_changed || title_changed)
    item_emit(item, "NewToolTip");
}

static void
register_next_item(void);

static void
register_item_cb(GObject *source, GAsyncResult *result, gpointer user_data)
{
  GVariant *reply;
  GError *error = NULL;

  registering = FALSE;

  reply =
    g_dbus_connection_call_finish(G_DBUS_CONNECTION(source), result, &error);
  if (reply) {
    g_variant_unref(reply);
  } else {
    /* The item may be removed or the watcher may be gone. The item
     * isn't sent again until a watcher appears. */
    g_debug("failed to register an item: %s", error->message);
    g_error_free(error);
  }

  register_next_item();
}

static void
register_next_item(void)
{
  guint i;

  if (!watcher_found || registering)
    return;

  for (i = 0; i < items->len; i++) {
    Item *item = g_ptr_array_index(items, i);

    if (item->generation == watcher_generation)
      continue;

    item->generation = watcher_generation;
    registering = TRUE;
    /* Use the item's connection because the watcher may take the
     * sender as the item. */
    g_dbus_connection_call(
      item->connection,
      WATCHER_NAME,
      WATCHER_PATH,
      WATCHER_INTERFACE,
      "RegisterStatusNotifierItem",
      g_variant_new("(s)", g_dbus_connection_get_unique_name(item->connection)),
      NULL,
      G_DBUS_CALL_FLAGS_NONE,
      -1,
      NULL,
      register_item_cb,
      NULL);
    return;
  }
}

static void
watcher_appeared_cb(GDBusConnection *connection,
                    const gchar *name,
                    const gchar *name_owner,
                    gpointer user_data)
{
  watcher_found = TRUE;
  /* A new watcher doesn't know the items. */
  watcher_generation++;
  register_next_item();
}

static void
watcher_vanished_cb(GDBusConnection *connection,
                    const gchar *name,
                    gpointer user_data)
{
  watcher_found = FALSE;
}

static const gchar *
safe_gettext(const gchar *msgid)
{
  const gchar *p;

  /* Labels in non-ASCII are already translated. */
  for (p = msgid; *p && isascii((unsigned char)*p); p++)
    continue;

  return *p ? msgid : gettext(msgid);
}

static void
handle_prop_list_update(gchar **lines)
{
  const gchar *charset = NULL;
  guint n_branches = 0;
  guint i;

  if (lines[1] && g_str_has_prefix(lines[1], "charset="))
    charset = lines[1] + strlen("charset=");

  for (i = 2; lines[i] && lines[i][0]; i++) {
    gchar *utf8_line = NULL;
    gchar **columns;

    if (charset && g_ascii_strcasecmp(charset, "UTF-8") != 0)
      utf8_line = g_convert(lines[i], -1, "UTF-8", charset, NULL, NULL, NULL);
    columns = g_strsplit(utf8_line ? utf8_line : lines[i], "\t", 0);
    g_free(utf8_line);

    /* branch \t indication_id \t iconic_label \t label_string */
    if (g_strv_length(columns) >= 4 && g_str_equal(columns[0], "branch")) {
      Item *item;

      if (n_branches < items->len) {
        item = g_ptr_array_index(items, n_branches);
      } else {
        item = item_new(n_branches);
        if (item)
          g_ptr_array_add(items, item);
      }
      if (item) {
        item_update(item, columns[1], safe_gettext(columns[3]));
        n_branches++;
      }
    }
    g_strfreev(columns);
  }

  /* The other input method may have fewer branches. */
  if (n_branches < items->len)
    g_ptr_array_set_size(items, n_branches);

  register_next_item();
}

static void
handle_message(gchar *message)
{
  gchar **lines = g_strsplit(message, "\n", 0);

  if (lines[0] && g_str_equal(lines[0], "prop_list_update"))
    handle_prop_list_update(lines);

  g_strfreev(lines);
}

static gboolean
helper_read_cb(gint fd, GIOCondition condition, gpointer user_data)
{
  gchar *message;

  /* This calls helper_disconnect_cb() on EOF. */
  uim_helper_read_proc(fd);
  while ((message = uim_helper_get_message())) {
    handle_message(message);
    free(message);
  }

  return G_SOURCE_CONTINUE;
}

static gboolean
helper_connect(void);

static gboolean
helper_reconnect_cb(gpointer user_data)
{
  if (helper_connect()) {
    helper_reconnect_id = 0;
    return G_SOURCE_REMOVE;
  }
  return G_SOURCE_CONTINUE;
}

static void
helper_schedule_reconnect(void)
{
  if (helper_reconnect_id == 0)
    helper_reconnect_id = g_timeout_add_seconds(HELPER_RECONNECT_INTERVAL,
                                                helper_reconnect_cb,
                                                NULL);
}

static void
helper_disconnect_cb(void)
{
  helper_fd = -1;
  if (helper_source_id > 0) {
    g_source_remove(helper_source_id);
    helper_source_id = 0;
  }
  helper_schedule_reconnect();
}

static gboolean
helper_connect(void)
{
  helper_fd = uim_helper_init_client_fd(helper_disconnect_cb);
  if (helper_fd < 0)
    return FALSE;

  helper_source_id = g_unix_fd_add(helper_fd,
                                   G_IO_IN | G_IO_HUP | G_IO_ERR,
                                   helper_read_cb,
                                   NULL);
  /* The current input method sends prop_list_update. */
  uim_helper_client_get_prop_list();
  return TRUE;
}

static void
name_acquired_cb(GDBusConnection *connection,
                 const gchar *name,
                 gpointer user_data)
{
  name_acquired = TRUE;
  watcher_id = g_bus_watch_name(G_BUS_TYPE_SESSION,
                                WATCHER_NAME,
                                G_BUS_NAME_WATCHER_FLAGS_NONE,
                                watcher_appeared_cb,
                                watcher_vanished_cb,
                                NULL,
                                NULL);
  if (!helper_connect())
    helper_schedule_reconnect();
}

static void
name_lost_cb(GDBusConnection *connection, const gchar *name, gpointer user_data)
{
  /* The name is lost after it's acquired only when the bus is gone. */
  if (!name_acquired) {
    if (connection)
      g_printerr("uim-sni: another uim-sni is running\n");
    else
      g_printerr("uim-sni: failed to connect to the session bus\n");
  }
  exit_status = EXIT_FAILURE;
  g_main_loop_quit(loop);
}

static gboolean
quit_cb(gpointer user_data)
{
  g_main_loop_quit(loop);
  return G_SOURCE_CONTINUE;
}

int
main(int argc, char *argv[])
{
  guint owner_id;

  setlocale(LC_ALL, "");
  bindtextdomain(PACKAGE, LOCALEDIR);
  textdomain(PACKAGE);
  bind_textdomain_codeset(PACKAGE, "UTF-8");

  if (uim_init() < 0)
    return EXIT_FAILURE;

  item_node_info = g_dbus_node_info_new_for_xml(item_xml, NULL);
  items = g_ptr_array_new_with_free_func(item_free);
  loop = g_main_loop_new(NULL, FALSE);

  owner_id = g_bus_own_name(G_BUS_TYPE_SESSION,
                            UIM_SNI_NAME,
                            G_BUS_NAME_OWNER_FLAGS_DO_NOT_QUEUE,
                            NULL,
                            name_acquired_cb,
                            name_lost_cb,
                            NULL,
                            NULL);
  g_unix_signal_add(SIGINT, quit_cb, NULL);
  g_unix_signal_add(SIGTERM, quit_cb, NULL);

  g_main_loop_run(loop);

  if (helper_fd >= 0)
    uim_helper_close_client_fd(helper_fd);
  if (helper_reconnect_id > 0)
    g_source_remove(helper_reconnect_id);
  if (watcher_id > 0)
    g_bus_unwatch_name(watcher_id);
  g_bus_unown_name(owner_id);
  g_ptr_array_unref(items);
  g_dbus_node_info_unref(item_node_info);
  g_main_loop_unref(loop);

  uim_quit();

  return exit_status;
}
