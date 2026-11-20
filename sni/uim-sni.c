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
#include <uim/uim-scm.h>

/* The freedesktop.org draft names them org.freedesktop.*, but the
 * draft has never been finished and the implementations use the
 * org.kde.* names: KDE Plasma, libappindicator, Waybar and so on.
 * Waybar doesn't provide org.freedesktop.StatusNotifierWatcher. */
#define WATCHER_NAME      "org.kde.StatusNotifierWatcher"
#define WATCHER_PATH      "/StatusNotifierWatcher"
#define WATCHER_INTERFACE "org.kde.StatusNotifierWatcher"
#define ITEM_PATH         "/StatusNotifierItem"
#define ITEM_INTERFACE    "org.kde.StatusNotifierItem"
#define MENU_PATH         "/MenuBar"
#define MENU_INTERFACE    "com.canonical.dbusmenu"
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
  "    <property name='Menu' type='o' access='read'/>"
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

/* The menu of an item: the leaves of the branch. */
static const gchar menu_xml[] =
  "<node>"
  "  <interface name='" MENU_INTERFACE "'>"
  "    <property name='Version' type='u' access='read'/>"
  "    <property name='TextDirection' type='s' access='read'/>"
  "    <property name='Status' type='s' access='read'/>"
  "    <property name='IconThemePath' type='as' access='read'/>"
  "    <method name='GetLayout'>"
  "      <arg name='parentId' type='i' direction='in'/>"
  "      <arg name='recursionDepth' type='i' direction='in'/>"
  "      <arg name='propertyNames' type='as' direction='in'/>"
  "      <arg name='revision' type='u' direction='out'/>"
  "      <arg name='layout' type='(ia{sv}av)' direction='out'/>"
  "    </method>"
  "    <method name='GetGroupProperties'>"
  "      <arg name='ids' type='ai' direction='in'/>"
  "      <arg name='propertyNames' type='as' direction='in'/>"
  "      <arg name='properties' type='a(ia{sv})' direction='out'/>"
  "    </method>"
  "    <method name='GetProperty'>"
  "      <arg name='id' type='i' direction='in'/>"
  "      <arg name='name' type='s' direction='in'/>"
  "      <arg name='value' type='v' direction='out'/>"
  "    </method>"
  "    <method name='Event'>"
  "      <arg name='id' type='i' direction='in'/>"
  "      <arg name='eventId' type='s' direction='in'/>"
  "      <arg name='data' type='v' direction='in'/>"
  "      <arg name='timestamp' type='u' direction='in'/>"
  "    </method>"
  "    <method name='EventGroup'>"
  "      <arg name='events' type='a(isvu)' direction='in'/>"
  "      <arg name='idErrors' type='ai' direction='out'/>"
  "    </method>"
  "    <method name='AboutToShow'>"
  "      <arg name='id' type='i' direction='in'/>"
  "      <arg name='needUpdate' type='b' direction='out'/>"
  "    </method>"
  "    <method name='AboutToShowGroup'>"
  "      <arg name='ids' type='ai' direction='in'/>"
  "      <arg name='updatesNeeded' type='ai' direction='out'/>"
  "      <arg name='idErrors' type='ai' direction='out'/>"
  "    </method>"
  "    <signal name='ItemsPropertiesUpdated'>"
  "      <arg name='updatedProps' type='a(ia{sv})'/>"
  "      <arg name='removedProps' type='a(ias)'/>"
  "    </signal>"
  "    <signal name='LayoutUpdated'>"
  "      <arg name='revision' type='u'/>"
  "      <arg name='parent' type='i'/>"
  "    </signal>"
  "    <signal name='ItemActivationRequested'>"
  "      <arg name='id' type='i'/>"
  "      <arg name='timestamp' type='u'/>"
  "    </signal>"
  "  </interface>"
  "</node>";

/* A leaf of prop_list_update: a menu item. */
typedef struct
{
  gchar *indication_id;
  gchar *label;
  gchar *action_id;
  gboolean selected;
} Leaf;

/* Each item has its own connection because StatusNotifierWatcher
 * forgets an item only when its connection is gone. */
typedef struct
{
  GDBusConnection *connection;
  guint registration_id;
  guint menu_registration_id;
  gchar *id;
  /* The indication_id of the branch. The icon name is made from it. */
  gchar *indication_id;
  gchar *title;
  /* The watcher_generation when the item was sent to the watcher. */
  guint generation;
  /* Leaf *: the menu items. The ID of a menu item is menu_id_base +
   * the index + 1 because 0 is the root. */
  GPtrArray *leaves;
  /* Increased whenever the leaves change so that an event for a menu
   * shown before doesn't choose another leaf. */
  gint32 menu_id_base;
  /* Incremented whenever the leaves change. */
  guint menu_revision;
} Item;

static GDBusNodeInfo *item_node_info;
static GDBusNodeInfo *menu_node_info;
/* UIM_PIXMAPSDIR, or $UIM_SNI_PIXMAPSDIR for the tests that run before
 * the icons are installed. */
static const gchar *pixmaps_dir;
/* toolbar-icon-for-dark-background? */
static gboolean dark_background;
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
helper_send(const gchar *message)
{
  if (helper_fd >= 0)
    uim_helper_send_message(helper_fd, message);
}

static void
leaf_activate(const Leaf *leaf)
{
  gchar *message = g_strdup_printf("prop_activate\n%s\n", leaf->action_id);

  helper_send(message);
  g_free(message);
}

/* Chooses the next (STEP is 1) or previous (STEP is -1) leaf of the
 * selected one, such as the next input mode. It wraps around. */
static void
item_step(Item *item, gint step)
{
  guint n_leaves = item->leaves->len;
  guint selected = 0;
  guint i;

  for (i = 0; i < n_leaves; i++) {
    const Leaf *leaf = g_ptr_array_index(item->leaves, i);

    if (leaf->selected) {
      selected = i;
      break;
    }
  }
  for (i = 1; i < n_leaves; i++) {
    guint index = (selected + (step > 0 ? i : n_leaves - i)) % n_leaves;
    const Leaf *leaf = g_ptr_array_index(item->leaves, index);

    /* A separator has no action. */
    if (leaf->action_id[0]) {
      leaf_activate(leaf);
      return;
    }
  }
}

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
  Item *item = user_data;

  if (g_str_equal(method_name, "Activate")) {
    item_step(item, 1);
  } else if (g_str_equal(method_name, "Scroll")) {
    gint32 delta;
    const gchar *orientation;

    g_variant_get(parameters, "(i&s)", &delta, &orientation);
    if (g_ascii_strcasecmp(orientation, "vertical") == 0 && delta != 0)
      item_step(item, delta > 0 ? 1 : -1);
  }
  /* SecondaryActivate and ContextMenu do nothing: there is nothing for
   * a middle click, and the panel shows the menu. */
  g_dbus_method_invocation_return_value(invocation, NULL);
}

/* The icons are given by name. */
static GVariant *
empty_pixmaps(void)
{
  return g_variant_new_array(G_VARIANT_TYPE("(iiay)"), NULL, 0);
}

static gboolean
pixmap_exists(const gchar *name)
{
  /* The icons are installed as SVG at least. */
  gchar *basename = g_strconcat(name, ".svg", NULL);
  gchar *path = g_build_filename(pixmaps_dir, basename, NULL);
  gboolean exists = g_file_test(path, G_FILE_TEST_IS_REGULAR);

  g_free(basename);
  g_free(path);
  return exists;
}

/* Uses the icon for a dark background if it's preferred and exists as
 * the other toolbars do. */
static gchar *
make_icon_name(const gchar *indication_id)
{
  if (dark_background) {
    gchar *name = g_strconcat(indication_id, "_dark_background", NULL);

    if (pixmap_exists(name))
      return name;
    g_free(name);
  }
  return g_strdup(indication_id);
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
    return g_variant_new_take_string(make_icon_name(item->indication_id));
  if (g_str_equal(property_name, "IconThemePath"))
    return g_variant_new_string(pixmaps_dir);
  /* Some panels such as swaybar complain about missing properties. */
  if (g_str_equal(property_name, "IconPixmap"))
    return empty_pixmaps();
  if (g_str_equal(property_name, "AttentionIconName"))
    return g_variant_new_string("");
  if (g_str_equal(property_name, "AttentionIconPixmap"))
    return empty_pixmaps();
  if (g_str_equal(property_name, "ToolTip"))
    return g_variant_new(
      "(@s@a(iiay)ss)",
      g_variant_new_take_string(make_icon_name(item->indication_id)),
      empty_pixmaps(),
      item->title,
      "");
  /* A click is Activate, not showing the menu, for the panels that
   * don't show the menu such as swaybar. The panels that show the menu
   * do so for a right click. */
  if (g_str_equal(property_name, "ItemIsMenu"))
    return g_variant_new_boolean(FALSE);
  if (g_str_equal(property_name, "Menu"))
    return g_variant_new_object_path(MENU_PATH);

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

static Leaf *
leaf_new(const gchar *indication_id,
         const gchar *label,
         const gchar *action_id,
         gboolean selected)
{
  Leaf *leaf = g_new0(Leaf, 1);

  leaf->indication_id = g_strdup(indication_id);
  leaf->label = g_strdup(label);
  leaf->action_id = g_strdup(action_id);
  leaf->selected = selected;
  return leaf;
}

static void
leaf_free(gpointer data)
{
  Leaf *leaf = data;

  g_free(leaf->indication_id);
  g_free(leaf->label);
  g_free(leaf->action_id);
  g_free(leaf);
}

static gboolean
leaf_equal(const Leaf *leaf1, const Leaf *leaf2)
{
  return g_str_equal(leaf1->indication_id, leaf2->indication_id) &&
         g_str_equal(leaf1->label, leaf2->label) &&
         g_str_equal(leaf1->action_id, leaf2->action_id) &&
         leaf1->selected == leaf2->selected;
}

/* Returns the leaf of the menu item ID, or NULL. */
static Leaf *
item_get_leaf(Item *item, gint32 id)
{
  gint64 index = (gint64)id - item->menu_id_base - 1;

  if (index < 0 || index >= item->leaves->len)
    return NULL;
  return g_ptr_array_index(item->leaves, index);
}

static GVariant *
leaf_properties(Leaf *leaf)
{
  GVariantBuilder builder;

  g_variant_builder_init(&builder, G_VARIANT_TYPE_VARDICT);
  /* An indication_id for "separator" makes a separator. */
  if (g_str_equal(leaf->indication_id, "separator")) {
    g_variant_builder_add(&builder,
                          "{sv}",
                          "type",
                          g_variant_new_string("separator"));
  } else {
    /* "_" in a label marks the access key. */
    gchar **parts = g_strsplit(leaf->label, "_", -1);
    gchar *label = g_strjoinv("__", parts);

    g_strfreev(parts);
    g_variant_builder_add(&builder,
                          "{sv}",
                          "label",
                          g_variant_new_take_string(label));
    g_variant_builder_add(&builder,
                          "{sv}",
                          "toggle-type",
                          g_variant_new_string("radio"));
    g_variant_builder_add(&builder,
                          "{sv}",
                          "toggle-state",
                          g_variant_new_int32(leaf->selected ? 1 : 0));
  }
  return g_variant_builder_end(&builder);
}

static GVariant *
root_properties(void)
{
  GVariantBuilder builder;

  g_variant_builder_init(&builder, G_VARIANT_TYPE_VARDICT);
  g_variant_builder_add(&builder,
                        "{sv}",
                        "children-display",
                        g_variant_new_string("submenu"));
  return g_variant_builder_end(&builder);
}

/* Returns the properties of the menu item ID, or NULL. */
static GVariant *
item_menu_properties(Item *item, gint32 id)
{
  Leaf *leaf;

  if (id == 0)
    return root_properties();
  leaf = item_get_leaf(item, id);
  return leaf ? leaf_properties(leaf) : NULL;
}

static GVariant *
leaf_layout(gint32 id, Leaf *leaf)
{
  return g_variant_new("(i@a{sv}@av)",
                       id,
                       leaf_properties(leaf),
                       g_variant_new_array(G_VARIANT_TYPE_VARIANT, NULL, 0));
}

/* The menu is a list of the leaves under the root. Returns the layout
 * under the menu item ID, or NULL. */
static GVariant *
item_menu_layout(Item *item, gint32 id)
{
  GVariantBuilder children;
  Leaf *leaf;
  guint i;

  if (id != 0) {
    leaf = item_get_leaf(item, id);
    return leaf ? leaf_layout(id, leaf) : NULL;
  }
  g_variant_builder_init(&children, G_VARIANT_TYPE("av"));
  for (i = 0; i < item->leaves->len; i++) {
    g_variant_builder_add(&children,
                          "v",
                          leaf_layout(item->menu_id_base + (gint32)i + 1,
                                      g_ptr_array_index(item->leaves, i)));
  }
  return g_variant_new("(i@a{sv}av)", 0, root_properties(), &children);
}

/* Returns whether ID is a menu item. */
static gboolean
item_menu_event(Item *item, gint32 id, const gchar *event_id)
{
  Leaf *leaf;

  if (id == 0)
    return TRUE;
  leaf = item_get_leaf(item, id);
  if (!leaf)
    return FALSE;
  if (g_str_equal(event_id, "clicked") && leaf->action_id[0])
    leaf_activate(leaf);
  return TRUE;
}

static void
menu_handle_method_call(GDBusConnection *connection,
                        const gchar *sender,
                        const gchar *object_path,
                        const gchar *interface_name,
                        const gchar *method_name,
                        GVariant *parameters,
                        GDBusMethodInvocation *invocation,
                        gpointer user_data)
{
  Item *item = user_data;

  /* The menu is small, so the whole of it is always sent: the depth and
   * the property names to send are ignored. */
  if (g_str_equal(method_name, "GetLayout")) {
    gint32 id;
    GVariant *layout;

    g_variant_get(parameters, "(iias)", &id, NULL, NULL);
    layout = item_menu_layout(item, id);
    if (layout)
      g_dbus_method_invocation_return_value(
        invocation,
        g_variant_new("(u@(ia{sv}av))", item->menu_revision, layout));
    else
      g_dbus_method_invocation_return_error(invocation,
                                            G_DBUS_ERROR,
                                            G_DBUS_ERROR_INVALID_ARGS,
                                            "Unknown menu item: %d",
                                            id);
  } else if (g_str_equal(method_name, "GetGroupProperties")) {
    GVariantIter *ids;
    gint32 id;
    GVariantBuilder properties;

    g_variant_get(parameters, "(aias)", &ids, NULL);
    g_variant_builder_init(&properties, G_VARIANT_TYPE("a(ia{sv})"));
    while (g_variant_iter_next(ids, "i", &id)) {
      GVariant *item_properties = item_menu_properties(item, id);

      if (item_properties)
        g_variant_builder_add(&properties, "(i@a{sv})", id, item_properties);
    }
    g_variant_iter_free(ids);
    g_dbus_method_invocation_return_value(
      invocation,
      g_variant_new("(a(ia{sv}))", &properties));
  } else if (g_str_equal(method_name, "GetProperty")) {
    gint32 id;
    const gchar *name;
    GVariant *item_properties;
    GVariant *value = NULL;

    g_variant_get(parameters, "(i&s)", &id, &name);
    item_properties = item_menu_properties(item, id);
    if (item_properties) {
      value = g_variant_lookup_value(item_properties, name, NULL);
      g_variant_unref(g_variant_ref_sink(item_properties));
    }
    if (value)
      g_dbus_method_invocation_return_value(
        invocation,
        g_variant_new("(@v)", g_variant_new_variant(value)));
    else
      g_dbus_method_invocation_return_error(invocation,
                                            G_DBUS_ERROR,
                                            G_DBUS_ERROR_INVALID_ARGS,
                                            "Unknown property: %d %s",
                                            id,
                                            name);
    if (value)
      g_variant_unref(value);
  } else if (g_str_equal(method_name, "Event")) {
    gint32 id;
    const gchar *event_id;

    g_variant_get(parameters, "(i&svu)", &id, &event_id, NULL, NULL);
    item_menu_event(item, id, event_id);
    g_dbus_method_invocation_return_value(invocation, NULL);
  } else if (g_str_equal(method_name, "EventGroup")) {
    GVariantIter *events;
    gint32 id;
    const gchar *event_id;
    GVariantBuilder id_errors;

    g_variant_get(parameters, "(a(isvu))", &events);
    g_variant_builder_init(&id_errors, G_VARIANT_TYPE("ai"));
    while (g_variant_iter_next(events, "(i&svu)", &id, &event_id, NULL, NULL)) {
      if (!item_menu_event(item, id, event_id))
        g_variant_builder_add(&id_errors, "i", id);
    }
    g_variant_iter_free(events);
    g_dbus_method_invocation_return_value(invocation,
                                          g_variant_new("(ai)", &id_errors));
  } else if (g_str_equal(method_name, "AboutToShow")) {
    /* The menu is updated with LayoutUpdated. */
    g_dbus_method_invocation_return_value(invocation,
                                          g_variant_new("(b)", FALSE));
  } else if (g_str_equal(method_name, "AboutToShowGroup")) {
    g_dbus_method_invocation_return_value(
      invocation,
      g_variant_new("(@ai@ai)",
                    g_variant_new_array(G_VARIANT_TYPE_INT32, NULL, 0),
                    g_variant_new_array(G_VARIANT_TYPE_INT32, NULL, 0)));
  } else {
    g_dbus_method_invocation_return_error(invocation,
                                          G_DBUS_ERROR,
                                          G_DBUS_ERROR_UNKNOWN_METHOD,
                                          "Unknown method: %s",
                                          method_name);
  }
}

static GVariant *
menu_get_property(GDBusConnection *connection,
                  const gchar *sender,
                  const gchar *object_path,
                  const gchar *interface_name,
                  const gchar *property_name,
                  GError **error,
                  gpointer user_data)
{
  if (g_str_equal(property_name, "Version"))
    return g_variant_new_uint32(3);
  if (g_str_equal(property_name, "TextDirection"))
    return g_variant_new_string("ltr");
  if (g_str_equal(property_name, "Status"))
    return g_variant_new_string("normal");
  if (g_str_equal(property_name, "IconThemePath"))
    return g_variant_new_strv(NULL, 0);

  g_set_error(error,
              G_DBUS_ERROR,
              G_DBUS_ERROR_UNKNOWN_PROPERTY,
              "Unknown property: %s",
              property_name);
  return NULL;
}

static const GDBusInterfaceVTable menu_vtable = {
  menu_handle_method_call,
  menu_get_property,
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
    if (item->menu_registration_id > 0)
      g_dbus_connection_unregister_object(item->connection,
                                          item->menu_registration_id);
    /* The watcher removes the item when the connection is closed. */
    g_dbus_connection_close(item->connection, NULL, NULL, NULL);
    g_object_unref(item->connection);
  }
  g_free(item->id);
  g_free(item->indication_id);
  g_free(item->title);
  g_ptr_array_unref(item->leaves);
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
  item->indication_id = g_strdup("");
  item->title = g_strdup("");
  item->leaves = g_ptr_array_new_with_free_func(leaf_free);
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

  item->menu_registration_id =
    g_dbus_connection_register_object(item->connection,
                                      MENU_PATH,
                                      menu_node_info->interfaces[0],
                                      &menu_vtable,
                                      item,
                                      NULL,
                                      &error);
  if (item->menu_registration_id == 0) {
    g_warning("failed to register a menu: %s", error->message);
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
item_update(Item *item, const gchar *indication_id, const gchar *title)
{
  gboolean icon_changed = !g_str_equal(item->indication_id, indication_id);
  gboolean title_changed = !g_str_equal(item->title, title);

  if (icon_changed) {
    g_free(item->indication_id);
    item->indication_id = g_strdup(indication_id);
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

/* Takes LEAVES. */
static void
item_set_leaves(Item *item, GPtrArray *leaves)
{
  gboolean changed = leaves->len != item->leaves->len;
  guint i;

  for (i = 0; !changed && i < leaves->len; i++)
    changed = !leaf_equal(g_ptr_array_index(leaves, i),
                          g_ptr_array_index(item->leaves, i));
  if (!changed) {
    g_ptr_array_unref(leaves);
    return;
  }
  item->menu_id_base += item->leaves->len;
  g_ptr_array_unref(item->leaves);
  item->leaves = leaves;
  item->menu_revision++;
  g_dbus_connection_emit_signal(item->connection,
                                NULL,
                                MENU_PATH,
                                MENU_INTERFACE,
                                "LayoutUpdated",
                                g_variant_new("(ui)", item->menu_revision, 0),
                                NULL);
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
  /* The item of the branch and its leaves being read. */
  Item *item = NULL;
  GPtrArray *leaves = NULL;
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
      if (item)
        item_set_leaves(item, leaves);
      else if (leaves)
        g_ptr_array_unref(leaves);
      leaves = g_ptr_array_new_with_free_func(leaf_free);

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
    /* leaf \t indication_id \t iconic_label \t label_string \t
     * short_desc \t action_id \t activity */
    else if (g_strv_length(columns) >= 7 && g_str_equal(columns[0], "leaf") &&
             leaves) {
      g_ptr_array_add(leaves,
                      leaf_new(columns[1],
                               safe_gettext(columns[3]),
                               columns[5],
                               g_str_equal(columns[6], "*")));
    }
    g_strfreev(columns);
  }
  if (item)
    item_set_leaves(item, leaves);
  else if (leaves)
    g_ptr_array_unref(leaves);

  /* The other input method may have fewer branches. */
  if (n_branches < items->len)
    g_ptr_array_set_size(items, n_branches);

  register_next_item();
}

static void
load_customs(void)
{
  dark_background =
    uim_scm_symbol_value_bool("toolbar-icon-for-dark-background?");
}

static void
apply_customs(void)
{
  gboolean old_dark_background = dark_background;
  guint i;

  load_customs();
  if (dark_background == old_dark_background)
    return;
  for (i = 0; i < items->len; i++) {
    Item *item = g_ptr_array_index(items, i);

    item_emit(item, "NewIcon");
    /* The tool tip shows the icon too. */
    item_emit(item, "NewToolTip");
  }
}

/* uim-pref sends custom_reload_notify after saving the customs. */
static void
handle_custom_reload_notify(void)
{
  uim_prop_reload_configs();
  apply_customs();
}

/* uim-custom-cli sends prop_update_custom with a value. */
static void
handle_prop_update_custom(gchar **lines)
{
  /* prop_update_custom \n custom_sym \n custom_value */
  if (!(lines[1] && lines[2]))
    return;
  /* Only the customs that uim-sni uses: uim-sni can't catch an error of
   * Scheme as uim_prop_update_custom() does, and it has no context to
   * pass to it. The handler doesn't use the context. */
  if (!g_str_equal(lines[1], "toolbar-icon-for-dark-background?"))
    return;
  uim_scm_callf("custom-prop-update-custom-handler",
                "bys",
                UIM_FALSE,
                lines[1],
                lines[2]);
  apply_customs();
}

static void
handle_message(gchar *message)
{
  gchar **lines = g_strsplit(message, "\n", 0);

  if (lines[0] && g_str_equal(lines[0], "prop_list_update"))
    handle_prop_list_update(lines);
  else if (lines[0] && g_str_equal(lines[0], "custom_reload_notify"))
    handle_custom_reload_notify();
  else if (lines[0] && g_str_equal(lines[0], "prop_update_custom"))
    handle_prop_update_custom(lines);

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

  pixmaps_dir = g_getenv("UIM_SNI_PIXMAPSDIR");
  if (!pixmaps_dir || !pixmaps_dir[0])
    pixmaps_dir = UIM_PIXMAPSDIR;
  load_customs();
  item_node_info = g_dbus_node_info_new_for_xml(item_xml, NULL);
  menu_node_info = g_dbus_node_info_new_for_xml(menu_xml, NULL);
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
  g_dbus_node_info_unref(menu_node_info);
  g_main_loop_unref(loop);

  uim_quit();

  return exit_status;
}
