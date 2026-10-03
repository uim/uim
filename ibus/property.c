/*
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

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS
  IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
  THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
  PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR
  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
  OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
  WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
  OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
  ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

/*
 * Properties: uim's widgets, such as the input mode, as IBus
 * properties, which the panel shows in its menu. GNOME Shell also
 * shows the symbol of the property keyed "InputMode" in the top bar.
 *
 * uim tells about all its widgets each time any of them changes, as
 * the lines of a "branch" for each widget, followed by a "leaf" for
 * each of its actions. A branch is a menu here and a leaf a radio item
 * in it, keyed by its action.
 *
 * GNOME Shell takes the properties only once after the engine
 * changes, and after that only updates to the properties it already
 * has, so a change that keeps the keys goes as updates.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <string.h>

#include <glib/gi18n-lib.h>

#include "ibus-engine-uim.h"

#define INPUT_MODE_KEY "InputMode"
#define WIDGET_KEY_PREFIX "widget:"
/* The input method switcher, which uim puts first by default. */
#define IM_SWITCHER_ACTION_PREFIX "action_imsw_"
/* The indication of a widget none of whose actions is active. */
#define UNKNOWN_INDICATION_ID "unknown"

/* Every field is given: one left out stays zero rather than its
 * spec's default, which hides the property and leaves the icon NULL. */
static IBusProperty *
new_property(const char *key, IBusPropType type, const char *label,
             const char *symbol, const char *tooltip, IBusPropState state,
             IBusPropList *sub_props)
{
  IBusProperty *prop =
    ibus_property_new(key, type, ibus_text_new_from_string(label), "",
                      ibus_text_new_from_string(tooltip), TRUE, TRUE, state,
                      sub_props);

  ibus_property_set_symbol(prop, ibus_text_new_from_string(symbol));
  return prop;
}

/* leaf, indication id, iconic label, label, short description, action
 * id and "*" if it is the active one. */
static IBusProperty *
new_leaf(char **cols)
{
  gboolean active = g_strcmp0(cols[6], "*") == 0;

  return new_property(cols[5], PROP_TYPE_RADIO, cols[3], cols[2], cols[4],
                      active ? PROP_STATE_CHECKED : PROP_STATE_UNCHECKED,
                      NULL);
}

/* libuim binds its text domain but leaves the codeset to the locale. */
static char *
translated(const char *text)
{
  char *utf8 = g_locale_to_utf8(text, -1, NULL, NULL, NULL);

  return utf8 ? utf8 : g_strdup(text);
}

/* What the widget is set to, or what it can be set to when it is set
 * to none of them. */
static char *
current_label(char **cols, IBusPropList *leaves)
{
  GString *labels;
  IBusProperty *leaf;
  guint i;

  if (strcmp(cols[1], UNKNOWN_INDICATION_ID) != 0)
    return g_strdup(cols[3]);
  labels = g_string_new(NULL);
  for (i = 0; (leaf = ibus_prop_list_get(leaves, i)); i++) {
    if (i > 0)
      g_string_append(labels, " / ");
    g_string_append(labels, ibus_text_get_text(ibus_property_get_label(leaf)));
  }
  return g_string_free(labels, FALSE);
}

/* branch, indication id, iconic label and label of the active action.
 * The first widget other than the input method switcher is the input
 * mode. The others are keyed by their first action, apart from the
 * action itself: ibus-daemon aborts when an update has the key of a
 * property of another type.
 *
 * uim tells no name for a widget, so only the input method switcher
 * and the input mode get one in their labels. */
static IBusProperty *
new_menu(char **cols, IBusPropList *leaves, gboolean *has_input_mode)
{
  IBusProperty *first = ibus_prop_list_get(leaves, 0);
  const char *action = first ? ibus_property_get_key(first) : cols[1];
  char *current = current_label(cols, leaves);
  char *name = NULL;
  char *key, *label;
  IBusProperty *menu;

  if (g_str_has_prefix(action, IM_SWITCHER_ACTION_PREFIX)) {
    key = g_strconcat(WIDGET_KEY_PREFIX, action, NULL);
    name = translated(_("Input method"));
  } else if (!*has_input_mode) {
    key = g_strdup(INPUT_MODE_KEY);
    name = translated(_("Input mode"));
    *has_input_mode = TRUE;
  } else {
    key = g_strconcat(WIDGET_KEY_PREFIX, action, NULL);
  }
  label = name ? g_strdup_printf("%s (%s)", name, current) :
    g_strdup(current);
  menu = new_property(key, PROP_TYPE_MENU, label, cols[2], "",
                      PROP_STATE_UNCHECKED, leaves);
  g_free(label);
  g_free(name);
  g_free(current);
  g_free(key);
  return menu;
}

static IBusPropList *
parse_prop_list(const char *str)
{
  IBusPropList *props = ibus_prop_list_new();
  char **lines = g_strsplit(str, "\n", -1);
  char **branch = NULL;
  IBusPropList *leaves = NULL;
  gboolean has_input_mode = FALSE;
  int i;

  g_object_ref_sink(props);
  for (i = 0; lines[i]; i++) {
    char **cols = g_strsplit(lines[i], "\t", -1);
    guint n = g_strv_length(cols);

    if (n >= 4 && strcmp(cols[0], "branch") == 0) {
      if (branch)
        ibus_prop_list_append(props,
                              new_menu(branch, leaves, &has_input_mode));
      g_strfreev(branch);
      branch = cols;
      leaves = ibus_prop_list_new();
      continue;
    }
    if (n >= 7 && strcmp(cols[0], "leaf") == 0 && branch)
      ibus_prop_list_append(leaves, new_leaf(cols));
    g_strfreev(cols);
  }
  if (branch)
    ibus_prop_list_append(props, new_menu(branch, leaves, &has_input_mode));
  g_strfreev(branch);
  g_strfreev(lines);
  return props;
}

static gboolean
same_text(IBusText *a, IBusText *b)
{
  return g_strcmp0(a ? ibus_text_get_text(a) : NULL,
                   b ? ibus_text_get_text(b) : NULL) == 0;
}

/* Whether the panel can take NEW as updates to OLD. */
static gboolean
same_keys(IBusPropList *old, IBusPropList *new)
{
  guint i;

  for (i = 0; ; i++) {
    IBusProperty *a = ibus_prop_list_get(old, i);
    IBusProperty *b = ibus_prop_list_get(new, i);

    if (!a || !b)
      return !a && !b;
    if (g_strcmp0(ibus_property_get_key(a), ibus_property_get_key(b)) != 0 ||
        ibus_property_get_prop_type(a) != ibus_property_get_prop_type(b) ||
        !same_keys(ibus_property_get_sub_props(a),
                   ibus_property_get_sub_props(b)))
      return FALSE;
  }
}

static void
update_changed(IBusUimEngine *engine, IBusPropList *old, IBusPropList *new)
{
  guint i;
  IBusProperty *a, *b;

  for (i = 0; (a = ibus_prop_list_get(old, i)) &&
              (b = ibus_prop_list_get(new, i)); i++) {
    if (!same_text(ibus_property_get_label(a), ibus_property_get_label(b)) ||
        !same_text(ibus_property_get_symbol(a),
                   ibus_property_get_symbol(b)) ||
        !same_text(ibus_property_get_tooltip(a),
                   ibus_property_get_tooltip(b)) ||
        ibus_property_get_state(a) != ibus_property_get_state(b))
      ibus_engine_update_property(IBUS_ENGINE(engine), b);
    update_changed(engine, ibus_property_get_sub_props(a),
                   ibus_property_get_sub_props(b));
  }
}

static void
update_all(IBusUimEngine *engine, IBusPropList *props)
{
  IBusProperty *prop;
  guint i;

  for (i = 0; (prop = ibus_prop_list_get(props, i)); i++) {
    ibus_engine_update_property(IBUS_ENGINE(engine), prop);
    update_all(engine, ibus_property_get_sub_props(prop));
  }
}

void
ibus_uim_property_update(IBusUimEngine *engine, const char *str)
{
  IBusPropList *props = parse_prop_list(str);

  if (engine->props_registered && engine->props &&
      same_keys(engine->props, props)) {
    update_changed(engine, engine->props, props);
  } else {
    ibus_engine_register_properties(IBUS_ENGINE(engine), props);
    /* GNOME Shell keeps the properties it took first, another input
     * method's for instance, but takes the updates to the keys it has:
     * the input method switcher and the input mode. */
    update_all(engine, props);
    engine->props_registered = TRUE;
  }
  if (engine->props)
    g_object_unref(engine->props);
  engine->props = props;
}

static gboolean
has_leaf(IBusPropList *props, const char *key)
{
  IBusProperty *prop;
  guint i;

  for (i = 0; (prop = ibus_prop_list_get(props, i)); i++) {
    if (ibus_property_get_prop_type(prop) == PROP_TYPE_RADIO &&
        strcmp(ibus_property_get_key(prop), key) == 0)
      return TRUE;
    if (has_leaf(ibus_property_get_sub_props(prop), key))
      return TRUE;
  }
  return FALSE;
}

/* uim runs an action it doesn't know as the indicator of a widget, so
 * only the actions it told about go there. GNOME Shell may still show
 * those of another input method. */
void
ibus_uim_property_activate(IBusUimEngine *engine, const char *key,
                           guint state)
{
  if (state != PROP_STATE_CHECKED || !engine->uc || !engine->props ||
      !has_leaf(engine->props, key))
    return;
  uim_prop_activate(engine->uc, key);
}
