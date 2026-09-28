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
 * An IBus engine that hands the keys to uim. ibus-daemon starts it
 * with --ibus when a client selects the "uim" engine; GNOME Shell is
 * such a client for every application that talks text-input to
 * Mutter. The preedit and the candidates go back through ibus-daemon,
 * so GNOME Shell draws them.
 *
 * Every IBus input context gets an engine of its own, and so a uim
 * context of its own.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <locale.h>
#include <stdio.h>
#include <string.h>

#include <ibus.h>

#include <uim/uim.h>
#include <uim/uim-util.h>

#define IBUS_UIM_BUS_NAME "org.freedesktop.IBus.uim"
#define IBUS_UIM_ENGINE_NAME "uim"

/* IBus key codes are evdev codes, which fit in this. */
#define IBUS_UIM_MAX_KEYCODE 768

static gboolean debug_enabled;

#define debug(...)                              \
  do {                                          \
    if (debug_enabled)                          \
      g_printerr("ibus-engine-uim: " __VA_ARGS__); \
  } while (0)

typedef struct {
  int attr;
  char *str;
} PreeditSegment;

typedef struct {
  IBusEngine parent;

  uim_context uc;

  GArray *segments; /* of PreeditSegment */
  gboolean preedit_shown;

  IBusLookupTable *table;
  int nr;
  int display_limit;
  int page;
  int index;

  /* The pressed keys uim took, so their releases don't reach the
   * application either. */
  guint8 consumed_keys[IBUS_UIM_MAX_KEYCODE / 8];
} IBusUimEngine;

typedef struct {
  IBusEngineClass parent;
} IBusUimEngineClass;

GType ibus_uim_engine_get_type(void);

G_DEFINE_TYPE(IBusUimEngine, ibus_uim_engine, IBUS_TYPE_ENGINE)

/* keys */

/* IBus keysyms are X11 keysyms, so this is the same table as
 * convert_key_event() in gtk4/immodule. */
static int
keyval_to_ukey(guint keyval)
{
  if (keyval < 256)
    return (int)keyval;
  if (keyval >= IBUS_KEY_F1 && keyval <= IBUS_KEY_F35)
    return keyval - IBUS_KEY_F1 + UKey_F1;
  if (keyval >= IBUS_KEY_KP_0 && keyval <= IBUS_KEY_KP_9)
    return keyval - IBUS_KEY_KP_0 + UKey_0;
  if (keyval >= IBUS_KEY_dead_grave && keyval <= IBUS_KEY_dead_horn)
    return keyval - IBUS_KEY_dead_grave + UKey_Dead_Grave;
  if (keyval >= IBUS_KEY_Kanji && keyval <= IBUS_KEY_Eisu_toggle)
    return keyval - IBUS_KEY_Kanji + UKey_Kanji;
  if (keyval >= IBUS_KEY_Hangul && keyval <= IBUS_KEY_Hangul_Special)
    return keyval - IBUS_KEY_Hangul + UKey_Hangul;
  if (keyval >= IBUS_KEY_kana_fullstop && keyval <= IBUS_KEY_semivoicedsound)
    return keyval - IBUS_KEY_kana_fullstop + UKey_Kana_Fullstop;

  switch (keyval) {
  case IBUS_KEY_BackSpace:
    return UKey_Backspace;
  case IBUS_KEY_Delete:
    return UKey_Delete;
  case IBUS_KEY_Insert:
    return UKey_Insert;
  case IBUS_KEY_Escape:
    return UKey_Escape;
  case IBUS_KEY_Tab:
  case IBUS_KEY_ISO_Left_Tab:
    return UKey_Tab;
  case IBUS_KEY_Return:
    return UKey_Return;
  case IBUS_KEY_Left:
    return UKey_Left;
  case IBUS_KEY_Up:
    return UKey_Up;
  case IBUS_KEY_Right:
    return UKey_Right;
  case IBUS_KEY_Down:
    return UKey_Down;
  case IBUS_KEY_Prior:
    return UKey_Prior;
  case IBUS_KEY_Next:
    return UKey_Next;
  case IBUS_KEY_Home:
    return UKey_Home;
  case IBUS_KEY_End:
    return UKey_End;
  case IBUS_KEY_Multi_key:
    return UKey_Multi_key;
  case IBUS_KEY_Codeinput:
    return UKey_Codeinput;
  case IBUS_KEY_SingleCandidate:
    return UKey_SingleCandidate;
  case IBUS_KEY_MultipleCandidate:
    return UKey_MultipleCandidate;
  case IBUS_KEY_PreviousCandidate:
    return UKey_PreviousCandidate;
  case IBUS_KEY_Mode_switch:
    return UKey_Mode_switch;
  case IBUS_KEY_Shift_L:
  case IBUS_KEY_Shift_R:
    return UKey_Shift_key;
  case IBUS_KEY_Control_L:
  case IBUS_KEY_Control_R:
    return UKey_Control_key;
  case IBUS_KEY_Alt_L:
  case IBUS_KEY_Alt_R:
    return UKey_Alt_key;
  case IBUS_KEY_Meta_L:
  case IBUS_KEY_Meta_R:
    return UKey_Meta_key;
  case IBUS_KEY_Super_L:
  case IBUS_KEY_Super_R:
    return UKey_Super_key;
  case IBUS_KEY_Hyper_L:
  case IBUS_KEY_Hyper_R:
    return UKey_Hyper_key;
  case IBUS_KEY_Caps_Lock:
    return UKey_Caps_Lock;
  case IBUS_KEY_Num_Lock:
    return UKey_Num_Lock;
  case IBUS_KEY_Scroll_Lock:
    return UKey_Scroll_Lock;
  default:
    return UKey_Other;
  }
}

static int
modifiers_to_umod(guint modifiers)
{
  int umod = 0;

  if (modifiers & IBUS_SHIFT_MASK)
    umod |= UMod_Shift;
  if (modifiers & IBUS_CONTROL_MASK)
    umod |= UMod_Control;
  if (modifiers & IBUS_MOD1_MASK)
    umod |= UMod_Alt;
  if (modifiers & (IBUS_SUPER_MASK | IBUS_MOD4_MASK))
    umod |= UMod_Super;
  if (modifiers & IBUS_HYPER_MASK)
    umod |= UMod_Hyper;
  return umod;
}

/* Keycode 0 is what clients that make up keys (virtual keyboards,
 * xdotool) send for all of them, so it tells no key from another and
 * isn't tracked: its releases go to the application. */
static void
set_consumed(IBusUimEngine *engine, guint keycode, gboolean consumed)
{
  if (keycode == 0 || keycode >= IBUS_UIM_MAX_KEYCODE)
    return;
  if (consumed)
    engine->consumed_keys[keycode / 8] |= 1 << (keycode % 8);
  else
    engine->consumed_keys[keycode / 8] &= ~(1 << (keycode % 8));
}

static gboolean
was_consumed(IBusUimEngine *engine, guint keycode)
{
  if (keycode == 0 || keycode >= IBUS_UIM_MAX_KEYCODE)
    return FALSE;
  return (engine->consumed_keys[keycode / 8] & (1 << (keycode % 8))) != 0;
}

/* text output */

static void
commit_cb(void *ptr, const char *str)
{
  IBusEngine *engine = ptr;

  if (!str || str[0] == '\0')
    return;
  debug("commit \"%s\"\n", str);
  ibus_engine_commit_text(engine, ibus_text_new_from_string(str));
}

static void
clear_segments(IBusUimEngine *engine)
{
  guint i;

  for (i = 0; i < engine->segments->len; i++)
    g_free(g_array_index(engine->segments, PreeditSegment, i).str);
  g_array_set_size(engine->segments, 0);
}

static void
preedit_clear_cb(void *ptr)
{
  clear_segments(ptr);
}

static void
preedit_pushback_cb(void *ptr, int attr, const char *str)
{
  IBusUimEngine *engine = ptr;
  PreeditSegment segment;

  if (!str)
    return;
  if (str[0] == '\0' &&
      !(attr & (UPreeditAttr_Cursor | UPreeditAttr_Separator)))
    return;
  segment.attr = attr;
  segment.str = g_strdup(str);
  g_array_append_val(engine->segments, segment);
}

static void
preedit_update_cb(void *ptr)
{
  IBusUimEngine *engine = ptr;
  GString *text = g_string_new(NULL);
  IBusAttrList *attrs = ibus_attr_list_new();
  IBusText *ibus_text;
  /* IBus counts characters, not bytes. */
  guint length = 0;
  int cursor = -1;
  guint i;

  for (i = 0; i < engine->segments->len; i++) {
    PreeditSegment *segment = &g_array_index(engine->segments,
                                             PreeditSegment, i);
    const char *str = segment->str;
    guint str_length;

    if (segment->attr & UPreeditAttr_Cursor)
      cursor = (int)length;
    if ((segment->attr & UPreeditAttr_Separator) && str[0] == '\0')
      str = "|";
    str_length = g_utf8_strlen(str, -1);
    if (str_length == 0)
      continue;
    g_string_append(text, str);
    if (segment->attr & UPreeditAttr_Reverse) {
      ibus_attr_list_append(attrs,
                            ibus_attr_foreground_new(0xffffff,
                                                     length,
                                                     length + str_length));
      ibus_attr_list_append(attrs,
                            ibus_attr_background_new(0x000000,
                                                     length,
                                                     length + str_length));
    } else if (segment->attr & UPreeditAttr_UnderLine) {
      ibus_attr_list_append(attrs,
                            ibus_attr_underline_new(IBUS_ATTR_UNDERLINE_SINGLE,
                                                    length,
                                                    length + str_length));
    }
    length += str_length;
  }

  if (length == 0 && !engine->preedit_shown) {
    g_string_free(text, TRUE);
    g_object_unref(g_object_ref_sink(attrs));
    return;
  }

  if (cursor < 0)
    cursor = (int)length;
  debug("preedit \"%s\" cursor %d\n", text->str, cursor);
  ibus_text = ibus_text_new_from_string(text->str);
  ibus_text_set_attributes(ibus_text, attrs);
  /* COMMIT: an application that loses the focus keeps what it shows,
   * as uim-wayland does. */
  ibus_engine_update_preedit_text_with_mode(IBUS_ENGINE(engine),
                                            ibus_text,
                                            (guint)cursor,
                                            length > 0,
                                            IBUS_ENGINE_PREEDIT_COMMIT);
  engine->preedit_shown = length > 0;
  g_string_free(text, TRUE);
}

/* candidates */

static int
page_size(IBusUimEngine *engine)
{
  return engine->display_limit > 0 ? engine->display_limit : engine->nr;
}

static int
n_pages(IBusUimEngine *engine)
{
  int size = page_size(engine);
  return (engine->nr + size - 1) / size;
}

/* Hand the table to ibus-daemon, which passes it on to the panel. */
static void
show_table(IBusUimEngine *engine)
{
  int size = page_size(engine);

  if (engine->index >= 0) {
    ibus_lookup_table_set_cursor_pos(engine->table, engine->index);
    ibus_lookup_table_set_cursor_visible(engine->table, TRUE);
  } else {
    /* The cursor picks the page even while it is hidden. */
    ibus_lookup_table_set_cursor_pos(engine->table, engine->page * size);
    ibus_lookup_table_set_cursor_visible(engine->table, FALSE);
  }
  ibus_engine_update_lookup_table(IBUS_ENGINE(engine), engine->table, TRUE);
}

static void
cand_activate_cb(void *ptr, int nr, int display_limit)
{
  IBusUimEngine *engine = ptr;
  int size;
  int i;

  debug("activate %d candidates, %d per page\n", nr, display_limit);
  engine->nr = nr;
  engine->display_limit = display_limit;
  engine->page = 0;
  engine->index = -1;
  if (nr <= 0)
    return;

  size = page_size(engine);
  if (engine->table)
    g_object_unref(engine->table);
  engine->table = g_object_ref_sink(ibus_lookup_table_new(size, 0, FALSE,
                                                          FALSE));
  for (i = 0; i < nr; i++) {
    uim_candidate cand = uim_get_candidate(engine->uc, i, i % size);
    const char *heading = uim_candidate_get_heading_label(cand);
    const char *str = uim_candidate_get_cand_str(cand);

    ibus_lookup_table_append_candidate(engine->table,
                                       ibus_text_new_from_string(str ? str
                                                                 : ""));
    /* The panel shows labels for the first page only; the rest have
     * the same labels anyway. */
    if (i < size)
      ibus_lookup_table_append_label(engine->table,
                                     ibus_text_new_from_string(heading ?
                                                               heading : ""));
    uim_candidate_free(cand);
  }
  show_table(engine);
}

static void
cand_select_cb(void *ptr, int index)
{
  IBusUimEngine *engine = ptr;

  if (!engine->table || engine->nr <= 0)
    return;
  if (index >= engine->nr)
    index = 0;
  engine->index = index;
  if (index >= 0)
    engine->page = index / page_size(engine);
  show_table(engine);
}

static void
shift_page(IBusUimEngine *engine, gboolean forward)
{
  int pages;
  int size;

  if (!engine->table || engine->nr <= 0)
    return;
  pages = n_pages(engine);
  size = page_size(engine);
  if (forward)
    engine->page = (engine->page + 1) % pages;
  else
    engine->page = (engine->page + pages - 1) % pages;

  if (engine->index >= 0) {
    int index = engine->page * size + engine->index % size;
    if (index >= engine->nr)
      index = engine->nr - 1;
    engine->index = index;
    /* The input method may answer this by selecting, reactivating or
     * deactivating, as in uim-wayland's candidate window. */
    uim_set_candidate_index(engine->uc, index);
  }
  if (engine->nr > 0)
    show_table(engine);
}

static void
cand_shift_page_cb(void *ptr, int direction)
{
  shift_page(ptr, direction != 0);
}

static void
cand_deactivate_cb(void *ptr)
{
  IBusUimEngine *engine = ptr;

  debug("deactivate candidates\n");
  engine->nr = 0;
  engine->index = -1;
  ibus_engine_hide_lookup_table(IBUS_ENGINE(engine));
}

/* IBusEngine */

static gboolean
ibus_uim_engine_process_key_event(IBusEngine *ibus_engine,
                                  guint keyval,
                                  guint keycode,
                                  guint modifiers)
{
  IBusUimEngine *engine = (IBusUimEngine *)ibus_engine;
  int ukey = keyval_to_ukey(keyval);
  int umod = modifiers_to_umod(modifiers);
  gboolean forward;

  if (!engine->uc)
    return FALSE;

  if (modifiers & IBUS_RELEASE_MASK) {
    uim_release_key(engine->uc, ukey, umod);
    /* A release goes wherever its press went, so the application
     * never sees an unbalanced key. A press we never saw, say one
     * made before this engine was up, went to the application. */
    forward = !was_consumed(engine, keycode);
    set_consumed(engine, keycode, FALSE);
  } else {
    forward = uim_press_key(engine->uc, ukey, umod) != 0;
    set_consumed(engine, keycode, !forward);
  }
  debug("key %#x (code %u ukey %d umod %#x) %s: %s\n",
        keyval, keycode, ukey, umod,
        (modifiers & IBUS_RELEASE_MASK) ? "released" : "pressed",
        forward ? "forwarded" : "consumed");
  return !forward;
}

static void
ibus_uim_engine_focus_in(IBusEngine *ibus_engine)
{
  IBusUimEngine *engine = (IBusUimEngine *)ibus_engine;

  if (engine->uc)
    uim_focus_in_context(engine->uc);
  IBUS_ENGINE_CLASS(ibus_uim_engine_parent_class)->focus_in(ibus_engine);
}

static void
ibus_uim_engine_focus_out(IBusEngine *ibus_engine)
{
  IBusUimEngine *engine = (IBusUimEngine *)ibus_engine;

  if (engine->uc) {
    uim_focus_out_context(engine->uc);
    /* ibus-daemon commits the preedit into the application we are
     * leaving, since we send it with IBUS_ENGINE_PREEDIT_COMMIT. Drop
     * it on our side too, or it shows up again with the next key and
     * gets committed twice: GNOME Shell uses one input context for
     * every application, so possibly into another one. The daemon has
     * already cleared what the application shows, so there is
     * nothing to tell it. */
    engine->preedit_shown = FALSE;
    uim_reset_context(engine->uc);
    clear_segments(engine);
  }
  IBUS_ENGINE_CLASS(ibus_uim_engine_parent_class)->focus_out(ibus_engine);
}

static void
ibus_uim_engine_reset(IBusEngine *ibus_engine)
{
  IBusUimEngine *engine = (IBusUimEngine *)ibus_engine;

  if (engine->uc)
    uim_reset_context(engine->uc);
  IBUS_ENGINE_CLASS(ibus_uim_engine_parent_class)->reset(ibus_engine);
}

static void
ibus_uim_engine_page_up(IBusEngine *ibus_engine)
{
  shift_page((IBusUimEngine *)ibus_engine, FALSE);
}

static void
ibus_uim_engine_page_down(IBusEngine *ibus_engine)
{
  shift_page((IBusUimEngine *)ibus_engine, TRUE);
}

static void
ibus_uim_engine_candidate_clicked(IBusEngine *ibus_engine,
                                  guint index,
                                  guint button,
                                  guint state)
{
  IBusUimEngine *engine = (IBusUimEngine *)ibus_engine;
  int size;
  (void)button;
  (void)state;

  if (!engine->uc || engine->nr <= 0)
    return;
  /* The index counts from the top of the page shown. */
  size = page_size(engine);
  if (engine->page * size + (int)index >= engine->nr)
    return;
  uim_set_candidate_index(engine->uc, engine->page * size + (int)index);
}

static void
ibus_uim_engine_constructed(GObject *object)
{
  IBusUimEngine *engine = (IBusUimEngine *)object;
  const char *im_name;

  G_OBJECT_CLASS(ibus_uim_engine_parent_class)->constructed(object);

  /* The returned string is only valid until the next libuim call. */
  im_name = uim_get_default_im_name(setlocale(LC_CTYPE, NULL));
  debug("create a context for %s\n", im_name);
  engine->uc = uim_create_context(engine, "UTF-8", NULL, im_name, uim_iconv,
                                  commit_cb);
  if (!engine->uc) {
    g_warning("cannot create uim context");
    return;
  }
  uim_set_preedit_cb(engine->uc, preedit_clear_cb, preedit_pushback_cb,
                     preedit_update_cb);
  uim_set_candidate_selector_cb(engine->uc, cand_activate_cb, cand_select_cb,
                                cand_shift_page_cb, cand_deactivate_cb);
}

static void
ibus_uim_engine_destroy(IBusObject *object)
{
  IBusUimEngine *engine = (IBusUimEngine *)object;

  if (engine->uc) {
    uim_release_context(engine->uc);
    engine->uc = NULL;
  }
  if (engine->table) {
    g_object_unref(engine->table);
    engine->table = NULL;
  }
  if (engine->segments) {
    clear_segments(engine);
    g_array_free(engine->segments, TRUE);
    engine->segments = NULL;
  }
  IBUS_OBJECT_CLASS(ibus_uim_engine_parent_class)->destroy(object);
}

static void
ibus_uim_engine_init(IBusUimEngine *engine)
{
  engine->segments = g_array_new(FALSE, FALSE, sizeof(PreeditSegment));
  engine->index = -1;
}

static void
ibus_uim_engine_class_init(IBusUimEngineClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS(klass);
  IBusObjectClass *ibus_object_class = IBUS_OBJECT_CLASS(klass);
  IBusEngineClass *engine_class = IBUS_ENGINE_CLASS(klass);

  object_class->constructed = ibus_uim_engine_constructed;
  ibus_object_class->destroy = ibus_uim_engine_destroy;
  engine_class->process_key_event = ibus_uim_engine_process_key_event;
  engine_class->focus_in = ibus_uim_engine_focus_in;
  engine_class->focus_out = ibus_uim_engine_focus_out;
  engine_class->reset = ibus_uim_engine_reset;
  engine_class->page_up = ibus_uim_engine_page_up;
  engine_class->page_down = ibus_uim_engine_page_down;
  engine_class->candidate_clicked = ibus_uim_engine_candidate_clicked;
}

/* main */

static IBusComponent *
create_component(void)
{
  IBusComponent *component;

  component = ibus_component_new(IBUS_UIM_BUS_NAME,
                                 "uim",
                                 PACKAGE_VERSION,
                                 "BSD-3-Clause",
                                 "uim Project",
                                 "https://github.com/uim/uim",
                                 "",
                                 "uim");
  ibus_component_add_engine(component,
                            ibus_engine_desc_new(IBUS_UIM_ENGINE_NAME,
                                                 "uim",
                                                 "uim input method",
                                                 "other",
                                                 "BSD-3-Clause",
                                                 "uim Project",
                                                 UIM_PIXMAPSDIR "/uim-icon.svg",
                                                 "default"));
  return component;
}

static void
bus_disconnected_cb(IBusBus *bus, gpointer user_data)
{
  (void)bus;
  (void)user_data;
  ibus_quit();
}

static void
usage(FILE *out)
{
  fprintf(out,
          "Usage: ibus-engine-uim [--ibus]\n"
          "\n"
          "  --ibus  ibus-daemon started this program from uim.xml\n"
          "\n"
          "Without --ibus, it registers the \"uim\" engine by itself.\n"
          "Set IBUS_UIM_DEBUG to print what it does.\n");
}

int
main(int argc, char *argv[])
{
  gboolean started_by_ibus = FALSE;
  IBusBus *bus;
  IBusFactory *factory;
  int i;

  for (i = 1; i < argc; i++) {
    if (strcmp(argv[i], "--ibus") == 0 || strcmp(argv[i], "-i") == 0) {
      started_by_ibus = TRUE;
    } else if (strcmp(argv[i], "--help") == 0 || strcmp(argv[i], "-h") == 0) {
      usage(stdout);
      return 0;
    } else {
      usage(stderr);
      return 1;
    }
  }

  setlocale(LC_ALL, "");
  debug_enabled = g_getenv("IBUS_UIM_DEBUG") != NULL;

  if (uim_init() < 0) {
    fprintf(stderr, "ibus-engine-uim: uim_init() failed\n");
    return 1;
  }

  ibus_init();
  bus = ibus_bus_new();
  if (!ibus_bus_is_connected(bus)) {
    fprintf(stderr, "ibus-engine-uim: cannot connect to ibus-daemon\n");
    uim_quit();
    return 1;
  }
  g_signal_connect(bus, "disconnected", G_CALLBACK(bus_disconnected_cb),
                   NULL);

  factory = ibus_factory_new(ibus_bus_get_connection(bus));
  ibus_factory_add_engine(factory, IBUS_UIM_ENGINE_NAME,
                          ibus_uim_engine_get_type());

  if (started_by_ibus) {
    ibus_bus_request_name(bus, IBUS_UIM_BUS_NAME, 0);
  } else {
    IBusComponent *component = create_component();
    ibus_bus_register_component(bus, component);
    g_object_unref(component);
  }

  ibus_main();

  g_object_unref(factory);
  g_object_unref(bus);
  uim_quit();
  return 0;
}
