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
 * Candidate window on a zwp_input_panel_surface_v1 overlay panel. The
 * compositor places an overlay panel next to the text cursor of the
 * focused client. There is no toolkit here: candidates are drawn with
 * cairo and pango into wl_shm buffers.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifndef _GNU_SOURCE
#define _GNU_SOURCE /* memfd_create */
#endif
#include <errno.h>
#include <fcntl.h>
#include <linux/input-event-codes.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>

#include <cairo.h>
#include <gio/gio.h>
#include <pango/pangocairo.h>

#include "uim-wayland.h"

/* Used when the desktop has no font-name, as GTK does. */
#define CANDWIN_FONT "sans 11"
#define DESKTOP_INTERFACE_SCHEMA "org.gnome.desktop.interface"
#define CANDWIN_PADDING 6
#define CANDWIN_COLUMN_GAP 8
#define CANDWIN_ROW_GAP 2
#define CANDWIN_N_BUFFERS 2
/* Axis distance per page: a wheel notch is 10 in Weston, 15 in KWin. */
#define CANDWIN_SCROLL_STEP 10.0

struct candidate {
  char *heading;
  char *str;
  char *annotation;
};

struct shm_buffer {
  struct uim_wayland_candwin *cw;
  struct wl_buffer *buffer;
  void *data;
  size_t size;
  int width;
  int height;
  int stride;
  bool busy;
};

struct uim_wayland_candwin {
  struct uim_wayland *uw;
  struct wl_surface *surface;
  struct zwp_input_panel_surface_v1 *panel_surface;
  struct shm_buffer buffers[CANDWIN_N_BUFFERS];
  bool shown;
  /* Set when a draw had to be skipped because the compositor still
   * held every buffer. The next release repaints. */
  bool dirty;

  int nr;
  int display_limit;
  int page;
  int index; /* -1: nothing selected */
  struct candidate *candidates; /* current page */
  int n_candidates;

  /* Row geometry of the last draw, for hit testing. */
  int rows_top;
  int row_height;

  bool pointer_inside;
  double pointer_y;
  double scroll;

  /* The buffer scale the compositor asks for. */
  int scale;

  /* The desktop's interface font, which GTK uses too. Plasma copies
   * its own font here. NULL without the schema. */
  GSettings *desktop_settings;
  char *font_name;
  PangoFontDescription *font;
};

/* shm buffers */

static int
create_anonymous_file(size_t size)
{
  int fd;
  bool sealable = false;

#ifdef __linux__
  fd = memfd_create("uim-wayland-candwin",
                    MFD_CLOEXEC | MFD_ALLOW_SEALING);
  sealable = fd >= 0;
#else
  fd = -1;
#endif
  if (fd < 0) {
    const char *runtime_dir = getenv("XDG_RUNTIME_DIR");
    char *path;
    if (!runtime_dir)
      runtime_dir = "/tmp";
    uim_asprintf(&path, "%s/uim-wayland-XXXXXX", runtime_dir);
    fd = mkstemp(path);
    if (fd >= 0) {
      unlink(path);
      fcntl(fd, F_SETFD, FD_CLOEXEC);
    }
    free(path);
    if (fd < 0)
      return -1;
  }
  if (ftruncate(fd, size) < 0) {
    close(fd);
    return -1;
  }
#ifdef __linux__
  /* Promise the compositor that the file won't shrink, so that it
   * doesn't have to guard its own reads against SIGBUS. Sealing is
   * optional: a compositor that doesn't care still works. */
  if (sealable)
    fcntl(fd, F_ADD_SEALS, F_SEAL_SHRINK | F_SEAL_SEAL);
#else
  (void)sealable;
#endif
  return fd;
}

static void
shm_buffer_destroy(struct shm_buffer *b)
{
  if (b->buffer)
    wl_buffer_destroy(b->buffer);
  if (b->data)
    munmap(b->data, b->size);
  memset(b, 0, sizeof(*b));
}

static void draw(struct uim_wayland_candwin *cw);

static void
buffer_release(void *data, struct wl_buffer *buffer)
{
  struct shm_buffer *b = data;
  struct uim_wayland_candwin *cw = b->cw;
  (void)buffer;

  b->busy = false;
  if (cw->dirty) {
    cw->dirty = false;
    draw(cw);
  }
}

static const struct wl_buffer_listener buffer_listener = {
  buffer_release
};

static bool
shm_buffer_create(struct uim_wayland_candwin *cw,
                  struct shm_buffer *b,
                  int width,
                  int height)
{
  int stride = cairo_format_stride_for_width(CAIRO_FORMAT_ARGB32, width);
  size_t size = (size_t)stride * height;
  struct wl_shm_pool *pool;
  int fd;

  fd = create_anonymous_file(size);
  if (fd < 0) {
    fprintf(stderr, "%s: cannot create shm file: %s\n",
            UIM_WAYLAND_PROGRAM_NAME, strerror(errno));
    return false;
  }
  b->data = mmap(NULL, size, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
  if (b->data == MAP_FAILED) {
    fprintf(stderr, "%s: mmap failed: %s\n",
            UIM_WAYLAND_PROGRAM_NAME, strerror(errno));
    close(fd);
    b->data = NULL;
    return false;
  }
  pool = wl_shm_create_pool(cw->uw->shm, fd, size);
  b->buffer = wl_shm_pool_create_buffer(pool, 0, width, height, stride,
                                        WL_SHM_FORMAT_ARGB8888);
  wl_shm_pool_destroy(pool);
  close(fd);
  wl_buffer_add_listener(b->buffer, &buffer_listener, b);
  b->cw = cw;
  b->size = size;
  b->width = width;
  b->height = height;
  b->stride = stride;
  b->busy = false;
  return true;
}

/*
 * Returns the buffer to draw into, or NULL. *all_busy tells the two
 * NULL cases apart: the compositor still holds every buffer, so a
 * release will come and the draw can be retried, versus a buffer that
 * couldn't be created at all, where retrying would wait forever.
 */
static struct shm_buffer *
get_buffer(struct uim_wayland_candwin *cw,
           int width,
           int height,
           bool *all_busy)
{
  int i;
  struct shm_buffer *b = NULL;

  *all_busy = false;
  for (i = 0; i < CANDWIN_N_BUFFERS; i++) {
    if (!cw->buffers[i].busy) {
      b = &cw->buffers[i];
      break;
    }
  }
  if (!b) {
    *all_busy = true;
    return NULL;
  }
  if (b->buffer && (b->width != width || b->height != height))
    shm_buffer_destroy(b);
  if (!b->buffer && !shm_buffer_create(cw, b, width, height))
    return NULL;
  return b;
}

/* candidates */

static void
free_candidates(struct uim_wayland_candwin *cw)
{
  int i;
  for (i = 0; i < cw->n_candidates; i++) {
    free(cw->candidates[i].heading);
    free(cw->candidates[i].str);
    free(cw->candidates[i].annotation);
  }
  free(cw->candidates);
  cw->candidates = NULL;
  cw->n_candidates = 0;
}

static int
page_size(struct uim_wayland_candwin *cw)
{
  return cw->display_limit > 0 ? cw->display_limit : cw->nr;
}

static int
n_pages(struct uim_wayland_candwin *cw)
{
  int size = page_size(cw);
  if (size <= 0)
    return 1;
  return (cw->nr + size - 1) / size;
}

static char *
dup_or_empty(const char *s)
{
  return uim_strdup(s ? s : "");
}

static void
fetch_page(struct uim_wayland_candwin *cw)
{
  int size = page_size(cw);
  int start = cw->page * size;
  int end = start + size;
  int i;

  free_candidates(cw);
  if (end > cw->nr)
    end = cw->nr;
  if (start >= end)
    return;

  cw->candidates = uim_malloc(sizeof(*cw->candidates) * (end - start));
  for (i = start; i < end; i++) {
    uim_candidate cand = uim_get_candidate(cw->uw->uc, i, i - start);
    struct candidate *c = &cw->candidates[cw->n_candidates++];
    c->heading = dup_or_empty(uim_candidate_get_heading_label(cand));
    c->str = dup_or_empty(uim_candidate_get_cand_str(cand));
    c->annotation = dup_or_empty(uim_candidate_get_annotation_str(cand));
    uim_candidate_free(cand);
  }
}

/* drawing */

struct row_metrics {
  int heading_width;
  int str_width;
  int annotation_width;
  int height;
};

static void
measure(PangoLayout *layout, const char *text, int *width, int *height)
{
  int w, h;
  pango_layout_set_text(layout, text, -1);
  pango_layout_get_pixel_size(layout, &w, &h);
  if (width)
    *width = w;
  if (height)
    *height = h;
}

static void
draw_text(cairo_t *cr,
          PangoLayout *layout,
          const char *text,
          double x,
          double y)
{
  pango_layout_set_text(layout, text, -1);
  cairo_move_to(cr, x, y);
  pango_cairo_show_layout(cr, layout);
}

static void
draw(struct uim_wayland_candwin *cw)
{
  cairo_surface_t *scratch;
  cairo_t *cr;
  PangoLayout *layout;
  struct row_metrics m = {0, 0, 0, 0};
  char footer[64];
  int footer_width, footer_height;
  int width, height, row_height, y, i;
  bool have_annotation = false;
  bool all_busy;
  struct shm_buffer *b;
  cairo_surface_t *surface;

  /* Measure with a scratch surface first: the buffer size depends on
   * the text. */
  scratch = cairo_image_surface_create(CAIRO_FORMAT_ARGB32, 1, 1);
  cr = cairo_create(scratch);
  cairo_scale(cr, cw->scale, cw->scale);
  layout = pango_cairo_create_layout(cr);
  pango_layout_set_font_description(layout, cw->font);

  for (i = 0; i < cw->n_candidates; i++) {
    struct candidate *c = &cw->candidates[i];
    int w, h;
    measure(layout, c->heading, &w, &h);
    if (w > m.heading_width)
      m.heading_width = w;
    if (h > m.height)
      m.height = h;
    measure(layout, c->str, &w, &h);
    if (w > m.str_width)
      m.str_width = w;
    if (h > m.height)
      m.height = h;
    if (c->annotation[0] != '\0') {
      have_annotation = true;
      measure(layout, c->annotation, &w, &h);
      if (w > m.annotation_width)
        m.annotation_width = w;
      if (h > m.height)
        m.height = h;
    }
  }
  if (cw->index >= 0)
    snprintf(footer, sizeof(footer), "%d / %d", cw->index + 1, cw->nr);
  else
    snprintf(footer, sizeof(footer), "- / %d", cw->nr);
  measure(layout, footer, &footer_width, &footer_height);

  row_height = m.height + CANDWIN_ROW_GAP;
  width = m.heading_width + CANDWIN_COLUMN_GAP + m.str_width;
  if (have_annotation)
    width += CANDWIN_COLUMN_GAP + m.annotation_width;
  if (footer_width > width)
    width = footer_width;
  width += CANDWIN_PADDING * 2;
  height = CANDWIN_PADDING * 2 + row_height * cw->n_candidates +
    CANDWIN_ROW_GAP + footer_height;

  g_object_unref(layout);
  cairo_destroy(cr);
  cairo_surface_destroy(scratch);

  b = get_buffer(cw, width * cw->scale, height * cw->scale, &all_busy);
  if (!b) {
    cw->dirty = all_busy;
    return;
  }

  surface = cairo_image_surface_create_for_data(b->data,
                                                CAIRO_FORMAT_ARGB32,
                                                b->width, b->height,
                                                b->stride);
  cr = cairo_create(surface);
  cairo_scale(cr, cw->scale, cw->scale);
  layout = pango_cairo_create_layout(cr);
  pango_layout_set_font_description(layout, cw->font);

  /* background and border */
  cairo_set_source_rgb(cr, 1.0, 1.0, 1.0);
  cairo_paint(cr);
  cairo_set_source_rgb(cr, 0.6, 0.6, 0.6);
  cairo_set_line_width(cr, 1.0);
  cairo_rectangle(cr, 0.5, 0.5, width - 1, height - 1);
  cairo_stroke(cr);

  y = CANDWIN_PADDING;
  /* A row is highlighted from half a gap above its text. */
  cw->rows_top = y - CANDWIN_ROW_GAP / 2;
  cw->row_height = row_height;
  for (i = 0; i < cw->n_candidates; i++) {
    struct candidate *c = &cw->candidates[i];
    int x = CANDWIN_PADDING;
    bool selected =
      cw->index >= 0 && cw->index - cw->page * page_size(cw) == i;

    if (selected) {
      cairo_set_source_rgb(cr, 0.2, 0.4, 0.8);
      cairo_rectangle(cr, 1, y - CANDWIN_ROW_GAP / 2.0,
                      width - 2, row_height);
      cairo_fill(cr);
      cairo_set_source_rgb(cr, 1.0, 1.0, 1.0);
    } else {
      cairo_set_source_rgb(cr, 0.0, 0.0, 0.0);
    }

    draw_text(cr, layout, c->heading, x, y);
    x += m.heading_width + CANDWIN_COLUMN_GAP;
    draw_text(cr, layout, c->str, x, y);
    x += m.str_width + CANDWIN_COLUMN_GAP;
    if (c->annotation[0] != '\0') {
      if (!selected)
        cairo_set_source_rgb(cr, 0.4, 0.4, 0.4);
      draw_text(cr, layout, c->annotation, x, y);
    }
    y += row_height;
  }

  y += CANDWIN_ROW_GAP;
  cairo_set_source_rgb(cr, 0.4, 0.4, 0.4);
  draw_text(cr, layout, footer, width - CANDWIN_PADDING - footer_width, y);

  g_object_unref(layout);
  cairo_destroy(cr);
  cairo_surface_flush(surface);
  cairo_surface_destroy(surface);

  b->busy = true;
  if (wl_surface_get_version(cw->surface) >=
      WL_SURFACE_SET_BUFFER_SCALE_SINCE_VERSION)
    wl_surface_set_buffer_scale(cw->surface, cw->scale);
  wl_surface_attach(cw->surface, b->buffer, 0, 0);
  wl_surface_damage(cw->surface, 0, 0, width, height);
  wl_surface_commit(cw->surface);
  cw->shown = true;
}

static void
hide(struct uim_wayland_candwin *cw)
{
  if (!cw->shown)
    return;
  wl_surface_attach(cw->surface, NULL, 0, 0);
  wl_surface_commit(cw->surface);
  cw->shown = false;
  cw->dirty = false;
}

/* surface */

#ifdef WL_SURFACE_PREFERRED_BUFFER_SCALE_SINCE_VERSION
static void
surface_enter(void *data, struct wl_surface *surface, struct wl_output *output)
{
  (void)data;
  (void)surface;
  (void)output;
}

static void
surface_leave(void *data, struct wl_surface *surface, struct wl_output *output)
{
  (void)data;
  (void)surface;
  (void)output;
}

static void
surface_preferred_buffer_scale(void *data,
                               struct wl_surface *surface,
                               int32_t factor)
{
  struct uim_wayland_candwin *cw = data;
  (void)surface;

  if (factor < 1 || factor == cw->scale)
    return;
  cw->scale = factor;
  if (cw->shown)
    draw(cw);
}

static void
surface_preferred_buffer_transform(void *data,
                                   struct wl_surface *surface,
                                   uint32_t transform)
{
  (void)data;
  (void)surface;
  (void)transform;
}

static const struct wl_surface_listener surface_listener = {
  surface_enter,
  surface_leave,
  surface_preferred_buffer_scale,
  surface_preferred_buffer_transform
};
#endif

/* font */

static GSettings *
desktop_settings_new(void)
{
  GSettingsSchemaSource *source = g_settings_schema_source_get_default();
  GSettingsSchema *schema;
  GSettings *settings = NULL;

  /* g_settings_new() aborts on a missing schema. */
  if (!source)
    return NULL;
  schema = g_settings_schema_source_lookup(source, DESKTOP_INTERFACE_SCHEMA,
                                           TRUE);
  if (!schema)
    return NULL;
  if (g_settings_schema_has_key(schema, "font-name"))
    settings = g_settings_new_full(schema, NULL, NULL);
  g_settings_schema_unref(schema);
  return settings;
}

/* Read on every activation: without a GLib main loop there is no
 * change notification. */
static void
update_font(struct uim_wayland_candwin *cw)
{
  char *name = NULL;

  if (cw->desktop_settings) {
    /* GSettings queues work for the default main context. */
    while (g_main_context_iteration(NULL, FALSE))
      ;
    name = g_settings_get_string(cw->desktop_settings, "font-name");
    if (name[0] == '\0') {
      g_free(name);
      name = NULL;
    }
  }
  if (!name)
    name = g_strdup(CANDWIN_FONT);
  if (cw->font && g_strcmp0(name, cw->font_name) == 0) {
    g_free(name);
    return;
  }
  g_free(cw->font_name);
  cw->font_name = name;
  if (cw->font)
    pango_font_description_free(cw->font);
  cw->font = pango_font_description_from_string(name);
}

/* public API */

struct uim_wayland_candwin *
uim_wayland_candwin_new(struct uim_wayland *uw)
{
  struct uim_wayland_candwin *cw;

  if (!uw->input_panel || !uw->compositor || !uw->shm)
    return NULL;

  cw = uim_malloc(sizeof(*cw));
  memset(cw, 0, sizeof(*cw));
  cw->uw = uw;
  cw->index = -1;
  cw->scale = 1;
  cw->desktop_settings = desktop_settings_new();
  update_font(cw);

  cw->surface = wl_compositor_create_surface(uw->compositor);
#ifdef WL_SURFACE_PREFERRED_BUFFER_SCALE_SINCE_VERSION
  wl_surface_add_listener(cw->surface, &surface_listener, cw);
#endif
  cw->panel_surface =
    zwp_input_panel_v1_get_input_panel_surface(uw->input_panel, cw->surface);
  /* An overlay panel is positioned by the compositor next to the
   * cursor of the focused text field. */
  zwp_input_panel_surface_v1_set_overlay_panel(cw->panel_surface);

  return cw;
}

void
uim_wayland_candwin_free(struct uim_wayland_candwin *cw)
{
  int i;

  if (!cw)
    return;
  free_candidates(cw);
  for (i = 0; i < CANDWIN_N_BUFFERS; i++)
    shm_buffer_destroy(&cw->buffers[i]);
  if (cw->panel_surface)
    zwp_input_panel_surface_v1_destroy(cw->panel_surface);
  if (cw->surface)
    wl_surface_destroy(cw->surface);
  if (cw->font)
    pango_font_description_free(cw->font);
  g_free(cw->font_name);
  if (cw->desktop_settings)
    g_object_unref(cw->desktop_settings);
  free(cw);
}

void
uim_wayland_candwin_activate(struct uim_wayland_candwin *cw,
                             int nr,
                             int display_limit)
{
  if (!cw)
    return;
  cw->nr = nr;
  cw->display_limit = display_limit;
  cw->page = 0;
  cw->index = -1;
  update_font(cw);
  fetch_page(cw);
  draw(cw);
}

void
uim_wayland_candwin_select(struct uim_wayland_candwin *cw, int index)
{
  int new_page;

  if (!cw || cw->nr <= 0)
    return;
  if (index >= cw->nr)
    index = 0;
  cw->index = index;
  new_page = index >= 0 ? index / page_size(cw) : cw->page;
  if (new_page != cw->page) {
    cw->page = new_page;
    fetch_page(cw);
  }
  draw(cw);
}

void
uim_wayland_candwin_shift_page(struct uim_wayland_candwin *cw, bool forward)
{
  int pages;
  int size;

  if (!cw || cw->nr <= 0)
    return;
  pages = n_pages(cw);
  size = page_size(cw);
  if (forward)
    cw->page = (cw->page + 1) % pages;
  else
    cw->page = (cw->page + pages - 1) % pages;
  fetch_page(cw);

  if (cw->index >= 0) {
    int index = cw->page * size + cw->index % size;
    if (index >= cw->nr)
      index = cw->nr - 1;
    cw->index = index;
    /* Tell uim which candidate is now selected. Some input methods
     * answer this from inside, re-entering the selector callbacks:
     * mozc goes through mozc-update-candidates, which can select,
     * reactivate or even deactivate the selector. */
    uim_set_candidate_index(cw->uw->uc, index);
  }
  /* Don't re-show a window the input method just closed. */
  if (cw->nr > 0)
    draw(cw);
}

void
uim_wayland_candwin_deactivate(struct uim_wayland_candwin *cw)
{
  if (!cw)
    return;
  hide(cw);
  cw->dirty = false;
  free_candidates(cw);
  cw->nr = 0;
  cw->index = -1;
  cw->page = 0;
}

/* pointer */

static struct uim_wayland_candwin *
pointer_candwin(struct uim_wayland *uw)
{
  struct uim_wayland_candwin *cw = uw->candwin;

  if (!cw || !cw->pointer_inside || !cw->shown || !uw->context)
    return NULL;
  return cw;
}

static void
pointer_enter(void *data,
              struct wl_pointer *pointer,
              uint32_t serial,
              struct wl_surface *surface,
              wl_fixed_t x,
              wl_fixed_t y)
{
  struct uim_wayland *uw = data;
  struct uim_wayland_candwin *cw = uw->candwin;
  (void)pointer;
  (void)serial;
  (void)x;

  if (!cw)
    return;
  cw->pointer_inside = surface == cw->surface;
  cw->pointer_y = wl_fixed_to_double(y);
  cw->scroll = 0;
}

static void
pointer_leave(void *data,
              struct wl_pointer *pointer,
              uint32_t serial,
              struct wl_surface *surface)
{
  struct uim_wayland *uw = data;
  (void)pointer;
  (void)serial;
  (void)surface;

  if (uw->candwin)
    uw->candwin->pointer_inside = false;
}

static void
pointer_motion(void *data,
               struct wl_pointer *pointer,
               uint32_t time,
               wl_fixed_t x,
               wl_fixed_t y)
{
  struct uim_wayland *uw = data;
  (void)pointer;
  (void)time;
  (void)x;

  if (uw->candwin)
    uw->candwin->pointer_y = wl_fixed_to_double(y);
}

/* A click selects without committing, as in the GTK and Qt windows. */
static void
pointer_button(void *data,
               struct wl_pointer *pointer,
               uint32_t serial,
               uint32_t time,
               uint32_t button,
               uint32_t state)
{
  struct uim_wayland_candwin *cw = pointer_candwin(data);
  int row, index;
  (void)pointer;
  (void)serial;
  (void)time;

  if (!cw || button != BTN_LEFT || state != WL_POINTER_BUTTON_STATE_PRESSED)
    return;
  if (cw->row_height <= 0 || cw->pointer_y < cw->rows_top)
    return;
  row = (int)((cw->pointer_y - cw->rows_top) / cw->row_height);
  if (row >= cw->n_candidates)
    return;
  index = cw->page * page_size(cw) + row;
  if (index >= cw->nr)
    return;

  cw->index = index;
  /* uim may re-enter the selector callbacks and close the window. */
  uim_set_candidate_index(cw->uw->uc, index);
  if (cw->nr > 0)
    draw(cw);
}

static void
pointer_axis(void *data,
             struct wl_pointer *pointer,
             uint32_t time,
             uint32_t axis,
             wl_fixed_t value)
{
  struct uim_wayland_candwin *cw = pointer_candwin(data);
  bool forward;
  (void)pointer;
  (void)time;

  if (!cw || axis != WL_POINTER_AXIS_VERTICAL_SCROLL)
    return;
  cw->scroll += wl_fixed_to_double(value);
  if (cw->nr <= 0 ||
      (cw->scroll < CANDWIN_SCROLL_STEP && cw->scroll > -CANDWIN_SCROLL_STEP))
    return;
  /* One page per event, dropping the rest: a KWin notch of 15 would
   * otherwise leave 5 behind and turn two pages every other notch. */
  forward = cw->scroll > 0;
  cw->scroll = 0;
  /* Otherwise uim isn't told about the page, as in GTK. */
  if (cw->index < 0)
    cw->index = cw->page * page_size(cw);
  uim_wayland_candwin_shift_page(cw, forward);
}

static void
pointer_frame(void *data, struct wl_pointer *pointer)
{
  (void)data;
  (void)pointer;
}

static void
pointer_axis_source(void *data,
                    struct wl_pointer *pointer,
                    uint32_t axis_source)
{
  (void)data;
  (void)pointer;
  (void)axis_source;
}

static void
pointer_axis_stop(void *data,
                  struct wl_pointer *pointer,
                  uint32_t time,
                  uint32_t axis)
{
  (void)data;
  (void)pointer;
  (void)time;
  (void)axis;
}

static void
pointer_axis_discrete(void *data,
                      struct wl_pointer *pointer,
                      uint32_t axis,
                      int32_t discrete)
{
  (void)data;
  (void)pointer;
  (void)axis;
  (void)discrete;
}

/* Up to wl_seat version 5. */
const struct wl_pointer_listener uim_wayland_candwin_pointer_listener = {
  pointer_enter,
  pointer_leave,
  pointer_motion,
  pointer_button,
  pointer_axis,
  pointer_frame,
  pointer_axis_source,
  pointer_axis_stop,
  pointer_axis_discrete
};
