/*

  Copyright (c) 2003,2004 uim Project http://uim.freedesktop.org/

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

  THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
  OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
  OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
  SUCH DAMAGE.

*/

#include "config.h"
#ifdef HAVE_M17NLIB
#include <stdlib.h>
#include <string.h>
#include <m17n.h>
#include "uim-scm.h"
#include "context.h"

static int m17nlib_ok;
static MConverter *converter;
static char buffer_for_converter[1024]; /* Currently, if preedit strings or
					   candidate strings over this buffer
					   size, they will simply ignore. */

static int nr_input_methods;
static struct im_ {
  char *lang;
  char *name;
  MInputMethod *im;
} *im_array;

static int max_input_contexts;
static struct ic_ {
  MInputContext *mic;
} *ic_array;

static char *
m17nlib_utf8_find_next_char(const char *p);

static int
unused_ic_id(void)
{
  int i;
  for (i = 0; i < max_input_contexts; i++) {
    if (!ic_array[i].mic) {
      return i;
    }
  }
  ic_array = realloc(ic_array, sizeof(struct ic_) * (max_input_contexts+1));
  ic_array[max_input_contexts].mic = NULL;
  max_input_contexts++;
  return max_input_contexts - 1;
}

static char *
remap_lang_name(char *lang)
{
  static struct lang_map_ {
    char *lib_lang;
    char *lang;
  } lang_map[] = {
    {"Japanese", "ja"},
    {"Amharic", "am"},
    {"Assamese", "as"},
    {"Bengali", "bn"},
    {"Tibetan", "bo"},
    {"Greek", "el"},
    {"Arabic", "ar"},
    /*    {"Farsi", ""},*/
    {"Gujarati", "gu"},
    {"Hebrew", "he"},
    {"Hindi", "hi"},
    {"Croatian", "hr"},
    {"Kazakh", "kk"},
    /*    {"Caombodia", ""},*/
    {"Kannada", "kn"},
    {"Korean", "ko"},
    {"Laothian", "lo"},
    {"Malayalam", "ml"},
    {"Oriya", "or"},
    {"Punjabi", "pa"},/* Panjabi ? */
    {"Russian", "ru"},
    {"Slovak", "sl"},/* Slovenia ? */
    {"Serbian", "sr"},
    {"Tamil", "ta"},
    {"Telugu", "te"},
    {"Thai", "th"},
    {"Vietnamese", "vi"},
    {"Chinese", "zh"},
    {NULL, NULL}
  };

  struct lang_map_ *l;
  for (l = lang_map; l->lib_lang; l++) {
    if (!strcmp(lang, l->lib_lang)) {
      return l->lang;
    }
  }
  return NULL;
}

static void
pushback_input_method(MInputMethod *im,
		      char *lib_lang, char *name)
{
  char *lang = remap_lang_name(lib_lang);
  if (!lang) {
    return ;
  }

  im_array = realloc(im_array, 
		     sizeof(struct im_) * (nr_input_methods + 1));
  im_array[nr_input_methods].lang = strdup(lang);
  im_array[nr_input_methods].name = strdup(name);
  im_array[nr_input_methods].im = im;
  nr_input_methods++;
}

static void
preedit_start_cb(MInputContext *ic, MSymbol command)
{
  fprintf(stderr,"preedit start\n");
}

static void 
preedit_draw_cb(MInputContext *ic, MSymbol command)
{

}

static void 
preedit_done_cb(MInputContext *ic, MSymbol command)
{
  /*  fprintf(stderr,"preedit done\n");*/
}

static void
status_start_cb(MInputContext *ic, MSymbol command)
{
  /*  fprintf(stderr,"status start\n");*/
}

static void 
status_draw_cb(MInputContext *ic, MSymbol command)
{
  /*  fprintf(stderr,"status draw\n"); */
}

static void 
status_done_cb(MInputContext *ic, MSymbol command)
{
  /*  fprintf(stderr,"status done\n");*/
}

static void
candidates_start_cb(MInputContext *ic, MSymbol command)
{
  /*  fprintf(stderr,"candidates_start\n");*/
}

static void 
candidates_draw_cb(MInputContext *ic, MSymbol command)
{
  /*  fprintf(stderr,"candidate draw\n"); */
}

static void 
candidates_done_cb(MInputContext *ic, MSymbol command)
{
  fprintf(stderr,"candidate done\n");
}

static MPlist *
register_callbacks(MPlist *callback_list)
{
  if(!callback_list)
    callback_list = mplist();

  mplist_add(callback_list, Minput_preedit_start, (void *)preedit_start_cb);
  mplist_add(callback_list, Minput_preedit_draw,  (void *)preedit_draw_cb);
  mplist_add(callback_list, Minput_preedit_done,  (void *)preedit_done_cb);
  mplist_add(callback_list, Minput_status_start,  (void *)status_start_cb);
  mplist_add(callback_list, Minput_status_draw,   (void *)status_draw_cb);
  mplist_add(callback_list, Minput_status_done,   (void *)status_done_cb);
  mplist_add(callback_list, Minput_candidates_start, (void *)candidates_start_cb);
  mplist_add(callback_list, Minput_candidates_draw,  (void *)candidates_draw_cb);
  mplist_add(callback_list, Minput_candidates_done,  (void *)candidates_done_cb);

  return callback_list;
}

static LISP
init_m17nlib()
{
  MPlist *imlist, *elm;
  MSymbol utf8 = msymbol("utf8");
  M17N_INIT();
  nr_input_methods = 0;
  im_array = NULL;
  max_input_contexts = 0;
  ic_array = NULL;

  imlist = mdatabase_list(msymbol("input-method"), Mnil, Mnil, Mnil);
  if (!imlist) {
    /* maybe user forgot to install m17n-db */
    return NIL;
  }
  for (elm = imlist; mplist_key(elm) != Mnil; elm = mplist_next(elm)) {
    MDatabase *mdb = mplist_value(elm);
    MSymbol *tag = mdatabase_tag(mdb);
    if (tag[1] != Mnil) {
      MInputMethod *im = minput_open_im(tag[1], tag[2], NULL);
      if (im) {
	MSymbol lang = msymbol_get(im->language, Mlanguage);
	pushback_input_method(im, msymbol_name(lang),
			      msymbol_name(im->name));
	
	im->driver.callback_list = register_callbacks(im->driver.callback_list);
      }
    }
  }
  m17n_object_unref(imlist);
  converter = mconv_buffer_converter(utf8, NULL, 0);
  if (!converter) {
    return NIL;
  }
  m17nlib_ok = 1;
  return uim_scm_t();
}

void
uim_quit_m17nlib(void)
{
  if (converter) {
    mconv_free_converter(converter);
    converter = NULL;
  }
  if (m17nlib_ok) {
    M17N_FINI();
    m17nlib_ok = 0;
  }
  free(im_array);
  free(ic_array);
}

static char *
convert_mtext2str(MText *mtext)
{
  mconv_rebind_buffer(converter, (unsigned char *)buffer_for_converter,
		      sizeof(buffer_for_converter));
  mconv_encode(converter, mtext);
  buffer_for_converter[converter->nbytes] = 0;
  return strdup(buffer_for_converter);
}

static LISP
compose_modep(LISP id_)
{
  int id = uim_scm_c_int(id_);
  /* range check of id might need. */
  MInputContext *ic = ic_array[id].mic;
  if(!ic) {
    return NIL;
  }
  if(ic->candidate_from == ic->candidate_to) {
    return NIL;
  } else {
    return uim_scm_t();
  }
}

static LISP
preedit_changedp(LISP id_)
{
  int id = uim_scm_c_int(id_);
  /* range check of id might need. */
  MInputContext *ic = ic_array[id].mic;
  if(!ic) {
    return uim_scm_f();
  }
  if(ic->preedit_changed == 1) {
    return uim_scm_f();
  } else {
    return uim_scm_t();
  }
}

static LISP
get_left_of_cursor(LISP id_)
{
  int id = uim_scm_c_int(id_);
  int buflen;
  int i;
  LISP buf_;
  char *buf;
  char *p;
  MInputContext *ic = ic_array[id].mic;
  if (!ic) {
    return uim_scm_str_from_c_str("");
  }
  if(ic->cursor_pos == 0) {
    return uim_scm_str_from_c_str("");
  }
  buf = convert_mtext2str(ic->preedit);
  p = (char *)buf;

  for(i=0; i<ic->cursor_pos ;i++) {
    p = m17nlib_utf8_find_next_char(p);
  }
  *p = 0;

  buflen = strlen((char *)buf);
  buf_ = uim_scm_str_from_c_str((char *)buf);
  return buf_;
}

static LISP
get_right_of_cursor(LISP id_)
{
  int id = uim_scm_c_int(id_);
  int buflen;
  int i;
  LISP buf_;
  char *buf;
  unsigned char *p;
  MInputContext *ic = ic_array[id].mic;
  if (!ic) {
    return uim_scm_str_from_c_str("");
  }

  buf = convert_mtext2str(ic->preedit);
  p = buf;

  for(i=0; i<ic->cursor_pos ;i++) {
    p = (unsigned char *)uim_scm_str_from_c_str("");
  }
  buflen = strlen((char *)p);
  buf_ = uim_scm_str_from_c_str((char *)p);
  return buf_;
}

static LISP
get_left_of_candidate(LISP id_)
{
  int id = uim_scm_c_int(id_);
  int buflen;
  int i;
  LISP buf_;
  unsigned char *buf;
  char *p;
  MInputContext *ic = ic_array[id].mic;
  if (!ic) {
    return uim_scm_str_from_c_str("");
  }
  if(ic->candidate_from == 0) {
    return uim_scm_str_from_c_str("");
  }
  buf = convert_mtext2str(ic->preedit);
  p = (char *)buf;

  for(i=0; i<ic->candidate_from ;i++) {
    p = m17nlib_utf8_find_next_char(p);
  }
  *p = 0;
  buflen = strlen((char *)buf);
  buf_ = uim_scm_str_from_c_str((char *)buf);
  free(buf);
  return buf_;
}

static LISP
get_selected_candidate(LISP id_)
{
  int id = uim_scm_c_int(id_);
  int buflen;
  int i;
  LISP buf_;
  unsigned char *buf;
  unsigned char *p;
  unsigned char *start;
  MInputContext *ic = ic_array[id].mic;
  if (!ic) {
    return uim_scm_str_from_c_str("");
  }
  buf = convert_mtext2str(ic->preedit);
  p = buf;

  if(!p) {
    return uim_scm_str_from_c_str("");
  }

  for(i=0; i<ic->candidate_from ;i++) {
    p = (unsigned char *)m17nlib_utf8_find_next_char((char *)p);
  }
  start = p;

  for(i=0; i<ic->candidate_to - ic->candidate_from ;i++) {
    p = (unsigned char *)m17nlib_utf8_find_next_char((char *)p);
  }
  *p = 0;

  buflen = strlen((char *)start);
  buf_ = uim_scm_str_from_c_str((char *)start);
  free(buf);
  return buf_;
}

static LISP
get_right_of_candidate(LISP id_)
{
  int id = uim_scm_c_int(id_);
  int buflen;
  int i;
  LISP buf_;
  unsigned char *buf;
  unsigned char *p;
  MInputContext *ic = ic_array[id].mic;
  if (!ic) {
    return uim_scm_str_from_c_str("");
  }
  buf = convert_mtext2str(ic->preedit);
  p = buf;

  for(i=0; i<ic->candidate_to ;i++) {
    p = (unsigned char *)m17nlib_utf8_find_next_char((char *)p);
  }
  buflen = strlen((char *)p);
  buf_ = uim_scm_str_from_c_str((char *)p);
  free(buf);
  return buf_;
}

static LISP
get_nr_input_methods()
{
  return uim_scm_int_from_c_int(nr_input_methods);
}

static LISP
get_input_method_name(LISP nth_)
{
  int nth = uim_scm_c_int(nth_);
  if (nth < nr_input_methods) {
    char *name = alloca(strlen(im_array[nth].name) + 20);
    sprintf(name, "m17n-%s-%s", im_array[nth].lang, im_array[nth].name);
    return uim_scm_str_from_c_str(name);
  }
  return NIL;
}

static LISP
get_input_method_lang(LISP nth_)
{
  int nth = uim_scm_c_int(nth_);
  if (nth < nr_input_methods) {
    char *lang = im_array[nth].lang;
    return uim_scm_str_from_c_str(lang);
  }
  return NIL;
}

static MInputMethod *
find_im_by_name(char *name)
{
  int i;
  if (strncmp(name, "m17n-", 5) != 0) {
    return NULL;
  }
  name = &name[5];
  for (i = 0; i < nr_input_methods; i++) {
    char buf[100];
    sprintf(buf, "%s-%s", im_array[i].lang, im_array[i].name);
    if (!strcmp(name, buf)) {
      return im_array[i].im;
    }
  }
  return NULL;
}

static LISP
alloc_id(LISP name_)
{
  int id = unused_ic_id();
  char *name = uim_scm_c_str(name_);
  MInputMethod *im = find_im_by_name(name);
  if (im) {
    ic_array[id].mic = minput_create_ic(im, NULL);
  }
  free(name);
  return uim_scm_int_from_c_int(id);
}

static LISP
free_id(LISP id_)
{
  int id = uim_scm_c_int(id_);
  if (id < max_input_contexts) {
    struct ic_ *ic = &ic_array[id];
    if (ic->mic) {
      minput_destroy_ic(ic->mic);
      ic->mic = NULL;
    }
  }
  return NIL;
}

static MSymbol
get_key_sym(int ch)
{
  if (ch < 127) {
    char buf[2];
    buf[0] = ch;
    buf[1] = 0;
    return msymbol(buf);
  }
  return Mnil;
}

static LISP
push_symbol_key(LISP id_, LISP key_)
{
  int id = uim_scm_c_int(id_);
  MSymbol key;
  MInputContext *ic = ic_array[id].mic;
  /*  printf("%s\n",uim_scm_c_str(key_)); */
  key = msymbol(uim_scm_c_str(key_));
  if (key == Mnil) {
    return uim_scm_t();
  }
  if (minput_filter(ic, key, NULL) == 1) {
    return uim_scm_t();
  }

  return NIL;
}

static LISP
push_key(LISP id_, LISP key_, LISP mod_)
{
  int id = uim_scm_c_int(id_);
  int ch;
  MSymbol key;
  MInputContext *ic = ic_array[id].mic;

  ch = uim_scm_c_int(key_);
  key = get_key_sym(ch);
  if (key == Mnil) {
    return uim_scm_t();
  }
  if (minput_filter(ic, key, NULL) == 1) {
    return uim_scm_t();
  }

  return NIL;
}

static LISP
get_commit_string(LISP id_)
{
  MText *produced;
  unsigned char *buf;
  int id = uim_scm_c_int(id_);
  MInputContext *ic = ic_array[id].mic;
  LISP buf_;

  produced = mtext();
  minput_lookup(ic, NULL, NULL, produced);
  buf = convert_mtext2str(produced);
  m17n_object_unref(produced);
  buf_ = uim_scm_str_from_c_str(buf);
  free(buf);
  return buf_;
}

static LISP
commit(LISP id_)
{
  int id = uim_scm_c_int(id_);
  MInputContext *ic = ic_array[id].mic;

/* To avoid a bug of m17n-lib */
  ic->candidate_to   = 0;
  return NIL;
}

static LISP
candidate_showp(LISP id_)
{
  int id = uim_scm_c_int(id_);
  MInputContext *ic = ic_array[id].mic;

  if (ic->candidate_show == 1) {
    return uim_scm_t();
  }
  return NIL;
}

static LISP
get_nr_candidates(LISP id_)
{
  MPlist *group;
  MPlist *elm;
  int result = 0;
  int id = uim_scm_c_int(id_);
  MInputContext *ic = ic_array[id].mic;

  if(!ic || !ic->candidate_list)
    return uim_scm_f();


  group = ic->candidate_list;

  while(mplist_value(group) != Mnil) {
    if(mplist_key(group) == Mtext) {
      for (; mplist_key(group) != Mnil; group = mplist_next(group)) {
        result += mtext_len(mplist_value(group));
      }
    } else {
      for (; mplist_key(group) != Mnil; group = mplist_next(group)) {
        result += mplist_length(mplist_value(group));
      }
    }
  }

  return uim_scm_int_from_c_int(result);
}

static LISP
get_nth_candidate(LISP id_, LISP nth_)
{
  MText *produced = NULL; /* Quiet gcc */
  MPlist *group;
  MPlist *elm;
  int i;
  unsigned char *buf = NULL; /* Quiet gcc */
  int id = uim_scm_c_int(id_);
  int nth = uim_scm_c_int(nth_);
  MInputContext *ic = ic_array[id].mic;
  LISP buf_;

  if(!ic || !ic->candidate_list)
    return NIL;

  group = ic->candidate_list;

  if(mplist_key(group) == Mtext) {
    for (i=0; mplist_key(group) != Mnil; group = mplist_next(group)) {
      int j;
      for (j=0; j < mtext_len(mplist_value(group)); j++, i++) {
        if(i == nth) {
          produced = mtext();
          mtext_cat_char(produced, mtext_ref_char(mplist_value(group), j));
          buf = convert_mtext2str(produced);
          m17n_object_unref(produced);
        }
      }
    }
  } else {
    for (i=0; mplist_key(group) != Mnil; group = mplist_next(group)) {
      for (elm = mplist_value(group); mplist_key(elm) != Mnil; elm = mplist_next(elm),i++) {
        if(i == nth) {
          produced = mplist_value(elm);
          buf = convert_mtext2str(produced);
        }
      }
    }
  }

  if(!buf) {
    return uim_scm_str_from_c_str("");
  } else {
    buf_ = uim_scm_str_from_c_str(buf);
    free(buf);
    return buf_;
  }
}

static LISP
get_candidate_index(LISP id_)
{
  int id = uim_scm_c_int(id_);
  MInputContext *ic = ic_array[id].mic;
  return uim_scm_int_from_c_int(ic->candidate_index);
}

/* Utility function */
static char *
m17nlib_utf8_find_next_char(const char *p)
{
  if (*p) {
    for (++p; (*p & 0xc0) == 0x80; ++p)
      ;
  }
  return (char *)p;
}

void
uim_init_m17nlib(void)
{
  uim_init_subr_0("m17nlib-lib-init", init_m17nlib);
  uim_init_subr_0("m17nlib-lib-nr-input-methods", get_nr_input_methods);
  uim_init_subr_1("m17nlib-lib-nth-input-method-lang", get_input_method_lang);
  uim_init_subr_1("m17nlib-lib-nth-input-method-name", get_input_method_name);
  uim_init_subr_1("m17nlib-lib-alloc-context", alloc_id);
  uim_init_subr_1("m17nlib-lib-free-context", free_id);
  uim_init_subr_3("m17nlib-lib-push-key", push_key);
  uim_init_subr_2("m17nlib-lib-push-symbol-key", push_symbol_key);
  uim_init_subr_1("m17nlib-lib-compose-mode?", compose_modep);
  uim_init_subr_1("m17nlib-lib-preedit-changed?", preedit_changedp);
  uim_init_subr_1("m17nlib-lib-get-left-of-cursor",     get_left_of_cursor);
  uim_init_subr_1("m17nlib-lib-get-right-of-cursor",    get_right_of_cursor);
  uim_init_subr_1("m17nlib-lib-get-left-of-candidate",  get_left_of_candidate);
  uim_init_subr_1("m17nlib-lib-get-selected-candidate", get_selected_candidate);
  uim_init_subr_1("m17nlib-lib-get-right-of-candidate", get_right_of_candidate);
  uim_init_subr_1("m17nlib-lib-get-commit-string", get_commit_string);
  uim_init_subr_1("m17nlib-lib-commit", commit);
  uim_init_subr_1("m17nlib-lib-candidate-show?", candidate_showp);
  uim_init_subr_1("m17nlib-lib-get-nr-candidates", get_nr_candidates);
  uim_init_subr_2("m17nlib-lib-get-nth-candidate", get_nth_candidate);
  uim_init_subr_1("m17nlib-lib-get-candidate-index", get_candidate_index);
}

void
plugin_init(void) {
  uim_init_m17nlib();
}

#endif /* HAVE_M17NLIB */
