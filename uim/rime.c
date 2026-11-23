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

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS IS'' AND
  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE
  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
  OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
  OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
  SUCH DAMAGE.
*/

/*
 * Procedures to use librime, the engine of Rime, from scm/rime.scm.
 * librime processes keys and has the composition and the candidates.
 * These procedures only pass keys to it and take what it has.
 */

#include <config.h>

#include <errno.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>

#include <rime_api.h>

#include "uim.h"
#include "uim-scm.h"
#include "uim-scm-abbrev.h"
#include "uim-util.h"
#include "dynlib.h"

static RimeApi *api;
/* librime can be set up only once in a process. */
static uim_bool set_up;
static uim_bool initialized;

static RimeSessionId
c_session(uim_lisp session_)
{
  return (RimeSessionId)(uintptr_t)C_PTR(session_);
}

/* Makes DIR and its parents. */
static uim_bool
make_directory(const char *dir)
{
  char *path = uim_strdup(dir);
  char *p;
  uim_bool succeeded = UIM_TRUE;

  for (p = path + 1; *p; p++) {
    if (*p == '/') {
      *p = '\0';
      if (mkdir(path, 0700) == -1 && errno != EEXIST) {
        succeeded = UIM_FALSE;
        break;
      }
      *p = '/';
    }
  }
  if (succeeded && mkdir(path, 0700) == -1 && errno != EEXIST)
    succeeded = UIM_FALSE;
  free(path);
  return succeeded;
}

/*
 * (rime-lib-init shared-data-dir user-data-dir)
 *
 * Starts librime. It deploys the schemas when they are changed, which
 * takes time when shared-data-dir doesn't have prebuilt ones.
 */
static uim_lisp
rime_lib_init(uim_lisp shared_data_dir_, uim_lisp user_data_dir_)
{
  RIME_STRUCT(RimeTraits, traits);
  const char *user_data_dir = REFER_C_STR(user_data_dir_);

  if (initialized)
    return uim_scm_t();

  api = rime_get_api();
  if (!api || !make_directory(user_data_dir))
    return uim_scm_f();

  traits.shared_data_dir = REFER_C_STR(shared_data_dir_);
  traits.user_data_dir = user_data_dir;
  traits.distribution_name = "uim";
  traits.distribution_code_name = "uim";
  traits.distribution_version = PACKAGE_VERSION;
  traits.app_name = "rime.uim";
  /* Only errors, to stderr. */
  traits.min_log_level = 2;
  traits.log_dir = "";
  if (!set_up) {
    api->setup(&traits);
    set_up = UIM_TRUE;
  }
  api->initialize(&traits);
  if (api->start_maintenance(False))
    api->join_maintenance_thread();
  initialized = UIM_TRUE;
  return uim_scm_t();
}

/* The directory that has the schemas of the librime used on build. */
static uim_lisp
rime_lib_default_shared_data_dir(void)
{
  return MAKE_STR(RIME_SHARED_DATA_DIR);
}

static uim_lisp
rime_lib_create_session(void)
{
  RimeSessionId session;

  if (!initialized)
    return uim_scm_f();
  session = api->create_session();
  if (!session)
    return uim_scm_f();
  return MAKE_PTR((void *)(uintptr_t)session);
}

static uim_lisp
rime_lib_destroy_session(uim_lisp session_)
{
  if (initialized)
    api->destroy_session(c_session(session_));
  return uim_scm_f();
}

/* Returns whether librime consumed the key. */
static uim_lisp
rime_lib_process_key(uim_lisp session_, uim_lisp keycode_, uim_lisp mask_)
{
  if (!initialized)
    return uim_scm_f();
  return MAKE_BOOL(api->process_key(c_session(session_),
                                    C_INT(keycode_),
                                    C_INT(mask_)));
}

/* Returns the string that librime committed, or #f. */
static uim_lisp
rime_lib_get_commit(uim_lisp session_)
{
  RIME_STRUCT(RimeCommit, commit);
  uim_lisp text_;

  if (!initialized || !api->get_commit(c_session(session_), &commit))
    return uim_scm_f();
  text_ = commit.text ? MAKE_STR(commit.text) : uim_scm_f();
  api->free_commit(&commit);
  return text_;
}

static uim_lisp
make_substring(const char *str, int start, int end)
{
  char *sub;
  uim_lisp sub_;

  sub = uim_malloc(end - start + 1);
  memcpy(sub, str + start, end - start);
  sub[end - start] = '\0';
  sub_ = MAKE_STR(sub);
  free(sub);
  return sub_;
}

static int
clamp(int value, int min, int max)
{
  if (value < min)
    return min;
  if (value > max)
    return max;
  return value;
}

/* The label of the Nth candidate of the page. */
static uim_lisp
make_label(RimeContext *context, int n)
{
  const char *keys = context->menu.select_keys;
  char label[16];

  if (RIME_STRUCT_HAS_MEMBER(*context, context->select_labels) &&
      context->select_labels && context->select_labels[n])
    return MAKE_STR(context->select_labels[n]);
  if (keys && (size_t)n < strlen(keys)) {
    label[0] = keys[n];
    label[1] = '\0';
  } else {
    snprintf(label, sizeof(label), "%d", (n + 1) % 10);
  }
  return MAKE_STR(label);
}

/*
 * (rime-lib-get-context session)
 *
 * Returns (preedit-before-cursor preedit-selected preedit-after-cursor
 * highlighted-index candidates), or #f. The preedit is split with the
 * selection that librime is converting now. candidates is ((text
 * comment label) ...) of the page shown now.
 */
static uim_lisp
rime_lib_get_context(uim_lisp session_)
{
  RIME_STRUCT(RimeContext, context);
  const char *preedit;
  int length, selection_start, selection_end, i;
  uim_lisp before_, selected_, after_, candidates_, context_;

  if (!initialized || !api->get_context(c_session(session_), &context))
    return uim_scm_f();

  preedit = context.composition.preedit ? context.composition.preedit : "";
  length = strlen(preedit);
  selection_start = clamp(context.composition.sel_start, 0, length);
  selection_end = clamp(context.composition.sel_end, selection_start, length);
  before_ = make_substring(preedit, 0, selection_start);
  selected_ = make_substring(preedit, selection_start, selection_end);
  after_ = make_substring(preedit, selection_end, length);

  candidates_ = uim_scm_null();
  for (i = context.menu.num_candidates - 1; i >= 0; i--) {
    RimeCandidate *candidate = &context.menu.candidates[i];
    candidates_ = CONS(LIST3(MAKE_STR(candidate->text ? candidate->text : ""),
                             MAKE_STR(candidate->comment ?
                                      candidate->comment : ""),
                             make_label(&context, i)),
                       candidates_);
  }
  context_ = LIST5(before_,
                   selected_,
                   after_,
                   MAKE_INT(context.menu.highlighted_candidate_index),
                   candidates_);
  api->free_context(&context);
  return context_;
}

/*
 * (rime-lib-get-status session)
 *
 * Returns (schema-id schema-name ascii-mode?), or #f.
 */
static uim_lisp
rime_lib_get_status(uim_lisp session_)
{
  RIME_STRUCT(RimeStatus, status);
  uim_lisp status_;

  if (!initialized || !api->get_status(c_session(session_), &status))
    return uim_scm_f();
  status_ = LIST3(MAKE_STR(status.schema_id ? status.schema_id : ""),
                  MAKE_STR(status.schema_name ? status.schema_name : ""),
                  MAKE_BOOL(status.is_ascii_mode));
  api->free_status(&status);
  return status_;
}

static uim_lisp
rime_lib_set_option(uim_lisp session_, uim_lisp option_, uim_lisp value_)
{
  if (initialized)
    api->set_option(c_session(session_), REFER_C_STR(option_),
                    C_BOOL(value_) ? True : False);
  return uim_scm_f();
}

static uim_lisp
rime_lib_clear_composition(uim_lisp session_)
{
  if (initialized)
    api->clear_composition(c_session(session_));
  return uim_scm_f();
}

/* Commits what is composed as is. Returns whether it has something. */
static uim_lisp
rime_lib_commit_composition(uim_lisp session_)
{
  if (!initialized)
    return uim_scm_f();
  return MAKE_BOOL(api->commit_composition(c_session(session_)));
}

/* Selects the INDEXth candidate of the page shown now. */
static uim_lisp
rime_lib_select_candidate(uim_lisp session_, uim_lisp index_)
{
  if (!initialized ||
      !RIME_API_AVAILABLE(api, select_candidate_on_current_page))
    return uim_scm_f();
  return MAKE_BOOL(api->select_candidate_on_current_page(c_session(session_),
                                                         C_INT(index_)));
}

/* Returns ((schema-id . schema-name) ...). */
static uim_lisp
rime_lib_get_schema_list(void)
{
  RimeSchemaList list;
  uim_lisp list_ = uim_scm_null();
  size_t i;

  if (!initialized || !api->get_schema_list(&list))
    return list_;
  for (i = list.size; i > 0; i--) {
    RimeSchemaListItem *item = &list.list[i - 1];
    list_ = CONS(CONS(MAKE_STR(item->schema_id ? item->schema_id : ""),
                      MAKE_STR(item->name ? item->name : "")),
                 list_);
  }
  api->free_schema_list(&list);
  return list_;
}

static uim_lisp
rime_lib_select_schema(uim_lisp session_, uim_lisp schema_id_)
{
  if (!initialized)
    return uim_scm_f();
  return MAKE_BOOL(api->select_schema(c_session(session_),
                                      REFER_C_STR(schema_id_)));
}

void
uim_plugin_instance_init(void)
{
  uim_scm_init_proc2("rime-lib-init", rime_lib_init);
  uim_scm_init_proc0("rime-lib-default-shared-data-dir",
                     rime_lib_default_shared_data_dir);
  uim_scm_init_proc0("rime-lib-create-session", rime_lib_create_session);
  uim_scm_init_proc1("rime-lib-destroy-session", rime_lib_destroy_session);
  uim_scm_init_proc3("rime-lib-process-key", rime_lib_process_key);
  uim_scm_init_proc1("rime-lib-get-commit", rime_lib_get_commit);
  uim_scm_init_proc1("rime-lib-get-context", rime_lib_get_context);
  uim_scm_init_proc1("rime-lib-get-status", rime_lib_get_status);
  uim_scm_init_proc3("rime-lib-set-option", rime_lib_set_option);
  uim_scm_init_proc1("rime-lib-clear-composition",
                     rime_lib_clear_composition);
  uim_scm_init_proc1("rime-lib-commit-composition",
                     rime_lib_commit_composition);
  uim_scm_init_proc2("rime-lib-select-candidate", rime_lib_select_candidate);
  uim_scm_init_proc0("rime-lib-get-schema-list", rime_lib_get_schema_list);
  uim_scm_init_proc2("rime-lib-select-schema", rime_lib_select_schema);
}

void
uim_plugin_instance_quit(void)
{
  if (initialized) {
    api->finalize();
    initialized = UIM_FALSE;
  }
}
