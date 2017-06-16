/*

  Copyright (c) 2009-2013 uim Project https://github.com/uim/uim

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

#include <config.h>

#include <stdio.h>
#include <stdlib.h>
#include <libintl.h>

#include <sqlite3.h>

#include "uim.h"
#include "uim-scm.h"
#include "uim-scm-abbrev.h"
#include "uim-helper.h"
#include "uim-notify.h"
#include "dynlib.h"


typedef struct {
  int flag;
  char *arg;
} opt_args;

static uim_lisp
make_arg_cons(const opt_args *arg)
{
  return CONS(MAKE_SYM(arg->arg), MAKE_INT(arg->flag));
}

static uim_lisp
make_arg_list(const opt_args *list)
{
  uim_lisp ret_;
  int i = 0;

  ret_ = uim_scm_null();
  while (list[i].arg != 0) {
    ret_ = CONS((uim_lisp)uim_scm_call_with_gc_ready_stack((uim_gc_gate_func_ptr)make_arg_cons,
                                                           (void *)&list[i]), ret_);
    i++;
  }
  return ret_;
}

static const opt_args sqlite3_results[] = {
  { SQLITE_OK, "$SQLITE_OK" },
  { SQLITE_ERROR, "$SQLITE_ERROR" },
  { SQLITE_INTERNAL, "$SQLITE_INTERNAL" },
  { SQLITE_PERM, "$SQLITE_PERM" },
  { SQLITE_ABORT, "$SQLITE_ABORT" },
  { SQLITE_BUSY, "$SQLITE_BUSY" },
  { SQLITE_LOCKED, "$SQLITE_LOCKED" },
  { SQLITE_NOMEM, "$SQLITE_NOMEM" },
  { SQLITE_READONLY, "$SQLITE_READONLY" },
  { SQLITE_INTERRUPT, "$SQLITE_INTERRUPT" },
  { SQLITE_IOERR, "$SQLITE_IOERR" },
  { SQLITE_CORRUPT, "$SQLITE_CORRUPT" },
  { SQLITE_NOTFOUND, "$SQLITE_NOTFOUND" },
  { SQLITE_FULL, "$SQLITE_FULL" },
  { SQLITE_CANTOPEN, "$SQLITE_CANTOPEN" },
  { SQLITE_PROTOCOL, "$SQLITE_PROTOCOL" },
  { SQLITE_EMPTY, "$SQLITE_EMPTY" },
  { SQLITE_SCHEMA, "$SQLITE_SCHEMA" },
  { SQLITE_TOOBIG, "$SQLITE_TOOBIG" },
  { SQLITE_CONSTRAINT, "$SQLITE_CONSTRAINT" },
  { SQLITE_MISMATCH, "$SQLITE_MISMATCH" },
  { SQLITE_MISUSE, "$SQLITE_MISUSE" },
  { SQLITE_NOLFS, "$SQLITE_NOLFS" },
  { SQLITE_AUTH, "$SQLITE_AUTH" },
  { SQLITE_FORMAT, "$SQLITE_FORMAT" },
  { SQLITE_RANGE, "$SQLITE_RANGE" },
  { SQLITE_NOTADB, "$SQLITE_NOTADB" },
  { SQLITE_ROW, "$SQLITE_ROW" },
  { SQLITE_DONE, "$SQLITE_DONE" },
  { 0, 0 }
};

static uim_lisp uim_lisp_sqlite3_results_;
static uim_lisp
c_uim_lisp_sqlite3_results(void)
{
  return uim_lisp_sqlite3_results_;
}

static uim_lisp
uim_sqlite3_libversion(void)
{
  return MAKE_STR(sqlite3_libversion());
}

static uim_lisp
uim_sqlite3_open(uim_lisp filename_)
{
  sqlite3 *db;

  if (sqlite3_open(REFER_C_STR(filename_), &db) != SQLITE_OK)
    ERROR_OBJ(sqlite3_errmsg(db), filename_);
  return MAKE_PTR(db);
}

static uim_lisp
uim_sqlite3_close(uim_lisp db_)
{
  if (sqlite3_close(C_PTR(db_)) != SQLITE_OK)
    ERROR_OBJ(sqlite3_errmsg(C_PTR(db_)), db_);
  return uim_scm_t();
}

static uim_lisp
uim_sqlite3_errmsg(uim_lisp db_)
{
  return MAKE_STR(sqlite3_errmsg(C_PTR(db_)));
}

static uim_lisp
uim_sqlite3_prepare(uim_lisp db_, uim_lisp zSql_, uim_lisp nBytes_)
{
  sqlite3_stmt *ppStmt;
  const char *pzTail;

  if (sqlite3_prepare(C_PTR(db_), REFER_C_STR(zSql_), C_INT(nBytes_), &ppStmt, &pzTail) != SQLITE_OK)
    ERROR_OBJ(sqlite3_errmsg(C_PTR(db_)), zSql_);
  return CONS(MAKE_PTR(ppStmt), MAKE_STR(pzTail));
}

static uim_lisp
uim_sqlite3_finalize(uim_lisp pStmt_)
{
  if (sqlite3_finalize(C_PTR(pStmt_)) != SQLITE_OK)
    uim_scm_f();
  return uim_scm_t();
}

static uim_lisp
uim_sqlite3_reset(uim_lisp pStmt_)
{
  if (sqlite3_reset(C_PTR(pStmt_)) != SQLITE_OK)
    uim_scm_f();
  return uim_scm_t();
}

static uim_lisp
uim_sqlite3_bind_int(uim_lisp pStmt_, uim_lisp idx_, uim_lisp val_)
{
  if (sqlite3_bind_int(C_PTR(pStmt_), C_INT(idx_), C_INT(val_)) != SQLITE_OK)
    uim_scm_f();
  return uim_scm_t();
}
static uim_lisp
uim_sqlite3_bind_null(uim_lisp pStmt_, uim_lisp idx_)
{
  if (sqlite3_bind_null(C_PTR(pStmt_), C_INT(idx_)) != SQLITE_OK)
    uim_scm_f();
  return uim_scm_t();
}
static uim_lisp
uim_sqlite3_bind_blob(uim_lisp pStmt_, uim_lisp idx_, uim_lisp str_, uim_lisp nBytes_)
{
  if (sqlite3_bind_blob(C_PTR(pStmt_), C_INT(idx_), REFER_C_STR(str_), C_INT(nBytes_), SQLITE_TRANSIENT) != SQLITE_OK)
    uim_scm_f();
  return uim_scm_t();
}
static uim_lisp
uim_sqlite3_bind_text(uim_lisp pStmt_, uim_lisp idx_, uim_lisp str_, uim_lisp nBytes_)
{
  if (sqlite3_bind_text(C_PTR(pStmt_), C_INT(idx_), REFER_C_STR(str_), C_INT(nBytes_), SQLITE_TRANSIENT) != SQLITE_OK)
    uim_scm_f();
  return uim_scm_t();
}

static uim_lisp
uim_sqlite3_clear_bindings(uim_lisp pStmt_)
{
  if (sqlite3_clear_bindings(C_PTR(pStmt_)) != SQLITE_OK)
    uim_scm_f();
  return uim_scm_t();
}

static uim_lisp
uim_sqlite3_step(uim_lisp pStmt_)
{
  int ret;

  ret = sqlite3_step(C_PTR(pStmt_));
  if (ret != SQLITE_OK)
    uim_scm_f();
  return MAKE_INT(ret);
}

static uim_lisp
uim_sqlite3_column_count(uim_lisp pStmt_)
{
  return MAKE_INT(sqlite3_column_count(C_PTR(pStmt_)));
}
static uim_lisp
uim_sqlite3_data_count(uim_lisp pStmt_)
{
  return MAKE_INT(sqlite3_data_count(C_PTR(pStmt_)));
}

static uim_lisp
uim_sqlite3_column_bytes(uim_lisp pStmt_, uim_lisp iCol_)
{
  return MAKE_INT(sqlite3_column_bytes(C_PTR(pStmt_), C_INT(iCol_)));
}
static uim_lisp
uim_sqlite3_column_int(uim_lisp pStmt_, uim_lisp iCol_)
{
  return MAKE_INT(sqlite3_column_int(C_PTR(pStmt_), C_INT(iCol_)));
}
static uim_lisp
uim_sqlite3_column_text(uim_lisp pStmt_, uim_lisp iCol_)
{
  const unsigned char *ret = sqlite3_column_text(C_PTR(pStmt_), C_INT(iCol_));
  if (ret)
    return MAKE_STR(ret);
  return uim_scm_f();
}
static uim_lisp
uim_sqlite3_column_blob(uim_lisp pStmt_, uim_lisp iCol_)
{
  /* XXX */
  const unsigned char *ret = sqlite3_column_blob(C_PTR(pStmt_), C_INT(iCol_));
  if (ret)
    return MAKE_STR(ret);
  return uim_scm_f();
}
static uim_lisp
uim_sqlite3_column_type(uim_lisp pStmt_, uim_lisp iCol_)
{
  return MAKE_INT(sqlite3_column_type(C_PTR(pStmt_), C_INT(iCol_)));
}

void
uim_plugin_instance_init(void)
{
  uim_lisp_sqlite3_results_ = make_arg_list(sqlite3_results);
  uim_scm_gc_protect(&uim_lisp_sqlite3_results_);
  uim_scm_init_proc0("sqlite3-results", c_uim_lisp_sqlite3_results);

  uim_scm_init_proc0("sqlite3-libversion", uim_sqlite3_libversion);
  uim_scm_init_proc1("sqlite3-open", uim_sqlite3_open);
  uim_scm_init_proc1("sqlite3-close", uim_sqlite3_close);

  uim_scm_init_proc1("sqlite3-sqlite3-errmsg", uim_sqlite3_errmsg);
  uim_scm_init_proc3("sqlite3-prepare", uim_sqlite3_prepare);
  uim_scm_init_proc1("sqlite3-finalize", uim_sqlite3_finalize);
  uim_scm_init_proc1("sqlite3-reset", uim_sqlite3_reset);

  uim_scm_init_proc3("sqlite3-bind-int", uim_sqlite3_bind_int);
  uim_scm_init_proc2("sqlite3-bind-null", uim_sqlite3_bind_null);
  uim_scm_init_proc4("sqlite3-bind-blob", uim_sqlite3_bind_blob);
  uim_scm_init_proc4("sqlite3-bind-text", uim_sqlite3_bind_text);
  uim_scm_init_proc1("sqlite3-clear-bindings", uim_sqlite3_clear_bindings);

  uim_scm_init_proc1("sqlite3-step", uim_sqlite3_step);

  uim_scm_init_proc1("sqlite3-column-count", uim_sqlite3_column_count);
  uim_scm_init_proc1("sqlite3-data-count", uim_sqlite3_data_count);

  uim_scm_init_proc2("sqlite3-column-bytes", uim_sqlite3_column_bytes);
  uim_scm_init_proc2("sqlite3-column-int", uim_sqlite3_column_int);
  uim_scm_init_proc2("sqlite3-column-text", uim_sqlite3_column_text);
  uim_scm_init_proc2("sqlite3-column-blob", uim_sqlite3_column_blob);
  uim_scm_init_proc2("sqlite3-column-type", uim_sqlite3_column_type);
}

void
uim_plugin_instance_quit(void)
{
  uim_scm_gc_unprotect(&uim_lisp_sqlite3_results_);
}
