/*===========================================================================
 *  FileName : io.c
 *  About    : io related functions
 *
 *  Copyright (C) 2005      by Kazuki Ohta (mover@hct.zaq.ne.jp)
 *
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions
 *  are met:
 *
 *  1. Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *  3. Neither the name of authors nor the names of its contributors
 *     may be used to endorse or promote products derived from this software
 *     without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS IS''
 *  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 *  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 *  ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR CONTRIBUTORS BE
 *  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 *  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
 *  GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 *  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 *  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 *  OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 *  SUCH DAMAGE.
===========================================================================*/
/*=======================================
  System Include
=======================================*/
#include <stdio.h>
#include <string.h>

/*=======================================
  Local Include
=======================================*/
#include "sigscheme.h"
#include "sigschemeinternal.h"
#include "sbcport.h"
#include "fileport.h"

/*=======================================
  File Local Struct Declarations
=======================================*/

/*=======================================
  File Local Macro Declarations
=======================================*/

/*=======================================
  Variable Declarations
=======================================*/
ScmObj scm_current_input_port   = NULL;
ScmObj scm_current_output_port  = NULL;
ScmObj scm_current_error_port  = NULL;

ScmObj SigScm_features      = NULL;

static const char *lib_path = NULL;

const ScmSpecialCharInfo Scm_special_char_table[] = {
    /* printable characters */
    {'\"',   "\\\"",  "\""},
    {'\\',   "\\\\",  "\\"},
    {' ',    " ",     "space"},  /* R5RS */
#if 0
    /* to avoid portability problem, we should not support #\Space and so on */
    {' ',    " ",     "Space"},
#endif

    /* control characters */
    {'\n',   "\\n",   "newline"},  /* R5RS */
#if SCM_USE_SRFI75_NAMED_CHARS
    {'\0',   "\\0",   "nul"},
    {'\a',   "\\a",   "alarm"},
    {'\b',   "\\b",   "backspace"},
    {'\t',   "\\t",   "tab"},
    {'\n',   "\\n",   "linefeed"},
    {'\v',   "\\v",   "vtab"},
    {'\f',   "\\f",   "page"},
    {'\r',   "\\r",   "return"},
    {'\x1b', "\\x1b", "esc"},
    {'\x7f', "\\x7f", "delete"},
#endif /* SCM_USE_SRFI75_NAMED_CHARS */
    {0, NULL, NULL}
};

/*=======================================
  File Local Function Declarations
=======================================*/
static ScmObj SigScm_load_internal(const char *c_filename);
static char*  create_valid_path(const char *c_filename);
#if SCM_USE_NONSTD_FEATURES
static ScmObj create_loaded_str(ScmObj filename);
static int    file_existsp(const char *filepath);
#endif

/*=======================================
  Function Implementations
=======================================*/
void SigScm_set_lib_path(const char *path)
{
    lib_path = path;
}

#if SCM_USE_NONSTD_FEATURES
/* SIOD compatible */
ScmObj ScmOp_load_path(void)
{
    DECLARE_FUNCTION("load-path", ProcedureFixed0);
    return Scm_NewStringCopying(lib_path);
}
#endif

ScmObj Scm_MakeSharedFilePort(FILE *file, const char *aux_info,
                              enum ScmPortFlag flag)
{
    ScmBytePort *bport;

    /* GC safe */
    bport = ScmFilePort_new_shared(file, aux_info);
    return Scm_NewPort(ScmSingleByteCharPort_new(bport), flag);
}

/*=======================================
  R5RS : 6.6 Input and Output
=======================================*/
/*===========================================================================
  R5RS : 6.6 Input and Output : 6.6.1 Ports
===========================================================================*/
ScmObj ScmOp_call_with_input_file(ScmObj filepath, ScmObj proc)
{
    ScmObj port = SCM_FALSE;
    ScmObj ret  = SCM_FALSE;
    DECLARE_FUNCTION("call-with-input-file", ProcedureFixed2);

    ASSERT_STRINGP(filepath);
    ASSERT_PROCEDUREP(proc);

    port = ScmOp_open_input_file(filepath);

    ret = Scm_call(proc, LIST_1(port));

    ScmOp_close_input_port(port);

    return ret;
}

ScmObj ScmOp_call_with_output_file(ScmObj filepath, ScmObj proc)
{
    ScmObj port = SCM_FALSE;
    ScmObj ret  = SCM_FALSE;
    DECLARE_FUNCTION("call-with-output-file", ProcedureFixed2);

    ASSERT_STRINGP(filepath);
    ASSERT_PROCEDUREP(proc);

    port = ScmOp_open_output_file(filepath);

    ret = Scm_call(proc, LIST_1(port));

    ScmOp_close_output_port(port);

    return ret;
}

ScmObj ScmOp_input_portp(ScmObj port)
{
    DECLARE_FUNCTION("input-port?", ProcedureFixed1);
    ASSERT_PORTP(port);

    return (SCM_PORT_FLAG(port) & SCM_PORTFLAG_INPUT) ? SCM_TRUE : SCM_FALSE;
}

ScmObj ScmOp_output_portp(ScmObj port)
{
    DECLARE_FUNCTION("output-port?", ProcedureFixed1);
    ASSERT_PORTP(port);

    return (SCM_PORT_FLAG(port) & SCM_PORTFLAG_OUTPUT) ? SCM_TRUE : SCM_FALSE;
}

ScmObj ScmOp_current_input_port(void)
{
    DECLARE_FUNCTION("current-input-port", ProcedureFixed0);
    return scm_current_input_port;
}

ScmObj ScmOp_current_output_port(void)
{
    DECLARE_FUNCTION("current-output-port", ProcedureFixed0);
    return scm_current_output_port;
}

ScmObj ScmOp_with_input_from_file(ScmObj filepath, ScmObj thunk)
{
    ScmObj tmp_port = SCM_FALSE;
    ScmObj ret      = SCM_FALSE;
    DECLARE_FUNCTION("with-input-from-file", ProcedureFixed2);

    ASSERT_STRINGP(filepath);
    ASSERT_PROCEDUREP(thunk);

    tmp_port = scm_current_input_port;
    scm_current_input_port = ScmOp_open_input_file(filepath);

    ret = Scm_call(thunk, SCM_NULL);

    ScmOp_close_input_port(scm_current_input_port);
    scm_current_input_port = tmp_port;

    return ret;
}

ScmObj ScmOp_with_output_to_file(ScmObj filepath, ScmObj thunk)
{
    ScmObj tmp_port = SCM_FALSE;
    ScmObj ret      = SCM_FALSE;
    DECLARE_FUNCTION("with-output-to-file", ProcedureFixed2);

    ASSERT_STRINGP(filepath);
    ASSERT_PROCEDUREP(thunk);

    tmp_port = scm_current_output_port;
    scm_current_output_port = ScmOp_open_output_file(filepath);

    ret = Scm_call(thunk, SCM_NULL);

    ScmOp_close_output_port(scm_current_output_port);
    scm_current_output_port = tmp_port;

    return ret;
}

ScmObj ScmOp_open_input_file(ScmObj filepath)
{
    ScmBytePort *bport;
    DECLARE_FUNCTION("open-input-file", ProcedureFixed1);

    ASSERT_STRINGP(filepath);

    bport = ScmFilePort_open_input_file(SCM_STRING_STR(filepath));
    if (!bport)
        ERR_OBJ("cannot open file ", filepath);

    return Scm_NewPort(ScmSingleByteCharPort_new(bport), SCM_PORTFLAG_INPUT);
}

ScmObj ScmOp_open_output_file(ScmObj filepath)
{
    ScmBytePort *bport;
    DECLARE_FUNCTION("open-output-file", ProcedureFixed1);

    ASSERT_STRINGP(filepath);

    bport = ScmFilePort_open_output_file(SCM_STRING_STR(filepath));
    if (!bport)
        ERR_OBJ("cannot open file ", filepath);

    return Scm_NewPort(ScmSingleByteCharPort_new(bport), SCM_PORTFLAG_OUTPUT);
}

ScmObj ScmOp_close_input_port(ScmObj port)
{
    int flag;
    DECLARE_FUNCTION("close-input-port", ProcedureFixed1);

    ASSERT_PORTP(port);

    flag = SCM_PORT_FLAG(port) & ~SCM_PORTFLAG_LIVE_INPUT;
    SCM_PORT_SET_FLAG(port, flag);
    if (!(flag & SCM_PORTFLAG_ALIVENESS_MASK) && SCM_PORT_IMPL(port))
        SCM_PORT_CLOSE_IMPL(port);

    return SCM_UNDEF;
}

ScmObj ScmOp_close_output_port(ScmObj port)
{
    int flag;
    DECLARE_FUNCTION("close-output-port", ProcedureFixed1);

    ASSERT_PORTP(port);

    flag = SCM_PORT_FLAG(port) & ~SCM_PORTFLAG_LIVE_OUTPUT;
    SCM_PORT_SET_FLAG(port, flag);
    if (!(flag & SCM_PORTFLAG_ALIVENESS_MASK) && SCM_PORT_IMPL(port))
        SCM_PORT_CLOSE_IMPL(port);

    return SCM_UNDEF;
}

/*===========================================================================
  R5RS : 6.6 Input and Output : 6.6.2 Input
===========================================================================*/
#define PREPARE_PORT(port, args, default_port)                               \
    do {                                                                     \
        port = POP_ARG(args);                                                \
        if (!VALIDP(port))                                                   \
            port = default_port;                                             \
        ASSERT_PORTP(port);                                                  \
        ASSERT_NO_MORE_ARG(args);                                            \
    } while (/* CONSTCOND */ 0)


ScmObj ScmOp_read(ScmObj args)
{
    ScmObj port = SCM_INVALID;
    DECLARE_FUNCTION("read", ProcedureVariadic0);

    PREPARE_PORT(port, args, scm_current_input_port);
    return SigScm_Read(port);
}

ScmObj ScmOp_read_char(ScmObj args)
{
    ScmObj port = SCM_INVALID;
    /* FIXME: use int as char */
    int    ch;
    char   buf[2];
    DECLARE_FUNCTION("read-char", ProcedureVariadic0);

    PREPARE_PORT(port, args, scm_current_input_port);

    ch = SCM_PORT_GET_CHAR(port);
    if (ch == EOF)
        return SCM_EOF;

    buf[0] = ch;
    buf[1] = '\0';

    return Scm_NewChar(strdup(buf));
}

ScmObj ScmOp_peek_char(ScmObj args)
{
    ScmObj port = SCM_INVALID;
    /* FIXME: use int as char */
    int    ch;
    char   buf[2];
    DECLARE_FUNCTION("peek-char", ProcedureVariadic0);

    PREPARE_PORT(port, args, scm_current_input_port);

    ch = SCM_PORT_PEEK_CHAR(port);
    if (ch == EOF)
        return SCM_EOF;

    buf[0] = ch;
    buf[1] = '\0';
    return Scm_NewChar(strdup(buf));
}

ScmObj ScmOp_eof_objectp(ScmObj obj)
{
    DECLARE_FUNCTION("eof-object?", ProcedureFixed1);
    return (EOFP(obj)) ? SCM_TRUE : SCM_FALSE;
}

ScmObj ScmOp_char_readyp(ScmObj args)
{
    ScmObj port = SCM_INVALID;
    DECLARE_FUNCTION("char-ready?", ProcedureVariadic0);

    PREPARE_PORT(port, args, scm_current_input_port);

    return (SCM_PORT_CHAR_READYP(port))? SCM_TRUE : SCM_FALSE;
}

/*===========================================================================
  R5RS : 6.6 Input and Output : 6.6.3 Output
===========================================================================*/
ScmObj ScmOp_write(ScmObj obj, ScmObj args)
{
    ScmObj port = SCM_INVALID;
    DECLARE_FUNCTION("write", ProcedureVariadic1);

    PREPARE_PORT(port, args, scm_current_output_port);
    SigScm_WriteToPort(port, obj);
    return SCM_UNDEF;
}

ScmObj ScmOp_display(ScmObj obj, ScmObj args)
{
    ScmObj port = SCM_INVALID;
    DECLARE_FUNCTION("display", ProcedureVariadic1);

    PREPARE_PORT(port, args, scm_current_output_port);
    SigScm_DisplayToPort(port, obj);
    return SCM_UNDEF;
}

ScmObj ScmOp_newline(ScmObj args)
{
    ScmObj port = SCM_INVALID;
    DECLARE_FUNCTION("newline", ProcedureVariadic0);

    PREPARE_PORT(port, args, scm_current_output_port);
    SigScm_DisplayToPort(port, Scm_NewStringCopying("\n"));
    return SCM_UNDEF;
}

ScmObj ScmOp_write_char(ScmObj obj, ScmObj args)
{
    ScmObj port = SCM_INVALID;
    DECLARE_FUNCTION("write-char", ProcedureVariadic1);

    ASSERT_CHARP(obj);

    PREPARE_PORT(port, args, scm_current_output_port);
    SigScm_DisplayToPort(port, obj);
    return SCM_UNDEF;
}

/*===========================================================================
  R5RS : 6.6 Input and Output : 6.6.4 System Interface
===========================================================================*/
ScmObj SigScm_load(const char *c_filename)
{
#if !SCM_GCC4_READY_GC
    ScmObj stack_start = NULL;
#endif
    ScmObj succeeded   = SCM_FALSE;

#if SCM_GCC4_READY_GC
    SCM_GC_PROTECTED_CALL(succeeded, ScmObj, SigScm_load_internal, (c_filename));
#else
    /* start protecting stack */
    SigScm_GC_ProtectStack(&stack_start);

    succeeded = SigScm_load_internal(c_filename);

    /* now no need to protect stack */
    SigScm_GC_UnprotectStack(&stack_start);
#endif

    return succeeded;
}

static ScmObj SigScm_load_internal(const char *c_filename)
{
    ScmObj port         = SCM_FALSE;
    ScmObj s_expression = SCM_FALSE;
    ScmObj filepath     = SCM_FALSE;
    char  *c_filepath   = create_valid_path(c_filename);

    CDBG((SCM_DBG_FILE, "loading %s", c_filename));

    /* sanity check */
    if (!c_filepath)
        SigScm_Error("SigScm_load_internal : file \"%s\" not found",
                     c_filename);

    filepath = Scm_NewString(c_filepath);
    port = ScmOp_open_input_file(filepath);

    /* read & eval cycle */
    while (s_expression = SigScm_Read(port), !EOFP(s_expression)) {
        EVAL(s_expression, SCM_INTERACTION_ENV);
    }

    ScmOp_close_input_port(port);

    CDBG((SCM_DBG_FILE, "done."));

    return SCM_TRUE;
}

/* FIXME:
 * - Simplify
 * - Avoid using strcat() and strcpy() to increase security. Use strncat(),
 *   strncpy() or other safe functions instead
 */
/* TODO: reject relative paths to ensure security */
static char* create_valid_path(const char *filename)
{
    char *c_filename = strdup(filename);
    char *filepath   = NULL;

    /* construct filepath */
    if (lib_path) {
        /* try absolute path */
        if (file_existsp(c_filename))
            return c_filename;

        /* use lib_path */
        filepath = (char*)malloc(strlen(lib_path) + strlen(c_filename) + 2);
        strcpy(filepath, lib_path);
        strcat(filepath, "/");
        strcat(filepath, c_filename);
        if (file_existsp(filepath)) {
            free(c_filename);
            return filepath;
        }
    }

    /* clear */
    if (filepath)
        free(filepath);

    /* fallback */
    filepath = (char*)malloc(strlen(c_filename) + 1);
    strcpy(filepath, c_filename);
    if (file_existsp(filepath)) {
        free(c_filename);
        return filepath;
    }

    free(c_filename);
    free(filepath);
    return NULL;
}

ScmObj ScmOp_load(ScmObj filename)
{
    char *c_filename = SCM_STRING_STR(filename);
    DECLARE_FUNCTION("load", ProcedureFixed1);

    SigScm_load_internal(c_filename);

#if SCM_STRICT_R5RS
    return SCM_UNDEF;
#else
    return SCM_TRUE;
#endif
}

#if SCM_USE_NONSTD_FEATURES
/* FIXME: add ScmObj SigScm_require(const char *c_filename) */

ScmObj ScmOp_require(ScmObj filename)
{
    ScmObj loaded_str = SCM_FALSE;
#if SCM_COMPAT_SIOD
    ScmObj retsym     = SCM_FALSE;
#endif
    DECLARE_FUNCTION("require", ProcedureFixed1);

    ASSERT_STRINGP(filename);

    loaded_str = create_loaded_str(filename);
    if (FALSEP(ScmOp_providedp(loaded_str))) {
        ScmOp_load(filename);
        ScmOp_provide(loaded_str);
    }

#if SCM_COMPAT_SIOD
    retsym = Scm_Intern(SCM_STRING_STR(loaded_str));
    SCM_SYMBOL_SET_VCELL(retsym, SCM_TRUE);

    return retsym;
#else
    return SCM_TRUE;
#endif
}

static ScmObj create_loaded_str(ScmObj filename)
{
    char  *loaded_str = NULL;
    int    size = 0;

    /* generate loaded_str, contents is filename-loaded* */
    size = (strlen(SCM_STRING_STR(filename)) + strlen("*-loaded*") + 1);
    loaded_str = (char*)malloc(sizeof(char) * size);
    snprintf(loaded_str, size, "*%s-loaded*", SCM_STRING_STR(filename));

    return Scm_NewString(loaded_str);
}

/*
 * TODO: replace original specification with a SRFI standard or other de facto
 * standard
 */
ScmObj ScmOp_provide(ScmObj feature)
{
    DECLARE_FUNCTION("provide", ProcedureFixed1);

    ASSERT_STRINGP(feature);

    /* record to SigScm_features */
    SCM_SYMBOL_SET_VCELL(SigScm_features,
                         CONS(feature, SCM_SYMBOL_VCELL(SigScm_features)));

    return SCM_TRUE;
}

/*
 * TODO: replace original specification with a SRFI standard or other de facto
 * standard
 */
ScmObj ScmOp_providedp(ScmObj feature)
{
    ScmObj provided = SCM_FALSE;
    DECLARE_FUNCTION("provided?", ProcedureFixed1);

    ASSERT_STRINGP(feature);

    provided = ScmOp_member(feature, SCM_SYMBOL_VCELL(SigScm_features));

    return (NFALSEP(provided)) ? SCM_TRUE : SCM_FALSE;
}

/*
 * TODO: describe compatibility with de facto standard of other Scheme
 * implementations
 */
ScmObj ScmOp_file_existsp(ScmObj filepath)
{
    DECLARE_FUNCTION("file-exists?", ProcedureFixed1);

    ASSERT_STRINGP(filepath);

    return (file_existsp(SCM_STRING_STR(filepath))) ? SCM_TRUE : SCM_FALSE;
}

/* TODO: remove to ensure security */
ScmObj ScmOp_delete_file(ScmObj filepath)
{
    DECLARE_FUNCTION("delete-file", ProcedureFixed1);

    ASSERT_STRINGP(filepath);

    if (remove(SCM_STRING_STR(filepath)) == -1)
        ERR_OBJ("delete failed. file = ", filepath);

    return SCM_TRUE;
}

static int file_existsp(const char *c_filepath)
{
    FILE *f = fopen(c_filepath, "r");
    if (!f)
        return 0;

    fclose(f);
    return 1;
}
#endif /* SCM_USE_NONSTD_FEATURES */


/* FIXME: link conditionally with autoconf */
#include "sbcport.c"
#include "fileport.c"
