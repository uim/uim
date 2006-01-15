/*===========================================================================
 *  FileName : io.c
 *  About    : io related functions
 *
 *  Copyright (C) 2005-2006 Kazuki Ohta <mover AT hct.zaq.ne.jp>
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
#if SCM_USE_MULTIBYTE_CHAR
#include "mbcport.h"
#else /* SCM_USE_MULTIBYTE_CHAR */
#include "sbcport.h"
#endif /* SCM_USE_MULTIBYTE_CHAR */
#include "fileport.h"

/*=======================================
  File Local Struct Declarations
=======================================*/

/*=======================================
  File Local Macro Declarations
=======================================*/
#if SCM_USE_SRFI22
/* SRFI-22: The <script prelude> line may not be longer than 64 characters. */
#define SCRIPT_PRELUDE_MAXLEN 64
#define SCRIPT_PRELUDE_DELIM  " \t\n\r"
#endif

/*=======================================
  Variable Declarations
=======================================*/
ScmObj scm_in;   /* current-input-port */
ScmObj scm_out;  /* current-output-port */
ScmObj scm_err;  /* current error port */

const char *scm_lib_path = NULL;

/*=======================================
  File Local Function Declarations
=======================================*/
static void scm_load_internal(const char *filename);
static char *find_path(const char *c_filename);
static scm_bool file_existsp(const char *filepath);
#if SCM_USE_SRFI22
static void interpret_script_prelude(ScmObj port);
static char **parse_script_prelude(ScmObj port);
#endif

/*=======================================
  Function Implementations
=======================================*/
void
scm_init_io(void)
{
    scm_fileport_init();
#if SCM_USE_MULTIBYTE_CHAR
    scm_mbcport_init();
#else
    scm_sbcport_init();
#endif

    scm_gc_protect_with_init(&scm_in,
                             scm_make_shared_file_port(stdin, "stdin",
                                                       SCM_PORTFLAG_INPUT));
    scm_gc_protect_with_init(&scm_out,
                             scm_make_shared_file_port(stdout, "stdout",
                                                       SCM_PORTFLAG_OUTPUT));
    scm_gc_protect_with_init(&scm_err,
                             scm_make_shared_file_port(stderr, "stderr",
                                                       SCM_PORTFLAG_OUTPUT));
}

void
scm_set_lib_path(const char *path)
{
    scm_lib_path = path;
}

ScmObj
scm_prepare_port(ScmObj args, ScmObj default_port)
{
    ScmObj port;
    DECLARE_INTERNAL_FUNCTION("prepare_port");

    ASSERT_PROPER_ARG_LIST(args);

    if (NULLP(args)) {
        port = default_port;
    } else {
        port = POP(args);
        ASSERT_NO_MORE_ARG(args);
        ENSURE_PORT(port);
    }

    return port;
}

ScmCharPort *
scm_make_char_port(ScmBytePort *bport)
{
#if  SCM_USE_MULTIBYTE_CHAR
    return ScmMultiByteCharPort_new(bport, scm_current_char_codec);
#else
    return ScmSingleByteCharPort_new(bport);
#endif
}

ScmObj
scm_make_shared_file_port(FILE *file, const char *aux_info,
                          enum ScmPortFlag flag)
{
    ScmBytePort *bport;
    ScmCharPort *cport;

    /* GC safe */
    bport = ScmFilePort_new_shared(file, aux_info);
    cport = scm_make_char_port(bport);
    return MAKE_PORT(cport, flag);
}

int
scm_port_printf(ScmObj port, const char *fmt, ...)
{
    int ret;
    va_list args;

    va_start(args, fmt);
    ret = scm_port_vprintf(port, fmt, args);
    va_end(args);

    return ret;
}

int
scm_port_vprintf(ScmObj port, const char *fmt, va_list args)
{
    int ret;

    SCM_ENSURE_LIVE_PORT(port);
    ret = SCM_CHARPORT_VPRINTF(SCM_PORT_IMPL(port), fmt, args);
#if SCM_VOLATILE_OUTPUT
    scm_port_flush(port);
#endif

    return ret;
}

int
scm_port_newline(ScmObj port)
{
    int err;

    err = scm_port_puts(port, SCM_NEWLINE_STR);
    scm_port_flush(port);  /* required */

    return err;
}

int
scm_port_close(ScmObj port)
{
    int err;

    err = SCM_CHARPORT_CLOSE(SCM_PORT_IMPL(port));
    SCM_PORT_SET_IMPL(port, NULL);

    return err;
}

ScmCharCodec *
scm_port_codec(ScmObj port)
{
    SCM_ENSURE_LIVE_PORT(port);
    return SCM_CHARPORT_CODEC(SCM_PORT_IMPL(port));
}

char *
scm_port_inspect(ScmObj port)
{
    SCM_ENSURE_LIVE_PORT(port);
    return SCM_CHARPORT_INSPECT(SCM_PORT_IMPL(port));
}

int
scm_port_get_char(ScmObj port)
{
    SCM_ENSURE_LIVE_PORT(port);
    return SCM_CHARPORT_GET_CHAR(SCM_PORT_IMPL(port));
}

int
scm_port_peek_char(ScmObj port)
{
    SCM_ENSURE_LIVE_PORT(port);
    return SCM_CHARPORT_PEEK_CHAR(SCM_PORT_IMPL(port));
}

scm_bool
scm_port_char_readyp(ScmObj port)
{
    SCM_ENSURE_LIVE_PORT(port);
    return SCM_CHARPORT_CHAR_READYP(SCM_PORT_IMPL(port));
}

int
scm_port_puts(ScmObj port, const char *str)
{
    SCM_ENSURE_LIVE_PORT(port);
    return SCM_CHARPORT_PUTS(SCM_PORT_IMPL(port), str);
}

int
scm_port_put_char(ScmObj port, int ch)
{
    SCM_ENSURE_LIVE_PORT(port);
    return SCM_CHARPORT_PUT_CHAR(SCM_PORT_IMPL(port), ch);
}

int
scm_port_flush(ScmObj port)
{
    SCM_ENSURE_LIVE_PORT(port);
    return SCM_CHARPORT_FLUSH(SCM_PORT_IMPL(port));
}

/*=======================================
  R5RS : 6.6 Input and Output
=======================================*/
/*===========================================================================
  R5RS : 6.6 Input and Output : 6.6.1 Ports
===========================================================================*/
ScmObj
scm_p_call_with_input_file(ScmObj filepath, ScmObj proc)
{
    ScmObj port, ret;
    DECLARE_FUNCTION("call-with-input-file", procedure_fixed_2);

    ENSURE_STRING(filepath);
    ENSURE_PROCEDURE(proc);

    port = scm_p_open_input_file(filepath);

    ret = scm_call(proc, LIST_1(port));

    scm_p_close_input_port(port);

    return ret;
}

ScmObj
scm_p_call_with_output_file(ScmObj filepath, ScmObj proc)
{
    ScmObj port, ret;
    DECLARE_FUNCTION("call-with-output-file", procedure_fixed_2);

    ENSURE_STRING(filepath);
    ENSURE_PROCEDURE(proc);

    port = scm_p_open_output_file(filepath);

    ret = scm_call(proc, LIST_1(port));

    scm_p_close_output_port(port);

    return ret;
}

ScmObj
scm_p_input_portp(ScmObj port)
{
    DECLARE_FUNCTION("input-port?", procedure_fixed_1);

    ENSURE_PORT(port);

    return MAKE_BOOL(SCM_PORT_FLAG(port) & SCM_PORTFLAG_INPUT);
}

ScmObj
scm_p_output_portp(ScmObj port)
{
    DECLARE_FUNCTION("output-port?", procedure_fixed_1);

    ENSURE_PORT(port);

    return MAKE_BOOL(SCM_PORT_FLAG(port) & SCM_PORTFLAG_OUTPUT);
}

ScmObj
scm_p_current_input_port(void)
{
    DECLARE_FUNCTION("current-input-port", procedure_fixed_0);

    return scm_in;
}

ScmObj
scm_p_current_output_port(void)
{
    DECLARE_FUNCTION("current-output-port", procedure_fixed_0);

    return scm_out;
}

ScmObj
scm_p_with_input_from_file(ScmObj filepath, ScmObj thunk)
{
    ScmObj saved_port, ret;
    DECLARE_FUNCTION("with-input-from-file", procedure_fixed_2);

    ENSURE_STRING(filepath);
    ENSURE_PROCEDURE(thunk);

    saved_port = scm_in;
    scm_in = scm_p_open_input_file(filepath);

    ret = scm_call(thunk, SCM_NULL);

    scm_p_close_input_port(scm_in);
    scm_in = saved_port;

    return ret;
}

ScmObj
scm_p_with_output_to_file(ScmObj filepath, ScmObj thunk)
{
    ScmObj saved_port, ret;
    DECLARE_FUNCTION("with-output-to-file", procedure_fixed_2);

    ENSURE_STRING(filepath);
    ENSURE_PROCEDURE(thunk);

    saved_port = scm_out;
    scm_out = scm_p_open_output_file(filepath);

    ret = scm_call(thunk, SCM_NULL);

    scm_p_close_output_port(scm_out);
    scm_out = saved_port;

    return ret;
}

ScmObj
scm_p_open_input_file(ScmObj filepath)
{
    ScmBytePort *bport;
    ScmCharPort *cport;
    DECLARE_FUNCTION("open-input-file", procedure_fixed_1);

    ENSURE_STRING(filepath);

    bport = ScmFilePort_open_input_file(SCM_STRING_STR(filepath));
    if (!bport)
        ERR_OBJ("cannot open file ", filepath);
    cport = scm_make_char_port(bport);

    return MAKE_PORT(cport, SCM_PORTFLAG_INPUT);
}

ScmObj
scm_p_open_output_file(ScmObj filepath)
{
    ScmBytePort *bport;
    ScmCharPort *cport;
    DECLARE_FUNCTION("open-output-file", procedure_fixed_1);

    ENSURE_STRING(filepath);

    bport = ScmFilePort_open_output_file(SCM_STRING_STR(filepath));
    if (!bport)
        ERR_OBJ("cannot open file ", filepath);
    cport = scm_make_char_port(bport);

    return MAKE_PORT(cport, SCM_PORTFLAG_OUTPUT);
}

ScmObj
scm_p_close_input_port(ScmObj port)
{
    int flag;
    DECLARE_FUNCTION("close-input-port", procedure_fixed_1);

    ENSURE_PORT(port);

    flag = SCM_PORT_FLAG(port) & ~SCM_PORTFLAG_LIVE_INPUT;
    SCM_PORT_SET_FLAG(port, flag);
    if (!(flag & SCM_PORTFLAG_ALIVENESS_MASK) && SCM_PORT_IMPL(port))
        scm_port_close(port);

    return SCM_UNDEF;
}

ScmObj
scm_p_close_output_port(ScmObj port)
{
    int flag;
    DECLARE_FUNCTION("close-output-port", procedure_fixed_1);

    ENSURE_PORT(port);

    flag = SCM_PORT_FLAG(port) & ~SCM_PORTFLAG_LIVE_OUTPUT;
    SCM_PORT_SET_FLAG(port, flag);
    if (!(flag & SCM_PORTFLAG_ALIVENESS_MASK) && SCM_PORT_IMPL(port))
        scm_port_close(port);

    return SCM_UNDEF;
}

/*===========================================================================
  R5RS : 6.6 Input and Output : 6.6.2 Input
===========================================================================*/
ScmObj
scm_p_read(ScmObj args)
{
    ScmObj port;
    DECLARE_FUNCTION("read", procedure_variadic_0);

    port = scm_prepare_port(args, scm_in);
    return scm_read(port);
}

ScmObj
scm_p_read_char(ScmObj args)
{
    ScmObj port;
    int ch;
    DECLARE_FUNCTION("read-char", procedure_variadic_0);

    port = scm_prepare_port(args, scm_in);

    ch = scm_port_get_char(port);
    if (ch == EOF)
        return SCM_EOF;

    return MAKE_CHAR(ch);
}

ScmObj
scm_p_peek_char(ScmObj args)
{
    ScmObj port;
    int ch;
    DECLARE_FUNCTION("peek-char", procedure_variadic_0);

    port = scm_prepare_port(args, scm_in);

    ch = scm_port_peek_char(port);
    if (ch == EOF)
        return SCM_EOF;

    return MAKE_CHAR(ch);
}

ScmObj
scm_p_eof_objectp(ScmObj obj)
{
    DECLARE_FUNCTION("eof-object?", procedure_fixed_1);

    return MAKE_BOOL(EOFP(obj));
}

ScmObj
scm_p_char_readyp(ScmObj args)
{
    ScmObj port;
    scm_bool res;
    DECLARE_FUNCTION("char-ready?", procedure_variadic_0);

    port = scm_prepare_port(args, scm_in);
    res = scm_port_char_readyp(port);

    return MAKE_BOOL(res);
}

/*===========================================================================
  R5RS : 6.6 Input and Output : 6.6.3 Output
===========================================================================*/
ScmObj
scm_p_write(ScmObj obj, ScmObj args)
{
    ScmObj port;
    DECLARE_FUNCTION("write", procedure_variadic_1);

    port = scm_prepare_port(args, scm_out);
    scm_write_to_port(port, obj);
    return SCM_UNDEF;
}

ScmObj
scm_p_display(ScmObj obj, ScmObj args)
{
    ScmObj port;
    DECLARE_FUNCTION("display", procedure_variadic_1);

    port = scm_prepare_port(args, scm_out);
    scm_display_to_port(port, obj);
    return SCM_UNDEF;
}

ScmObj
scm_p_newline(ScmObj args)
{
    ScmObj port;
    DECLARE_FUNCTION("newline", procedure_variadic_0);

    port = scm_prepare_port(args, scm_out);
    scm_port_newline(port);
    return SCM_UNDEF;
}

ScmObj
scm_p_write_char(ScmObj obj, ScmObj args)
{
    ScmObj port;
    DECLARE_FUNCTION("write-char", procedure_variadic_1);

    ENSURE_CHAR(obj);

    port = scm_prepare_port(args, scm_out);
    scm_display_to_port(port, obj);
    return SCM_UNDEF;
}

/*===========================================================================
  R5RS : 6.6 Input and Output : 6.6.4 System Interface
===========================================================================*/
void
scm_load(const char *filename)
{
#if !SCM_GCC4_READY_GC
    ScmObj stack_start;
#endif

#if SCM_GCC4_READY_GC
    SCM_GC_PROTECTED_CALL_VOID(scm_load_internal, (filename));
#else
    scm_gc_protect_stack(&stack_start);

    scm_load_internal(filename);

    scm_gc_unprotect_stack(&stack_start);
#endif
}

static void
scm_load_internal(const char *filename)
{
    ScmObj path, port, sexp;
    char *c_path;
    ScmCharCodec *saved_codec;

    CDBG((SCM_DBG_FILE, "loading %s", filename));

    c_path = find_path(filename);
    if (!c_path)
        ERR("scm_load_internal: file \"%s\" not found", filename);

    path = MAKE_IMMUTABLE_STRING(c_path, STRLEN_UNKNOWN);
    port = scm_p_open_input_file(path);

    saved_codec = scm_current_char_codec;
#if SCM_USE_SRFI22
    if (scm_port_peek_char(port) == '#')
        interpret_script_prelude(port);
#endif

    /* read & eval cycle */
    while (sexp = scm_read(port), !EOFP(sexp))
        EVAL(sexp, SCM_INTERACTION_ENV);

    scm_p_close_input_port(port);
    scm_current_char_codec = saved_codec;

    CDBG((SCM_DBG_FILE, "done."));
}

/* FIXME: reject relative paths to ensure security */
static char *
find_path(const char *filename)
{
    char *path;
    int lib_path_len, filename_len, path_len;

    SCM_ASSERT(filename);

    /* try absolute and relative path */
    if (file_existsp(filename))
        return scm_strdup(filename);

    /* try under scm_lib_path */
    if (scm_lib_path) {
        lib_path_len = scm_lib_path ? strlen(scm_lib_path) : 0;
        filename_len = strlen(filename);
        path_len = lib_path_len + sizeof((char)'/') + filename_len + sizeof("");

        path = scm_malloc(path_len);
        snprintf(path, path_len, "%s/%s", scm_lib_path, filename);
        if (file_existsp(path))
            return path;
        free(path);
    }

    return NULL;
}

static scm_bool
file_existsp(const char *c_filepath)
{
    FILE *f;

    f = fopen(c_filepath, "r");
    if (f) {
        fclose(f);
        return scm_true;
    } else {
        return scm_false;
    }
}

ScmObj
scm_p_load(ScmObj filename)
{
    DECLARE_FUNCTION("load", procedure_fixed_1);

    ENSURE_STRING(filename);

    scm_load_internal(SCM_STRING_STR(filename));

    return SCM_UNDEF;
}

#if SCM_USE_SRFI22
static void
interpret_script_prelude(ScmObj port)
{
    char **argv;

    argv = parse_script_prelude(port);
    scm_interpret_argv(argv);
#if SCM_USE_MULTIBYTE_CHAR
    if (SCM_CHARPORT_DYNAMIC_CAST(ScmMultiByteCharPort, SCM_PORT_IMPL(port))) {
        ScmMultiByteCharPort_set_codec(SCM_PORT_IMPL(port),
                                       scm_current_char_codec);
    }
#endif
    scm_free_argv(argv);
}

static char **
parse_script_prelude(ScmObj port)
{
    int argc, c, len, line_len;
    char **argv, *arg, *p, line[SCRIPT_PRELUDE_MAXLEN];
    DECLARE_INTERNAL_FUNCTION("parse_script_prelude");

    for (p = line; p < &line[SCRIPT_PRELUDE_MAXLEN]; p++) {
        c = scm_port_get_char(port);
        if (!isascii(c))
            ERR("non-ASCII char appeared in UNIX script prelude");
        if (c == SCM_NEWLINE_STR[0]) {
            *p = '\0';
            break;
        }
        *p = c;
    }
    if (*p)
        ERR("too long UNIX script prelude (max 64)");
    line_len = p - line;

    if (line[0] != '#' || line[1] != '!') {
        ERR("Invalid UNIX script prelude");
    }
#if 1
    /* strict check */
    if (line[2] != ' ') {
        ERR("Invalid UNIX script prelude: "
            "SRFI-22 requires a space after hash-bang sequence");
    }
#endif

    argv = scm_malloc(sizeof(char *));
    argv[0] = NULL;
    argc = 0;
    for (p = &line[3]; p < &line[line_len]; p += len + 1) {
        p += strspn(p, SCRIPT_PRELUDE_DELIM);
        len = strcspn(p, SCRIPT_PRELUDE_DELIM);
        if (!len)
            break;
        p[len] = '\0';
        arg = scm_strdup(p);
        argv[argc] = arg;
        argv = scm_realloc(argv, sizeof(char *) * (++argc + 1));
        argv[argc] = NULL;
    }

    return argv;
}
#endif

/* FIXME: link conditionally with autoconf */
#if SCM_USE_MULTIBYTE_CHAR
#include "mbcport.c"
#else /* SCM_USE_MULTIBYTE_CHAR */
#include "sbcport.c"
#endif /* SCM_USE_MULTIBYTE_CHAR */
