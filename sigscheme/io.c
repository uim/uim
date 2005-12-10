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
ScmObj scm_current_input_port   = NULL;
ScmObj scm_current_output_port  = NULL;
ScmObj scm_current_error_port  = NULL;

const char *scm_lib_path = NULL;

/*=======================================
  File Local Function Declarations
=======================================*/
static ScmObj SigScm_load_internal(const char *c_filename);
static char *find_path(const char *c_filename);
static int    file_existsp(const char *filepath);
#if SCM_USE_SRFI22
static void interpret_script_prelude(ScmObj port);
static char **parse_script_prelude(ScmObj port);
#endif

/*=======================================
  Function Implementations
=======================================*/
void Scm_InitIO(void)
{
    Scm_fileport_init();
#if SCM_USE_MULTIBYTE_CHAR
    Scm_mbcport_init();
#else
    Scm_sbcport_init();
#endif

    scm_current_input_port  = Scm_MakeSharedFilePort(stdin, "stdin",
                                                     SCM_PORTFLAG_INPUT);
    scm_current_output_port = Scm_MakeSharedFilePort(stdout, "stdout",
                                                     SCM_PORTFLAG_OUTPUT);
    scm_current_error_port  = Scm_MakeSharedFilePort(stderr, "stderr",
                                                     SCM_PORTFLAG_OUTPUT);
    SigScm_GC_Protect(&scm_current_input_port);
    SigScm_GC_Protect(&scm_current_output_port);
    SigScm_GC_Protect(&scm_current_error_port);
}

void SigScm_set_lib_path(const char *path)
{
    scm_lib_path = path;
}

ScmCharPort *Scm_NewCharPort(ScmBytePort *bport)
{
#if  SCM_USE_MULTIBYTE_CHAR
    return ScmMultiByteCharPort_new(bport, Scm_current_char_codec);
#else
    return ScmSingleByteCharPort_new(bport);
#endif
}

ScmObj Scm_MakeSharedFilePort(FILE *file, const char *aux_info,
                              enum ScmPortFlag flag)
{
    ScmBytePort *bport;

    /* GC safe */
    bport = ScmFilePort_new_shared(file, aux_info);
    return Scm_NewPort(Scm_NewCharPort(bport), flag);
}

void SigScm_PortPrintf(ScmObj port, const char *fmt, ...)
{
    va_list args;

    va_start(args, fmt);
    SigScm_VPortPrintf(port, fmt, args);
    va_end(args);
}

void SigScm_VPortPrintf(ScmObj port, const char *fmt, va_list args)
{
    SCM_PORT_VPRINTF(port, fmt, args);
#if SCM_VOLATILE_OUTPUT
    SCM_PORT_FLUSH(port);
#endif
}

void SigScm_PortNewline(ScmObj port)
{
    SCM_PORT_PUTS(port, SCM_NEWLINE_STR);
    SCM_PORT_FLUSH(port);  /* required */
}

void SigScm_ErrorPrintf(const char *fmt, ...)
{
    va_list args;

    va_start(args, fmt);
    SigScm_VErrorPrintf(fmt, args);
    va_end(args);
}

void SigScm_VErrorPrintf(const char *fmt, va_list args)
{
    SigScm_VPortPrintf(scm_current_error_port, fmt, args);
}

void SigScm_ErrorNewline(void)
{
    SigScm_PortNewline(scm_current_error_port);
}

/*=======================================
  R5RS : 6.6 Input and Output
=======================================*/
/*===========================================================================
  R5RS : 6.6 Input and Output : 6.6.1 Ports
===========================================================================*/
ScmObj ScmOp_call_with_input_file(ScmObj filepath, ScmObj proc)
{
    ScmObj port, ret;
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
    ScmObj port, ret;
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
    ScmObj saved_port, ret;
    DECLARE_FUNCTION("with-input-from-file", ProcedureFixed2);

    ASSERT_STRINGP(filepath);
    ASSERT_PROCEDUREP(thunk);

    saved_port = scm_current_input_port;
    scm_current_input_port = ScmOp_open_input_file(filepath);

    ret = Scm_call(thunk, SCM_NULL);

    ScmOp_close_input_port(scm_current_input_port);
    scm_current_input_port = saved_port;

    return ret;
}

ScmObj ScmOp_with_output_to_file(ScmObj filepath, ScmObj thunk)
{
    ScmObj saved_port, ret;
    DECLARE_FUNCTION("with-output-to-file", ProcedureFixed2);

    ASSERT_STRINGP(filepath);
    ASSERT_PROCEDUREP(thunk);

    saved_port = scm_current_output_port;
    scm_current_output_port = ScmOp_open_output_file(filepath);

    ret = Scm_call(thunk, SCM_NULL);

    ScmOp_close_output_port(scm_current_output_port);
    scm_current_output_port = saved_port;

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

    return Scm_NewPort(Scm_NewCharPort(bport), SCM_PORTFLAG_INPUT);
}

ScmObj ScmOp_open_output_file(ScmObj filepath)
{
    ScmBytePort *bport;
    DECLARE_FUNCTION("open-output-file", ProcedureFixed1);

    ASSERT_STRINGP(filepath);

    bport = ScmFilePort_open_output_file(SCM_STRING_STR(filepath));
    if (!bport)
        ERR_OBJ("cannot open file ", filepath);

    return Scm_NewPort(Scm_NewCharPort(bport), SCM_PORTFLAG_OUTPUT);
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
    ScmObj port;
    DECLARE_FUNCTION("read", ProcedureVariadic0);

    PREPARE_PORT(port, args, scm_current_input_port);
    return SigScm_Read(port);
}

ScmObj ScmOp_read_char(ScmObj args)
{
    ScmObj port;
    int ch;
    DECLARE_FUNCTION("read-char", ProcedureVariadic0);

    PREPARE_PORT(port, args, scm_current_input_port);

    ch = SCM_PORT_GET_CHAR(port);
    if (ch == EOF)
        return SCM_EOF;

    return Scm_NewChar(ch);
}

ScmObj ScmOp_peek_char(ScmObj args)
{
    ScmObj port;
    int ch;
    DECLARE_FUNCTION("peek-char", ProcedureVariadic0);

    PREPARE_PORT(port, args, scm_current_input_port);

    ch = SCM_PORT_PEEK_CHAR(port);
    if (ch == EOF)
        return SCM_EOF;

    return Scm_NewChar(ch);
}

ScmObj ScmOp_eof_objectp(ScmObj obj)
{
    DECLARE_FUNCTION("eof-object?", ProcedureFixed1);

    return (EOFP(obj)) ? SCM_TRUE : SCM_FALSE;
}

ScmObj ScmOp_char_readyp(ScmObj args)
{
    ScmObj port;
    DECLARE_FUNCTION("char-ready?", ProcedureVariadic0);

    PREPARE_PORT(port, args, scm_current_input_port);

    return (SCM_PORT_CHAR_READYP(port))? SCM_TRUE : SCM_FALSE;
}

/*===========================================================================
  R5RS : 6.6 Input and Output : 6.6.3 Output
===========================================================================*/
ScmObj ScmOp_write(ScmObj obj, ScmObj args)
{
    ScmObj port;
    DECLARE_FUNCTION("write", ProcedureVariadic1);

    PREPARE_PORT(port, args, scm_current_output_port);
    SigScm_WriteToPort(port, obj);
    return SCM_UNDEF;
}

ScmObj ScmOp_display(ScmObj obj, ScmObj args)
{
    ScmObj port;
    DECLARE_FUNCTION("display", ProcedureVariadic1);

    PREPARE_PORT(port, args, scm_current_output_port);
    SigScm_DisplayToPort(port, obj);
    return SCM_UNDEF;
}

ScmObj ScmOp_newline(ScmObj args)
{
    ScmObj port;
    DECLARE_FUNCTION("newline", ProcedureVariadic0);

    PREPARE_PORT(port, args, scm_current_output_port);
    SigScm_PortNewline(port);
    return SCM_UNDEF;
}

ScmObj ScmOp_write_char(ScmObj obj, ScmObj args)
{
    ScmObj port;
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
    ScmObj stack_start;
#endif
    ScmObj succeeded;

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
    ScmObj path, port, sexp;
    char *c_path;
    ScmCharCodec *saved_codec;

    CDBG((SCM_DBG_FILE, "loading %s", c_filename));

    c_path = find_path(c_filename);
    if (!c_path)
        ERR("SigScm_load_internal: file \"%s\" not found", c_filename);

    path = Scm_NewImmutableString(c_path);
    port = ScmOp_open_input_file(path);

    saved_codec = Scm_current_char_codec;
#if SCM_USE_SRFI22
    if (SCM_PORT_PEEK_CHAR(port) == '#')
        interpret_script_prelude(port);
#endif

    /* read & eval cycle */
    while (sexp = SigScm_Read(port), !EOFP(sexp))
        EVAL(sexp, SCM_INTERACTION_ENV);

    ScmOp_close_input_port(port);
    Scm_current_char_codec = saved_codec;

    CDBG((SCM_DBG_FILE, "done."));

    return SCM_TRUE;
}

/* FIXME: reject relative paths to ensure security */
static char *find_path(const char *filename)
{
    char *path;
    int lib_path_len, filename_len, path_len;

    SCM_ASSERT(filename);

    /* try absolute and relative path */
    if (file_existsp(filename))
        return strdup(filename);

    /* try under scm_lib_path */
    if (scm_lib_path) {
        lib_path_len = scm_lib_path ? strlen(scm_lib_path) : 0;
        filename_len = strlen(filename);
        path_len = lib_path_len + sizeof((char)'/') + filename_len + sizeof((char)'\0');

        path = malloc(path_len);
        snprintf(path, path_len, "%s/%s", scm_lib_path, filename);
        if (file_existsp(path))
            return path;
        free(path);
    }

    return NULL;
}

static int file_existsp(const char *c_filepath)
{
    FILE *f;

    f = fopen(c_filepath, "r");
    if (f) {
        fclose(f);
        return 1;
    } else {
        return 0;
    }
}

ScmObj ScmOp_load(ScmObj filename)
{
    DECLARE_FUNCTION("load", ProcedureFixed1);

    ASSERT_STRINGP(filename);

    SigScm_load_internal(SCM_STRING_STR(filename));

    return SCM_UNDEF;
}

#if SCM_USE_SRFI22
static void interpret_script_prelude(ScmObj port)
{
    char **argv;

    argv = parse_script_prelude(port);
    Scm_InterpretArgv(argv);
#if SCM_USE_MULTIBYTE_CHAR
    if (SCM_CHARPORT_DYNAMIC_CAST(ScmMultiByteCharPort, SCM_PORT_IMPL(port))) {
        ScmMultiByteCharPort_set_codec(SCM_PORT_IMPL(port),
                                       Scm_current_char_codec);
    }
#endif
    Scm_FreeArgv(argv);
}

static char **parse_script_prelude(ScmObj port)
{
    int argc, c, len;
    char **argv, *arg, *p, line[SCRIPT_PRELUDE_MAXLEN];
    DECLARE_INTERNAL_FUNCTION("parse_script_prelude");

    for (p = line; p < &line[SCRIPT_PRELUDE_MAXLEN]; p++) {
        c = SCM_PORT_GET_CHAR(port);
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

    argv = malloc(sizeof(char *));
    argc = 0;
    for (p = &line[3]; p < &line[SCRIPT_PRELUDE_MAXLEN]; p += len + 1) {
        p += strspn(p, SCRIPT_PRELUDE_DELIM);
        len = strcspn(p, SCRIPT_PRELUDE_DELIM);
        if (len) {
            p[len] = '\0';
            arg = strdup(p);
            argv[argc] = arg;
            argv = realloc(argv, sizeof(char *) * (++argc + 1));
            argv[argc] = NULL;
        }
    }         
    argv[argc] = NULL;

    return argv;
}
#endif

/* FIXME: link conditionally with autoconf */
#if SCM_USE_MULTIBYTE_CHAR
#include "mbcport.c"
#else /* SCM_USE_MULTIBYTE_CHAR */
#include "sbcport.c"
#endif /* SCM_USE_MULTIBYTE_CHAR */
