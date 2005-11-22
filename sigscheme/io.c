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

const char *scm_lib_path = NULL;

const ScmSpecialCharInfo Scm_special_char_table[] = {
    /* printable characters */
    {'\"',   "\\\"",  "\""},         /* 34, R5RS */
    {'\\',   "\\\\",  "\\"},         /* 92, R5RS */
    {' ',    " ",     "space"},      /* 32, R5RS */
#if 0
    /* to avoid portability problem, we should not support #\Space and so on */
    {' ',    " ",     "Space"},
#endif

    /* control characters */
    {'\n',   "\\n",   "newline"},    /*  10, R5RS */
#if SCM_USE_SRFI75_NAMED_CHARS
    {'\0',   "\\x00", "nul"},        /*   0 */
    {'\a',   "\\a",   "alarm"},      /*   7 */
    {'\b',   "\\b",   "backspace"},  /*   8 */
    {'\t',   "\\t",   "tab"},        /*   9 */
    {'\n',   "\\n",   "linefeed"},   /*  10 */
    {'\v',   "\\v",   "vtab"},       /*  11 */
    {'\f',   "\\f",   "page"},       /*  12 */
    {'\r',   "\\r",   "return"},     /*  13 */
    {0x1b,   "\\x1b", "esc"},        /*  27 */
    {0x7f,   "\\x7f", "delete"},     /* 127 */
#endif /* SCM_USE_SRFI75_NAMED_CHARS */
    {0, NULL, NULL}
};

/*=======================================
  File Local Function Declarations
=======================================*/
static ScmObj SigScm_load_internal(const char *c_filename);
static char*  create_valid_path(const char *c_filename);
static int    file_existsp(const char *filepath);

/*=======================================
  Function Implementations
=======================================*/
void SigScm_set_lib_path(const char *path)
{
    scm_lib_path = path;
}

ScmObj Scm_MakeSharedFilePort(FILE *file, const char *aux_info,
                              enum ScmPortFlag flag)
{
    ScmBytePort *bport;

    /* GC safe */
    bport = ScmFilePort_new_shared(file, aux_info);
    return Scm_NewPort(ScmSingleByteCharPort_new(bport), flag);
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
    SigScm_PortNewline(port);
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

/* TODO: reject relative paths to ensure security */
static char* create_valid_path(const char *filename)
{
    char *filepath   = NULL;
    int lib_path_len = 0;
    int filename_len = 0;

    /* sanity check */
    SCM_ASSERT(filename);

    lib_path_len = scm_lib_path ? strlen(scm_lib_path) : 0;
    filename_len = strlen(filename);

    /* try absolute and relative path */
    if (file_existsp(filename))
        return strdup(filename);

    /* try under scm_lib_path */
    if (scm_lib_path) {
        filepath = (char*)malloc(lib_path_len + 1 + filename_len + 1);
        snprintf(filepath,
                 lib_path_len + 1 + filename_len + 1,
                 "%s/%s",
                 scm_lib_path,
                 filename);
        if (file_existsp(filepath))
            return filepath;
        free(filepath);
    }

    return NULL;
}

static int file_existsp(const char *c_filepath)
{
    FILE *f = fopen(c_filepath, "r");
    if (!f)
        return 0;

    fclose(f);
    return 1;
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

/* FIXME: link conditionally with autoconf */
#include "sbcport.c"
#include "fileport.c"
