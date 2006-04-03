/*===========================================================================
 *  FileName : port.c
 *  About    : R5RS ports
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
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS
 *  IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
 *  THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 *  PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR
 *  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 *  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 *  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
 *  OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 *  WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
 *  OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
 *  ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
===========================================================================*/

#include "config.h"

/*=======================================
  System Include
=======================================*/
#include <stddef.h>
#include <stdio.h>

/*=======================================
  Local Include
=======================================*/
#include "sigscheme.h"
#include "sigschemeinternal.h"
#if SCM_USE_MULTIBYTE_CHAR
#include "scmport-mbchar.h"
#else /* SCM_USE_MULTIBYTE_CHAR */
#include "scmport-sbchar.h"
#endif /* SCM_USE_MULTIBYTE_CHAR */
#include "scmport-file.h"

/*=======================================
  File Local Struct Declarations
=======================================*/

/*=======================================
  File Local Macro Declarations
=======================================*/

/*=======================================
  Variable Declarations
=======================================*/
SCM_DEFINE_EXPORTED_VARS(port);

#if (SCM_USE_READER || SCM_USE_WRITER)
const ScmSpecialCharInfo scm_special_char_table[] = {
    /* printable characters */
    {'\"',   "\\\"",  "\""},         /* 34, R5RS */
    {'\\',   "\\\\",  "\\"},         /* 92, R5RS */
    {' ',    " ",     "space"},      /* 32, R5RS */
#if SCM_USE_SRFI75
    {'|',    "\\|",   "|"},
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
#endif /* (SCM_USE_READER || SCM_USE_WRITER) */

/*=======================================
  File Local Function Declarations
=======================================*/

/*=======================================
  Function Implementations
=======================================*/
void
scm_init_port(void)
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
scm_port_put_char(ScmObj port, scm_ichar_t ch)
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
    scm_int_t flag;
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
    scm_int_t flag;
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
/* scm_p_read() is separated into read.c */

ScmObj
scm_p_read_char(ScmObj args)
{
    ScmObj port;
    scm_ichar_t ch;
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
    scm_ichar_t ch;
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
    scm_bool ret;
    DECLARE_FUNCTION("char-ready?", procedure_variadic_0);

    port = scm_prepare_port(args, scm_in);
    ret = scm_port_char_readyp(port);

    return MAKE_BOOL(ret);
}

/*===========================================================================
  R5RS : 6.6 Input and Output : 6.6.3 Output
===========================================================================*/
/* scm_p_write() and scm_p_display() are separated into write.c */

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
    scm_port_put_char(port, SCM_CHAR_VALUE(obj));
    return SCM_UNDEF;
}
