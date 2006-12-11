/*===========================================================================
 *  Filename : module-srfi6.c
 *  About    : SRFI-6 Basic String Ports
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

#include <config.h>

#include <stdlib.h>

#include "sigscheme.h"
#include "sigschemeinternal.h"
#include "scmport-config.h"
#include "scmport.h"
#include "scmport-str.h"

/*=======================================
  File Local Macro Definitions
=======================================*/

/*=======================================
  File Local Type Definitions
=======================================*/

/*=======================================
  Variable Definitions
=======================================*/
#include "functable-srfi6.c"

/*=======================================
  File Local Function Declarations
=======================================*/
static void istrport_finalize(char **str, scm_bool ownership, void **opaque);

/*=======================================
  Function Definitions
=======================================*/
SCM_EXPORT void
scm_initialize_srfi6(void)
{
    scm_strport_init();

    scm_register_funcs(scm_functable_srfi6);
}

static void
istrport_finalize(char **str, scm_bool ownership, void **opaque)
{
    SCM_ASSERT(!ownership);

    scm_gc_unprotect((ScmObj *)opaque);
}

SCM_EXPORT ScmObj
scm_p_srfi6_open_input_string(ScmObj str)
{
    ScmObj *hold_str;
    ScmBytePort *bport;
    ScmCharPort *cport;
    DECLARE_FUNCTION("open-input-string", procedure_fixed_1);

    ENSURE_STRING(str);

    bport = ScmInputStrPort_new_const(SCM_STRING_STR(str), istrport_finalize);
    hold_str = (ScmObj *)ScmInputStrPort_ref_opaque(bport);
    scm_gc_protect_with_init(hold_str, str);
    cport = scm_make_char_port(bport);
    return MAKE_PORT(cport, SCM_PORTFLAG_INPUT);
}

SCM_EXPORT ScmObj
scm_p_srfi6_open_output_string(void)
{
    ScmBytePort *bport;
    ScmCharPort *cport;
    DECLARE_FUNCTION("open-output-string", procedure_fixed_0);

    bport = ScmOutputStrPort_new(NULL);
    cport = scm_make_char_port(bport);
    return MAKE_PORT(cport, SCM_PORTFLAG_OUTPUT);
}

SCM_EXPORT ScmObj
scm_p_srfi6_get_output_string(ScmObj port)
{
    ScmBaseCharPort *cport;
    const char *str;
    char *new_str;
    scm_int_t mb_len;
#if SCM_USE_NULL_CAPABLE_STRING
    size_t size;
#endif
    DECLARE_FUNCTION("get-output-string", procedure_fixed_1);

    ENSURE_PORT(port);

    SCM_ENSURE_LIVE_PORT(port);
    cport = SCM_CHARPORT_DYNAMIC_CAST(ScmBaseCharPort, SCM_PORT_IMPL(port));

    str = ScmOutputStrPort_str(cport->bport);
    /* FIXME: incorrect length for null-capable string */
    mb_len = scm_mb_bare_c_strlen(scm_port_codec(port), str);
#if SCM_USE_NULL_CAPABLE_STRING
    size = ScmOutputStrPort_c_strlen(cport->bport) + sizeof("");
    new_str = scm_malloc(size);
    memcpy(new_str, str, size);
#else
    new_str = scm_strdup(str);
#endif

    return MAKE_STRING(new_str, mb_len);
}
