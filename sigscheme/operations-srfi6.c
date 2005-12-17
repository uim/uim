/*===========================================================================
 *  FileName : operations-srfi6.c
 *  About    : Basic String Ports
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

/*=======================================
  Local Include
=======================================*/
#include "sigscheme.h"
#include "sigschemeinternal.h"
#include "baseport.h"
#include "strport.h"

/*=======================================
  File Local Struct Declarations
=======================================*/

/*=======================================
  File Local Macro Declarations
=======================================*/

/*=======================================
  Variable Declarations
=======================================*/

/*=======================================
  File Local Function Declarations
=======================================*/
static void istrport_finalize(char **str, int ownership, void **opaque);

/*=======================================
  Function Implementations
=======================================*/
void
scm_initialize_srfi6(void)
{
    scm_strport_init();

    /*=======================================================================
      SRFI-6 Procedures
    =======================================================================*/
    REGISTER_FUNC_TABLE(srfi6_func_info_table);
}

static void
istrport_finalize(char **str, int ownership, void **opaque)
{
    scm_gc_unprotect((ScmObj *)opaque);
}

ScmObj
scm_p_srfi6_open_input_string(ScmObj str)
{
    ScmObj      *hold_str;
    ScmBytePort *bport;
    DECLARE_FUNCTION("open-input-string", procedure_fixed_1);

    ASSERT_STRINGP(str);

    bport = ScmInputStrPort_new_const(SCM_STRING_STR(str), istrport_finalize);
    hold_str = (ScmObj *)ScmInputStrPort_ref_opaque(bport);
    *hold_str = str;
    scm_gc_protect(hold_str);
    return scm_make_port(scm_make_char_port(bport), SCM_PORTFLAG_INPUT);
}

ScmObj
scm_p_srfi6_open_output_string(void)
{
    ScmBytePort *bport;
    DECLARE_FUNCTION("open-output-string", procedure_fixed_0);

    bport = ScmOutputStrPort_new(NULL);
    return scm_make_port(scm_make_char_port(bport), SCM_PORTFLAG_OUTPUT);
}

ScmObj
scm_p_srfi6_get_output_string(ScmObj port)
{
    ScmBaseCharPort *cport;
    DECLARE_FUNCTION("get-output-string", procedure_fixed_1);

    ASSERT_PORTP(port);

    SCM_ASSERT_LIVE_PORT(port);
    cport = SCM_CHARPORT_DYNAMIC_CAST(ScmBaseCharPort, SCM_PORT_IMPL(port));

    return MAKE_STRING_COPYING(ScmOutputStrPort_str(cport->bport));
}


/* FIXME: link conditionally with autoconf */
#include "strport.c"
