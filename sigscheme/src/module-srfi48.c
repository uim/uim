/*===========================================================================
 *  FileName : module-srfi48.c
 *  About    : SRFI-48 Intermediate Format Strings
 *
 *  Copyright (C) 2006 YamaKen <yamaken AT bp.iij4u.or.jp>
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

/*=======================================
  Local Include
=======================================*/
#include "sigscheme.h"
#include "sigschemeinternal.h"

/*=======================================
  File Local Macro Definitions
=======================================*/

/*=======================================
  File Local Type Definitions
=======================================*/

/*=======================================
  Variable Declarations
=======================================*/
#include "functable-srfi48.c"

/*=======================================
  File Local Function Declarations
=======================================*/
static ScmObj srfi48_format_internal(enum ScmFormatCapability fcap,
                                     ScmObj fmt_or_port, ScmObj rest);

/*=======================================
  Function Implementations
=======================================*/
SCM_EXPORT void
scm_initialize_srfi48(void)
{
    /* FIXME: duplicate call with scm_initialize_internal() */
    scm_init_format();
    scm_register_funcs(scm_srfi48_func_info_table);

#if !SCM_USE_SSCM_FORMAT_EXTENSION
    SCM_SYMBOL_SET_VCELL(scm_intern("format+"), SCM_UNBOUND);
#endif

    /* SRFI-28 is a subset of SRFI-48. To prevent being overridden by SRFI-28,
     * provide it here. */
    scm_provide(SCM_CONST_STRING("srfi-28"));
}

static ScmObj
srfi48_format_internal(enum ScmFormatCapability fcap,
                       ScmObj fmt_or_port, ScmObj rest)
{
    ScmObj port, fmt, objs;
    DECLARE_INTERNAL_FUNCTION("format");

    if (STRINGP(fmt_or_port)) {
        port = SCM_FALSE;
        fmt = fmt_or_port;
    } else {
        port = fmt_or_port;
        fmt = MUST_POP_ARG(rest);
        ENSURE_STRING(fmt);
    }
    objs = rest;

    return scm_lformat(port, fcap, SCM_STRING_STR(fmt), objs);
}

/* format [port] format-string [obj ...] */
SCM_EXPORT ScmObj
scm_p_srfi48_format(ScmObj fmt_or_port, ScmObj rest)
{
    DECLARE_FUNCTION("format", procedure_variadic_1);

    return srfi48_format_internal(SCM_FMT_SRFI48, fmt_or_port, rest);
}

/* SigScheme specific procedure */
/* format+ [port] format-string [obj ...] */
SCM_EXPORT ScmObj
scm_p_formatplus(ScmObj fmt_or_port, ScmObj rest)
{
    DECLARE_FUNCTION("format+", procedure_variadic_1);

    return srfi48_format_internal(SCM_FMT_SSCM, fmt_or_port, rest);
}
