/*===========================================================================
 *  FileName : constants.c
 *  About    : scheme constants
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
/* special constant initialization */
#define SCM_CONSTANT_BIND_SUBSTANCE(obj, cell)                          \
    do {                                                                \
        (obj) = &(cell);                                                \
        SCM_ENTYPE((obj), ScmConstant);                                 \
    } while(/* CONSTCOND */ 0)

/*=======================================
  File Local Type Definitions
=======================================*/

/*=======================================
  Variable Declarations
=======================================*/
ScmObj SigScm_null, SigScm_true, SigScm_false, SigScm_eof;
ScmObj SigScm_unbound, SigScm_undef;

static ScmCell SigScm_null_cell, SigScm_true_cell, SigScm_false_cell, SigScm_eof_cell;
static ScmCell SigScm_unbound_cell, SigScm_undef_cell;

/*=======================================
  File Local Function Declarations
=======================================*/

/*=======================================
  Function Implementations
=======================================*/
/*
 * To keep storage representation abstract, the special constants
 * initialization is encapsulated in this file. Upper layers must only use
 * abstract interfaces such as SCM_NULL and SCM_NULLP().
 */
void SigScm_InitConstants(void)
{
    SCM_CONSTANT_BIND_SUBSTANCE(SigScm_null,    SigScm_null_cell);
    SCM_CONSTANT_BIND_SUBSTANCE(SigScm_true,    SigScm_true_cell);
    SCM_CONSTANT_BIND_SUBSTANCE(SigScm_false,   SigScm_false_cell);
    SCM_CONSTANT_BIND_SUBSTANCE(SigScm_eof,     SigScm_eof_cell);
    SCM_CONSTANT_BIND_SUBSTANCE(SigScm_unbound, SigScm_unbound_cell);
    SCM_CONSTANT_BIND_SUBSTANCE(SigScm_undef,   SigScm_undef_cell);
#if SCM_COMPAT_SIOD_BUGS
    SigScm_false = SigScm_null;
#endif

#if 0 && SCM_COMPAT_SIOD_BUGS
    SigScm_GC_Protect(&SigScm_true);
    SigScm_true = Scm_NewInt(1);
#endif
}

void SigScm_FinalizeConstants(void)
{
    ;
}
