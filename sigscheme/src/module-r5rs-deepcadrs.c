/*===========================================================================
 *  FileName : module-r5rs-deepcadrs.c
 *  About    : Deep c[ad]+r operations of R5RS
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

/*=======================================
  File Local Function Declarations
=======================================*/

/*=======================================
  Function Implementations
=======================================*/
ScmObj
scm_p_caaar(ScmObj lst)
{
    DECLARE_FUNCTION("caaar", procedure_fixed_1);

    return scm_p_car( scm_p_car( scm_p_car(lst) ));
}

ScmObj
scm_p_caadr(ScmObj lst)
{
    DECLARE_FUNCTION("caadr", procedure_fixed_1);

    return scm_p_car( scm_p_car( scm_p_cdr(lst) ));
}

ScmObj
scm_p_cadar(ScmObj lst)
{
    DECLARE_FUNCTION("cadar", procedure_fixed_1);

    return scm_p_car( scm_p_cdr( scm_p_car(lst) ));
}

ScmObj
scm_p_cdaar(ScmObj lst)
{
    DECLARE_FUNCTION("cdaar", procedure_fixed_1);

    return scm_p_cdr( scm_p_car( scm_p_car(lst) ));
}

ScmObj
scm_p_cdadr(ScmObj lst)
{
    DECLARE_FUNCTION("cdadr", procedure_fixed_1);

    return scm_p_cdr( scm_p_car( scm_p_cdr(lst) ));
}

ScmObj
scm_p_cddar(ScmObj lst)
{
    DECLARE_FUNCTION("cddar", procedure_fixed_1);

    return scm_p_cdr( scm_p_cdr( scm_p_car(lst) ));
}

ScmObj
scm_p_caaaar(ScmObj lst)
{
    DECLARE_FUNCTION("caaaar", procedure_fixed_1);

    return scm_p_car( scm_p_car( scm_p_car( scm_p_car(lst) )));
}

ScmObj
scm_p_caaadr(ScmObj lst)
{
    DECLARE_FUNCTION("caaadr", procedure_fixed_1);

    return scm_p_car( scm_p_car( scm_p_car( scm_p_cdr(lst) )));
}

ScmObj
scm_p_caadar(ScmObj lst)
{
    DECLARE_FUNCTION("caadar", procedure_fixed_1);

    return scm_p_car( scm_p_car( scm_p_cdr( scm_p_car(lst) )));
}

ScmObj
scm_p_caaddr(ScmObj lst)
{
    DECLARE_FUNCTION("caaddr", procedure_fixed_1);

    return scm_p_car( scm_p_car( scm_p_cdr( scm_p_cdr(lst) )));
}

ScmObj
scm_p_cadaar(ScmObj lst)
{
    DECLARE_FUNCTION("cadaar", procedure_fixed_1);

    return scm_p_car( scm_p_cdr( scm_p_car( scm_p_car(lst) )));
}

ScmObj
scm_p_cadadr(ScmObj lst)
{
    DECLARE_FUNCTION("cadadr", procedure_fixed_1);

    return scm_p_car( scm_p_cdr( scm_p_car( scm_p_cdr(lst) )));
}

ScmObj
scm_p_caddar(ScmObj lst)
{
    DECLARE_FUNCTION("caddar", procedure_fixed_1);

    return scm_p_car( scm_p_cdr( scm_p_cdr( scm_p_car(lst) )));
}

ScmObj
scm_p_cadddr(ScmObj lst)
{
    DECLARE_FUNCTION("cadddr", procedure_fixed_1);

    return scm_p_car( scm_p_cdr( scm_p_cdr( scm_p_cdr(lst) )));
}

ScmObj
scm_p_cdaaar(ScmObj lst)
{
    DECLARE_FUNCTION("cdaaar", procedure_fixed_1);

    return scm_p_cdr( scm_p_car( scm_p_car( scm_p_car(lst) )));
}

ScmObj
scm_p_cdaadr(ScmObj lst)
{
    DECLARE_FUNCTION("cdaadr", procedure_fixed_1);

    return scm_p_cdr( scm_p_car( scm_p_car( scm_p_cdr(lst) )));
}

ScmObj
scm_p_cdadar(ScmObj lst)
{
    DECLARE_FUNCTION("cdadar", procedure_fixed_1);

    return scm_p_cdr( scm_p_car( scm_p_cdr( scm_p_car(lst) )));
}

ScmObj
scm_p_cdaddr(ScmObj lst)
{
    DECLARE_FUNCTION("cdaddr", procedure_fixed_1);

    return scm_p_cdr( scm_p_car( scm_p_cdr( scm_p_cdr(lst) )));
}

ScmObj
scm_p_cddaar(ScmObj lst)
{
    DECLARE_FUNCTION("cddaar", procedure_fixed_1);

    return scm_p_cdr( scm_p_cdr( scm_p_car( scm_p_car(lst) )));
}

ScmObj
scm_p_cddadr(ScmObj lst)
{
    DECLARE_FUNCTION("cddadr", procedure_fixed_1);

    return scm_p_cdr( scm_p_cdr( scm_p_car( scm_p_cdr(lst) )));
}

ScmObj
scm_p_cdddar(ScmObj lst)
{
    DECLARE_FUNCTION("cdddar", procedure_fixed_1);

    return scm_p_cdr( scm_p_cdr( scm_p_cdr( scm_p_car(lst) )));
}

ScmObj
scm_p_cddddr(ScmObj lst)
{
    DECLARE_FUNCTION("cddddr", procedure_fixed_1);

    return scm_p_cdr( scm_p_cdr( scm_p_cdr( scm_p_cdr(lst) )));
}
