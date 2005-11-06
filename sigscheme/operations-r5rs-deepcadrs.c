/*===========================================================================
 *  FileName : operations-r5rs-deepcadrs.c
 *  About    : deep c[ad]+r operations
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
ScmObj ScmOp_caaar(ScmObj lst)
{
    DECLARE_FUNCTION("caaar", ProcedureFixed1);
    return ScmOp_car( ScmOp_car( ScmOp_car(lst) ));
}
ScmObj ScmOp_caadr(ScmObj lst)
{
    DECLARE_FUNCTION("caadr", ProcedureFixed1);
    return ScmOp_car( ScmOp_car( ScmOp_cdr(lst) ));
}
ScmObj ScmOp_cadar(ScmObj lst)
{
    DECLARE_FUNCTION("cadar", ProcedureFixed1);
    return ScmOp_car( ScmOp_cdr( ScmOp_car(lst) ));
}
ScmObj ScmOp_cdaar(ScmObj lst)
{
    DECLARE_FUNCTION("cdaar", ProcedureFixed1);
    return ScmOp_cdr( ScmOp_car( ScmOp_car(lst) ));
}
ScmObj ScmOp_cdadr(ScmObj lst)
{
    DECLARE_FUNCTION("cdadr", ProcedureFixed1);
    return ScmOp_cdr( ScmOp_car( ScmOp_cdr(lst) ));
}
ScmObj ScmOp_cddar(ScmObj lst)
{
    DECLARE_FUNCTION("cddar", ProcedureFixed1);
    return ScmOp_cdr( ScmOp_cdr( ScmOp_car(lst) ));
}

ScmObj ScmOp_caaaar(ScmObj lst)
{
    DECLARE_FUNCTION("caaaar", ProcedureFixed1);
    return ScmOp_car( ScmOp_car( ScmOp_car( ScmOp_car(lst) )));
}
ScmObj ScmOp_caaadr(ScmObj lst)
{
    DECLARE_FUNCTION("caaadr", ProcedureFixed1);
    return ScmOp_car( ScmOp_car( ScmOp_car( ScmOp_cdr(lst) )));
}
ScmObj ScmOp_caadar(ScmObj lst)
{
    DECLARE_FUNCTION("caadar", ProcedureFixed1);
    return ScmOp_car( ScmOp_car( ScmOp_cdr( ScmOp_car(lst) )));
}
ScmObj ScmOp_caaddr(ScmObj lst)
{
    DECLARE_FUNCTION("caaddr", ProcedureFixed1);
    return ScmOp_car( ScmOp_car( ScmOp_cdr( ScmOp_cdr(lst) )));
}
ScmObj ScmOp_cadaar(ScmObj lst)
{
    DECLARE_FUNCTION("cadaar", ProcedureFixed1);
    return ScmOp_car( ScmOp_cdr( ScmOp_car( ScmOp_car(lst) )));
}
ScmObj ScmOp_cadadr(ScmObj lst)
{
    DECLARE_FUNCTION("cadadr", ProcedureFixed1);
    return ScmOp_car( ScmOp_cdr( ScmOp_car( ScmOp_cdr(lst) )));
}
ScmObj ScmOp_caddar(ScmObj lst)
{
    DECLARE_FUNCTION("caddar", ProcedureFixed1);
    return ScmOp_car( ScmOp_cdr( ScmOp_cdr( ScmOp_car(lst) )));
}
ScmObj ScmOp_cadddr(ScmObj lst)
{
    DECLARE_FUNCTION("cadddr", ProcedureFixed1);
    return ScmOp_car( ScmOp_cdr( ScmOp_cdr( ScmOp_cdr(lst) )));
}
ScmObj ScmOp_cdaaar(ScmObj lst)
{
    DECLARE_FUNCTION("cdaaar", ProcedureFixed1);
    return ScmOp_cdr( ScmOp_car( ScmOp_car( ScmOp_car(lst) )));
}
ScmObj ScmOp_cdaadr(ScmObj lst)
{
    DECLARE_FUNCTION("cdaadr", ProcedureFixed1);
    return ScmOp_cdr( ScmOp_car( ScmOp_car( ScmOp_cdr(lst) )));
}
ScmObj ScmOp_cdadar(ScmObj lst)
{
    DECLARE_FUNCTION("cdadar", ProcedureFixed1);
    return ScmOp_cdr( ScmOp_car( ScmOp_cdr( ScmOp_car(lst) )));
}
ScmObj ScmOp_cdaddr(ScmObj lst)
{
    DECLARE_FUNCTION("cdaddr", ProcedureFixed1);
    return ScmOp_cdr( ScmOp_car( ScmOp_cdr( ScmOp_cdr(lst) )));
}
ScmObj ScmOp_cddaar(ScmObj lst)
{
    DECLARE_FUNCTION("cddaar", ProcedureFixed1);
    return ScmOp_cdr( ScmOp_cdr( ScmOp_car( ScmOp_car(lst) )));
}
ScmObj ScmOp_cddadr(ScmObj lst)
{
    DECLARE_FUNCTION("cddadr", ProcedureFixed1);
    return ScmOp_cdr( ScmOp_cdr( ScmOp_car( ScmOp_cdr(lst) )));
}
ScmObj ScmOp_cdddar(ScmObj lst)
{
    DECLARE_FUNCTION("cdddar", ProcedureFixed1);
    return ScmOp_cdr( ScmOp_cdr( ScmOp_cdr( ScmOp_car(lst) )));
}
ScmObj ScmOp_cddddr(ScmObj lst)
{
    DECLARE_FUNCTION("cddddr", ProcedureFixed1);
    return ScmOp_cdr( ScmOp_cdr( ScmOp_cdr( ScmOp_cdr(lst) )));
}
