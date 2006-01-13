/*===========================================================================
 *  FileName : sigschemefunctable.c
 *  About    : Built-in function table
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

/*=======================================
  System Include
=======================================*/

/*=======================================
  Local Include
=======================================*/
#include "sigscheme.h"
#include "sigschemefunctable.h"

/*=======================================
  Macro Definitions
=======================================*/

/*=======================================
  Type Definitions
=======================================*/

/*=======================================
   Builtin Function Tables
=======================================*/
#include "sigschemefunctable-r5rs.c"
#include "sigschemefunctable-error.c"

#if SCM_USE_DEEP_CADRS
#include "sigschemefunctable-r5rs-deepcadrs.c"
#endif
#if SCM_USE_NONSTD_FEATURES
#include "sigschemefunctable-nonstd.c"
#endif
#if SCM_USE_SRFI1
#include "sigschemefunctable-srfi1.c"
#endif
#if SCM_USE_SRFI2
#include "sigschemefunctable-srfi2.c"
#endif
#if SCM_USE_SRFI6
#include "sigschemefunctable-srfi6.c"
#endif
#if SCM_USE_SRFI8
#include "sigschemefunctable-srfi8.c"
#endif
#if SCM_USE_SRFI23
#include "sigschemefunctable-srfi23.c"
#endif
#if SCM_USE_SRFI34
#include "sigschemefunctable-srfi34.c"
#endif
#if SCM_USE_SRFI38
#include "sigschemefunctable-srfi38.c"
#endif
#if SCM_USE_SRFI60
#include "sigschemefunctable-srfi60.c"
#endif
#if SCM_COMPAT_SIOD
#include "sigschemefunctable-siod.c"
#endif
