/*===========================================================================
 *  FileName : sigscheme-combined.h
 *  About    : Header for combined-source version of SigScheme
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

/* This file must be included before the client package's config.h */

/*
 * The combined-source version of SigScheme is provided for
 * symbol-conflict-sensitive applications such as libuim. Since SigScheme uses
 * semi-common 'scm_' and 'Scm' prefixes for exported symbols, it may conflict
 * with other Scheme implementations such as Guile if the client is linked with
 * a Scheme-based library. To avoid such conflict, the combined-source makes
 * all symbols of SigScheme static (although not finished yet). A client of
 * SigScheme can use this combined-source version by directly including
 * sigscheme-combined.c into a C source of the client. Once included, all
 * configured SigScheme features can be used as file-local code, and it can
 * also be linked with other arbitrary codes via user-written wrapper.
 *
 * Although libtool has an useful option -export-symbols-regex for such
 * purpose, libtool does not ensure its portability. Currently supported
 * platforms are limited (at least 2.1a 2006-03-30) and some platforms seems
 * that cannot be supported. So I prepare this portable method since
 * portability is very important for SigScheme.
 *
 *   -- YamaKen 2006-03-31
 */

#ifndef __SCM_SIGSCHEME_COMBINED_H
#define __SCM_SIGSCHEME_COMBINED_H

#ifdef __cplusplus
extern "C" {
#endif

#include "config.h"
/* Since sigscheme-combined.c is intended to be directly included into another
 * package as subordinate code module, these SigScheme-defined macros should be
 * undefined to avoid conflict. */
#undef PACKAGE
#undef PACKAGE_BUGREPORT
#undef PACKAGE_NAME
#undef PACKAGE_STRING
#undef PACKAGE_TARNAME
#undef PACKAGE_VERSION
#undef VERSION

/* Although SigScheme only uses old specifications, client package may need
 * more recent ones such as (_XOPEN_SOURCE == 600). To avoid the function
 * availability problem, undef the SigScheme-restricted values here and back to
 * the system default. */
#undef _POSIX_C_SOURCE
#undef _XOPEN_SOURCE

/*=======================================
  Macro Definitions
=======================================*/
#define SCM_COMBINED_SOURCE 1

/*=======================================
  Type Definitions
=======================================*/

/*=======================================
  Variable Declarations
=======================================*/

/*=======================================
  Function Declarations
=======================================*/


#ifdef __cplusplus
}
#endif

#endif /* __SCM_SIGSCHEME_COMBINED_H */
