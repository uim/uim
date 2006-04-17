/*===========================================================================
 *  FileName : global-aggregated.c
 *  About    : Aggregated global variable handlings
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
#if (defined(__SYMBIAN32__) && !defined(EKA2))
#include <string.h>
#include <stdlib.h>
#include <e32std.h>
#elif BREW_MAJ_VER  /* FIXME: inappropriate detection method */
#include "AEEStdLib.h"
#else
#include <string.h>
#endif

/*=======================================
  Local Include
=======================================*/
#include "global.h"

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
#if (defined(__SYMBIAN32__) && !defined(EKA2))
/*** EXPERIMENTAL AND NOT TESTED ***/

/*
 * EKA2 supports writable static data on dlls. See following references for the
 * global variable handlings on Symbian OS.
 *
 * Essential idioms - Static data:
 * http://www.symbian.com/developer/techlib/v9.1docs/doc_source/guide/N1001E/StaticData.html
 *
 * Class Dll:
 * http://www.symbian.com/developer/techlib/v9.1docs/doc_source/reference/reference-cpp/N101CA/DllClass.html
 * http://www.symbian.com/developer/techlib/v8.1adocs/doc_source/reference/reference-cpp/N101BA/ThreadsAndProcesses/DllClass.html
 * http://www.symbian.com/developer/techlib/v70docs/SDL_v7.0/doc_source/reference/cpp/ThreadsAndProcesses/DllClass.html
 */
void
scm_aggregated_global_vars_init(void)
{
    struct scm_g_aggregated *vars;

    vars = malloc(sizeof(struct scm_g_aggregated));
    if (!vars)
        exit(EXIT_FAILURE);    /* FIXME: more appropriate handling */
    memset(vars, 0, sizeof(struct scm_g_aggregated));
    if (KErrNone != Dll::SetTls(vars))
        exit(EXIT_FAILURE);    /* FIXME: more appropriate handling */
}

void
scm_aggregated_global_vars_fin(void)
{
    free(SCM_AGGREGATED_GLOBAL_VARS_INSTANCE());
#if 0
    Dll::FreeTls();  /* not available on earlier versions */
#else
    if (KErrNone != Dll::SetTls(NULL))
        exit(EXIT_FAILURE);    /* FIXME: more appropriate handling */
#endif
}

#elif BREW_MAJ_VER  /* FIXME: inappropriate detection method */
/*** EXPERIMENTAL AND NOT TESTED ***/

/*
 * Usage:
 *
 * #include "sigscheme-combined.c"
 *
 * #define SCM_BREW_USER_APPLET_T CMyApplet
 *
 * typedef struct _CMyApplet {
 *   AEEApplet a;
 *   ...
 *   struct scm_g_aggregated m_scm_g_aggregated_instance;
 *   ...
 * } CMyApplet;
 */

void
scm_aggregated_global_vars_init(void)
{
    MEMSET(SCM_AGGREGATED_GLOBAL_VARS_INSTANCE(),
           0, sizeof(struct scm_g_aggregated));
}

#elif SCM_HAVE_WRITABLE_GLOBAL_VARS
void
scm_aggregated_global_vars_init(void)
{
    memset(&scm_g_aggregated_instance, 0, sizeof(struct scm_g_aggregated));
}

#else
#error "This platform is not supported yet"
#endif
