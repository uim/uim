/*===========================================================================
 *  Filename : global.h
 *  About    : Global object handlings
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

/* FIXME: SCM_WRITABLE_STATICLESS_PLATFORM is not available yet */

#ifndef __SCM_GLOBAL_H
#define __SCM_GLOBAL_H

#include <config.h>

#if (defined(__SYMBIAN32__) && !defined(EKA2))
#include <string.h>
#include <stdlib.h>
#include <e32std.h>
#elif BREW_MAJ_VER  /* FIXME: inappropriate detection method */
#include "AEEStdLib.h"
#else
#include <string.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

/*=======================================
  Macro Definitions
=======================================*/
/* Consumes sizeof(void *) per struct to suppress the extra semicolon warnings
 * by default. Disable SCM_USE_WARNING_SUPPRESSOR to minimize memory
 * consumption. */
#if SCM_USE_WARNING_SUPPRESSOR
#define SCM_GLOBAL_STRUCT_WARNING_SUPPRESSOR void *dummy
#else
#define SCM_GLOBAL_STRUCT_WARNING_SUPPRESSOR
#endif

#if SCM_USE_AGGREGATED_GLOBAL_VARS
#define SCM_AGGREGATED_GLOBAL_VARS_BEGIN                                     \
    /* dummy statement to prevent static prefix */                           \
    struct scm_g_dummy_aggregated_begin { int dummy; };                      \
    struct scm_g_aggregated {                                                \
        SCM_GLOBAL_STRUCT_WARNING_SUPPRESSOR
#define SCM_AGGREGATED_GLOBAL_VARS(_name)                                    \
        struct scm_g_##_name _name
#define SCM_AGGREGATED_GLOBAL_VARS_END                                       \
    }

#if (defined(__SYMBIAN32__) && !defined(EKA2))
/*** EXPERIMENTAL AND NOT TESTED ***/

#define SCM_DECLARE_AGGREGATED_GLOBAL_VARS() extern int dummy
#define SCM_DEFINE_AGGREGATED_GLOBAL_VARS()  extern int dummy

#define SCM_AGGREGATED_GLOBAL_VARS_INIT() (scm_aggregated_global_vars_init())
#define SCM_AGGREGATED_GLOBAL_VARS_FIN()  (scm_aggregated_global_vars_fin())
#define SCM_AGGREGATED_GLOBAL_VARS_INSTANCE()                                \
    ((struct scm_g_aggregated *)Dll::Tls())

#elif BREW_MAJ_VER  /* FIXME: inappropriate detection method */
/*** EXPERIMENTAL AND NOT TESTED ***/

#define SCM_DECLARE_AGGREGATED_GLOBAL_VARS() extern int dummy
#define SCM_DEFINE_AGGREGATED_GLOBAL_VARS()  extern int dummy

#define SCM_AGGREGATED_GLOBAL_VARS_INIT() (scm_aggregated_global_vars_init())
#define SCM_AGGREGATED_GLOBAL_VARS_FIN()  SCM_EMPTY_EXPR
#define SCM_AGGREGATED_GLOBAL_VARS_INSTANCE()                                \
    (&((SCM_BREW_USER_APPLET_T *)GETAPPINSTANCE())->m_scm_g_aggregated_instance)

#elif SCM_HAVE_WRITABLE_GLOBAL_VARS
#define SCM_DECLARE_AGGREGATED_GLOBAL_VARS()                                 \
    extern struct scm_g_aggregated scm_g_aggregated_instance;
#define SCM_DEFINE_AGGREGATED_GLOBAL_VARS()                                  \
    /* dummy statement to prevent static prefix */                           \
    struct scm_g_dummy_aggregated_define { int dummy; };                     \
    struct scm_g_aggregated scm_g_aggregated_instance

#define SCM_AGGREGATED_GLOBAL_VARS_INIT() (scm_aggregated_global_vars_init())
#define SCM_AGGREGATED_GLOBAL_VARS_FIN()  SCM_EMPTY_EXPR
#define SCM_AGGREGATED_GLOBAL_VARS_INSTANCE() (scm_g_aggregated_instance)
#else
#error "This platform is not supported yet"
#endif

#define SCM_DEFINE_STATIC_VARS(_namespace)                                   \
    static struct scm_g_##_namespace *scm_g_instance_##_namespace(void)

#define SCM_GLOBAL_VARS_INIT(_namespace)   SCM_EMPTY_EXPR
#define SCM_GLOBAL_VARS_FIN(_namespace)    SCM_EMPTY_EXPR

#define SCM_GLOBAL_VARS_INSTANCE(_namespace) (scm_g_instance_##_namespace())

#define SCM_DEFINE_GLOBAL_VARS_INSTANCE_ACCESSOR(_namespace)                 \
    struct scm_g_##_namespace *                                              \
    scm_g_instance_##_namespace(void)                                        \
    {                                                                        \
        return &SCM_AGGREGATED_GLOBAL_VARS_INSTANCE()._namespace;            \
    }                                                                        \
    extern int dummy

#else /* SCM_USE_AGGREGATED_GLOBAL_VARS */

#define SCM_DECLARE_AGGREGATED_GLOBAL_VARS()                                 \
    extern int dummy
#define SCM_DEFINE_AGGREGATED_GLOBAL_VARS()                                  \
    extern int dummy

#define SCM_AGGREGATED_GLOBAL_VARS_INIT() SCM_EMPTY_EXPR
#define SCM_AGGREGATED_GLOBAL_VARS_FIN()  SCM_EMPTY_EXPR

#define SCM_DEFINE_STATIC_VARS(_namespace)                                   \
    static struct scm_g_##_namespace scm_g_instance_##_namespace
#define SCM_GLOBAL_VARS_INIT(_namespace)                                     \
    (memset(&scm_g_instance_##_namespace, 0,                                 \
            sizeof(scm_g_instance_##_namespace)))
#define SCM_GLOBAL_VARS_FIN(_namespace) SCM_EMPTY_EXPR

#define SCM_GLOBAL_VARS_INSTANCE(_namespace)                                 \
    (scm_g_instance_##_namespace)
#endif /* SCM_USE_AGGREGATED_GLOBAL_VARS */

#define SCM_GLOBAL_VARS_BEGIN(_namespace)                                    \
    struct scm_g_##_namespace {                                              \
    SCM_GLOBAL_STRUCT_WARNING_SUPPRESSOR
#define SCM_GLOBAL_VARS_END(_namespace)                                      \
    }

#define SCM_GLOBAL_VAR(_namespace, _var_name)                                \
    (SCM_GLOBAL_VARS_INSTANCE(_namespace)._var_name)

#if SCM_USE_AGGREGATED_GLOBAL_VARS
#define SCM_DECLARE_EXPORTED_VARS(_namespace)                                \
    SCM_EXPORT struct scm_g_##_namespace *scm_g_instance_##_namespace(void)
#define SCM_DEFINE_EXPORTED_VARS(_namespace)                                 \
    extern int dummy
#elif SCM_COMBINED_SOURCE
/* define at declaration in the header file */
#define SCM_DECLARE_EXPORTED_VARS(_namespace)                                \
    SCM_DEFINE_STATIC_VARS(_namespace)
#define SCM_DEFINE_EXPORTED_VARS(_namespace)                                 \
    /* dummy statement to prevent static prefix */                           \
    struct scm_g_dummy_##_namespace { int dummy; }
#else
#define SCM_DECLARE_EXPORTED_VARS(_namespace)                                \
    extern struct scm_g_##_namespace scm_g_instance_##_namespace
#define SCM_DEFINE_EXPORTED_VARS(_namespace)                                 \
    /* dummy statement to prevent static prefix */                           \
    struct scm_g_dummy_##_namespace { int dummy; };                          \
    struct scm_g_##_namespace scm_g_instance_##_namespace
#endif

#if (SCM_COMBINED_SOURCE && !SCM_EXPORT_API)
#define SCM_EXTERN(_decl) extern int scm_dummy
#define SCM_EXPORT static

#elif defined(__SYMBIAN32__)
#define SCM_EXTERN(_decl) extern _decl
#if SCM_COMPILING_LIBSSCM
#define SCM_EXPORT EXPORT_C
#else /* SCM_COMPILING_LIBSSCM */
#define SCM_EXPORT IMPORT_C
#endif /* SCM_COMPILING_LIBSSCM */

#elif BREW_MAJ_VER  /* FIXME: inappropriate detection method */
#define SCM_EXTERN(_decl) extern _decl
#define SCM_EXPORT extern  /* respect coding style of BREW */

#elif (defined(_WIN32) || defined(_WIN64))
#define SCM_EXTERN(_decl) extern _decl
#if SCM_COMPILING_LIBSSCM
#define SCM_EXPORT __declspec(dllexport)
#else /* SCM_COMPILING_LIBSSCM */
#define SCM_EXPORT __declspec(dllimport)
#endif /* SCM_COMPILING_LIBSSCM */

#else
#define SCM_EXTERN(_decl) extern _decl
#define SCM_EXPORT
#endif

/*=======================================
  Type Definitions
=======================================*/

/*=======================================
  Variable Declarations
=======================================*/

/*=======================================
  Function Declarations
=======================================*/
#if SCM_USE_AGGREGATED_GLOBAL_VARS
SCM_EXPORT void scm_aggregated_global_vars_init(void);
#if (defined(__SYMBIAN32__) && !defined(EKA2))
SCM_EXPORT void scm_aggregated_global_vars_fin(void);
#endif /* (defined(__SYMBIAN32__) && !defined(EKA2)) */
#endif /* SCM_USE_AGGREGATED_GLOBAL_VARS */


#ifdef __cplusplus
}
#endif

#endif /* __SCM_GLOBAL_H */
