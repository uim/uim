/*===========================================================================
 *  FileName : global.h
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

#ifdef __cplusplus
extern "C" {
#endif

/*=======================================
  System Include
=======================================*/
#include <string.h>

/*=======================================
  Local Include
=======================================*/

/*=======================================
  Macro Definitions
=======================================*/
/* Consumes sizeof(void *) per struct to suppress the extra semicolon warnings
 * by default. To turn it off to minimize memory consumption,
 * make CPPFLAGS=SCM_NO_GLOBAL_STRUCT_WARNING_SUPPRESSOR=1 */
#if SCM_NO_GLOBAL_STRUCT_WARNING_SUPPRESSOR
#define SCM_GLOBAL_STRUCT_WARNING_SUPPRESSOR
#else /* SCM_NO_GLOBAL_STRUCT_WARNING_SUPPRESSOR */
#define SCM_GLOBAL_STRUCT_WARNING_SUPPRESSOR void *dummy
#endif /* SCM_NO_GLOBAL_STRUCT_WARNING_SUPPRESSOR */

#if SCM_WRITABLE_STATICLESS_PLATFORM
#define SCM_AGGREGATED_GLOBAL_VARS_BEGIN                                     \
    /* dummy statement to prevent static prefix */                           \
    struct scm_v_dummy_aggregated_begin { int dummy; };                      \
    struct scm_v_aggregated {                                                \
        SCM_GLOBAL_STRUCT_WARNING_SUPPRESSOR
#define SCM_AGGREGATED_GLOBAL_VARS(_name)                                    \
        struct scm_v_##_name _name;
#define SCM_AGGREGATED_GLOBAL_VARS_END                                       \
    } scm_v_aggregated_instance

#define SCM_DECLARE_AGGREGATED_GLOBAL_VARS()                                 \
    extern struct scm_v_aggregated scm_v_aggregated_instance;
#define SCM_DEFINE_AGGREGATED_GLOBAL_VARS()                                  \
    /* dummy statement to prevent static prefix */                           \
    struct scm_v_dummy_aggregated_define { int dummy; };                     \
    struct scm_v_aggregated scm_v_aggregated_instance

#define SCM_AGGREGATED_GLOBAL_VARS_INIT()                                    \
    (memset(&SCM_AGGREGATED_GLOBAL_VARS_INSTANCE(), 0,                       \
            sizeof(scm_v_aggregated_instance)))
#define SCM_AGGREGATED_GLOBAL_VARS_FIN()
#define SCM_AGGREGATED_GLOBAL_VARS_INSTANCE()                                \
    (scm_v_aggregated_instance)

#define SCM_DEFINE_STATIC_VARS(_namespace)
#define SCM_GLOBAL_VARS_INIT(_namespace)

#define SCM_GLOBAL_VAR(_namespace, _var_name)                                \
    (SCM_AGGREGATED_GLOBAL_VARS_INSTANCE()._namespace._var_name)

#else /* SCM_WRITABLE_STATICLESS_PLATFORM */

#define SCM_DECLARE_AGGREGATED_GLOBAL_VARS()
#define SCM_DEFINE_AGGREGATED_GLOBAL_VARS()

#define SCM_AGGREGATED_GLOBAL_VARS_INIT()
#define SCM_AGGREGATED_GLOBAL_VARS_FIN()

#define SCM_DEFINE_STATIC_VARS(_namespace)                                   \
    static struct scm_v_##_namespace scm_v_instance_##_namespace
#define SCM_GLOBAL_VARS_INIT(_namespace)                                     \
    (memset(&scm_v_instance_##_namespace, 0,                                 \
            sizeof(scm_v_instance_##_namespace)))

#define SCM_GLOBAL_VAR(_namespace, _var_name)                                \
    (scm_v_instance_##_namespace._var_name)
#endif /* SCM_WRITABLE_STATICLESS_PLATFORM */

#define SCM_GLOBAL_VARS_BEGIN(_namespace)                                    \
    struct scm_v_##_namespace {                                              \
    SCM_GLOBAL_STRUCT_WARNING_SUPPRESSOR
#define SCM_GLOBAL_VARS_END(_namespace)                                      \
    }

#if SCM_WRITABLE_STATICLESS_PLATFORM
#define SCM_DECLARE_EXPORTED_VARS(_namespace)
#define SCM_DEFINE_EXPORTED_VARS(_namespace)
#elif SCM_COMBINED_SOURCE
/* define at declaration in the header file */
#define SCM_DECLARE_EXPORTED_VARS(_namespace)                                \
    SCM_DEFINE_STATIC_VARS(_namespace)
#define SCM_DEFINE_EXPORTED_VARS(_namespace)                                 \
    /* dummy statement to prevent static prefix */                           \
    struct scm_v_dummy_##_namespace { int dummy; }
#else
#define SCM_DECLARE_EXPORTED_VARS(_namespace)                                \
    extern struct scm_v_##_namespace scm_v_instance_##_namespace
#define SCM_DEFINE_EXPORTED_VARS(_namespace)                                 \
    /* dummy statement to prevent static prefix */                           \
    struct scm_v_dummy_##_namespace { int dummy; };                          \
    struct scm_v_##_namespace scm_v_instance_##_namespace
#endif

#if SCM_COMBINED_SOURCE
#define SCM_EXTERN(_decl) extern int scm_dummy
#define SCM_EXPORT static
#else /* SCM_COMBINED_SOURCE */
#define SCM_EXTERN(_decl) extern _decl
#define SCM_EXPORT
#endif /* SCM_COMBINED_SOURCE */

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

#endif /* __SCM_GLOBAL_H */
