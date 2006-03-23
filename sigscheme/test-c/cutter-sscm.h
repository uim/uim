/*
 *  cutter-sscm.h: A customization code for Cutter to cooperate with SigScheme
 *  Copyright (C) 2006  YamaKen <yamaken AT bp.iij4u.or.jp>
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#define UT_USE_MESSAGELESS_ASSERTIONS 1
#include <cutter/cutter.h>

#include "sigscheme.h"


#undef  UT_INITIALIZER
#define UT_INITIALIZER default_suite_init
#undef  UT_FINALIZER
#define UT_FINALIZER   default_suite_fin

#undef  UT_DEF
#define UT_DEF(name)                                                         \
static void name##_internal(utest_info *UT_INFO);                            \
                                                                             \
static void                                                                  \
name(utest_info *UT_INFO)                                                    \
{                                                                            \
    SCM_GC_PROTECTED_CALL_VOID(name##_internal, (UT_INFO));                  \
}                                                                            \
                                                                             \
static void                                                                  \
name##_internal(utest_info *UT_INFO)

#undef  UT_REGISTER_END
#define UT_REGISTER_END                                                      \
    {"null", NULL}                                                           \
  }                                                                          \
};                                                                           \
                                                                             \
SSCM_SUITE_INITIALIZER                                                       \
SSCM_SUITE_FINALIZER                                                         \
SSCM_REGISTER_SUITE

utest_suite *register_suite(void);
static bool default_suite_init(utest_info *uinfo);
static bool default_suite_fin(utest_info *uinfo);

/* Redefine these macros to override the functions. Don't forget
 * initialize/finalize SigScheme when do so. */

#define SSCM_REGISTER_SUITE                                                  \
utest_suite *                                                                \
register_suite(void)                                                         \
{                                                                            \
  return &UT_SUITE;                                                          \
}

#define SSCM_SUITE_INITIALIZER                                               \
static bool                                                                  \
default_suite_init(utest_info *uinfo)                                        \
{                                                                            \
    scm_initialize(NULL);                                                    \
    return TRUE;                                                             \
}

#define SSCM_SUITE_FINALIZER                                                 \
static bool                                                                  \
default_suite_fin(utest_info *uinfo)                                         \
{                                                                            \
    scm_finalize();                                                          \
    return TRUE;                                                             \
}
