/*===========================================================================
 *  FileName : symbol.c
 *  About    : Symbol hash handling
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

/*
 * Symbol Name Hash Related Functions
 *
 * - Data Structure of Symbol Name Hash
 *
 *     - n = symbol_name_hash(name)
 *     - symbol_hash[n] = sym_list
 *     - sym_list = ( ScmObj(SYMBOL) ScmObj(SYMBOL) ... )
 *
 */

/*=======================================
  System Include
=======================================*/
#include <stdlib.h>
#include <string.h>

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
ScmObj *scm_symbol_hash = NULL;

/*=======================================
  File Local Function Declarations
=======================================*/
static void initialize_symbol_hash(void);
static void finalize_symbol_hash(void);
static int  symbol_name_hash(const char *name);

/*=======================================
  Function Implementations
=======================================*/
void SigScm_InitSymbol(void)
{
    initialize_symbol_hash();
}

void SigScm_FinalizeSymbol(void)
{
    finalize_symbol_hash();
}

ScmObj Scm_Intern(const char *name)
{
    int n = symbol_name_hash(name);
    ScmObj sym     = SCM_FALSE;
    ScmObj lst     = SCM_FALSE;
    ScmObj sym_lst = scm_symbol_hash[n];

    /* Search Symbol by name */
    for (lst = sym_lst; !NULLP(lst); lst = CDR(lst)) {
        sym = CAR(lst);

        if (strcmp(SCM_SYMBOL_NAME(sym), name) == 0) {
            return sym;
        }
    }

    /* If not in the sym_lst, allocate new Symbol */
    sym = Scm_NewSymbol(strdup(name), SCM_UNBOUND);

    /* And Append it to the head of scm_symbol_hash */
    sym_lst = CONS(sym, sym_lst);
    scm_symbol_hash[n] = sym_lst;

    return sym;
}

static void initialize_symbol_hash(void)
{
    int i = 0;
    scm_symbol_hash = (ScmObj*)malloc(sizeof(ScmObj) * NAMEHASH_SIZE);
    for (i = 0; i < NAMEHASH_SIZE; i++) {
        scm_symbol_hash[i] = SCM_NULL;
    }
}

static void finalize_symbol_hash(void)
{
    free(scm_symbol_hash);
}

static int symbol_name_hash(const char *name)
{
    int hash = 0;
    int c;
    char *cname = (char *)name;
    while ((c = *cname++)) {
        hash = ((hash * 17) ^ c) % NAMEHASH_SIZE;
    }
    return hash;
}
