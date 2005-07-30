/*===========================================================================
 *  FileName : debug.c
 *  About    : Functions for debugging
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
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS IS''
 *  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 *  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 *  ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR CONTRIBUTORS BE
 *  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 *  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
 *  GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 *  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 *  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 *  OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 *  SUCH DAMAGE.
===========================================================================*/
/*=======================================
  System Include
=======================================*/
#include <stdio.h>

/*=======================================
  Local Include
=======================================*/
#include "sigscheme.h"

/*=======================================
  File Local Struct Declarations
=======================================*/

/*=======================================
  File Local Macro Declarations
=======================================*/

/*=======================================
  Variable Declarations
=======================================*/

/*=======================================
  File Local Function Declarations
=======================================*/
static void print_ScmObj_internal(FILE *f, ScmObj obj);
static void print_list(FILE *f, ScmObj list);
static void print_vector(FILE *f, ScmObj vec);

/*=======================================
   Function Implementations
=======================================*/
void SigScm_Display(ScmObj obj)
{
    print_ScmObj_internal(SCM_PORTINFO_FILE(current_output_port), obj);
    fprintf(SCM_PORTINFO_FILE(current_output_port), "\n");
}

void SigScm_DisplayToPort(ScmObj port, ScmObj obj)
{
    FILE *f = SCM_PORTINFO_FILE(port);
    print_ScmObj_internal(f, obj);
}

static void print_ScmObj_internal(FILE *f, ScmObj obj)
{
    if (SCM_CONSP(obj)) {
	print_list(f, obj);
    } else if (SCM_INTP(obj)) {
	fprintf(f, "%d", SCM_INT_VALUE(obj));
    } else if (SCM_SYMBOLP(obj)) {
	fprintf(f, "%s", SCM_SYMBOL_NAME(obj));
    } else if (SCM_CHARP(obj)) {
	if (strcmp(SCM_CHAR_CH(obj), " ") == 0)
	    fprintf(f, "#\\space");
	else if(strcmp(SCM_CHAR_CH(obj), "\n") == 0)
	    fprintf(f, "#\\newline");
	else
	    fprintf(f, "#\\%s", SCM_CHAR_CH(obj));
    } else if (SCM_STRINGP(obj)) {
	fprintf(f, "%s", SCM_STRING_STR(obj));
    } else if (SCM_FUNCP(obj)) {
	fprintf(f, "[ Func ]");
    } else if (SCM_CLOSUREP(obj)) {
	fprintf(f, "#<closure:");
	print_ScmObj_internal(f, SCM_CLOSURE_EXP(obj));
	fprintf(f, ">");
    } else if (SCM_VECTORP(obj)) {
	print_vector(f, obj);
    } else if (SCM_FREECELLP(obj)) {
	fprintf(f, "[ FreeCell ] \n");
    } else if (SCM_PORTP(obj)) {
	fprintf(f, "#<");
	/* input or output */
	if (SCM_PORT_PORTDIRECTION(obj) == PORT_INPUT)
	    fprintf(f, "i");
	else
	    fprintf(f, "o");
	fprintf(f, "port ");
	/* file or string */
	if (SCM_PORT_PORTTYPE(obj) == PORT_FILE) {
	    fprintf(f, "file %s", SCM_PORTINFO_FILENAME(obj));
	} else if (SCM_PORT_PORTTYPE(obj) == PORT_STRING) {
	    fprintf(f, "string");
	}
	fprintf(f, ">");
    } else if (SCM_CONTINUATIONP(obj)) {
	fprintf(f, "(continuation)");
    } else {
        if (EQ(obj, SCM_NIL)) {
            fprintf(f, "()");
        } else if (EQ(obj, SCM_TRUE)) {
            fprintf(f, "#t");
        } else if (EQ(obj, SCM_FALSE)) {
            fprintf(f, "#f");
	} else if (EQ(obj, SCM_EOF)) {
	    fprintf(f, "EOF");
        } else if (EQ(obj, SCM_QUOTE)) {
            fprintf(f, "QUOTE");
        } else if (EQ(obj, SCM_QUASIQUOTE)) {
            fprintf(f, "QUASIQUOTE");
        } else if (EQ(obj, SCM_UNQUOTE)) {
            fprintf(f, "UNQUOTE");
        } else if (EQ(obj, SCM_UNQUOTE_SPLICING)) {
            fprintf(f, "UNQUOTE_SPLICING");
        } else if (EQ(obj, SCM_UNBOUND)) {
	    fprintf(f, "UNBOUND");
	} else if (EQ(obj, SCM_UNSPECIFIED)) {
	    fprintf(f, "UNSPECIFIED");
	} else if (EQ(obj, SCM_UNDEF)) {
	    fprintf(f, "UNDEF");
	}
    }
}

static void print_list(FILE *f, ScmObj list)
{
    ScmObj car = SCM_NIL;
    ScmObj cdr = SCM_NIL;
    ScmObj tmp = SCM_NIL;

    /* print left parenthesis */
    fprintf(f, "(");

    /* get car and cdr */
    car = SCM_CAR(list);
    cdr = SCM_CDR(list);
    
    /* print car */
    print_ScmObj_internal(f, car);
    if (!SCM_NULLP(cdr))
	fprintf(f, " ");

    /* print else for-each */
    for (tmp = cdr; ; tmp = SCM_CDR(tmp)) {
	if (SCM_CONSP(tmp)) {
	    print_ScmObj_internal(f, SCM_CAR(tmp));
	    if (SCM_NULLP(SCM_CDR(tmp))) {
		fprintf(f, ")");
		return;
	    } else {
		if (!SCM_NULLP(SCM_CDR(tmp)))
		    fprintf(f, " ");
	    }
	} else {
	    if (!SCM_NULLP(tmp)) {
		fprintf(f, ". ");
		print_ScmObj_internal(f, tmp);
	    }

	    fprintf(f, ")");
	    return;
	}
    }
}

static void print_vector(FILE *f, ScmObj vec)
{
    ScmObj *v = SCM_VECTOR_VEC(vec); 
    int c_len = SCM_VECTOR_LEN(vec);
    int i     = 0;

    /* print left parenthesis with '#' */
    fprintf(f, "#(");

    /* print each element */
    for (i = 0; i < c_len; i++) {
	print_ScmObj_internal(f, v[i]);

	if (i != c_len - 1)
	    fprintf(f, " ");
    }

    fprintf(f, ")");
}
