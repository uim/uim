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
#include "sigschemeinternal.h"

/*=======================================
  File Local Struct Declarations
=======================================*/
enum OutputType {
    AS_WRITE,   /* string is enclosed by ", char is written using #\ notation. */
    AS_DISPLAY, /* string and char is written as-is */
    UNKNOWN
};

/*=======================================
  File Local Macro Declarations
=======================================*/

/*=======================================
  Variable Declarations
=======================================*/

/*=======================================
  File Local Function Declarations
=======================================*/
static void print_ScmObj_internal(FILE *f, ScmObj obj, enum OutputType otype);
static void print_char(FILE *f, ScmObj obj, enum OutputType otype);
static void print_string(FILE *f, ScmObj obj, enum OutputType otype);
static void print_list(FILE *f, ScmObj list, enum OutputType otype);
static void print_vector(FILE *f, ScmObj vec, enum OutputType otype);
static void print_port(FILE *f, ScmObj port, enum OutputType otype);
static void print_etc(FILE *f, ScmObj obj, enum  OutputType otype);

/*=======================================
   Function Implementations
=======================================*/
void SigScm_Display(ScmObj obj)
{
    print_ScmObj_internal(SCM_PORTINFO_FILE(current_output_port), obj, AS_WRITE);
    fprintf(SCM_PORTINFO_FILE(current_output_port), "\n");
}

void SigScm_WriteToPort(ScmObj port, ScmObj obj)
{
    FILE *f = NULL;

    if (SCM_PORTINFO_PORTTYPE(port) == PORT_FILE) {
        f = SCM_PORTINFO_FILE(port);
        print_ScmObj_internal(f, obj, AS_WRITE);
        return;
    }

    SigScm_Error("SigScm_WriteToPort : support write only for file port.");
}

void SigScm_DisplayToPort(ScmObj port, ScmObj obj)
{
    FILE *f = NULL;

    if (SCM_PORTINFO_PORTTYPE(port) == PORT_FILE) {
        f = SCM_PORTINFO_FILE(port);
        print_ScmObj_internal(f, obj, AS_DISPLAY);
        return;
    }

    SigScm_Error("SigScm_DisplayToPort : support display only for file port.");
}

static void print_ScmObj_internal(FILE *f, ScmObj obj, enum OutputType otype)
{
    switch (SCM_TYPE(obj)) {
    case ScmInt:
        fprintf(f, "%d", SCM_INT_VALUE(obj));
        break;
    case ScmCons:
        print_list(f, obj, otype);      
        break;
    case ScmSymbol:
        fprintf(f, "%s", SCM_SYMBOL_NAME(obj));         
        break;
    case ScmChar:
        print_char(f, obj, otype);
        break;
    case ScmString:
        print_string(f, obj, otype);
        break;
    case ScmFunc:
        fprintf(f, "#<subr>");
        break;
    case ScmClosure:
        fprintf(f, "#<closure:");
        print_ScmObj_internal(f, SCM_CLOSURE_EXP(obj), otype);
        fprintf(f, ">");
        break;
    case ScmVector:
        print_vector(f, obj, otype);
        break;
    case ScmPort:
        print_port(f, obj, otype);
        break;
    case ScmContinuation:
        fprintf(f, "#<subr continuation>");     
        break;
    case ScmValuePacket:
        fputs("#<values ", f);
        print_list(f, SCM_VALUEPACKET_VALUES(obj), otype);
        putc('>', f);
        break;
    case ScmEtc:
        print_etc(f, obj, otype);
        break;
    case ScmFreeCell:
        SigScm_Error("You cannot print ScmFreeCell, may be GC bug.\n");
        break;
    case ScmCPointer:
        fprintf(f, "#<c_pointer %p>", SCM_C_POINTER_DATA(obj));
        break;
    case ScmCFuncPointer:
        fprintf(f, "#<c_func_pointer %p>", (void*)SCM_C_FUNCPOINTER_FUNC(obj));
        break;
    }   
}

static void print_char(FILE *f, ScmObj obj, enum OutputType otype)
{
    switch (otype) {
    case AS_WRITE:
        /*
         * in write, character objects are written using the #\ notation.
         */ 
        if (strcmp(SCM_CHAR_CH(obj), " ") == 0) {
            fprintf(f, "#\\space");
        } else if(strcmp(SCM_CHAR_CH(obj), "\n") == 0) {
            fprintf(f, "#\\newline");
        } else {
            fprintf(f, "#\\%s", SCM_CHAR_CH(obj));
        }
        break;
    case AS_DISPLAY:
        /*
         * in display, character objects appear in the reqpresentation as
         * if writen by write-char instead of by write.
         */
        fprintf(f, "%s", SCM_CHAR_CH(obj));
        break;
    default:
        SigScm_Error("print_char : unknown output type\n");
        break;
    }
}

static void print_string(FILE *f, ScmObj obj, enum OutputType otype)
{
    const char *str = SCM_STRING_STR(obj);
    int  size = strlen(str);
    int  i = 0;
    char c = 0;

    switch (otype) {
    case AS_WRITE:
        /*
         * in write, strings that appear in the written representation are
         * enclosed in doublequotes, and within those strings backslash and
         * doublequote characters are escaped by backslashes.
         */
        fprintf(f, "\""); /* first doublequote */
        for (i = 0; i < size; i++) {
            c = str[i];
            switch (c) {
            case '\"': fprintf(f, "\\\""); break;
            case '\n': fprintf(f, "\\n"); break;
            case '\r': fprintf(f, "\\r"); break;
            case '\f': fprintf(f, "\\f"); break;
            case '\t': fprintf(f, "\\t"); break;
            default:
                fprintf(f, "%c", str[i]); break;
            }
        }
        fprintf(f, "\""); /* last doublequote */
        break;
    case AS_DISPLAY:
        fprintf(f, "%s", SCM_STRING_STR(obj));
        break;
    default:
        SigScm_Error("print_string : unknown output type\n");
        break;
    }
}

static void print_list(FILE *f, ScmObj list, enum OutputType otype)
{
    ScmObj car = SCM_NIL;
    ScmObj cdr = SCM_NIL;
    ScmObj tmp = SCM_NIL;

    /* print left parenthesis */
    fprintf(f, "(");

    /* get car and cdr */
    car = CAR(list);
    cdr = CDR(list);
    
    /* print car */
    print_ScmObj_internal(f, car, otype);
    if (!NULLP(cdr))
        fprintf(f, " ");

    /* print else for-each */
    for (tmp = cdr; ; tmp = CDR(tmp)) {
        if (CONSP(tmp)) {
            print_ScmObj_internal(f, CAR(tmp), otype);
            if (NULLP(CDR(tmp))) {
                fprintf(f, ")");
                return;
            } else {
                if (!NULLP(CDR(tmp)))
                    fprintf(f, " ");
            }
        } else {
            if (!NULLP(tmp)) {
                fprintf(f, ". ");
                print_ScmObj_internal(f, tmp, otype);
            }

            fprintf(f, ")");
            return;
        }
    }
}

static void print_vector(FILE *f, ScmObj vec, enum OutputType otype)
{
    ScmObj *v = SCM_VECTOR_VEC(vec); 
    int c_len = SCM_VECTOR_LEN(vec);
    int i     = 0;

    /* print left parenthesis with '#' */
    fprintf(f, "#(");

    /* print each element */
    for (i = 0; i < c_len; i++) {
        print_ScmObj_internal(f, v[i], otype);

        if (i != c_len - 1)
            fprintf(f, " ");
    }

    fprintf(f, ")");
}

static void print_port(FILE *f, ScmObj port, enum OutputType otype)
{
    fprintf(f, "#<");

    /* input or output */
    if (SCM_PORT_PORTDIRECTION(port) == PORT_INPUT)
        fprintf(f, "i");
    else
        fprintf(f, "o");

    fprintf(f, "port ");

    /* file or string */
    if (SCM_PORTINFO_PORTTYPE(port) == PORT_FILE)
        fprintf(f, "file %s", SCM_PORTINFO_FILENAME(port));
    else if (SCM_PORTINFO_PORTTYPE(port) == PORT_STRING)
        fprintf(f, "string %s", SCM_PORTINFO_STR(port));

    fprintf(f, ">");
}

static void print_etc(FILE *f, ScmObj obj, enum  OutputType otype)
{
    if (EQ(obj, SCM_NIL))
        fprintf(f, "()");
    else if (EQ(obj, SCM_TRUE))
        fprintf(f, "#t");
    else if (EQ(obj, SCM_FALSE))
        fprintf(f, "#f");
    else if (EQ(obj, SCM_EOF))
        fprintf(f, "#<eof>");
    else if (EQ(obj, SCM_QUOTE))
        fprintf(f, "#<quote>");
    else if (EQ(obj, SCM_QUASIQUOTE))
        fprintf(f, "#<quasiquote>");
    else if (EQ(obj, SCM_UNQUOTE))
        fprintf(f, "#<unquote>");
    else if (EQ(obj, SCM_UNQUOTE_SPLICING))
        fprintf(f, "#<unquote_splicing>");
    else if (EQ(obj, SCM_UNBOUND))
        fprintf(f, "#<unbound>");
    else if (EQ(obj, SCM_UNDEF))
        fprintf(f, "#<undef>");
}
