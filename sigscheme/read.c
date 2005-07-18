/*===========================================================================
 *  FileName : read.c
 *  About    : S-Expression reader
 *
 *  Copyright (C) 2000-2001 by Shiro Kawai (shiro@acm.org)
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
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

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
#define SCM_PORT_GETC(port, c) 				\
    do {						\
	if (SCM_PORTINFO_UNGOTTENCHAR(port)) {		\
	    c = SCM_PORTINFO_UNGOTTENCHAR(port);	\
	    SCM_PORTINFO_UNGOTTENCHAR(port) = 0;	\
	} else {					\
	    c = getc(SCM_PORTINFO_FILE(port));		\
	    SCM_PORTINFO_UNGOTTENCHAR(port) = 0;	\
	}						\
    } while (0);

#define SCM_PORT_UNGETC(port,c )	\
    SCM_PORTINFO_UNGOTTENCHAR(port) = c;

/*=======================================
  Variable Declarations
=======================================*/

/*=======================================
  File Local Function Declarations
=======================================*/
static int    skip_comment_and_space(ScmObj port);
static char*  read_char_sequence(ScmObj port);

static ScmObj read_sexpression(ScmObj port);
static ScmObj read_list(ScmObj port, int closeParen);
static ScmObj read_char(ScmObj port);
static ScmObj read_string(ScmObj port);
static ScmObj read_symbol(ScmObj port);
static ScmObj read_number_or_symbol(ScmObj port);
static ScmObj read_quote(ScmObj port, ScmObj quoter);

/*=======================================
  Function Implementations
=======================================*/
/*===========================================================================
  S-Expression Parser
===========================================================================*/
ScmObj SigScm_Read(ScmObj port)
{
    if (!SCM_PORTP(port))
        SigScm_ErrorObj("SigScm_Read : port required but got ", port);

    return read_sexpression(port);
}

ScmObj SigScm_Read_Char(ScmObj port)
{
    if (!SCM_PORTP(port))
        SigScm_ErrorObj("SigScm_Read_Char : port required but got ", port);

    return read_char(port);
}


static int skip_comment_and_space(ScmObj port)
{
    int c = 0;
    while (1) {
	SCM_PORT_GETC(port, c);
        if (c == EOF) {
            return c;
        } else if(c == ';') {
            while (1) {
		SCM_PORT_GETC(port, c);
                if (c == '\n') break;
                if (c == EOF ) return c;
            }
            continue;
        } else if(isspace(c)) {
            continue;
        }

        return c;
    }
}

static ScmObj read_sexpression(ScmObj port)
{
#if DEBUG_PARSER
    printf("read_sexpression\n");
#endif

    int c  = 0;
    int c1 = 0;
    while (1) {
        c = skip_comment_and_space(port);

#if DEBUG_PARSER
        printf("read_sexpression c = %c\n", c);
#endif

        switch (c) {
            case '(':
                return read_list(port, ')');
            case '\"':
                return read_string(port);
            case '0': case '1': case '2': case '3': case '4':
            case '5': case '6': case '7': case '8': case '9':
		SCM_PORT_UNGETC(port, c);
                return read_number_or_symbol(port);
	    case '+': case '-':
                SCM_PORT_UNGETC(port, c);
                return read_number_or_symbol(port);
            case '\'':
                return read_quote(port, SCM_QUOTE);
	    case '`':
		return read_quote(port, SCM_QUASIQUOTE);
	    case ',':
		{
		    SCM_PORT_GETC(port, c1);
		    if (c1 == EOF) {
			SigScm_Error("EOF in unquote\n");
		    } else if (c1 == '@') {
			return read_quote(port, SCM_UNQUOTE_SPLICING);
		    } else {
			SCM_PORT_UNGETC(port, c1);
			return read_quote(port, SCM_UNQUOTE);
		    }
		}
            case '#':
                {
		    SCM_PORT_GETC(port, c1);
                    switch (c1) {
                        case 't': case 'T':
                            return SCM_TRUE;
                        case 'f': case 'F':
                            return SCM_FALSE;
			case '(':
			    return ScmOp_list_to_vector(read_list(port, ')'));
			case '\\':
			    return read_char(port);
                        case EOF:
                            SigScm_Error("end in #\n");
                        default:
                            SigScm_Error("Unsupported #\n");
                    }
                }
		break;

	    /* Error sequence */
            case ')':
                SigScm_Error("invalid close parenthesis\n");
                break;
            case EOF:
                return SCM_EOF;

            default:
                SCM_PORT_UNGETC(port, c);
                return read_symbol(port);
        }
    }
}

static ScmObj read_list(ScmObj port, int closeParen)
{
#if DEBUG_PARSER
    printf("read_list\n");
#endif

    ScmObj list_head = SCM_NIL;
    ScmObj list_tail = SCM_NIL;
    ScmObj item = SCM_NIL;

    int c = 0;
    while (1) {
        c = skip_comment_and_space(port);

#if DEBUG_PARSER
        printf("read_list c = [%c]\n", c);
#endif

        if (c == EOF) {
            SigScm_Error("EOF inside list.\n");
        } else if (c == closeParen) {
            return list_head;
        } else if (c == '.') {
	    int c2 = 0;
	    SCM_PORT_GETC(port, c2);
#if DEBUG_PARSER
        printf("read_list process_dot c2 = [%c]\n", c2);
#endif
            if (isspace(c2)) {
                ScmObj cdr = read_sexpression(port);
                if (SCM_NULLP(list_tail))
                    SigScm_Error(".(dot) at the start of the list.\n");

		c = skip_comment_and_space(port);
		if (c != ')')
		    SigScm_Error("bad dot syntax\n");

                SCM_SETCDR(list_tail, cdr);
		return list_tail;
            }
        } else {
            SCM_PORT_UNGETC(port, c);
            item = read_sexpression(port);
        }

        /* Append item to the list_tail. */
        if (SCM_NULLP(list_tail)) {
            /* create new list */
            list_head = Scm_NewCons(item, SCM_NIL);
            list_tail = list_head;
        } else {
            /* update list_tail */
            SCM_SETCDR(list_tail, Scm_NewCons(item, SCM_NIL));
            list_tail = SCM_CDR(list_tail);
        }
    }
}

static ScmObj read_char(ScmObj port)
{
#if DEBUG_PARSER
    printf("read_char\n");
#endif
   
    char *ch = read_char_sequence(port);

#if DEBUG_PARSER
    printf("ch = %s\n", ch);
#endif

    /* check special sequence "space" and "newline" */
    if (strcmp(ch, "space") == 0) {
	ch[0] = ' ';
	ch[1] = '\0';
    } else if (strcmp(ch, "newline") == 0) {
	ch[0] = '\n';
	ch[1] = '\0';
    }

    return Scm_NewChar(ch);
}

static ScmObj read_string(ScmObj port)
{
    char  stringbuf[1024];
    int   stringlen = 0;
    char *dst = NULL;
    int   c = 0;

#if DEBUG_PARSER
    printf("read_string\n");
#endif

    while (1) {
	SCM_PORT_GETC(port, c);

#if DEBUG_PARSER
        printf("read_string c = %c\n", c);
#endif

        switch (c) {
            case EOF:
                SigScm_Error("EOF in the string\n");
                break;
            case '\"':
                {
                    stringbuf[stringlen] = '\0';
                    dst = (char *)malloc(strlen(stringbuf) + 1);
                    strcpy(dst, stringbuf);
                    return Scm_NewString(dst);
                }
            case '\\':
                {
		    /*
		     * (R5RS) 6.3.5 String
		     * A double quote can be written inside a string only by
		     * escaping it with a backslash (\).
		     */
		    SCM_PORT_GETC(port, c);
		    switch (c) {
			case '\"':
			    stringbuf[stringlen] = c;
			    break;
			case 'n':
			    stringbuf[stringlen] = '\n';
			    break;
			case 't':
			    stringbuf[stringlen] = '\t';
			    break;
			default:
			    stringbuf[stringlen] = '\\';
			    stringbuf[++stringlen] = c;
			    break;			    
		    }
		    stringlen++;

#if DEBUG_PARSER
		    printf("read_string following \\ : c = %c\n", c);
#endif
                }
		break;
            default:
                stringbuf[stringlen] = c;
                stringlen++;
                break;
        }
    }
}

static ScmObj read_symbol(ScmObj port)
{
    char  *sym_name = read_char_sequence(port);
    ScmObj sym = Scm_Intern(sym_name);
    free(sym_name);

#if DEBUG_PARSER
    printf("read_symbol\n");
#endif

    return sym;
}

static ScmObj read_number_or_symbol(ScmObj port)
{
    int i = 0;
    int is_str  = 0;
    int str_len = 0;
    char  *str = NULL;
    ScmObj obj = SCM_NIL;

#if DEBUG_PARSER
    printf("read_number_or_symbol\n");
#endif

    /* read char sequence */
    str = read_char_sequence(port);
    if (strlen(str) == 1
	&& (strcmp(str, "+") == 0 || strcmp(str, "-") == 0))
    {
#if DEBUG_PARSER
	printf("determined as symbol : %s\n", str);
#endif

	obj = Scm_Intern(str);
	free(str);
	return obj;
    }

    /* check whether each char is the digit */
    for (i = 0; i < str_len; i++) {
	if (i == 0 && (str[i] == '+' || str[i] == '-'))
	    continue;

	if (!isdigit(str[i])) {
	    is_str = 1;
	    break;
	}
    }

    /* if symbol, then intern it. if number, return new int obj */
    if (is_str) {
#if DEBUG_PARSER
	printf("determined as symbol : %s\n", str);
#endif
	obj = Scm_Intern(str);
    } else {
#if DEBUG_PARSER
	printf("determined as num : %s\n", str);
#endif
	obj = Scm_NewInt((int)atof(str));
    }
    free(str);

    return obj;
}


static char *read_char_sequence(ScmObj port)
{
    char  stringbuf[1024];
    int   stringlen = 0;
    int   c = 0;
    char *dst = NULL;

    while (1) {
	SCM_PORT_GETC(port, c);

#if DEBUG_PARSER
	printf("c = %c\n", c);
#endif

        switch (c) {
            case EOF:
                SigScm_Error("EOF in the char sequence.\n");
                break;

            case '(':  case ')':  case ' ':  case ';':
            case '\n': case '\t': case '\"': case '\'':
                SCM_PORT_UNGETC(port, c);
                stringbuf[stringlen] = '\0';
		dst = (char *)malloc(strlen(stringbuf) + 1);
                strcpy(dst, stringbuf);
                return dst;

            default:
                stringbuf[stringlen] = (char)c;
                stringlen++;
                break;
        }
    }
}

static ScmObj read_quote(ScmObj port, ScmObj quoter)
{
    return Scm_NewCons(quoter, read_sexpression(port));
}

