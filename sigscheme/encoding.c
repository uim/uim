/*===========================================================================
 *  FileName : encoding.c
 *  About    : handling encoding
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
static int eucjp_strlen(const char *p);
static const char* eucjp_str_startpos(const char *p, int k);
static const char* eucjp_str_endpos(const char *p, int k);

/*=======================================
  Function Implementations
=======================================*/
int SigScm_default_encoding_strlen(const char *str)
{
#if USE_EUCJP
    return eucjp_strlen(str);
#endif
}

const char* SigScm_default_encoding_str_startpos(const char *str, int k)
{
#if USE_EUCJP
    return eucjp_str_startpos(str, k);
#endif    
}

const char* SigScm_default_encoding_str_endpos(const char *str, int k)
{
#if USE_EUCJP
    return eucjp_str_endpos(str, k);
#endif    
}

static int eucjp_strlen(const char *str)
{
    int len = 0;
    const unsigned char *cur = (const unsigned char *)str;
    while (*cur) {
	if (*cur > 127) {
	    /* 2 bytes */
	    cur++;
	}

	cur++;
	len++;
    }

    return len;
}

static const char* eucjp_str_startpos(const char *str, int k)
{
    int len = 0;
    const unsigned char *cur = (const unsigned char *)str;
    while (*cur) {
	if (len == k)
	    return (const char *)cur;

	if (*cur > 127) {
	    /* 2 bytes */
	    cur++;
	}

	cur++;
	len++;
    }

    SigScm_Error("eucjp_str_startpos : unreachable point\n");
    return NULL;
}

static const char* eucjp_str_endpos(const char *str, int k)
{
    int len = 0;
    const unsigned char *cur = (const unsigned char *)str;
    while (*cur) {
	if (*cur > 127) {
	    /* 2 bytes */
	    cur++;
	}

	cur++;
	len++;

	if (len == k + 1)
	    return (const char *)cur;
    }
    
    if (len == k + 1)
	return (const char *)cur;

    SigScm_Error("eucjp_str_startpos : unreachable point\n");
    return NULL;
}
