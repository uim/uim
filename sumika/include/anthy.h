/*
 *  $Id:$
 *  Copyright (c) 2003,2004 Masahito Omote <omote@utyuuzin.net>
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
 *  THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 *  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 *  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 *  ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 *  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 *  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 *  OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 *  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 *  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 *  OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 *  SUCH DAMAGE.
 */

#ifndef __ANTHY_H__
#define __ANTHY_H__

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include "word.h"

typedef enum {
    ANTHY_MEISHI_KANKUZYOSHI_SETUZOKU = 1 << 0,
    ANTHY_MEISHI_GOKANNOMIDE_BUNSETU  = 1 << 1,
    ANTHY_MEISHI_SURU_SETUZOKU        = 1 << 2,
    ANTHY_MEISHI_SA_SETUZOKU          = 1 << 3,
    ANTHY_MEISHI_NA_SETUZOKU          = 1 << 4
} touroku_meishi_attr_list;

typedef enum {
    ANTHY_FUKUSHI_GOKANNOMIDE_BUNSETU = 1 << 0,
    ANTHY_FUKUSHI_SURU_SETUZOKU       = 1 << 1,
    ANTHY_FUKUSHI_TARU_SETUZOKU       = 1 << 2,
    ANTHY_FUKUSHI_TO_SETUZOKU         = 1 << 3
} touroku_fukushi_attr_list;

/* Prototypes */
int   anthydic_init  (void);
int   anthydic_close (void);

int   read_anthy_priv_dic_list      (word **);
int   add_anthy_priv_dic_with_flags (char *, char *, char *, int);
int   delete_anthy_priv_dic         (char *, char *, char *);

char *find_meishi_code_from_flag    (const int);
char *find_fukushi_code_from_flag   (const int);
char *find_hinshi_name_from_code    (const char*);

GList *anthy_read_privatediclist(void);
#endif /* __ANTHY_H__ */
