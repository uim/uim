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

#ifndef __WORD_H__
#define __WORD_H__
typedef enum _word_type {
    WORD_TYPE_ANTHY,
    WORD_TYPE_CANNA,
    WORD_TYPE_SKK,
    WORD_TYPE_PRIME
} word_type;

typedef struct _word {
    word_type   type;               /* 形式: word_type*/

    /* common fields */
    char       *phon;		    /* 読み */
    char       *desc;	            /* 漢字 */
    char       *cclass_code;	    /* 品詞コード or 品詞 */
    int	        freq;		    /* 頻度 */

    /* SKK specific fields */
    int	        okuri;	            /* 送りの有無 (boolean) */
    char       *following_kana;     /* 送り有りの場合に次に続く送りがな XXX */
    char       *annotation;	    /* アノテーション */

    struct _word *next;
} word;

/* 仕様が決まってないので省略せず */
/* prototypes */
void  word_append    (word ** head, word_type type,
		      char *phon, char *desc,
		      char *cclass_code, int freq,
		      int okuri, char *annotation);
void  word_free_list (word *head);
word *word_last      (word *list);
#endif /* __WORD_H__ */
