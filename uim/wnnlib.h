/*
  Copyright (c) 2008-2013 uim Project https://github.com/uim/uim

  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions
  are met:

  1. Redistributions of source code must retain the above copyright
  notice, this list of conditions and the following disclaimer.
  2. Redistributions in binary form must reproduce the above copyright
  notice, this list of conditions and the following disclaimer in the
  documentation and/or other materials provided with the distribution.
  3. Neither the name of authors nor the names of its contributors
  may be used to endorse or promote products derived from this software
  without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS IS'' AND
  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE
  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
  OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
  OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
  SUCH DAMAGE.

*/
/* $Id: wnnlib.h,v 10.8 1999/05/25 06:21:10 ishisone Exp $ */

/*
 *	wnnlib.h -- wnnlib 用ヘッダファイル (Wnn Version4/6 対応版)
 *		version 5.0
 *		ishisone@sra.co.jp
 */

/*
 * Copyright (c) 1989  Software Research Associates, Inc.
 * Copyright (c) 1998  MORIBE, Hideyuki
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Software Research Associates not be
 * used in advertising or publicity pertaining to distribution of the
 * software without specific, written prior permission.  Software Research
 * Associates makes no representations about the suitability of this software
 * for any purpose.  It is provided "as is" without express or implied
 * warranty.
 *
 * Author:  Makoto Ishisone, Software Research Associates, Inc., Japan
 *          MORIBE, Hideyuki
 */

#ifndef _wnnlib_h
#define _wnnlib_h

#include <config.h>

#include	<commonhd.h>
#include	<jllib.h>
#include	<wnnerror.h>

#include "uim.h"
#include "uim-scm.h"

#ifndef WCHAR_DEFINED
#define WCHAR_DEFINED
#undef wchar
typedef unsigned short wchar;
#endif


/* 定数 */
#define JC_FORWARD	1
#define JC_BACKWARD	0
#define JC_NEXT		0
#define JC_PREV		1
#define JC_HIRAGANA	0
#define JC_KATAKANA	1

/* エラー番号 */
#define JE_NOERROR		0
#define JE_WNNERROR		1	/* jllib のエラー */
#define JE_NOCORE		2	/* メモリが確保できない */
#define JE_NOTCONVERTED		3	/* 対象文節がまだ変換されていない */
#define JE_CANTDELETE		4	/* バッファの先頭の前、あるいは
					 * 最後の次の文字を削除しようとした */
#define JE_NOSUCHCLAUSE		5	/* 指定された番号の文節が存在しない */
#define JE_CANTSHRINK		6	/* 1 文字の文節を縮めようとした */
#define JE_CANTEXPAND		7	/* 最後の文節を伸ばそうとした */
#define JE_NOCANDIDATE		8	/* 次候補がない */
#define JE_NOSUCHCANDIDATE	9	/* 指定された番号の候補が存在しない */
#define JE_CANTMOVE		10	/* バッファの先頭の前、あるいは
					 * 最後の次に移動しようとした */
#define JE_CLAUSEEMPTY		11	/* 空の文を変換しようとした */
#define JE_ALREADYFIXED		12	/* すでに確定されている文に対して
					 * 操作を行なった */

/* エラー番号 */
extern int	jcErrno;	/* エラー番号 */

/* データタイプ */

/* 各小文節の情報 */
typedef struct {
	wchar	*kanap;		/* 読み文字列 */
	wchar	*dispp;		/* 表示文字列 */
	char	conv;		/* 変換済みか */
				/* 0: 未変換 1: 変換済 -1: で疑似変換 */
	char	ltop;		/* 大文節の先頭か? */
} jcClause;


/* 作業域 */
typedef struct {
    /* public member */
	int		nClause;	/* 文節数 */
	int		curClause;	/* カレント文節番号 */
	int		curLCStart;	/* カレント大文節開始文節番号 */
	int		curLCEnd;	/* カレント大文節終了文節番号 */
	wchar		*kanaBuf;	/* かなバッファ */
	wchar		*kanaEnd;
	wchar		*displayBuf;	/* ディスプレイバッファ */
	wchar		*displayEnd;
	jcClause	*clauseInfo;	/* 文節情報 */
	struct wnn_buf	*wnn;
    /* private member */
	int		fixed;		/* 確定されたかどうか */
	wchar		*dot;		/* ドットの位置 */
	int		candKind;	/* 大文節の全候補か小文節の候補かを
					   表すフラグ */
	int		candClause;	/* 全候補をとっている文節番号 */
	int		candClauseEnd;	/* 大文節の全候補の時、終了文節番号 */
	int		bufferSize;	/* kanaBuf/displayBuf の大きさ */
	int		clauseSize;	/* clauseInfo の大きさ */
} jcConvBuf;

struct wnn_buf *jcOpen(char *, char *, int, char *, void (*)(), int (*)(), int);
struct wnn_buf *jcOpen2(char *, char *, int, char *, char *, void (*)(), int (*)(), int);
void jcClose(struct wnn_buf *);
int jcIsConnect(struct wnn_buf *);
jcConvBuf *jcCreateBuffer(struct wnn_buf *, int, int);
int jcDestroyBuffer(jcConvBuf *, int);
int jcClear(jcConvBuf *);
int jcInsertChar(jcConvBuf *, int);
int jcDeleteChar(jcConvBuf *, int);
int jcKillLine(jcConvBuf *);
int jcConvert(jcConvBuf *, int, int, int);
int jcUnconvert(jcConvBuf *);
int jcCancel(jcConvBuf *);
int jcExpand(jcConvBuf *, int, int);
int jcShrink(jcConvBuf *, int, int);
int jcKana(jcConvBuf *, int, int);
int jcFix(jcConvBuf *);
int jcFix1(jcConvBuf *);
int jcNext(jcConvBuf *, int, int);
int jcCandidateInfo(jcConvBuf *, int, int *, int *);
int jcGetCandidate(jcConvBuf *, int, wchar *, int);
int jcSelect(jcConvBuf *, int);
int jcDotOffset(jcConvBuf *);
int jcIsConverted(jcConvBuf *, int);
int jcMove(jcConvBuf *, int, int);
int jcTop(jcConvBuf *);
int jcBottom(jcConvBuf *);
int jcChangeClause(jcConvBuf *, wchar *);
int jcSaveDic(jcConvBuf *);

#endif /* _wnnlib_h */
