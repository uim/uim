/*
 *  $Id:$
 *  Copyright (c) 2003,2004 Masahito Omote <omote@utyuuzin.net>
 *                2005-2013 uim Project https://github.com/uim/uim
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
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS IS'' AND
 *  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 *  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 *  ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE
 *  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 *  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 *  OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 *  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 *  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 *  OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 *  SUCH DAMAGE.
 */

/*
 * 2004-10-15 Takuro Ashie <ashie@good-day.co.jp>
 *   * 将来のplugin化のためのメモ書きを追加
 *     (元が日本語だったので，一応日本語で)
 * 2004-09-30 Takuro Ashie <ashie@good-day.co.jp>
 *   * uim-pref用に修正
 */

#ifndef UIM_DICT_WORD_H
#define UIM_DICT_WORD_H

/*
 *  enumではなく文字列に変更?
 */
typedef enum _uim_word_type {
    WORD_TYPE_ANTHY,
    WORD_TYPE_CANNA,
    WORD_TYPE_SKK,
    WORD_TYPE_PRIME,
    WORD_TYPE_UNKNOWN
} uim_word_type;

/*
 *  - ハッシュテーブルか何かでデータを管理し，charsetとphonとdesc以外の
 *    全てのメンバーを隠したほうが良いかもしれない．
 *    全てのデータは関数経由でアクセス
 *  - キャラクターセットはオリジナルの状態を保持し，使用時に適宜変換すること
 *  - とりあえず現時点ではGLib依存を排除すること
 *    (現時点では)このオブジェクトを使用するのはGtk+フロントエンドのみとは
 *    限らない．
 *  - 他の言語の場合はその言語上でデータ構造を記述した方が楽であり，またコード
 *    量はそう多くなく，おそらく全ての変換エンジンの分を含めても1日もあれば記述
 *    できると思われるので，再利用性を考慮するのは無意味かもしれない．
 */
typedef struct _uim_word {
    uim_word_type   type;           /* 形式: word_type*/
				    /* FIXME! enumよりも文字列の方が良いかも
				       (plugin化を考慮) */

    /* common fields */
    char       *charset;            /* 文字列のキャラクターセット */

    char       *phon;		    /* 読み */
    char       *desc;	            /* 漢字 */
    char       *cclass_code;	    /* 品詞 現在は(コードではなく)品詞名(狭義)
				       が格納されている */
    char       *cclass_native;	    /* native cclass code (like #T01). */
    int	        freq;		    /* 頻度 */

    /* SKK specific fields */       /* FIXME! 共用体か継承で分離 */
    int	        okuri;	            /* 送りの有無 (boolean) */
    char       *following_kana;     /* 送り有りの場合に次に続く送りがな XXX */
    char       *annotation;	    /* アノテーション */

    struct _uim_word *next;
} uim_word;

/* 仕様が決まってないので省略せず */
/* prototypes */
void      word_append    (uim_word **head, uim_word_type type,
			  char *charset,
			  char *phon, char *desc,
			  const char *cclass_code,
			  const char *cclass_native,
			  int freq,
			  int okuri, char *annotation);
void      word_free_list (uim_word *head);
uim_word *word_last      (uim_word *list);

uim_word_type dict_identifier_to_word_type(char *identifier);
int dict_identifier_to_support_type(char *identifier);
#endif /* UIM_DICT_WORD_H */
