/*
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
 *
 * This code is based on canna's code. For more information about canna,
 * visit http://canna.sourceforge.jp/ . Canna license is as follows,
 *
 * Copyright (c) 2002 Canna Project. All rights reserved.
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without
 * fee, provided that the above copyright notice appear in all copies
 * and that both that copyright notice and this permission notice
 * appear in supporting documentation, and that the name of the
 * author and contributors not be used in advertising or publicity
 * pertaining to distribution of the software without specific, written
 * prior permission.  The author and contributors no representations
 * about the suitability of this software for any purpose.  It is
 * provided "as is" without express or implied warranty.
 *
 * THE AUTHOR AND CONTRIBUTORS DISCLAIMS ALL WARRANTIES WITH REGARD TO
 * THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
 * FITNESS, IN NO EVENT SHALL THE AUTHOR AND CONTRIBUTORS BE LIABLE FOR
 * ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
 * RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF
 * CONTRACT, NEGLIGENCE OR OTHER TORTUOUS ACTION, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * Copyright 1994 NEC Corporation, Tokyo, Japan.
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without
 * fee, provided that the above copyright notice appear in all copies
 * and that both that copyright notice and this permission notice
 * appear in supporting documentation, and that the name of NEC
 * Corporation not be used in advertising or publicity pertaining to
 * distribution of the software without specific, written prior
 * permission.	NEC Corporation makes no representations about the
 * suitability of this software for any purpose.  It is provided "as
 * is" without express or implied warranty.
 *
 * NEC CORPORATION DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN
 * NO EVENT SHALL NEC CORPORATION BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF
 * USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
 * OTHER TORTUOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 */

#include <config.h>

#include <string.h>
#include "canna-cclass.h"

category_code substantive_code[]= {
  { "#T00", "名詞(な,さ,する,語幹,格助接続)"  , "共通,孝行", 31, 3 },
  { "#T01", "名詞(な,さ,する,語幹,格助接続△)", "", 31, 3 },
  { "#T02", "名詞(な,さ,する,語幹)"           , "", 30, 3 },
  { "#T03", "名詞(な,さ,する,格助接続)"       , "きちきち", 29, 3 },
  { "#T04", "名詞(な,さ,する)"                , "", 28, 3 },
  { "#T05", "名詞(な,さ,語幹,格助接続)"       , "質実,幸運,哀れ,簡潔,謙虚,貴重", 27, 3 },
  { "#T06", "名詞(な,さ,語幹,格助接続△)"     , "一様,奇異,健やか,明快", 27, 3 },
  { "#T08", "名詞(な,さ,格助接続△)"          , "意外,懸命", 25, 3 },
  { "#T09", "名詞(な,さ)"                     , "静か,気軽", 24, 3 },
  { "#T10", "名詞(な,する,語幹,格助接続)"     , "安心,浮気,横着,感心", 23, 3 },
  { "#T11", "名詞(な,する,語幹,格助接続△)"   , "格段,格別,大層,直接", 23, 3 },
  { "#T12", "名詞(な,する,語幹)"              , "存分", 22, 3 },
  { "#T13", "名詞(な,する,格助接続△)"        , "大慌て", 21, 3 },
  { "#T14", "名詞(な,する)"                   , "", 20, 3 },
  { "#T15", "名詞・副詞(な,語幹,格助接続)"    , "同じ?,色々", 19, 3 },
  { "#T18", "名詞(な,格助接続△)"             , "当り前,甘口,安穏,内気", 17, 3 },
  { "#T19", "名詞(な)"                        , "危なげ,嫌み,大ざっぱ", 16, 3 },
  { "#T20", "名詞(さ,する,語幹,格助接続)"     , "", 15, 3 },
  { "#T21", "名詞(さ,する,語幹,格助接続△)"   , "", 15, 3 },
  { "#T22", "名詞(さ,する,語幹)"              , "", 14, 3 },
  { "#T23", "名詞(さ,する,格助接続△)"        , "", 13, 3 },
  { "#T24", "名詞(さ,する)"                   , "", 12, 3 },
  { "#T25", "名詞(さ,語幹,格助接続)"          , "平常", 11, 3 },
  { "#T26", "名詞(さ,語幹,格助接続△)"        , "", 11, 3 },
  { "#T27", "名詞(さ,語幹)"                   , "", 10, 3 },
  { "#T28", "名詞(さ,格助接続△)"             , "", 9,  3 },
  { "#T29", "名詞(さ)"                        , "", 8,  3 },
  { "#T30", "名詞(する,語幹,格助接続)"        , "哀願,愛好,挨拶,安置", 7,  3 },
  { "#T32", "名詞(する,語幹)"                 , "", 6,  3 },
  { "#T33", "名詞(する,格助接続△)"           , "", 5,  3 },
  { "#T34", "名詞(する)"                      , "", 4,  3 },
  { "#T35", "名詞(語幹,格助接続)"             , "合印,合鍵,山,会社", 3,  3 },
  { "#T39", "名詞"                            , "", 0,  3 },
  { "#CN",  "地名"                            , "東京", 0, 3 },
  { "#CNS", "地名(接尾語)"                    , "東京都", 0, 3 },
  { "#JCN", "地名(姓も可)"                    , "長崎", 0, 3 },
  { "#JN",  "人名"                            , "菅井,勝", 0, 3 },
  { "#JNS", "人名(姓)"                        , "蔵本", 0, 3 },
  { "#JNM", "人名(名)"                        , "栄二", 0, 1 },
  { "#KK",  "会社/団体"                       , "日本電気", 0, 3 },
};

category_code adverb_code[] = {
  { "#T07", "副詞(な,さ,語幹)"                , "十分", 26, 3 },
  { "#T16", "副詞(な,語幹,格助接続△)"        , "案外,生憎,かなり", 19, 3 },
  { "#T17", "副詞(な,語幹)"                   , "やたら,ぴったり,意識的,印象的,科学的", 18, 3 },
  { "#T31", "副詞(する,語幹,格助接続△)"      , "ちょっと", 7,  3 },
  { "#T36", "副詞(語幹,格助接続△)"           , "よほど,たぶん,折からの", 3,  3 },
  { "#T37", "副詞(語幹)"                      , "だいぶ,はなはだ", 2,  3 },
  { "#T38", "副詞(格助接続△)"                      , "", 1,  3 },

  { "#F00", "副詞(と,たる,する,語幹)", "決然,ぐるぐる,ひそひそ,青々", 15, 3 },
  { "#F01", "副詞(と,たる,する)"     , "判然,依然,公然,雑然,釈然,泰然", 14, 3 },
  { "#F02", "副詞(と,たる,語幹)"     , "猛然,断然,断固,堂々,延々,延々", 13, 3 },
  { "#F03", "副詞(と,たる,)"         , "広漠,安閑,憮然,唖然,黙々,切切", 12, 3 },
  { "#F04", "副詞(と,する,語幹)"     , "ふっくら,ゆっくり", 11, 3 },
  { "#F05", "副詞(と,する)"          , "晴れ晴れ,広々,ごわごわ,ごつごつ", 10, 3 },
  { "#F06", "副詞(と,語幹)"          , "全然,突然,度々,重ね重ね,まざまざ,むざむざ",  9, 3 },
  { "#F07", "副詞(と)"               , "",  8, 3 },
  { "#F08", "副詞(たる,する,語幹)"   , "",  7, 3 },
  { "#F09", "副詞(たる,する)"        , "",  6, 3 },
  { "#F10", "副詞(たる,語幹)"        , "",  5, 3 },
  { "#F11", "副詞(たる)"             , "確",  4, 3 },
  { "#F12", "副詞(する,語幹)"        , "そっと,ぞっと,ほっと,ふと,ほっと,むっと",  3, 3 },
  { "#F13", "副詞(する)"             , "",  2, 3 },
  { "#F14", "副詞(語幹)"             , "相変わらず,敢えて,飽くまで",  1, 3 },
  { "#F15", "副詞(未定義)"           , "",  0, 3 },
};

/**
 * K5,  か行5段,              置く
 * K5r, か行5段:連用形が名詞, 書く
 * C5r, 行く5段:連用形が名詞, 行く
 * G5,  が行5段,              仰ぐ
 * G5r, が行5段:連用形が名詞, 急ぐ
 * S5,  さ行5段,              帰す
 * S5r, さ行5段:連用形が名詞, 移す
 * T5,  た行5段,              絶つ
 * T5r, た行5段:連用形が名詞, 打つ
 * N5,  な行5段,              死ぬ
 * B5,  ば行5段,              転ぶ
 * B5r, ば行5段:連用形が名詞, 遊ぶ
 * M5,  ま行5段,              住む
 * M5r, ま行5段:連用形が名詞, 編む
 * R5,  ら行5段,              威張る
 * R5r, ら行5段:連用形が名詞, 謝る
 * L5,  ラ行5段:命令形がイ,   いらっしゃる
 * W5,  わ行5段,              言う
 * W5r, わ行5段:連用形が名詞, 扱う
 * U5,  乞う5段,              乞う
 * U5r, 乞う5段:連用形が名詞, 問う
 * KS,  上下1段,              降りる
 *                            与える
 * KSr, 上下1段:語幹が名詞,   生きる
 *                            預ける
 * KX,  カ変活用動詞,         来る
 * SX,  サ変活用動詞,         関する
 * ZX,  ザ変活用動詞,         感ずる
 * NZX, ンザ変活用動詞,       重んずる
 **/

category_code verb_code[]= {
  { "#K5",  "か行5段"       , "置/か(ない)/き(ます)/く/く(こと)/け(ば)/こ(う)", 0, 3 },
  { "#K5r", "か行5段:連名"  , "書/か(ない)/き(ます)/く/く(こと)/け(ば)/こ(う)", 0, 3 },
  { "#C5r", "行く5段"       , "行/か(ない)/き(ます)/く/く(こと)/け(ば)/こ(う)", 0, 3 },
  { "#G5" , "が行5段"       , "仰/が(ない)/ぎ(ます)/ぐ/ぐ(こと)/げ(ば)/ご(う)", 0, 3 },
  { "#G5r", "が行5段:連名"  , "急/が(ない)/ぎ(ます)/ぐ/ぐ(こと)/げ(ば)/ご(う)", 0, 3 },
  { "#S5" , "さ行5段"       , "帰/さ(ない)/し(ます)/す/す(こと)/せ(ば)/そ(う)", 0, 3 },
  { "#S5r", "さ行5段:連名"  , "移/さ(ない)/し(ます)/す/す(こと)/せ(ば)/そ(う)", 0, 3 },
  { "#T5" , "た行5段"       , "絶/た(ない)/ち(ます)/つ/つ(こと)/て(ば)/と(う)", 0, 3 },
  { "#T5r", "た行5段:連名"  , "打/た(ない)/ち(ます)/つ/つ(こと)/て(ば)/と(う)", 0, 3 },
  { "#N5",  "な行5段"       , "死/な(ない)/に(ます)/ぬ/ぬ(こと)/ね(ば)/の(う)", 0, 3 },
  { "#N5r", "な行5段:連名"  , "", 0, 1 },
  { "#B5",  "ば行5段"       , "転/ば(ない)/び(ます)/ぶ/ぶ(こと)/べ(ば)/ぼ(う)", 0, 3 },
  { "#B5r", "ば行5段:連名"  , "遊/ば(ない)/び(ます)/ぶ/ぶ(こと)/べ(ば)/ぼ(う)", 0, 3 },
  { "#M5",  "ま行5段"       , "住/ま(ない)/み(ます)/む/む(こと)/め(ば)/も(う)", 0, 3 },
  { "#M5r", "ま行5段:連名"  , "編/ま(ない)/み(ます)/む/む(こと)/め(ば)/も(う)", 0, 3 },
  { "#R5" , "ら行5段"       , "威張/ら(ない)/り(ます)/る/る(こと)/れ(ば)/ろ(う)", 0, 3 },
  { "#R5r", "ら行5段:連名"  , "謝/ら(ない)/り(ます)/る/る(こと)/れ(ば)/ろ(う)", 0, 3 },
  { "#L5",  "ラ行5段:命令イ", "いらっしゃ/ら(ない)/い(ます)/る/る(こと)/れ(ば)/ろ(う)", 0, 3 },
  { "#W5",  "わ行5段"       , "言/わ(ない)/い(ます)/う/う(こと)/え(ば)/お(う)", 0, 3 },
  { "#W5r", "わ行5段:連名"  , "扱/わ(ない)/い(ます)/う/う(こと)/え(ば)/お(う)", 0, 3 },
  { "#U5" , "乞う5段"       , "乞/わ(ない)/い(ます)/う/う(こと)/え(ば)/お(う)", 0, 3 },
  { "#U5r", "乞う5段:連名"  , "問/わ(ない)/い(ます)/う/う(こと)/え(ば)/お(う)", 0, 3 },

  { "#KS",  "上下1段"       , "降,与/り,え(ない)/り,え(ます)/りる,える/りる,える(こと)/りれ,えれ(ば)/りよ,えよ(う)", 0, 3 },
  { "#KSr", "上下1段:語幹名", "生,預/き,け(ない)/き,け(ます)/きる,ける/きる,ける(こと)/きれ,けれ(ば)/きよ,けよ(う)", 0, 3 },
  { "#KX",  "カ変活用動詞"  , "来/こ(ない)/き(ます)/くる/くる(こと)/くれ(ば)/こよ(う)", 0, 2 },
  { "#SX",  "サ変活用動詞"  , "関/し(ない)/し(ます)/する/する(こと)/すれ(ば)/せよ", 0, 3 },
  { "#ZX",  "ザ変活用動詞"  , "感/じ(ない)/じ(ます)/ずる/ずる(こと)/ずれ(ば)/ぜよ", 0, 3 },
  { "#NZX", "ンザ変活用動詞", "重ん/が(ない)/ぎ(ます)/ぐ/ぐ(こと)/げ(ば)/ご(う)", 0, 2 },
};

/***
 * KY,    形                   美しい
 * KYT,   名ナノ・形    四角い,黄色い
 * KYna,  形            小さい,大きい  (活用に[な(連体用法ナ]がある形容詞)
 * KYmi,  形            強い、重い     (みで終ると名詞に転成する形容詞)
 * KYme,  形            強め、長め     (めで終ると形容動詞に転成する形容詞)
 * KYmime,形            強み、強め     KYmi, KYme の両方の性質を合わせ持つ
 * KYU,   形:ウ音便     美しゅう
 **/

category_code adjective_code[] = {
  { "#KY",     "形容詞"           , "美しい", 0, 3 },
  { "#KYT",    "形容詞:名ナノ"    , "四角い, 黄色い", 0, 3 },
  { "#KYna",   "形容詞:連体用法ナ", "小さい, 大きい", 0, 3 },
  { "#KYmi",   "形容詞:み"        , "強い, 重い", 0, 3 },
  { "#KYme",   "形容詞:め"        , "強め, 長め", 0, 3 },
  { "#KYmime", "形容詞:みめ"      , "強み, 強め", 0, 3 },
  { "#KYU",    "形容詞:ウ音便"    , "美しゅう", 0, 3 },
};

category_code etc_code[] = {
  { "#KJ",    "一文字漢字"                 , "単漢字変換用", 0, 3 },
/*
  { "#CN",    "地名"                       , "東京", 0, 3 },
  { "#CNS",   "地名(接尾語)"               , "東京都", 0, 3 },
  { "#JCN",   "地名(姓も可)"               , "長崎", 0, 3 },
  { "#JN",    "人名"                       , "菅井,勝", 0, 3 },
  { "#JNS",   "人名(姓)"                   , "蔵本", 0, 3 },
  { "#JNM",   "人名(名)"                   , "栄二", 0, 1 },
  { "#KK",    "会社/団体"                  , "日本電気", 0, 3 },
*/
  { "#CJ",    "接続詞/感動詞/連語"         , "", 0, 3 },
  { "#RT",    "連体詞"                     , "", 0, 3 },
  { "#OKX",   "動詞の丁寧表現の語幹"       , "お聞き", 0, 3 },
  { "#NN",    "数詞:一般"                  , "数,幾", 0, 3 },
  { "#N00",   "数詞:x万,x億,x兆"           , "x万,x億,x兆", 0, 3 },
  { "#N01",   "数詞:千,二千,…,数千"       , "千,二千,…,数千", 0, 3 },
  { "#N02",   "数詞:百,二百,…,数百"       , "百,二百,…,数百", 0, 3 },
  { "#N03",   "数詞:十,二十,…,数十"       , "十,二十,…,数十", 0, 3 },
  { "#KN",    "形式名詞"                   , "あと/うち/おり/こと/", 0, 3 },
  { "#TKN",   "新形式名詞"                 , "はず/わけ", 0, 2 },
  { "#JTNO",  "準体言名詞"                 , "くらい/ぐらい/こそ", 0, 2 },
  { "#PRE",   "接頭語"                     , "", 0, 3 },
  { "#CNPRE", "接頭語:一般"                , "", 0, 3 },
  { "#JNPRE", "接頭語:地名"                , "", 0, 2 },
  { "#NNPRE", "接頭語:数詞"                , "", 0, 3 },
  { "#SNPRE", "接頭語:サ変名詞"            , "", 0, 2 },
  { "#SUN",   "接尾語:一般"                , "", 0, 2 },
  { "#CNSUC1","接尾語:地名 1"              , "", 0, 3 },
  { "#CNSUC2","接尾語:地名 2"              , "", 0, 3 },
  { "#JNSUC" ,"接尾語:人名"                , "", 0, 3 },
  { "#N2T30", "接尾語:サ変名詞化"          , "(名)+化,視", 0, 3 },
  { "#N2T35", "接尾語:名詞化"              , "", 0, 3 },
  { "#D2T35", "接尾語:動詞連用形+名詞化"   , "(動詞連用形)+っぱなし", 0, 3 },
  { "#D2T16", "接尾語:形容動詞化"          , "(動詞連用形)+がち", 0, 3 },
  { "#ND2KY", "接尾語:形容詞化"            , "(名,動用)+がましい,強(づよ)い: 押し付けがましい", 0, 3 },
  { "#D2KY",  "接尾語:形容詞化(動詞連用形)", "(動連)+しづらい,難(がた)い", 0, 3 },
  { "#N2KYT", "接尾語:形容詞化(名ナノ)"    , "(名)+高(だか)い,早(ばや)い: 名高い,手早い", 0, 3 },
  { "#N2T10", "特殊活用:形容動詞化(T10)"   , "(名)+づかい", 0, 3 },
  { "#N2T15", "特殊活用:形容動詞化(T15)"   , "(名)+ずくめ", 0, 2 },
  { "#N2T16", "特殊活用:形容動詞化(T16)"   , "(名)+的,式,風,流", 0, 3 },
  { "#N2T17", "特殊活用:形容動詞化(T17)"   , "", 0, 1 },
  { "#N2T18", "特殊活用:形容動詞化(T18)"   , "(名)+みたい,がち", 0, 2 },
  { "#JS",    "数助詞"                     , "", 0, 3 },
  { "#JSSUC", "数助詞接尾語"               , "", 0, 3 },
  { "#JNMUC", "接尾語:人名(名)"            , "", 0, 1 },
  { "#JNMSUC","接尾語:名"                  , "", 0, 2 },
  { "#JNSSUC","接尾語:姓"                  , "", 0, 3 },
};

unsigned int nr_substantive_code = sizeof(substantive_code) / sizeof(category_code);
unsigned int nr_adverb_code = sizeof(adverb_code) / sizeof(category_code);
unsigned int nr_verb_code = sizeof(verb_code) / sizeof(category_code);
unsigned int nr_adjective_code = sizeof(adjective_code) / sizeof(category_code);
unsigned int nr_etc_code = sizeof(etc_code) / sizeof(category_code);

const char *find_desc_from_code(const char *code)
{
  int pos;
  const char *cclass_desc = NULL;

  for (pos = 0; pos < NR_POS; pos++) {
    cclass_desc = find_desc_from_code_with_type(code, pos);
    if (cclass_desc)
      break;
  }

  return cclass_desc;
}

/* short cut of find_desc_from_code */
const char *find_desc_from_code_with_type(const char *code, int type) {
  /* need to be more smart */
  int i = 0, j = 0;
  const char *pos = NULL;
  category_code *category[] = {
    substantive_code,
    verb_code,
    adjective_code,
    adverb_code,
    etc_code,
    NULL,
  };
  int num[] = {
    sizeof(substantive_code) / sizeof(substantive_code[0]),
    sizeof(verb_code)        / sizeof(verb_code[0]),
    sizeof(adjective_code)   / sizeof(adjective_code[0]),
    sizeof(adverb_code)      / sizeof(adverb_code[0]),
    sizeof(etc_code)         / sizeof(etc_code[0]),
    0,
  };

  do {
    for (j = 0; j < num[i]; j++) {
      if ((i == type ) && !strcmp(code, (category[i])[j].code))
	pos = (category[i])[j].desc;
    }
    i++;
  } while (category[i] != NULL);

  return pos;
}

const char *find_code_from_desc(const char *desc, int type) {
    /* need to be more smart */
    int i = 0, j = 0;
    const char *code = NULL;
    category_code *category[] = {
      substantive_code,
      verb_code,
      adjective_code,
      adverb_code,
      etc_code,
      NULL,
    };
    int num[] = {
      sizeof(substantive_code) / sizeof(substantive_code[0]),
      sizeof(verb_code)        / sizeof(verb_code[0]),
      sizeof(adjective_code)   / sizeof(adjective_code[0]),
      sizeof(adverb_code)      / sizeof(adverb_code[0]),
      sizeof(etc_code)         / sizeof(etc_code[0]),
      0,
    };

    do {
      for (j = 0; j < num[i]; j++) {
	if ((i == type) && !strcmp(desc, (category[i])[j].desc))
	  code = (category[i])[j].code;
      }
      i++;
    } while (category[i] != NULL);

    return code;
}

int
find_cclass_type_from_code (const char *code)
{
  int pos;
  const char *cclass_desc = NULL;

  for (pos = 0; pos < NR_POS; pos++) {
    cclass_desc = find_desc_from_code_with_type(code, pos);
    if (cclass_desc)
      return pos;
  }

  return -1;
}

int
find_cclass_type_from_desc (const char *desc)
{
  int pos;
  const char *cclass_code = NULL;

  for (pos = 0; pos < NR_POS; pos++) {
    cclass_code = find_code_from_desc(desc, pos);
    if (cclass_code)
      return pos;
  }

  return -1;
}
