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
  { "#T00", "̾��(��,��,����,�촴,�ʽ���³)"  , "����,����", 31, 3 },
  { "#T01", "̾��(��,��,����,�촴,�ʽ���³��)", "", 31, 3 },
  { "#T02", "̾��(��,��,����,�촴)"           , "", 30, 3 },
  { "#T03", "̾��(��,��,����,�ʽ���³)"       , "��������", 29, 3 },
  { "#T04", "̾��(��,��,����)"                , "", 28, 3 },
  { "#T05", "̾��(��,��,�촴,�ʽ���³)"       , "����,����,����,�ʷ�,����,����", 27, 3 },
  { "#T06", "̾��(��,��,�촴,�ʽ���³��)"     , "����,���,��䤫,����", 27, 3 },
  { "#T08", "̾��(��,��,�ʽ���³��)"          , "�ճ�,��̿", 25, 3 },
  { "#T09", "̾��(��,��)"                     , "�Ť�,����", 24, 3 },
  { "#T10", "̾��(��,����,�촴,�ʽ���³)"     , "�¿�,�ⵤ,����,����", 23, 3 },
  { "#T11", "̾��(��,����,�촴,�ʽ���³��)"   , "����,����,����,ľ��", 23, 3 },
  { "#T12", "̾��(��,����,�촴)"              , "¸ʬ", 22, 3 },
  { "#T13", "̾��(��,����,�ʽ���³��)"        , "�繲��", 21, 3 },
  { "#T14", "̾��(��,����)"                   , "", 20, 3 },
  { "#T15", "̾�졦����(��,�촴,�ʽ���³)"    , "Ʊ��?,����", 19, 3 },
  { "#T18", "̾��(��,�ʽ���³��)"             , "������,�Ÿ�,�²�,�ⵤ", 17, 3 },
  { "#T19", "̾��(��)"                        , "��ʤ�,����,�礶�ä�", 16, 3 },
  { "#T20", "̾��(��,����,�촴,�ʽ���³)"     , "", 15, 3 },
  { "#T21", "̾��(��,����,�촴,�ʽ���³��)"   , "", 15, 3 },
  { "#T22", "̾��(��,����,�촴)"              , "", 14, 3 },
  { "#T23", "̾��(��,����,�ʽ���³��)"        , "", 13, 3 },
  { "#T24", "̾��(��,����)"                   , "", 12, 3 },
  { "#T25", "̾��(��,�촴,�ʽ���³)"          , "ʿ��", 11, 3 },
  { "#T26", "̾��(��,�촴,�ʽ���³��)"        , "", 11, 3 },
  { "#T27", "̾��(��,�촴)"                   , "", 10, 3 },
  { "#T28", "̾��(��,�ʽ���³��)"             , "", 9,  3 },
  { "#T29", "̾��(��)"                        , "", 8,  3 },
  { "#T30", "̾��(����,�촴,�ʽ���³)"        , "����,����,����,����", 7,  3 },
  { "#T32", "̾��(����,�촴)"                 , "", 6,  3 },
  { "#T33", "̾��(����,�ʽ���³��)"           , "", 5,  3 },
  { "#T34", "̾��(����)"                      , "", 4,  3 },
  { "#T35", "̾��(�촴,�ʽ���³)"             , "���,�縰,��,���", 3,  3 },
  { "#T39", "̾��"                            , "", 0,  3 },
  { "#CN",  "��̾"                            , "���", 0, 3 },
  { "#CNS", "��̾(������)"                    , "�����", 0, 3 },
  { "#JCN", "��̾(�����)"                    , "Ĺ��", 0, 3 },
  { "#JN",  "��̾"                            , "����,��", 0, 3 },
  { "#JNS", "��̾(��)"                        , "¢��", 0, 3 },
  { "#JNM", "��̾(̾)"                        , "����", 0, 1 },
  { "#KK",  "���/����"                       , "�����ŵ�", 0, 3 },
};

category_code adverb_code[] = {
  { "#T07", "����(��,��,�촴)"                , "��ʬ", 26, 3 },
  { "#T16", "����(��,�촴,�ʽ���³��)"        , "�Ƴ�,����,���ʤ�", 19, 3 },
  { "#T17", "����(��,�촴)"                   , "�䤿��,�Ԥä���,�ռ�Ū,����Ū,�ʳ�Ū", 18, 3 },
  { "#T31", "����(����,�촴,�ʽ���³��)"      , "����ä�", 7,  3 },
  { "#T36", "����(�촴,�ʽ���³��)"           , "��ۤ�,���֤�,�ޤ����", 3,  3 },
  { "#T37", "����(�촴)"                      , "������,�ϤʤϤ�", 2,  3 },
  { "#T38", "����(�ʽ���³��)"                      , "", 1,  3 },

  { "#F00", "����(��,����,����,�촴)", "����,���뤰��,�Ҥ��Ҥ�,�ġ�", 15, 3 },
  { "#F01", "����(��,����,����)"     , "Ƚ��,����,����,����,����,����", 14, 3 },
  { "#F02", "����(��,����,�촴)"     , "����,����,�Ǹ�,Ʋ��,�䡹,�䡹", 13, 3 },
  { "#F03", "����(��,����,)"         , "����,�´�,����,����,�ۡ�,����", 12, 3 },
  { "#F04", "����(��,����,�촴)"     , "�դä���,��ä���", 11, 3 },
  { "#F05", "����(��,����)"          , "��������,����,���老��,���Ĥ���", 10, 3 },
  { "#F06", "����(��,�촴)"          , "����,����,�١�,�ŤͽŤ�,�ޤ��ޤ�,�श�श",  9, 3 },
  { "#F07", "����(��)"               , "",  8, 3 },
  { "#F08", "����(����,����,�촴)"   , "",  7, 3 },
  { "#F09", "����(����,����)"        , "",  6, 3 },
  { "#F10", "����(����,�촴)"        , "",  5, 3 },
  { "#F11", "����(����)"             , "��",  4, 3 },
  { "#F12", "����(����,�촴)"        , "���ä�,���ä�,�ۤä�,�դ�,�ۤä�,��ä�",  3, 3 },
  { "#F13", "����(����)"             , "",  2, 3 },
  { "#F14", "����(�촴)"             , "���Ѥ�餺,������,˰���ޤ�",  1, 3 },
  { "#F15", "����(̤���)"           , "",  0, 3 },
};

/**
 * K5,  ����5��,              �֤�
 * K5r, ����5��:Ϣ�ѷ���̾��, ��
 * C5r, �Ԥ�5��:Ϣ�ѷ���̾��, �Ԥ�
 * G5,  ����5��,              �Ĥ�
 * G5r, ����5��:Ϣ�ѷ���̾��, �ޤ�
 * S5,  ����5��,              ����
 * S5r, ����5��:Ϣ�ѷ���̾��, �ܤ�
 * T5,  ����5��,              ���
 * T5r, ����5��:Ϣ�ѷ���̾��, �Ǥ�
 * N5,  �ʹ�5��,              ���
 * B5,  �й�5��,              ž��
 * B5r, �й�5��:Ϣ�ѷ���̾��, ͷ��
 * M5,  �޹�5��,              ����
 * M5r, �޹�5��:Ϣ�ѷ���̾��, �Ԥ�
 * R5,  ���5��,              ��ĥ��
 * R5r, ���5��:Ϣ�ѷ���̾��, �դ�
 * L5,  ���5��:̿�������,   ����ä����
 * W5,  ���5��,              ����
 * W5r, ���5��:Ϣ�ѷ���̾��, ����
 * U5,  ��5��,              ��
 * U5r, ��5��:Ϣ�ѷ���̾��, �䤦
 * KS,  �岼1��,              �ߤ��
 *                            Ϳ����
 * KSr, �岼1��:�촴��̾��,   ������
 *                            �¤���
 * KX,  ���ѳ���ư��,         ���
 * SX,  ���ѳ���ư��,         �ؤ���
 * ZX,  ���ѳ���ư��,         ������
 * NZX, ���ѳ���ư��,       �Ť󤺤�
 **/

category_code verb_code[]= {
  { "#K5",  "����5��"       , "��/��(�ʤ�)/��(�ޤ�)/��/��(����)/��(��)/��(��)", 0, 3 },
  { "#K5r", "����5��:Ϣ̾"  , "��/��(�ʤ�)/��(�ޤ�)/��/��(����)/��(��)/��(��)", 0, 3 },
  { "#C5r", "�Ԥ�5��"       , "��/��(�ʤ�)/��(�ޤ�)/��/��(����)/��(��)/��(��)", 0, 3 },
  { "#G5" , "����5��"       , "��/��(�ʤ�)/��(�ޤ�)/��/��(����)/��(��)/��(��)", 0, 3 },
  { "#G5r", "����5��:Ϣ̾"  , "��/��(�ʤ�)/��(�ޤ�)/��/��(����)/��(��)/��(��)", 0, 3 },
  { "#S5" , "����5��"       , "��/��(�ʤ�)/��(�ޤ�)/��/��(����)/��(��)/��(��)", 0, 3 },
  { "#S5r", "����5��:Ϣ̾"  , "��/��(�ʤ�)/��(�ޤ�)/��/��(����)/��(��)/��(��)", 0, 3 },
  { "#T5" , "����5��"       , "��/��(�ʤ�)/��(�ޤ�)/��/��(����)/��(��)/��(��)", 0, 3 },
  { "#T5r", "����5��:Ϣ̾"  , "��/��(�ʤ�)/��(�ޤ�)/��/��(����)/��(��)/��(��)", 0, 3 },
  { "#N5",  "�ʹ�5��"       , "��/��(�ʤ�)/��(�ޤ�)/��/��(����)/��(��)/��(��)", 0, 3 },
  { "#N5r", "�ʹ�5��:Ϣ̾"  , "", 0, 1 },
  { "#B5",  "�й�5��"       , "ž/��(�ʤ�)/��(�ޤ�)/��/��(����)/��(��)/��(��)", 0, 3 },
  { "#B5r", "�й�5��:Ϣ̾"  , "ͷ/��(�ʤ�)/��(�ޤ�)/��/��(����)/��(��)/��(��)", 0, 3 },
  { "#M5",  "�޹�5��"       , "��/��(�ʤ�)/��(�ޤ�)/��/��(����)/��(��)/��(��)", 0, 3 },
  { "#M5r", "�޹�5��:Ϣ̾"  , "��/��(�ʤ�)/��(�ޤ�)/��/��(����)/��(��)/��(��)", 0, 3 },
  { "#R5" , "���5��"       , "��ĥ/��(�ʤ�)/��(�ޤ�)/��/��(����)/��(��)/��(��)", 0, 3 },
  { "#R5r", "���5��:Ϣ̾"  , "��/��(�ʤ�)/��(�ޤ�)/��/��(����)/��(��)/��(��)", 0, 3 },
  { "#L5",  "���5��:̿�ᥤ", "����ä���/��(�ʤ�)/��(�ޤ�)/��/��(����)/��(��)/��(��)", 0, 3 },
  { "#W5",  "���5��"       , "��/��(�ʤ�)/��(�ޤ�)/��/��(����)/��(��)/��(��)", 0, 3 },
  { "#W5r", "���5��:Ϣ̾"  , "��/��(�ʤ�)/��(�ޤ�)/��/��(����)/��(��)/��(��)", 0, 3 },
  { "#U5" , "��5��"       , "��/��(�ʤ�)/��(�ޤ�)/��/��(����)/��(��)/��(��)", 0, 3 },
  { "#U5r", "��5��:Ϣ̾"  , "��/��(�ʤ�)/��(�ޤ�)/��/��(����)/��(��)/��(��)", 0, 3 },

  { "#KS",  "�岼1��"       , "��,Ϳ/��,��(�ʤ�)/��,��(�ޤ�)/���,����/���,����(����)/���,����(��)/���,����(��)", 0, 3 },
  { "#KSr", "�岼1��:�촴̾", "��,��/��,��(�ʤ�)/��,��(�ޤ�)/����,����/����,����(����)/����,����(��)/����,����(��)", 0, 3 },
  { "#KX",  "���ѳ���ư��"  , "��/��(�ʤ�)/��(�ޤ�)/����/����(����)/����(��)/����(��)", 0, 2 },
  { "#SX",  "���ѳ���ư��"  , "��/��(�ʤ�)/��(�ޤ�)/����/����(����)/����(��)/����", 0, 3 },
  { "#ZX",  "���ѳ���ư��"  , "��/��(�ʤ�)/��(�ޤ�)/����/����(����)/����(��)/����", 0, 3 },
  { "#NZX", "���ѳ���ư��", "�Ť�/��(�ʤ�)/��(�ޤ�)/��/��(����)/��(��)/��(��)", 0, 2 },
};

/***
 * KY,    ��                   ������
 * KYT,   ̾�ʥΡ���    �ͳѤ�,������
 * KYna,  ��            ������,�礭��  (���Ѥ�[��(Ϣ����ˡ��]��������ƻ�)
 * KYmi,  ��            �������Ť�     (�ߤǽ����̾���ž��������ƻ�)
 * KYme,  ��            ���ᡢĹ��     (��ǽ���ȷ���ư���ž��������ƻ�)
 * KYmime,��            ���ߡ�����     KYmi, KYme ��ξ�����������碌����
 * KYU,   ��:������     �����夦
 **/

category_code adjective_code[] = {
  { "#KY",     "���ƻ�"           , "������", 0, 3 },
  { "#KYT",    "���ƻ�:̾�ʥ�"    , "�ͳѤ�, ������", 0, 3 },
  { "#KYna",   "���ƻ�:Ϣ����ˡ��", "������, �礭��", 0, 3 },
  { "#KYmi",   "���ƻ�:��"        , "����, �Ť�", 0, 3 },
  { "#KYme",   "���ƻ�:��"        , "����, Ĺ��", 0, 3 },
  { "#KYmime", "���ƻ�:�ߤ�"      , "����, ����", 0, 3 },
  { "#KYU",    "���ƻ�:������"    , "�����夦", 0, 3 },
};

category_code etc_code[] = {
  { "#KJ",    "��ʸ������"                 , "ñ�����Ѵ���", 0, 3 },
/*
  { "#CN",    "��̾"                       , "���", 0, 3 },
  { "#CNS",   "��̾(������)"               , "�����", 0, 3 },
  { "#JCN",   "��̾(�����)"               , "Ĺ��", 0, 3 },
  { "#JN",    "��̾"                       , "����,��", 0, 3 },
  { "#JNS",   "��̾(��)"                   , "¢��", 0, 3 },
  { "#JNM",   "��̾(̾)"                   , "����", 0, 1 },
  { "#KK",    "���/����"                  , "�����ŵ�", 0, 3 },
*/
  { "#CJ",    "��³��/��ư��/Ϣ��"         , "", 0, 3 },
  { "#RT",    "Ϣ�λ�"                     , "", 0, 3 },
  { "#OKX",   "ư�����ǫɽ���θ촴"       , "��ʹ��", 0, 3 },
  { "#NN",    "����:����"                  , "��,��", 0, 3 },
  { "#N00",   "����:x��,x��,x��"           , "x��,x��,x��", 0, 3 },
  { "#N01",   "����:��,����,��,����"       , "��,����,��,����", 0, 3 },
  { "#N02",   "����:ɴ,��ɴ,��,��ɴ"       , "ɴ,��ɴ,��,��ɴ", 0, 3 },
  { "#N03",   "����:��,��,��,����"       , "��,��,��,����", 0, 3 },
  { "#KN",    "����̾��"                   , "����/����/����/����/", 0, 3 },
  { "#TKN",   "������̾��"                 , "�Ϥ�/�櫓", 0, 2 },
  { "#JTNO",  "���θ�̾��"                 , "���餤/���餤/����", 0, 2 },
  { "#PRE",   "��Ƭ��"                     , "", 0, 3 },
  { "#CNPRE", "��Ƭ��:����"                , "", 0, 3 },
  { "#JNPRE", "��Ƭ��:��̾"                , "", 0, 2 },
  { "#NNPRE", "��Ƭ��:����"                , "", 0, 3 },
  { "#SNPRE", "��Ƭ��:����̾��"            , "", 0, 2 },
  { "#SUN",   "������:����"                , "", 0, 2 },
  { "#CNSUC1","������:��̾ 1"              , "", 0, 3 },
  { "#CNSUC2","������:��̾ 2"              , "", 0, 3 },
  { "#JNSUC" ,"������:��̾"                , "", 0, 3 },
  { "#N2T30", "������:����̾�첽"          , "(̾)+��,��", 0, 3 },
  { "#N2T35", "������:̾�첽"              , "", 0, 3 },
  { "#D2T35", "������:ư��Ϣ�ѷ�+̾�첽"   , "(ư��Ϣ�ѷ�)+�äѤʤ�", 0, 3 },
  { "#D2T16", "������:����ư�첽"          , "(ư��Ϣ�ѷ�)+����", 0, 3 },
  { "#ND2KY", "������:���ƻ첽"            , "(̾,ư��)+���ޤ���,��(�Ť�)��: �����դ����ޤ���", 0, 3 },
  { "#D2KY",  "������:���ƻ첽(ư��Ϣ�ѷ�)", "(ưϢ)+���Ť餤,��(����)��", 0, 3 },
  { "#N2KYT", "������:���ƻ첽(̾�ʥ�)"    , "(̾)+��(����)��,��(�Ф�)��: ̾�⤤,���ᤤ", 0, 3 },
  { "#N2T10", "�ü����:����ư�첽(T10)"   , "(̾)+�Ť���", 0, 3 },
  { "#N2T15", "�ü����:����ư�첽(T15)"   , "(̾)+������", 0, 2 },
  { "#N2T16", "�ü����:����ư�첽(T16)"   , "(̾)+Ū,��,��,ή", 0, 3 },
  { "#N2T17", "�ü����:����ư�첽(T17)"   , "", 0, 1 },
  { "#N2T18", "�ü����:����ư�첽(T18)"   , "(̾)+�ߤ���,����", 0, 2 },
  { "#JS",    "������"                     , "", 0, 3 },
  { "#JSSUC", "������������"               , "", 0, 3 },
  { "#JNMUC", "������:��̾(̾)"            , "", 0, 1 },
  { "#JNMSUC","������:̾"                  , "", 0, 2 },
  { "#JNSSUC","������:��"                  , "", 0, 3 },
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
