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
/*
 *	wnnlib -- かな漢字変換用ライブラリ (jllib 対応版)
 *
 *	このライブラリは、kinput V2 に付属していた、SRA の石曽根さんの
 *	jclib 5.2 をベースに作成しました。
 *
 *                                                        森部 英之
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
 *		ishisone@sra.co.jp
 *          MORIBE, Hideyuki
 */

/*
 * Portability issue:
 *
 *	+ define SYSV, SVR4 or USG if you don't have bcopy() or bzero().
 *
 *	  if you define USG (which should be defined if your OS is based
 *	  on System V Rel 2) or SYSV (in case of System V Rel 3),
 *	  memchr() is used for bzero(), and my own version of bcopy()
 *	  is used in order to handle overlapping regions.
 *
 *	  if you define SVR4 (yes, System V Rel4), memmove() is used for
 *	  bcopy(), and memchr() is used for bzero().
 *
 *	+ wnnlib assumes bcopy() can handle overlapping data blocks.
 *	  If your bcopy() can't, you should define OVERLAP_BCOPY,
 *	  which force to use my own bcopy() rather than the one
 *	  in libc.a.
 */

/*
 * 概要
 *
 * wnnlib は Wnn6 にも対応した kinput の CcWnn オブジェクト向けの高レベル
 * かな漢字変換ライブラリである。
 *
 * 従来の Kinput においては、Wnn とのインタフェースは、jslib ベースの
 * jilib と jclib であった。ところが、Wnn6 で拡張された機能を使用したくて
 * も、jslib レベルの仕様がほとんど判らなかった。このため、mule の egg イ
 * ンタフェースで使用している jllib を用いて、従来の jilib と jclib のイ
 * ンタフェースをできるだけ変更しないようにして、新たに wnnlib として書き
 * 換えた。
 *
 * wnnlib は、Wnn6 だけでなく、それ以前の Wnn4 にも対応しているはずで
 * あるがテストはしていない。
 *
 * wnnlib は、従来の jclib と同様に、かなバッファと表示バッファという２つ
 * のバッファを持つ。かなバッファには読み文字列が入り、表示バッファには変
 * 換結果(表示文字列)が入る。かなバッファと言う呼び方はあまり正確ではない。
 * Wnn Version 4 以降では漢字かな変換もできるからである。
 *
 * ドットとカレント文節という概念を持ち、文字の挿入 / 削除はドットの位置に
 * 対して行なわれ、変換その他の操作はカレント文節に対して行なわれる。
 * Wnn Version 4 以降では大文節と小文節という２種類の文節の概念があるが、
 * それに対応して wnnlib も当然この２種類を扱うことができる。
 *
 * このライブラリは次のような機能を提供する。
 *	・かなバッファへの文字の挿入 / 削除
 *	・かな漢字変換 / 再変換 / 無変換
 *	・ひらがな⇔カタカナ変換
 *	・確定
 *	・文節の拡大 / 縮小
 *	・カレント文節 / ドットの移動
 *	・次候補/前候補置き換え
 *	・候補取り出し / 選択
 *	・バッファのクリア
 *
 * 文字コードとしては Wnn と同じく EUC 内部コード (process code) を使用する。
 */

/*
 * Wnn Version 6 (FI Wnn) 対応にあたって
 *
 * 従来の Kinput2 では、Wnn とのインターフェースは、jslib をベースに
 * したjilib と jclib で、もともと Wnn Version 3 の libjd の上に作ら
 * れたライブラリである。
 *
 * Wnn Version 6 対応にあたって、jslib レベルの追加機能や詳細インタフェー
 * スが判らなかったため、jslib の代わりに mule の egg インタフェースで使
 * 用されている jllib をベースにして、jilib と jclib を新たに wnnlib とし
 * て書き換えることにした。書き換えは、以下の方針で行った。
 *
 * 1. データ構造、インタフェース (関数名や変数名も) をなるべく従来の
 * jclibと同じにする。
 *
 * 2. かなバッファと表示バッファの二つの文字バッファを持ち、
 * かなバッファには読み、表示バッファには変換結果が入るとか
 * 様々な操作はカレント文節と呼ばれる文節に対して行なわれるとかいった
 * 基本的なコンセプトは変えない。
 *
 * 3. 従来のライブラリを使ったアプリケーションが新しいライブラリに
 * 移行しやすいように、関数インターフェイスもできるだけ似たものにする。
 *
 * 4. 1,2,3 の方針をできるだけ守りつつ、Wnn6 で導入された次のような
 * 機能をサポートする。
 *	・FI 変換/学習
 *	・無変換学習
 *
 * 5. 1 から 4 までの方針に従いつつ、クイック・ハックする。
 */

/*
 * メモ (注: 最初の部分は、石曽根さんの jclib 作成メモ)
 *
 * ver 0.0	89/07/21
 *	とりあえず作りはじめる
 * ver 0.1	89/08/02
 *	半分くらいかけた
 *	次候補関連がまだできていない
 * ver 0.2	89/08/04
 *	jcInsertChar() / jcDeleteChar() を作成
 * ver 0.3	89/08/07
 *	一応できた
 *	まだいくつか疑問点があるけれど
 * ver 0.4	89/08/08
 *	今使ったよビットの扱いを残して、ほぼできたのではないかと
 *	思われる
 *	細かいバグをかなり修正
 * ver 0.5	89/08/09
 *	立木さん@KABA に質問した所、今使ったよビットを落すのも
 *	クライアント側の責任であることがわかる
 *	これへの対応
 *	ついでにデータ構造の説明を追加
 *	ファイルのサイズが 80KB を越えてしまった
 *	コメントをとればかなり小さくなるんだけど
 * ver 0.6	89/08/22
 *	jcDeleteChar() を全面的に書き直す
 *	これで一応正しく動作するようになった
 *	jcInsertChar() で最後の clauseInfo の設定が間違っていたので
 *	それを修正
 *	jcPrintDetail() に簡単な clauseInfo データの consistency check を
 *	入れる
 * ver 0.7	89/08/26
 *	jcExpand() のバグ修正
 *	小文節の単文節変換を少し修正
 * ver 0.8	89/08/30
 *	changecinfo() で conv フラグをセットするのを忘れていた
 *	moveKBuf()/moveDBuf()/moveCInfo() を少し修正
 *	SYSV が define されていれば bcopy()/bzero() の代わりに
 *	memcpy()/memset() を使うように修正
 * ver 0.9	89/09/22
 *	setLCandData() で次候補バッファの候補数にカレント大文節の
 *	分を加えるのを忘れていた
 * ver 0.10	89/10/16
 *	wnn-4.0.1 で commonheader.h -> commonhd.h になったので
 *	それの修正
 * ver 0.11	89/10/18
 *	USG が define されていても memcpy()/memset() を使うように修正
 * ver 0.12	89/10/19
 *	resizeBuffer() でドットの再設定を忘れているという重大なバグを修正
 * ver 4.0	89/10/27
 *	バージョン番号を修正して 4.0 にする。
 * --- kinput を R4 に contribute ---
 * ver 4.1	90/06/04
 *	クライアント側にある辞書・頻度ファイルのセーブができないという
 *	重大なバグを修正
 * ver 4.2	90/06/15
 *	辞書が登録可能かどうかの判定が間違っていて、逆変換可能辞書の
 *	セーブができないというまたまた重大なバグを修正
 *	今のところ kinput/wterm とも単語登録機能がついてないので
 *	実害はなかったが
 * ver 4.3	91/08/15
 *	文字データ型として wchar_t ではなく、wchar を使うようにする
 *	最終的には Wnn の次期バージョンの型に合わせるつもり
 * ver 4.4	91/09/18
 *	SYSV または USG が定義されている場合には自動的に OVERLAP_BCOPY
 *	も定義するようにした
 *	SVR4 が定義されている場合には bcopy の代わりに memmove() を使用
 *	するようにした
 * ver 4.5	91/09/23
 *	DEBUG を DEBUG_JCLIB に変更
 * ver 5.0	91/10/01
 *	kinput2 リリース向けにバージョン番号を修正して 5.0 にする。
 * --- kinput2 を R5 に contribute ---
 * ver 5.1	92/02/07
 *	John Yates さん (yates@bldrsoft.com) から getLCandDataLen() で
 *	文字数を数え間違えていたのを指摘されたのでそれの修正
 * ver 5.2	92/12/24
 *	jcInsertChar() でデータの初期化をしていなかった部分があった
 *	ので修正 (値が代入されるまで使用されないのでバグではないのだが
 *	ちょっと気持ちわるいので)
 *
 * ---  wnnlib 作成メモ ---
 *
 * ver 0.1	98/03/12
 *	とりあえず、jllib インタフェースに書換えを始める。
 * ver 0.2	98/03/16
 *	まだいくつか懸念事項はあるものの、基本的な書換えが終わったので、
 *	デバッグを始める。それなりに、動いている様子。
 * ver 0.3	98/03/18
 *	いくかバグが見つかった (コア・ダンプした) ので、それらを修正。
 *	まだ、Wnn6 の機能が有効になっているか良くわからない。
 * ver 0.4	98/07/01
 *	以前から気になっていたループに陥る現象の原因がやっと判った。
 *	原因は、変換の cancel の延長で呼ばれる expandOrShrink の中で、
 *	無変換指定の時でも ltop (大文節) フラッグをリセットしていなかっ
 *	たためで、それを修正した。
 * ver 0.5	98/10/15
 *	最後の修正から約 3 ヶ月間、使用したが特に問題がなかったので、
 *	kinput2-fix5 の alpha 版がでたのを機会に、kinput2 メーリング・
 *      リストへ投稿。
 * ver 0.6	98/12/03
 *	石曽根さんより、文節拡大のでのバグの報告があったので (kinput2
 *      メーリング・リスト 2106 〜 2118 参照)、それを修正。
 *
 * --- kinput2-fix-alpha2 に取り込まれる ---
 *
 * ver 0.7	98/12/23
 *	doKantanSCConvert() で小文節として単文節変換しなければいけない
 *	ところを、大文節として変換していたバグを修正。
 *
 * ver 0.8	99/01/06
 *	kinput2-fix5-alpha4 がでたのを機会に、ドット以降を削除する編集
 *	機能 (kill-line) を実現する jcKillLine() を追加する (実は、wnnlib
 *	作成時から実現しようと思っていて、ダミー関数だけは wnnlib に存
 *	在していた)。これのデバッグ中に、上の ver 0.4 で修正したはずの
 *	バグが再現。
 *
 * ver 0.9	99/01/18
 *	やはり、cancel の延長の処理がうまくないことが判明。つまり、文
 *	節拡大による cancel 処理では、文節情報が CcWnn が期待するもの
 *	と異っているため (これが、jclib と wnnlib の違い)、誤動作をし
 *	た。このため、独立した cancel 処理を jcCancel() ファンクション
 *	として実現することにした。でも、expand-noconv や shrink-noconv
 *	などでは同様の問題が存在するので、jclib との互換を保つ意味で、
 *	expandOrShrink の中で特別扱いすることにした (使う人は、いない
 *	と思うが。doc/ccdef 参照)。
 *	また、getHint() と forceStudy() の処理を若干の見直した。
 *
 * ver 0.99	99/03/05
 *	前回の getHint() の処理の副作用で、setCandiate() で次候補取り出し
 *	後の大文節情報の変更方法にあった潜在バグを修正。
 *
 * ver ?.??	99/03/29				ishisone
 *	前に取り出した候補一覧を再利用するかどうかの判定を変更。
 *	再利用の条件をきつくする。また Wnn4 の jl ライブラリの不具合
 *	(仕様かも) の回避策の組み込み。
 *
 * ver ?.??	99/04/12				ishisone
 *	jcOpen() に加えて jcOpen2() を実装。これは Wnn4 と Wnn6 それぞれの
 *	初期化ファイルを指定することができ、実際に接続したサーバの
 *	バージョンをチェックして、どちらを使用するか決めるというもの。
 *
 * ver ?.??	99/05/07				ishisone
 *	「無量大数」問題の回避策の実装。とはいっても文節伸縮の際の
 *	前文節との接続をやめるだけ。「無量大数」問題については
 *	expandOrShrink() のコメント参照。
 *
 * ver ?.??	99/05/25				ishisone
 *	config.h をインクルードしないようにする。必要なのは LIBDIR だけ
 *	だし、config.h の LIBDIR の値が正しいという保証もないため。
 *	/usr/local/lib/wnn に決めうち。(オーバーライドすることはできる)
 *
 * --- kinput2 version 3.0 リリース ---
 *
 * ver ?.??	01/01/10
 *	Wnn7 対応。とはいっても最小限の対応で、Wnn7 の新しい機能を
 *	利用できるわけではない。
 *	使用されていない変数を削除。
 */


/*
 * ファンクション
 *
 * struct wnn_buf jcOpen(char *servername, char *envname,
 *			 int override, char *rcfilename,
 *			 void (*errmsgfunc)(), int (*confirmfunc)(),
 *			 int timeout)
 *	jl_open あるいは jl_open_lang に対応した wnnlib のインタフェー
 *	スで、この関数の中で実際に jl_open あるいは jl_open_lang を呼
 * 	び出す。override が True の場合、既に環境がサーバ側にあっても、
 *	環境を再初期化する。
 *
 * void jcClose(struct wnn_buf *wnnbuf)
 *	jl_close を呼び出し、jcOpen で獲得した wnnbuf の解放とサーバと
 *	の接続を切る。
 *
 * int jcIsConnect(struct wnn_buf *wnnbuf)
 *	サーバとの接続状態を jl_isconnect で調べる。wnnbuf が NULL、
 *	環境が作成されていない、あるいはサーバと接続されていない場合には 0。
 *	wnnbuf がサーバと接続されていれば、1 を返す。
 *
 * jcConvBuf *jcCreateBuffer(struct wnn_env *env, int nclause, int buffersize)
 *	指定された環境を使って変換のバッファを作成する。バッファは
 *	複数作ることができる。一つのバッファでは同時に複数の文を
 *	変換することはできないので、複数の文を並行して変換したい場合には
 *	幾つかのバッファを用意しなくてはならない。
 *	環境の設定までを予めやっておく必要がある。つまりサーバとの接続、
 *	環境の生成、辞書の設定などは jcOpen で行っておく必要がある。
 *	引数の nclause と buffersize で、それぞれ初期化時にアロケートする
 *	文節情報およびかな/表示バッファの大きさが指定できる。
 *	ただしこれらは、サイズが足りなくなれば必要に応じて自動的に
 *	増やされるため、ここに指定した以上の数の文節や、文字列が変換できない
 *	わけではない。それぞれ 0 または負の値を指定すると、デフォルトの
 *	サイズでアロケートされる。従って通常は nclause/buffersize とも
 *	0 を指定しておけばよい。
 *	リターンバリューとしてバッファを返す。エラーの時には NULL が
 *	返される。
 *
 * int jcDestroyBuffer(jcConvBuf *buf, int savedic)
 *	バッファの使用を終了する。環境を消したり、サーバとの接続を切ったり
 *	することは、jcClose で行う。
 *	引数 savedic が 0 でなければ、環境中で使用されている全ての辞書を
 *	セーブする。
 *
 * int jcClear(jcConvBuf *buf)
 *	バッファをクリアする。新たに変換を始める際には最初にこの
 *	ファンクションを呼ばなければならない。
 *
 * int jcInsertChar(jcConvBuf *buf, int c)
 *	ドットに１文字挿入する。
 *	カレント文節が既に変換されていれば無変換の状態に戻る。
 *	カレント文節は大文節である。
 *
 * int jcDeleteChar(jcConvBuf *buf, int prev)
 *	ドットの前又は後ろの１文字を削除する。
 *	カレント文節が既に変換されていれば無変換の状態に戻る。
 *	カレント文節は大文節である。
 *
 * int jcConvert(jcConvBuf *buf, int small, int tan, int jump)
 *	カレント文節から後ろを変換する。
 *	引数 tan が 0 なら連文節変換、そうでなければカレント文節を
 *	単文節変換し、そのあとを連文節変換する。
 *	引数 small が 0 でなければ小文節が、そうでなければ大文節が
 *	カレント文節として使われる。
 *	引数 jump で、変換後のカレント文節の位置が決まる。jump が
 *	0 ならカレント文節の位置は変換しても変わらない (ただし
 *	カレント文節として大文節を指定した場合、変換後のカレント
 *	小文節はカレント大文節の最初の小文節になる) が、0 でなければ
 *	最後の文節の次 (空文節) に移動する。逐次変換していくような
 *	アプリケーションではこれを 1 にするとよいだろう。
 *
 * int jcUnconvert(jcConvBuf *buf)
 *	カレント大文節を無変換の状態に戻す。
 *	カレント大文節がいくつかの小文節からできていた場合、これらの
 *	小文節はまとめられ、一つの無変換状態の文節になる。
 *	カレント小文節を無変換に戻す機能は用意しない。なぜかというと、
 *	大文節の中の 1 小文節のみが無変換になってしまうと、その文節に
 *	関して jcMove() で移動を行なった時、どう移動すればよいのか
 *	よくわからない、つまり移動のセマンティクスが不明確になってしまう
 *	からである。
 *
 * int jcKana(jcConvBuf *buf, int small, int kind)
 *	カレント文節をかなにする。
 *	引数 kind が、JC_HIRAGANA ならひらがな、JC_KATAKANA ならカタカナに
 *	変わる。文節の変換状態は変化しない。つまり変換されていれば
 *	変換状態のまま、未変換の状態なら未変換のままである。
 *	引数 small が 0 でなければカレント小文節が、そうでなければ
 *	カレント大文節が変わる。
 *	カレント大文節をかなにする場合、その中の小文節は一つにまとめられる。
 *
 * int jcFix(jcConvBuf *buf)
 *	現在、バッファにはいっている変換文字列を確定させる。
 *
 * int jcFix1(jcConvBuf *buf)
 *	現在、バッファにはいっている変換文字列の先頭一文字だけを確定させる。
 *
 * int jcExpand(jcConvBuf *buf, int small, int convf)
 *	カレント文節の長さを１文字伸ばす。引数 convf が 0 でなければ
 *	伸ばしたあと再変換する。
 *	引数 small が 0 でなければ小文節が、そうでなければ大文節が
 *	カレント文節として使われる。
 *
 * int jcShrink(jcConvBuf *buf, int small, int convf)
 *	カレント文節の長さを１文字縮める。引数 convf が 0 でなければ
 *	縮めたあと再変換する。
 *	引数 small が 0 でなければ小文節が、そうでなければ大文節が
 *	カレント文節として使われる。
 *
 * int jcNext(jcConvBuf *buf, int small, int prev)
 *	カレント文節を次候補又は前候補で置き換える。
 *	引数 small が 0 でなければ小文節が、そうでなければ大文節が
 *	カレント文節として使われる。
 *
 * int jcCandidateInfo(jcConvBuf *buf, int small, int *ncandp, int *curcandp)
 *	次候補の情報を返す。
 *	次候補一覧を出すためには最初にこの関数を呼ぶとよい。
 *
 * int jcGetCandidate(jcConvBuf *buf, int n, wchar *candstr, int len)
 *	指定された候補番号の文字列を返す。カレント候補番号はこの番号に
 *	変わる。表示バッファは変化しない。
 *	大昔の wnnlib は次候補が用意されていなければ用意したが、このバージョン
 *	ではエラーになる。jcNext や jcCandidateInfo を先に呼んでおかなければ
 *	ならない。
 *
 * int jcSelect(jcConvBuf *buf, int n)
 *	指定された番号の候補で表示バッファを置き換える。
 *	カレント候補番号はこの番号に変わる。
 *
 * int jcDotOffset(jcConvBuf *buf)
 *	大文節の先頭からのドットのオフセットを返す。
 *	例えば 0 ならドットがカレント文節の先頭にあることになる。
 *
 * int jcIsConverted(jcConvBuf *buf, int cl)
 *	指定された文節が変換されているかどうかを返す
 *	0 なら無変換状態
 *	1 なら変換状態
 *	-1 なら エラー
 *
 * int jcMove(jcConvBuf *buf, int small, int dir)
 *	ドット・カレント文節を移動する。
 *	カレント文節が変換済みであれば文節移動し、そうでなければ
 *	ドットのみが移動する。
 *	文節移動時に、引数 small が 0 でなければ小文節単位で移動し、
 *	そうでなければ大文節単位に移動する。
 *
 * int jcTop(jcConvBuf *buf)
 *	ドット・カレント文節を文の先頭に移動する。カレント小文節・
 *	カレント大文節ともに移動する。
 *
 * int jcBottom(jcConvBuf *buf)
 *	ドット・カレント文節を文の最後に移動する。カレント小文節・
 *	カレント大文節ともに移動する。
 *	もし、最後の文節が無変換状態であればカレント文節はその文節になり、
 *	ドットはその文節の最後に来る。そうでなければカレント文節は
 *	最後の文節の次 (つまり空の文節) に来る。
 *
 * int jcChangeClause(jcConvBuf *buf, wchar *str)
 *	カレント大文節を指定された文字列で入れ換える。
 *	表示バッファだけではなく、かなバッファの内容も
 *	置き換わる。文節は無変換状態になる。
 *
 * int jcSaveDic(jcConvBuf *buf)
 *	使用中の環境で使われている全ての辞書並びに頻度ファイルを
 *	セーブする。
 *	このファンクションは常に 0 を返す。本当にセーブされたかの
 *	チェックはしない。
 *
 * int jcCancel(jcConvBuf *buf)
 *      現在入力中のすべての文字列を、変換済みのものを含めて、すべて未
 *      変換状態にする。オリジナルの CcWnn と jclib インタフェースでは、
 *	先頭文節を全入力文字列の長さまで拡張することで、この処理を行なっ
 *	ていたが、この処理と jllib とのインタフェースがうまく合わず、
 *	wnnlib では独立したファンクションとした。
 *
 * int jcKillLine(jcConvBuf *buf)
 *      現在のドットあるいはカレント文節以降を削除する。ドットがある文
 *      節が既に変換されていれば、その文節、つまりカレント文節を含めて
 *      削除する。ドットあるいはカレント文節が先頭であれば、jcClear()
 *      と同じ動作をする。つまり、jcClear() 自体は不要になるのだが、旧
 *      インタフェースを考慮して、jcClear() はそのまま残す。
 *      なお、削除後のドットとカレント文節は、全変換対象文字列の末尾、
 *      あるいは最終文節の末尾にある空文節になる。
 *
 * これらのファンクションは特に書かれていなければ成功の場合には 0,
 * エラーの場合には -1 を返す。
 *
 */

/*
 * グローバル変数
 *
 * wnnlib で使われるグローバル変数は jcErrno ただ一つである。
 *
 * extern int jcErrno
 *	エラーの際に、エラーコードが代入される。エラーコードは wnnlib.h で
 *	定義されている。
 */

/*
 * データ構造
 *
 * wnnlib の持つデータで、アプリケーションから直接アクセスしてよいのは
 * 変換バッファ jcConvBuf 型の public member と書かれた部分のみである。
 * 直接アクセスしてよいといっても、値を参照するだけで、値を変更することは
 * 許されない。アプリケーションが勝手に値を変更した場合の wnnlib の動作は
 * 保証されない。
 *
 * <変換バッファ>
 *
 * jcConvBuf 型は wnnlib.h で次のように定義されている。
 *
 * typedef struct {
 *    /-* public member *-/
 *	int		nClause;	文節数
 *	int		curClause;	カレント文節番号
 *	int		curLCStart;	カレント大文節開始文節番号
 *	int		curLCEnd;	カレント大文節終了文節番号
 *	wchar		*kanaBuf;	かなバッファの先頭
 *	wchar		*kanaEnd;	かなバッファの末尾
 *	wchar		*displayBuf;	表示バッファの先頭
 *	wchar		*displayEnd;	表示バッファの末尾
 *	jcClause	*clauseInfo;	文節情報
 *	struct wnn_env	*wnn;
 *    /-* private member *-/
 *	[ 省略 ]
 * } jcConvBuf;
 *
 * nClause は現在の文節数を表す。これは小文節の数である。
 * curClause はカレント小文節の番号である。
 * curLCStart と curLCEnd はカレント大文節の範囲を示す。curLCStart から
 * curLCEnd-1 の範囲の文節がカレント大文節である。つまり、curLCEnd は
 * 次の大文節の先頭の番号である。
 *
 * kanaBuf と displayBuf がかなバッファと表示バッファである。
 * jcInsertChar() 等を使って入力された読みはかなバッファと表示バッファに入る。
 * これを変換すると、表示バッファの方だけが漢字の文字列になる。
 * kanaEnd および displayEnd はそれぞれのバッファに入れられた文字列の最後
 * の文字の次を指している。かなバッファ・表示バッファはどちらも NULL ターミ
 * ネートされない。
 *
 * clauseInfo は文節情報の入った配列である。これはあとで説明する。
 *
 * env はこの変換バッファの使用する環境である。
 *
 * <文節情報>
 *
 * 各文節の情報は clauseInfo という名前の jcClause 型の配列に入っている。
 * jcClause 型は wnnlib.h で次のように定義されている。
 *
 * typedef struct {
 *	wchar	*kanap;		読み文字列 (かなバッファの中を指す)
 *	wchar	*dispp;		表示文字列 (表示バッファの中を指す)
 *	char	conv;		変換済みか
 *				0: 未変換 1: 変換済 -1: wnnlib で疑似変換
 *	char	ltop;		大文節の先頭か?
 * } jcClause;
 *
 * kanap は、かなバッファ上の、その文節の読みの始まりの位置を示すポインタ
 * である。また、dispp は、表示バッファ 上で、その文節の始まりの位置を示す。
 * 従って、n 番の文節は、
 *	よみ:	clauseInfo[n].kanap から clauseInfo[n+1].kanap の前まで
 *	漢字:	clauseInfo[n].dispp から clauseInfo[n+1].dispp の前まで
 * となる。このように n 番目の文節の範囲を示すのに n+1 番目の clauseInfo が
 * 必要なため、clauseInfo の配列の要素は常に先頭から文節数+1個が有効である。
 * なお、先頭文節は 0 番目から始まるものとする。
 *
 * conv はその文節の変換状態を表す。0 なら未変換状態、1 なら変換状態、
 * -1 なら jcKana() によって疑似変換されたことを示す。これは、変換の学習と
 * 頻度情報の更新のために使用する。
 *
 * ltop が 0 でなければその文節が大文節の先頭であることを示す。imabit は
 * その文節の幹語の今使ったよビットが入っている。
 *
 * kanap, dispp 等で、n 番目の文節の範囲を示すのに n+1 番目の文節情報が
 * 必要なため、clauseInfo の配列の要素は常に先頭から文節数+1個が有効である。
 * 文節数+1 個目の文節情報 (clauseInfo[nClause]) は
 *	kanap, dispp: それぞれ kanaEnd, displayEnd に等しい
 *	conv: 0 (未変換状態)
 *	ltop: 1
 * である。
 *
 * 文節情報の kanap, dispp を例を使って示しておく。
 *
 * 例文: これはデータ構造を示すための例文です (文節数 6)
 *
 * kanap:   ０    １    ２        ３    ４    ５          ６(=kanaEnd)
 *	    ↓    ↓    ↓        ↓    ↓    ↓          ↓
 * kanaBuf: これはでーたこうぞうをしめすためのれいぶんです
 *
 * dispp:      ０    １    ２    ３  ４    ５      ６(=displayEnd)
 *	       ↓    ↓    ↓    ↓  ↓    ↓      ↓
 * displayBuf: これはデータ構造を示すための例文です
 */

#include <config.h>
#ifdef DEBUG_WNNLIB
#include	<stdio.h>
#endif
#include	"wnnlib.h"
#include	<stdlib.h>
#include	<unistd.h>
#include	<string.h>
#include	<sys/types.h>
#include	<pwd.h>
#include	<gettext.h>

#ifndef WNNENVDIR
#define WNNENVDIR	WNNLIBDIR "/wnn"
#endif

#include "uim.h"
#include "uim-scm.h"
#include "uim-scm-abbrev.h"
#include "uim-helper.h"
#include "uim-notify.h"
#include "gettext.h"
#include "dynlib.h"

/*
 * Wnn7 では大胆にもいくつかの API 関数にバッファサイズを指定する
 * 引数を追加しているため、バージョンを調べ、それによって引数を
 * 変更しなければならない。とりあえず本プログラムでは Wnn7 の引数に合わせる。
 */

/* Wnn7 かどうかの判定 */
#ifdef WNN_RENSOU
#define WNN7
#endif

#ifdef WNN7
#define ki2_jl_get_yomi			jl_get_yomi
#define ki2_jl_get_kanji		jl_get_kanji
#define ki2_jl_get_zenkouho_kanji	jl_get_zenkouho_kanji
#define ki2_jl_fuzokugo_get		jl_fuzokugo_get
#else
#define ki2_jl_get_yomi(a, b, c, d, sz)		jl_get_yomi(a, b, c, d)
#define ki2_jl_get_kanji(a, b, c, d, sz)	jl_get_kanji(a, b, c, d)
#define ki2_jl_get_zenkouho_kanji(a, b, c, sz)	jl_get_zenkouho_kanji(a, b, c)
#define ki2_jl_fuzokugo_get(a, b, sz)		jl_fuzokugo_get(a, b)
#endif /* WNN7 */

#ifdef DEBUG_WNNLIB
static void showBuffers(jcConvBuf *, char *);
static void printBuffer(wchar *start, wchar *end);
#define	TRACE(f, m)	fprintf(stderr, "%s: %s\n", (f), (m));
#else
#define	TRACE(f, m)
#endif

#define CHECKFIXED(buf)	\
	{ if ((buf)->fixed) { jcErrno = JE_ALREADYFIXED; return -1; } }
#define Free(p)		{if (p) free((char *)(p));}
#define DotSet(buf)	(buf)->dot = (buf)->clauseInfo[(buf)->curLCStart].kanap

#define KANABEG	0xa4a1	/* 'ぁ' */
#define KANAEND	0xa4f3	/* 'ん' */
#define KATAOFFSET	0x100	/* カタカナとひらがなのコード・オフセット */

/* 1文節の読み・漢字を取り出すバッファのサイズ */
#define CL_BUFSZ	512

/* デフォルトのバッファサイズ */
#define DEF_BUFFERSIZE	100	/* 100 文字 */
#define DEF_CLAUSESIZE	20	/* 20 文節 */
#define DEF_CANDSIZE	1024	/* 1K バイト */
#define DEF_RESETSIZE	10	/* 10 単語 */

/* buf->candKind の値 */
#define CAND_SMALL	0	/* 小文節候補 */
#define CAND_LARGE	1	/* 大文節候補 */

#define MAXFZK	LENGTHBUNSETSU

#ifdef SVR4
#define bcopy(p, q, l)	memmove(q, p, l)
#define bzero(p, l)	memset(p, 0, l)
#else
#if defined(SYSV) || defined(USG)
#define OVERLAP_BCOPY
extern char	*memset();
#define bzero(p, l)	memset(p, 0, l)
#endif
#endif

/* ファンクションプロトタイプ宣言 */
static wchar *wstrncpy(wchar *, wchar *, int);
static int wstrlen(wchar *);
static void moveKBuf(jcConvBuf *, int, int);
static void moveDBuf(jcConvBuf *, int, int);
static void moveCInfo(jcConvBuf *, int, int);
static int resizeBuffer(jcConvBuf *, int);
static int resizeCInfo(jcConvBuf *, int);
static void setCurClause(jcConvBuf *, int);
static int getHint(jcConvBuf *, int, int);
static int renConvert(jcConvBuf *, int);
static int tanConvert(jcConvBuf *, int);
static int doKanrenConvert(jcConvBuf *, int);
static int doKantanDConvert(jcConvBuf *, int, int);
static int doKantanSConvert(jcConvBuf *, int);
static int unconvert(jcConvBuf *, int, int);
static int expandOrShrink(jcConvBuf *, int, int, int);
static int makeConverted(jcConvBuf *, int);
static int getCandidates(jcConvBuf *, int);
static int setCandidate(jcConvBuf *, int);
static void checkCandidates(jcConvBuf *, int, int);
static int forceStudy(jcConvBuf *, int);

/* エラー番号 */
int	jcErrno;

/*
 *	portability のためのファンクション
 */

#ifdef OVERLAP_BCOPY
#undef bcopy
static void
bcopy(char *from, char *to, int n)
{
	if (n <= 0 || from == to) return;

	if (from < to) {
		from += n;
		to += n;
		while (n-- > 0)
			*--to = *--from;
	} else {
		while (n-- > 0)
			*to++ = *from++;
	}
}
#endif

/*
 *	wnnlib 内部で使われるファンクション
 */
static int
wstrcmp(wchar *s1, wchar *s2)
{
        while (*s1 && *s1 == *s2)
                s1++, s2++;
        return (int)(*s1 - *s2);
}

wchar *
wstrncpy(wchar *s1, wchar *s2, int n)
{
	wchar	*ret = s1;

	while (n-- > 0 && (*s1++ = *s2++))
		;
	while (n-- > 0)
		*s1++ = 0;

	return ret;
}

/* wstrlen -- wchar 型文字列の strlen */
static int
wstrlen(wchar *s)
{
	int	n = 0;

	while (*s++)
		n++;
	return n;
}

static int
euctows(wchar *wstr, const char *euc, int len)
{
  int i, j;
  wchar wc;

  j = 0;
  for (i = 0; i < len; i++) {
    wc = (euc[j + 1] << 8) | (euc[j] & 0xff);
    wstr[i] = htons(wc);
    j += sizeof(wchar);
  }
  return j;
}

static int
wstoeuc(char *euc, const wchar *wstr, int len)
{
  int i, j;
  wchar wc;

  j = 0;
  for (i = 0; i < len; i += sizeof(wchar)) {
    wc = ntohs(wstr[j]);
    euc[i]     = wc & 0xff;
    euc[i + 1] = wc >> 8;
    j++;
  }
  return j;
}

/* moveKBuf -- かなバッファの指定された文節の先頭からあとを動かす */
static void
moveKBuf(jcConvBuf *buf, int cl, int move)
{
	jcClause	*clp = buf->clauseInfo + cl;
	jcClause	*clpend;
	int		movelen;

	TRACE("moveKBuf", "Enter")

	if (move == 0) return;

	if ((movelen = buf->kanaEnd - clp->kanap) > 0) {
		/* かなバッファの内容を動かす */
		(void)bcopy((char *)clp->kanap, (char *)(clp->kanap + move),
			    movelen * sizeof(wchar));
	}

	/* かなバッファの変更に合わせて clauseInfo をアップデートする */
	clpend = buf->clauseInfo + buf->nClause;
	while (clp <= clpend) {
		clp->kanap += move;
		clp++;
	}

	/* kanaEnd のアップデート */
	buf->kanaEnd += move;
}

/* moveDBuf -- 表示バッファの指定された文節の先頭からあとを動かす */
static void
moveDBuf(jcConvBuf *buf, int cl, int move)
{
	jcClause	*clp = buf->clauseInfo + cl;
	jcClause	*clpend;
	int		movelen;

	TRACE("moveDBuf", "Enter")

	if (move == 0) return;

	if ((movelen = buf->displayEnd - clp->dispp) > 0) {
		/* 表示バッファの内容を動かす */
		(void)bcopy((char *)clp->dispp, (char *)(clp->dispp + move),
			    movelen * sizeof(wchar));
	}

	/* 表示バッファの変更に合わせて clauseInfo を
	 * アップデートする
	 */
	clpend = buf->clauseInfo + buf->nClause;
	while (clp <= clpend) {
		clp->dispp += move;
		clp++;
	}

	/* displayEnd のアップデート */
	buf->displayEnd += move;
}

/* moveCInfo -- ClauseInfo の指定された文節の先頭からあとを動かす */
static void
moveCInfo(jcConvBuf *buf, int cl, int move)
{
	jcClause	*clp = buf->clauseInfo + cl;
	int		len;

	TRACE("moveCInfo", "Enter")

	/* move に正の数を指定すれば文節の挿入、負なら文節の削除になる */

	if (move == 0) return;

	if ((len = buf->nClause + 1 - cl) > 0) {
		(void)bcopy((char *)clp, (char *)(clp + move),
			    len * sizeof(jcClause));
	}
	buf->nClause += move;

	/*
	 * 候補を取り出している文節があれば、無効にしておく。
	 *
	 * ただし、候補を取り出した結果、文節数が変化した場合には、
	 * setCandidate() の中で設定しなおされる、また、jllib 内でも
	 * 同じ文節に対する全候補取り出しがあった場合の考慮がある。
	 * ということで、ここは安全サイドでいく。
	 */
	if (buf->candClause >= 0) {
		buf->candClause = -1;
		buf->candClauseEnd = -1;
	}
}

/* resizeBuffer -- かな/表示バッファの大きさを変える */
static int
resizeBuffer(jcConvBuf *buf, int len)
{
	wchar	*kbufold, *dbufold;
	wchar	*kbufnew, *dbufnew;
	int	allocsize;
	jcClause	*clp, *clpend;

	TRACE("resizeBuffer", "Enter")

	kbufold = buf->kanaBuf;
	dbufold = buf->displayBuf;

	/* realloc する */
	allocsize = (len + 1) * sizeof(wchar);
	kbufnew = (wchar *)realloc((char *)kbufold, allocsize);
	dbufnew = (wchar *)realloc((char *)dbufold, allocsize);

	if (kbufnew == NULL || dbufnew == NULL) {
		Free(kbufnew);
		Free(dbufnew);
		jcErrno = JE_NOCORE;
		return -1;
	}

	buf->bufferSize = len;

	if (kbufnew == kbufold && dbufnew == dbufold) {
		/* ポインタは前と変わっていない */
		return 0;
	}

	/* 各種ポインタをつけ変える */

	buf->kanaBuf = kbufnew;
	buf->kanaEnd = kbufnew + (buf->kanaEnd - kbufold);
	buf->displayBuf = dbufnew;
	buf->displayEnd = dbufnew + (buf->displayEnd - dbufold);

	buf->dot = kbufnew + (buf->dot - kbufold);

	clp = buf->clauseInfo;
	clpend = clp + buf->nClause;
	while (clp <= clpend) {
		clp->kanap = kbufnew + (clp->kanap - kbufold);
		clp->dispp = dbufnew + (clp->dispp - dbufold);
		clp++;
	}

	return 0;
}

/* resizeCInfo -- clauseInfo バッファの大きさを変える */
static int
resizeCInfo(jcConvBuf *buf, int size)
{
	jcClause	*cinfonew;

	TRACE("resizeCInfo", "Enter")

	/* realloc する */
	cinfonew = (jcClause *)realloc((char *)buf->clauseInfo,
				       (size + 1) * sizeof(jcClause));
	if (cinfonew == NULL) {
		jcErrno = JE_NOCORE;
		return -1;
	}

	buf->clauseSize = size;
	buf->clauseInfo = cinfonew;
	return 0;
}

/* setCurClause -- カレント文節を設定する */
static void
setCurClause(jcConvBuf *buf, int cl)
{
	jcClause	*clp = buf->clauseInfo;
	int		i;

	TRACE("setCurClause", "Enter")

	/* カレント小文節 */
	buf->curClause = cl;

	/* カレント大文節開始文節 */
	for (i = cl; i > 0 && !clp[i].ltop; i--)
		;
	buf->curLCStart = i;

	/* カレント大文節終了文節 (の次) */
	for (i = cl + 1; i <= buf->nClause && !clp[i].ltop; i++)
		;
	buf->curLCEnd = i;
}

/* getHint -- 文節の前後の接続情報を得る */
static int
getHint(jcConvBuf *buf, int start, int end)
{
	jcClause *cinfo = buf->clauseInfo;
	int hint = 0;

	TRACE("getHint", "Enter")

	/*
	 * 最初の文節の直前の文節が変換されていれば、前の文節と接続をする
	 */
	if (start > 0 && cinfo[start - 1].conv == 1)
		hint |= WNN_USE_MAE;

	/*
	 * 最後の文節の直後が変換されていていれば、後の文節と接続をする
	 */
	if (end > 0 && end < jl_bun_suu(buf->wnn) && cinfo[end].conv == 1)
		hint |= WNN_USE_ATO;

	return hint;
}


/* renConvert -- カレント文節から後ろを連文節変換する */
static int
renConvert(jcConvBuf *buf, int small)
{
	TRACE("renConvert", "Enter")

	/* 連文節変換する */
	if (doKanrenConvert(buf,
			    small ? buf->curClause : buf->curLCStart) < 0) {
		return -1;
	}

	/*
	 * カレント文節の設定
	 * small が 0 なら、
	 *	カレント大文節の先頭は buf->curLCStart で変わらず
	 *	カレント大文節終りは ltop フラグをサーチして探す
	 *	カレント小文節はカレント大文節先頭に移動
	 * small が 0 でないなら、
	 *	カレント小文節は buf->curClause で変わらず
	 *	カレント大文節の先頭および終りは、カレント小文節の
	 *	前後を ltop フラグをサーチして探す
	 */
	setCurClause(buf, small ? buf->curClause : buf->curLCStart);

	/* ドットの設定 */
	DotSet(buf);

	return 0;
}

/* tanConvert -- カレント文節を単文節変換する */
static int
tanConvert(jcConvBuf *buf, int small)
{
	TRACE("tanConvert", "Enter")

	/*
	 * 単文節変換の場合、基本的に 2 段階の処理を行なうことになる
	 * まず、カレント文節を単文節変換
	 * 次に、そのあとを連文節変換
	 */

	if (small) {
		/* まず単文節変換する */
		if (doKantanSConvert(buf, buf->curClause) < 0)
			return -1;

		/* カレント文節の設定
		 *	カレント小文節は buf->curClause で変わらず
		 *	カレント大文節の先頭と最後はカレント小文節の
		 *	前後に ltop フラグをサーチして探す
		 */
		setCurClause(buf, buf->curClause);
		/* ドットの設定 */
		DotSet(buf);

		/* 連文節変換 */
		if (buf->curClause + 1 < buf->nClause &&
		    buf->clauseInfo[buf->curClause + 1].conv == 0) {
			/* 小文節の単文節変換モードで、次の文節が
			 * 無変換だった場合、ltop フラグを 0 にして
			 * 前と接続できるようにする
			 */
			buf->clauseInfo[buf->curClause + 1].ltop = 0;
		}
		if (doKanrenConvert(buf, buf->curClause + 1) < 0)
			return -1;

		/* もう一度カレント文節の設定
		 * 連文節変換の結果によってはカレント大文節の最後が
		 * 移動することがある
		 */
		setCurClause(buf, buf->curClause);

		/* ドットは移動しないので再設定しなくてよい */
	} else {
		/* まず単文節変換する */
		if (doKantanDConvert(buf, buf->curLCStart, buf->curLCEnd) < 0)
			return -1;

		/* カレント文節の設定
		 *	カレント大文節の先頭は buf->curLCStart で変わらず
		 *	カレント大文節終りは ltop フラグをサーチして探す
		 *	カレント小文節はカレント大文節先頭に移動
		 */
		setCurClause(buf, buf->curLCStart);
		DotSet(buf);

		/* 連文節変換 */
		if (doKanrenConvert(buf, buf->curLCEnd) < 0)
			return -1;
		/* こちらは small の時と違って連文節変換の結果カレント文節が
		 * 移動することはない
		 */
	}

	return 0;
}

/* doKanrenConvert -- 指定された文節から後ろを連文節変換する */
static int
doKanrenConvert(jcConvBuf *buf, int cl)
{
	jcClause	*clp;
	wchar	*kanap, *dispp;
	wchar	savechar;
	int	nsbun;
	int	len, n;

	TRACE("doKanrenConvert", "Enter")

	/*
	 * 指定された文節から後ろを連文節変換する
	 * カレント文節の再設定などはしない
	 */

	if (cl >= buf->nClause) {
		/* 指定された文節はない
		 * エラーにはしない
		 * 空の文節を変換しようとした時に、それを事前にチェックして
		 * エラーにするのは上位の関数の責任である
		 */
		return 0;
	}

	/*
	 * 変換する前に、少なくとも指定された文節の直前までが変換されて
         * いることを保証する
	 */
	if (makeConverted(buf, cl) < 0)
		return -1;

	clp = buf->clauseInfo + cl;

	/* かなバッファを NULL ターミネートさせておく */
	*(buf->kanaEnd) = 0;

	/* 連文節変換する */
#ifdef WNN6
	nsbun = jl_fi_ren_conv(buf->wnn, clp->kanap,
				 cl, -1, getHint(buf, cl, -1));
#else
	nsbun = jl_ren_conv(buf->wnn, clp->kanap,
				 cl, -1, getHint(buf, cl, -1));
#endif

	if (nsbun < 0) {
		jcErrno = JE_WNNERROR;
		return -1;
	}

	/* clauseInfo のサイズのチェック */
	if (nsbun > buf->clauseSize) {
		if (resizeCInfo(buf, cl + nsbun) < 0)
			return -1;
	}

	/* 次に変換文字列の長さのチェック */
	clp = buf->clauseInfo + cl;
	len = (clp->dispp - buf->displayBuf) + jl_kanji_len(buf->wnn, cl, -1);

	if (len > buf->bufferSize) {
		if (resizeBuffer(buf, len) < 0)
			return -1;
	}

	buf->nClause = nsbun;

	/* では clauseInfo に変換結果を入れていく */
	clp = buf->clauseInfo + cl;
	kanap = clp->kanap;
	dispp = clp->dispp;
	while (cl < buf->nClause) {
		n = cl + 1;

		/* 文節情報の設定 */
		clp->conv = 1;
		clp->kanap = kanap;
		clp->dispp = dispp;
		clp->ltop = jl_dai_top(buf->wnn, cl);

		/* 表示バッファへ変換文字列をコピーする */
		/* jl_get_kanji は、NULL までコピーするので注意 */
		len = jl_kanji_len(buf->wnn, cl, n);
		savechar = dispp[len];
		(void)ki2_jl_get_kanji(buf->wnn, cl, n, dispp, len);
		dispp[len] = savechar;
		dispp += len;

		/* かなバッファの位置を文節の最後にする */
		kanap += jl_yomi_len(buf->wnn, cl, n);

		/* カレント文節の更新 */
		cl = n;
		clp++;
	}

	/* 最後の clauseInfo の設定 */
	clp->kanap = buf->kanaEnd;
	clp->dispp = buf->displayEnd = dispp;
	clp->conv = 0;
	clp->ltop = 1;

#ifdef DEBUG_WNNLIB
	showBuffers(buf, "after doKanrenConvert");
#endif

	return 0;
}

/* doKantanDConvert -- 指定された範囲の文節を大文節として単文節変換する */
static int
doKantanDConvert(jcConvBuf *buf, int cls, int cle)
{
	jcClause	*clps, *clpe;
	int	len, diff, newlen;
	int	cldiff, nclausenew;
	wchar	*kanap, *dispp;
	wchar	savechar;
	wchar	*savep;
	int	nsbunnew, nsbunold;
	int	i, n;

	TRACE("doKantanDConvert", "Enter")

	/*
	 * 変換する前に、少なくとも指定された文節の直前までが変換されて
         * いることを保証する
	 */
	if (makeConverted(buf, cls) < 0)
		return -1;

	/*
	 * 指定された範囲の文節を大文節として単文節変換する
	 * カレント文節の再設定などはしない
	 */

	clps = buf->clauseInfo + cls;
	clpe = buf->clauseInfo + cle;
	nsbunold = jl_bun_suu(buf->wnn);
	if (nsbunold < 0) {
		jcErrno = JE_WNNERROR;
		return -1;
	}

	/*
	 * 読みを NULL ターミネートする
	 * 単に 0 を入れると次の文節が壊れるので、その前にセーブしておく
	 */
	savep = clpe->kanap;
	savechar = *savep;
	*savep = 0;

	/* 単文節変換する */
	nsbunnew = jl_tan_conv(buf->wnn, clps->kanap, cls, cle,
				 getHint(buf, cls, cle), WNN_DAI);

	/* すかさずセーブしてあった文字をもとに戻す */
	*savep = savechar;

	if (nsbunnew < 0) {
		jcErrno = JE_WNNERROR;
		return -1;
	}

	cldiff = (cle - cls) - (nsbunold - nsbunnew);
	nclausenew = buf->nClause + cldiff;
	/* clauseInfo のサイズのチェック */
	if (nclausenew > buf->clauseSize) {
		if (resizeCInfo(buf, nclausenew) < 0)
			return -1;
	}

	/* 変換文字列の長さのチェック */
	len = jl_kanji_len(buf->wnn, cls, cle + cldiff);
	diff = len - (clpe->dispp - clps->dispp);
	newlen = (buf->displayEnd - buf->displayBuf) + diff;
	if (newlen > buf->bufferSize) {
		if (resizeBuffer(buf, newlen) < 0)
			return -1;
	}

	/*
	 * 文節を挿入するので、表示バッファの内容を移動させる。
	 *
	 * どうせあとから連文節変換するからいいではないかという考え方もあるが、
	 * どこでエラーが起こっても一応の consistency が保たれるように
	 * するというのが目標である
	 */
	moveDBuf(buf, cle, diff);

	/* clauseInfo を動かす (同時に nClause もアップデートされる) */
	moveCInfo(buf, cle, cldiff);

	/* では clauseInfo に変換結果を入れる */
	clps = buf->clauseInfo + cls;
	kanap = clps->kanap;
	dispp = clps->dispp;
	cldiff += (cle - cls);
	for (i = 0; i < cldiff; i++) {
		n = cls + 1;

		/* 文節情報を設定する */
		clps->conv = 1;
		clps->ltop = jl_dai_top(buf->wnn, cls);
		clps->kanap = kanap;
		clps->dispp = dispp;

		/* 表示バッファへの変換文字列のコピー */
		/* jl_get_kanji は、NULL までコピーするので注意 */
		len = jl_kanji_len(buf->wnn, cls, n);
		savechar = dispp[len];
		(void)ki2_jl_get_kanji(buf->wnn, cls, n, dispp, len);
		dispp[len] = savechar;
		dispp += len;

		/* かなバッファの位置を更新 */
		kanap += jl_yomi_len(buf->wnn, cls, n);

		/* 次の文節情報の更新 */
		cls = n;
		clps++;
	}

	/* 次の clauseInfo の設定 */
	if (cls < jl_bun_suu(buf->wnn))
		clps->ltop = jl_dai_top(buf->wnn, cls);
	else
		clps->ltop = 1;

	return 0;
}

/* doKantanSConvert -- 指定された文節を小文節として単文節変換する */
static int
doKantanSConvert(jcConvBuf *buf, int cl)
{
	int	next = cl + 1;
	jcClause	*clp;
	int	len, newlen, diff;
	wchar	savechar;
	wchar	*savep;
	int	nsbun;

	TRACE("doKantanSConvert", "Enter")

	/*
	 * 変換する前に、少なくとも指定された文節の直前までが変換されて
         * いることを保証する
	 */
	if (makeConverted(buf, cl) < 0)
		return -1;

	/*
	 * 指定された文節を小文節として単文節変換する
	 * カレント文節の再設定などはしない
	 */

	clp = buf->clauseInfo + cl;

	/*
	 * 読みを NULL ターミネートする
	 * 単に 0 を入れると次の文節が壊れるので、その前にセーブしておく
	 */
	savep = (clp + 1)->kanap;
	savechar = *savep;
	*savep = 0;

	/* 単文節変換する */
	nsbun = jl_tan_conv(buf->wnn, clp->kanap, cl, next,
				getHint(buf, cl, next), WNN_SHO);


	/* すかさずセーブしてあった文字をもとに戻す */
	*savep = savechar;

	if (nsbun < 0) {
		jcErrno = JE_WNNERROR;
		return -1;
	}

	/* 変換文字列の長さのチェック */
	clp = buf->clauseInfo + cl;
	len = jl_kanji_len(buf->wnn, cl, -1);
	diff = len - ((clp + 1)->dispp - clp->dispp);
	newlen = (buf->displayEnd - buf->displayBuf) + diff;
	if (newlen > buf->bufferSize) {
		if (resizeBuffer(buf, newlen) < 0)
			return -1;
	}

	/* 文節を挿入するので、表示バッファの内容を移動させる */
	/* どうせあとから連文節変換するからいいではないかという考え方もあるが、
	 * どこでエラーが起こっても一応の consistency が保たれるように
	 * するというのが目標である
	 */
	moveDBuf(buf, next, diff);

	/* では clauseInfo に変換結果を入れる */
	clp = buf->clauseInfo + cl;
	clp->conv = 1;
	clp->ltop = jl_dai_top(buf->wnn, cl);

	/* 表示バッファへ変換文字列をコピー */
	/* jl_get_kanji では、最後の NULL もコピーされるので注意 */
	savechar = clp->dispp[len];
	(void)ki2_jl_get_kanji(buf->wnn, cl, next, clp->dispp, len);
	clp->dispp[len] = savechar;

	/* 次の clauseInfo の設定 */
	if (next < jl_bun_suu(buf->wnn))
		(clp + 1)->ltop = jl_dai_top(buf->wnn, next);

	return 0;
}


/* makeConverted -- 指定された文節の直前までが jllib で変換されている
   ことを保証する */
static int
makeConverted(jcConvBuf *buf, int cl)
{
	int	nsbun;
	int	next;
	int	status;
	wchar	savechar;
	jcClause	*clpc, *clpn;

	TRACE("makeConverted", "Enter")

#ifdef DEBUG_WNNLIB
	showBuffers(buf, "before makeConverted");
#endif

	/* 既に変換されているかチェックする */
	nsbun = jl_bun_suu(buf->wnn);
	if (cl <= nsbun)
		return 0;

	/* 変換されていない文節を登録する */
	clpc = buf->clauseInfo + nsbun;
	for (; nsbun < cl; nsbun = next, clpc = clpn) {
		clpn = clpc + 1;
		next = nsbun + 1;

		/* 既に登録されていれば、何もしない */
		if (clpc->conv == 1)
			continue;

		/* 表示文字列を NULL ターミネートする */
		savechar = *clpn->dispp;
		*clpn->dispp = 0;

		/*
		 * jllib には無変換の文節を登録する機能がないので、
		 * とりあえず前後の接続なしで単文節変換することにする
		 */
		status = jl_tan_conv(buf->wnn, clpc->dispp,
					nsbun, next, WNN_NO_USE, WNN_SHO);

		/* セーブした文字を戻す */
		*clpn->dispp = savechar;

		if (status < 0) {
			jcErrno = JE_WNNERROR;
			return -1;
		}
	}

#ifdef DEBUG_WNNLIB
	showBuffers(buf, "after makeConverted");
#endif

	return 0;
}

/* unconvert -- 指定された範囲の文節を一つの無変換の文節にする */
static int
unconvert(jcConvBuf *buf, int start, int end)
{
	jcClause	*clps, *clpe;
	int	diff, len;
	wchar	savechar;

	TRACE("unconvert", "Enter")

	if (end <= start)
		return 0;

	if (start >= buf->nClause)
		return 0;

#ifdef DEBUG_WNNLIB
	showBuffers(buf, "before unconvert");
#endif

	clps = buf->clauseInfo + start;
	clpe = buf->clauseInfo + end;

	/*
	 * 表示バッファの内容をかなバッファの内容で置き換える
	 * …といっても実際の動作はそれほど簡単ではない
	 *
	 * ・まず、置き換えた結果、表示バッファがあふれないか調べ、
	 *   あふれるようならバッファのサイズを大きくする
	 * ・表示バッファに、かなバッファからデータを移す
	 * ・clauseInfo を書き換えて、start から end-1 までの文節を
	 *   一つの無変換の文節にまとめる
	 * ・もちろん nClause も変える
	 * ・start+1 から最後までの文節の clauseInfo の dispp を
	 *   表示バッファのずれに応じて調整する
	 *
	 * その他に次のことも行なう必要があるが、この関数ではやらない
	 * 上位の関数で設定すること
	 * ・大文節フラグ (ltop) の設定
	 * ・カレント文節、および次候補文節の移動
	 *   次候補文節が無変換の文節になってしまった時の処理
	 * ・ドットの移動
	 */

	/* 読みの長さと漢字の長さの差を調べる */
	diff = (clpe->kanap - clps->kanap) - (clpe->dispp - clps->dispp);
	/* 置き換えた場合の表示バッファの長さ */
	len = (buf->displayEnd - buf->displayBuf) + diff;
	/* バッファのサイズが足りなければサイズを大きくする */
	if (len > buf->bufferSize) {
		if (resizeBuffer(buf, len) < 0) {
			/* サイズが変えられなかった */
			return -1;
		}
	}

	/* 置き換え */
	/* まず後ろの部分を動かしてから */
	moveDBuf(buf, end, diff);
	/* 読みを入れる */
	(void)bcopy((char *)clps->kanap, (char *)clps->dispp,
		    (clpe->kanap - clps->kanap) * sizeof(wchar));

	/*
	 * start から end までの文節を一つにまとめる
	 */

	/* 無変換状態になった文節の clauseInfo の設定 */
	clps->conv = 0;

	/* end からあとの clauseInfo を'つめる' */
	moveCInfo(buf, end, start + 1 - end);

	/* 文節を登録する */
	/* 登録されている文節の長さをチェック */
	if (jl_bun_suu(buf->wnn) < end)
		end = -1;

	/* 登録する前に、読みを NULL ターミネートしておく */
	clpe = clps + 1;
	savechar = *clpe->kanap;
	*clpe->kanap = 0;

	/* 無変換で登録したいができないので、前後の接続なしで、単文節
	 * 変換する
	 */

	len = jl_tan_conv(buf->wnn, clps->kanap,
				 start, end, WNN_NO_USE, WNN_SHO);

	/* 読みを元に、戻しておく */
	*clpe->kanap = savechar;

#ifdef DEBUG_WNNLIB
	showBuffers(buf, "after unconvert");
#endif

	/* 登録できたかを、チェック */
	if (len < 0) {
		jcErrno = JE_WNNERROR;
		return -1;
	}

	return 0;
}

static int
expandOrShrink(jcConvBuf *buf, int small, int expand, int convf)
{
	jcClause	*clp, *clpe;
	wchar	*kanap, *dispp;
	int	start, end;
	int	len;
	int	nsbun;

	TRACE("expandOrShrink", "Enter")

	start = small ? buf->curClause : buf->curLCStart;
	end = small ? start + 1 : buf->curLCEnd;

	clp = buf->clauseInfo + start;
	clpe = buf->clauseInfo + end;

	/*
	 * 伸び縮みできるかのチェック
	 */
	if (expand) {
		/*
		 * カレント文節が最後の文節の時には
		 * もう広げられない
		 */
		if (end >= buf->nClause) {
			jcErrno = JE_CANTEXPAND;
			return -1;
		}
		len = 1;
	} else {
		if (buf->curClause == buf->nClause ||
		    clpe->kanap - clp->kanap <= 1) {
			/* カレント文節が空か、あるいは長さが１以下 */
			jcErrno = JE_CANTSHRINK;
			return -1;
		}
		len = -1;
	}

	/* 全候補文節がカレント大文節かそれ以降にあれば無効にする */
	checkCandidates(buf, start, buf->nClause);

	/* jclib と互換を保つため、再変換指定でない場合は、特別に処理する */
	if (!convf) {
		/* jclib と同様に unconvert() を使って、処理をしても良いの
		 * だが、無駄があるので独自の処理とする
		 */
		int ksize;
		int dsize;

		/* jllib の情報があれば、カレント文節以降を無効にする */
		if (start < jl_bun_suu(buf->wnn))
			jl_kill(buf->wnn, start, -1);

		/* カレント文節以降の表示バッファの内容を、かなバッファ
		 * の内容で置換える (unconvert() 参照)
		 */

		clp = buf->clauseInfo + start;

		/* まず、表示バッファの大きさを調べ、必要ならばバッファ
		 * を拡張する
		 */
		ksize = buf->kanaEnd - clp->kanap;
		dsize = ksize + (clp->dispp - buf->displayBuf);
		if (dsize > buf->bufferSize) {
			if (resizeBuffer(buf, dsize))
				return -1;
		}

		/* 表示バッファの内容を、かなバッファの内容で置換える */
		bcopy(clp->kanap, clp->dispp, ksize * sizeof (wchar));

		/* 表示バッファの終りを設定する */
		buf->displayEnd = clp->dispp + ksize;

		/* カレント文節を設定する
		 */
		buf->curClause = buf->curLCStart = start;
		buf->dot = clp->kanap;
		clp->conv = 0;
		clp->ltop = 1;

		/* 伸縮した結果、文節数は start + 1 (カレント文節の長
		 * さが 1 であった時、縮めた結果カレント文節がなくなる。
		 * または、カレント文節の後にひとつの文節しかなく、そ
		 * の文節の長さが 1 であった場合、伸ばした結果カレント
		 * 文節より後の文節がなくなる) か start + 2 になる
		 */

		/* まず、伸縮後のカレント文節の長さを計算する */
		ksize = buf->clauseInfo[end].kanap - clp->kanap + len;

		/* そして、カレント文節の後にある文節を設定する */
		if (ksize == 0 || buf->displayEnd == clp->dispp + ksize) {
			/* 縮めた結果カレント文節がなくなったか、
			 * 伸ばした結果カレント文節の後の文節がなくなった
			 *
			 * これらの場合は、前のカレント文節以降をひと
			 * まとめ (ひとつの大文節) にして、それをカレ
			 * ント文節 (大文節) としてしまう
			 *
			 * この時、clauseInfo の大きさは、必ず start + 1
			 * より大きいことが保証されている
			 */
			buf->nClause = buf->curLCEnd = start + 1;

			/* 末尾文節をポイントさせる */
			clp++;
		} else if (start + 2 > buf->clauseSize
				&& resizeCInfo(buf, start + 1) < 0) {
			/* 縮めようとする文節が最後の文節だった場合、
			 * 長さが 1 の文節が増えることになる。
			 * が、clauseInfo の大きさをチェックし、それを
			 * 増やせなかったので、カレント文節以降を全部ひ
			 * とまとめにする (バッファの整合性を保つため)
			 */
			buf->nClause = buf->curLCEnd = start + 1;
			clp++;
			clp->kanap = buf->kanaEnd;
			clp->dispp = buf->displayEnd;
			clp->conv = 0;
			clp->ltop = 1;

			/* でも、エラーはエラーなので、エラーとして返す */
#ifdef DEBUG_WNNLIB
			showBuffers(buf,
				"after expandOrShrink [noconv, error]");
#endif
			return -1;
		} else {
			/* 伸縮できたので、カレント文節の後の文節を設定する
			 * (あまり、意味はないとは思うが、小文節の伸縮後の
			 *  大文節の設定は、jclib の設定と同じにしておく)
			 */
			buf->curLCEnd = start + (small ? 2 : 1);
			buf->nClause = start + 2;
			clpe = clp + 1;
			clpe->kanap = clp->kanap + ksize;
			clpe->dispp = clp->dispp + ksize;
			clpe->conv = 0;
			clpe->ltop = small ? 0 : 1;

			/* 末尾文節をポイントさせる */
			clp += 2;
		}

		/* 末尾文節の情報を設定する */
		clp->kanap = buf->kanaEnd;
		clp->dispp = buf->displayEnd;
		clp->conv = 0;
		clp->ltop = 1;

#ifdef DEBUG_WNNLIB
		showBuffers(buf, "after expandOrShrink [noconv]");
#endif
		return 0;
	}

	/* すべての文節が変換されていることを保証する */
	makeConverted(buf, buf->nClause);

	/*
	 * 文節の長さを変更する。この時、前文節に接続可能にしておくと
	 * 困ることがある。例えば「無量大数」と入力しようとして、
	 *   a) "むりょうたいすう" を変換すると"無料 対数" となる。
	 *   b) "無料" を "無量" に直す。
	 *   c) "対数" を "大数" に直そうと思ったが候補にないので2文字分
	 *      文節を縮めて "大 数" に分けようとする。
	 *   d) ところが "たい" が前候補に接続してしまい、"無量体 数" になる。
	 *   e) "無量大" という候補はないので、仕方なく2文字文節を縮めると
	 *      "無料 対数" になってしまった。
	 *   f) b) に戻る。
	 * (ま、この場合にははじめから「無量大数」を登録しておけばいいのだが)
	 */
       len += jl_yomi_len(buf->wnn, start, end);
#ifdef WNN6
	nsbun = jl_fi_nobi_conv(buf->wnn, start, len, -1, 0,
					small ? WNN_SHO : WNN_DAI);
#else
	nsbun = jl_nobi_conv(buf->wnn, start, len, -1, 0,
				small ? WNN_SHO : WNN_DAI);
#endif

	if (nsbun < 0) {
		jcErrno = JE_WNNERROR;
		return -1;
	}

	/* clauseInfo のサイズのチェックして、必要ならば増やす */
	if (nsbun > buf->clauseSize) {
		if (resizeCInfo(buf, nsbun) < 0)
			return -1;
	}
	buf->nClause = nsbun;

	/* 表示バッファの大きさをチェックして、必要ならば増やす */
	clp = buf->clauseInfo + start;
	len = clp->dispp - buf->displayBuf + jl_kanji_len(buf->wnn, start, -1);
	if (len > buf->bufferSize) {
		if (resizeBuffer(buf, len) < 0)
			return -1;
	}

	/* カレント文節を覚えておく */
	buf->curClause = start;

	/* 変換結果を、表示バッファに入れていく */
	clp = buf->clauseInfo + start;
	kanap = clp->kanap;
	dispp = clp->dispp;
	while (start < nsbun) {
		end = start + 1;

		/* 文節情報の設定 */
		clp->kanap = kanap;
		clp->dispp = dispp;

		/* 表示バッファに変換文字列をコピー
		 * jl_get_kanji は最後の NULL までコピーされるので注意
		 */
		{
			int i = jl_kanji_len(buf->wnn, start, end);
			wchar c = dispp[i];

			(void)ki2_jl_get_kanji(buf->wnn, start, end, dispp, i);
			dispp[i] = c;	/* 元に戻す */
			dispp += i;	/* 位置の更新 */
			clp->conv = 1;
			clp->ltop = jl_dai_top(buf->wnn, start);
		}

		/* かなバッファの位置を更新 */
		kanap += jl_yomi_len(buf->wnn, start, end);

		/* 次の文節へ */
		start = end;
		clp++;
	}

	/* 最後の clauseInfo の設定 */
	clp->kanap = buf->kanaEnd;
	clp->dispp = buf->displayEnd = dispp;
	clp->conv = 0;
	clp->ltop = 1;

	/* カレント文節を再設定する */
	setCurClause(buf, buf->curClause);

	/* ドットの再設定 */
	DotSet(buf);

#ifdef DEBUG_WNNLIB
	showBuffers(buf, "after expand_or_shrink");
#endif
	return 0;
}

/* getCandidates -- 全候補を取り出す。ただし、既に取り出し済みなら何もしない */
static int
getCandidates(jcConvBuf *buf, int small)
{
	int start, end;

	TRACE("getCandidates", "Enter")

	/*
	 * 既に候補が取り出されている場合、カレント文節と候補文節が一致
	 * しないこともあるが、候補文節の設定を優先することにする。この
	 * 場合、候補文節は、必ずカレント文節に等しいかそれに含まれてい
	 * るはず。
	 */
	if (small) {
		/* 候補が取り出し済みなら、何もしない */
		if (buf->candKind == CAND_SMALL &&
		    buf->candClause == buf->curClause)
			return 0;

		/* カレント小文節の候補を取り出す */
		start = buf->curClause;
		end = start + 1;
		if (jl_zenkouho(buf->wnn,
				 start,
				 getHint(buf, start, end) & WNN_USE_MAE,
				 WNN_UNIQ) < 0) {
			buf->candClause = -1;
			jcErrno = JE_WNNERROR;
			return -1;
		}
	} else {
		/* 候補が取り出し済みなら、何もしない */
#if 0
		if (buf->candKind == CAND_LARGE &&
		    buf->candClause >= buf->curLCStart &&
		    buf->candClauseEnd <= buf->curLCEnd)
			return 0;
#else
		if (buf->candKind == CAND_LARGE &&
		    buf->candClause >= buf->curLCStart &&
		    buf->candClauseEnd <= buf->curLCEnd &&
		    buf->candClause <= buf->curClause &&
		    buf->candClauseEnd > buf->curClause)
			return 0;
#endif

		/* カレント大文節の候補を取り出す */
		start = buf->curLCStart;
		end = buf->curLCEnd;
#ifndef WNN6
		/*
		 * jl ライブラリの候補バッファ内容を破棄する。
		 * curLCStart が以前と同じでかつ curLCEnd が
		 * 異なる場合 (つまりカレント大文節が後ろに伸びた場合)、
		 * こうしないと Wnn4 の jl ライブラリは候補を再取得
		 * してくれない。
		 */
		jl_kill(buf->wnn, 0, 0);
#endif
		if (jl_zenkouho_dai(buf->wnn,
					start,
					end,
					getHint(buf, start, end),
					WNN_UNIQ) < 0) {
			buf->candClause = -1;
			jcErrno = JE_WNNERROR;
			return -1;
		}
	}

	/* 次候補の取り出しのための情報を覚えておく */
	buf->candKind = small ? CAND_SMALL : CAND_LARGE;
	buf->candClause = start;
	buf->candClauseEnd = end;
	return 0;
}

/* setCandidate -- 指定された候補でバッファを置き換える */
static int
setCandidate(jcConvBuf *buf, int n)
{
	int	start = buf->candClause;
	int 	end = buf->candClauseEnd;
	int	oldlen, newlen, bdiff;
	int	oldclen, newclen, cdiff;
	int	newend;
	jcClause	*clp;

	TRACE("setCandidate", "Enter")
#ifdef DEBUG_WNNLIB
	fprintf(stderr, "setCandidate for %d as %s\n",
		n, buf->candKind == CAND_SMALL ? "small" : "large");
	showBuffers(buf, "setCandiate (before)");
#endif

	clp = buf->clauseInfo + start;
	oldlen = (buf->clauseInfo + end)->dispp - clp->dispp;
	oldclen = jl_bun_suu(buf->wnn);

	if (buf->candKind == CAND_SMALL) {
		/* カレント小文節を、指定候補で置き換える */
		if (jl_set_jikouho(buf->wnn, n) < 0) {
			jcErrno = JE_WNNERROR;
			return -1;
		}
	} else {
		/* カレント大文節を、指定候補で置き換える */
		if (jl_set_jikouho_dai(buf->wnn, n) < 0) {
			jcErrno = JE_WNNERROR;
			return -1;
		}
	}

	/* 変換後の文節数のチェックする */
	newclen = jl_bun_suu(buf->wnn);
	if (newclen < 0) {
		jcErrno = JE_WNNERROR;
		return -1;
	}
	cdiff = newclen - oldclen;
	newend = end + cdiff;

	/* 変換後のディスプレイバッファのサイズをチェックする */
	newlen = jl_kanji_len(buf->wnn, start, newend);
	if (newlen <= 0) {
		jcErrno = JE_WNNERROR;
		return -1;
	}
	bdiff = newlen - oldlen;

#ifdef DEBUG_WNNLIB
	{
		wchar	candstr[1024];

		fprintf(stderr, "Candidate[%s]: '",
			buf->candKind == CAND_SMALL ? "small" : "large");
		if (newlen >= 1024) {
			fprintf(stderr,
				 "* candidate string is too large [%d] *",
				 newlen);
		} else {
			candstr[0] = 0;
			ki2_jl_get_zenkouho_kanji(buf->wnn, n, candstr, sizeof(candstr));
			printBuffer (candstr, candstr + newlen);
		}
		fprintf(stderr, "'\n");
	}
#endif

	/*
	 * ディスプレイバッファを再設定する
	 *
	 * 最初に、充分なディスプレイバッファの大きさを確保しておく。
	 * 次に、置き換え後の候補文字列のための場所を確保する。最後に、
	 * 置き換える候補文字列をディスプレイバッファに挿入する。
	 */
	{
		int	buflen = (buf->displayEnd - buf->displayBuf) + bdiff;
		wchar	*dispp = clp->dispp;
		wchar	tmp;

		if (buflen > buf->bufferSize
		    && resizeBuffer(buf, buflen) < 0) {
			return -1;
		}

		moveDBuf(buf, end, bdiff);

		/*
		 * 候補文字列の挿入は、jl_get_kanji() を用いるので、
		 * それが設定する最後の NUL 文字に注意。
		 */
		tmp = dispp[newlen];
		if (ki2_jl_get_kanji(buf->wnn, start, newend,
				     dispp, newlen) <= 0) {
			jcErrno = JE_WNNERROR;
			return -1;
		}
		dispp[newlen] = tmp;
	}


	/*
	 * clauseInfoを再設定する
	 *
	 * 最初に、充分な clauseInfo の大きさを確保しておく。次に、
	 * 候補置き換え後の文節情報のための場所を確保する。最後に、
	 * 置き換えた候補のバッファ情報を設定する。
	 */
	{
		wchar	*kanap, *dispp;
		int	i, j;

		if (buf->nClause + cdiff > buf->clauseSize
        	    && resizeCInfo(buf, buf->nClause + cdiff) < 0) {
			return -1;
		}

		moveCInfo(buf, end, cdiff);

		kanap = clp->kanap;
		dispp = clp->dispp;
		for (i = start; i < newend; i = j) {
			clp->kanap = kanap;
			clp->dispp = dispp;
			clp->conv = 1;
			clp->ltop = jl_dai_top(buf->wnn, i);
			j = i + 1;
			kanap += jl_yomi_len(buf->wnn, i, j);
			dispp += jl_kanji_len(buf->wnn, i, j);
			clp++;
		}

		/*
		 * 候補の取り出しによって、前後の大文節が変更になって
		 * いるかもしれないので、それらを再設定する。直後の文
		 * 節だけで良いはずだが、念のため、すべてをチェックす
		 * ることにする。
		 */
		for (i = 0; i < start; i++)
			buf->clauseInfo[i].ltop = jl_dai_top(buf->wnn, i);
		for (i = newend; i < newclen; i++)
			buf->clauseInfo[i].ltop = jl_dai_top(buf->wnn, i);
	}

	/*
	 * 次候補で置き換えた結果、置き換え対象文節とその前後の大文節
	 * が移動しているかもしれないので、カレント文節を再設定する。
	 */
	setCurClause(buf, start);

	/*
	 * 文節の移動に伴い、候補文節も移動しているはずなので、再設定
	 * しておく (moveCInfo() 参照)
	 */
	buf->candClause = start;
	buf->candClauseEnd = end + cdiff;

#ifdef DEBUG_WNNLIB
	showBuffers(buf, "setCandiate (after)");
#endif
	return 0;
}

/* checkCandidates -- 全候補が有効かチェックして、必要な処理を行なう */
static void
checkCandidates(jcConvBuf *buf, int cls, int cle)
{
	/* 文節番号 cls から cle - 1 までの文節が変更される
	 * 次候補バッファにはいっている候補文節がこの中に含まれていれば
	 * 次候補バッファの内容を無効にしなくてはならない
	 *
	 * どのような場合かというと、
	 * 1. buf->candKind が CAND_SMALL で、
	 *      cls <= buf->candClause < cle
	 * 2. buf->candKind が CAND_LARGE で、
	 *      buf->candClause < cle かつ cls < buf->candClauseEnd
         */
	if (buf->candKind == CAND_SMALL)
		buf->candClauseEnd = buf->candClause + 1; /* 念のため */
        if (buf->candClause < cle && cls < buf->candClauseEnd) {
		/* 無効にする */
		buf->candClause = buf->candClauseEnd = -1;
	}
}


/* forceStudy -- 未変換および疑似変換文節の学習 */
static int
forceStudy(jcConvBuf *buf, int n)
{
	int i, j, k;
	int status;
	wchar yomi[CL_BUFSZ], kanji[CL_BUFSZ];

	TRACE("forceStudy", "Enter")

#ifdef DEBUG_WNNLIB
	showBuffers(buf, "forceStudy");
#endif

	if (n < 0 || n > buf->nClause)
		n = buf->nClause;

	/* ここでいう学習とは、頻度情報の更新と考えてよい */

	/*
	 * Wnn6 では、無変換学習機能があり、wnnlib による疑似変換や未
	 * 変換の文節を学習させることができる。ただし、未変換の文節に
	 * 対して単純に頻度の更新はできないので注意
	 */

	/*
	 * 最初に、変換済みの文節数を調べる。入力された文節がすべて変
	 * 換済み (conv == 1) であれば、全文節をの文節の頻度情報をまと
	 * めて更新する。変換済みでない文節があった場合、とりあえず変
	 * 換して、変換結果が表示バッファの内容と一致していれば、頻度
	 * 情報を更新することにする
	 */
	status = 0;
	for (i = 0; i < n; i++) {
		if (buf->clauseInfo[i].conv == 1)
			status++;
	}

	/* すべての文節が変換されていたら、全ての文節の頻度を更新する */
	if (status == n) {
#ifdef WNN6
		status = jl_optimize_fi(buf->wnn, 0, -1);
#else
		status = jl_update_hindo(buf->wnn, 0, -1);
#endif
		if (status < 0) {
			jcErrno = JE_WNNERROR;
			return -1;
		}
		return 0;
	}

	/*
	 * 文節単位で頻度情報を更新する
	 * 未変換の文節があれば、未変換として頻度情報を更新する (Wnn6
	 * の無変換学習機能)
	 */

	/* 頻度情報を更新する前に、全文節を変換しておく */
	if (makeConverted(buf, n) < 0)
		return -1;

	for (i = 0; i < n; i = j) {
		j = i + 1;
		/*
		 * 変換済みの文節であれば、そのまま頻度情報を更新する
		 */
		if (buf->clauseInfo[i].conv == 1) {
#ifdef WNN6
			status = jl_optimize_fi(buf->wnn, i, j);
#else
			status = jl_update_hindo(buf->wnn, i, j);
#endif
			if (status < 0) {
				jcErrno = JE_WNNERROR;
				return -1;
			}
			continue;
		}

		/*
		 * 未変換と疑似変換の文節に対しては、読みを学習する
		 * 未変換と疑似変換の場合でも、wnnlib では表示バッファとか
		 * なバッファの両方が一致しているので (jcKana() 参照)、
		 * ここでは jllib の読みデータを使用する
		 */

		/* 読み文字列と変換済文字列の長さチェック */
		if (jl_yomi_len(buf->wnn, i, j) >= CL_BUFSZ ||
		    jl_kanji_len(buf->wnn, i, j) >= CL_BUFSZ) {
			/* バッファオーバフローを避ける */
			continue;
		}

		/* 読み文字列の取り出し */
		if (ki2_jl_get_yomi(buf->wnn, i, j, yomi, CL_BUFSZ) < 0) {
			jcErrno = JE_WNNERROR;
			return -1;
		}
		/* 変換済み文字列を取り出す */
		if (ki2_jl_get_kanji(buf->wnn, i, j, kanji, CL_BUFSZ) < 0) {
			jcErrno = JE_WNNERROR;
			return -1;
		}

		/*
		 * 読みと変換後が一致していれば、学習済みとみなして、
		 * そのまま頻度情報を更新する
		 */
		if (wstrcmp (yomi, kanji) == 0) {
#ifdef WNN6
			status = jl_optimize_fi(buf->wnn, i, j);
#else
			status = jl_update_hindo(buf->wnn, i, j);
#endif
			if (status < 0) {
				jcErrno = JE_WNNERROR;
				return -1;
			}
			continue;
		}

		/*
		 * 読みと変換後が一致しないので、全候補の中から探す
		 * もし、一致するものがあれば、頻度情報を更新し、そう
		 * でなければ頻度情報は更新しない
		 */
		if (jl_zenkouho(buf->wnn, i,
			 	getHint(buf, -1, -1), WNN_UNIQ) < 0) {
			jcErrno = JE_WNNERROR;
			return -1;
		}
		status = jl_zenkouho_suu(buf->wnn);
		if (status < 0) {
			jcErrno = JE_WNNERROR;
			return -1;
		}
		for (k = 0; k < status; k++) {
			ki2_jl_get_zenkouho_kanji(buf->wnn, k, kanji,
						  CL_BUFSZ);
			/* 必ず NUL ターミネートされるようにしておく */
			kanji[CL_BUFSZ - 1] = 0;
			if (wstrcmp(yomi, kanji) != 0)
				continue;
			if (jl_set_jikouho(buf->wnn, k) < 0) {
				jcErrno = JE_WNNERROR;
				return -1;
			}
#ifdef WNN6
			status = jl_optimize_fi(buf->wnn, i, j);
#else
			status = jl_update_hindo(buf->wnn, i, j);
#endif
			if (status < 0) {
				jcErrno = JE_WNNERROR;
				return -1;
			}
			break;
		}
	}

	return 0;
}


/*
 *	ここから Public なファンクション
 */

/* jcCreateBuf -- 変換バッファの作成 */
jcConvBuf *
jcCreateBuffer(struct wnn_buf *wnn, int nclause, int buffersize)
{
	jcConvBuf	*buf;

	TRACE("jcCreateBuffer", "Enter")

	/* まず jcConvBuf の確保 */
	if ((buf = (jcConvBuf *)malloc(sizeof(jcConvBuf))) == NULL) {
		jcErrno = JE_NOCORE;
		return NULL;
	}
	(void)bzero((char *)buf, sizeof(jcConvBuf));
	buf->wnn = wnn;

	/* 次に各種バッファの確保 */

	/* まず、かなバッファと表示バッファ */
	buf->bufferSize = (buffersize <= 0) ? DEF_BUFFERSIZE : buffersize;
	/* バッファの最後を NULL ターミネートすることがあるので、
	 * 1文字文大きくしておく
	 */
	buf->kanaBuf = (wchar *)malloc((buf->bufferSize + 1) *
					 sizeof(wchar));
	buf->displayBuf = (wchar *)malloc((buf->bufferSize + 1) *
					    sizeof(wchar));

	/* 次に clauseInfo バッファ */
	buf->clauseSize = (nclause <= 0) ? DEF_CLAUSESIZE : nclause;
	/* clauseInfo バッファは nclause + 1 個アロケートする
	 * なぜかというと clauseinfo はデリミタとして要素を
	 * 1個使うので nclause 個の文節を扱うためには nclause + 1 個の
	 * 大きさを持たなければならないからである
	 */
	buf->clauseInfo = (jcClause *)malloc((buf->clauseSize + 1)
					     * sizeof(jcClause));

	if (buf->kanaBuf == NULL || buf->displayBuf == NULL ||
	    buf->clauseInfo == NULL) {
		/* malloc() できなかった */
		Free(buf->kanaBuf);
		Free(buf->displayBuf);
		Free(buf->clauseInfo);
		Free(buf);
		jcErrno = JE_NOCORE;
		return NULL;
	}

	(void)jcClear(buf);
	return buf;
}

static uim_lisp
uim_wnn_jcCreateBuffer(uim_lisp wnn_, uim_lisp nclause_, uim_lisp buffersize_)
{
  jcConvBuf *buf;

  buf = jcCreateBuffer(C_PTR(wnn_), C_INT(nclause_), C_INT(buffersize_));
  if (!buf)
    return uim_scm_f();
  return MAKE_PTR(buf);
}

/* jcDestroyBuffer -- 変換バッファの消去 */
int
jcDestroyBuffer(jcConvBuf *buf, int savedic)
{
	TRACE("jcDestroyBuffer", "Enter")

	if (buf == NULL)
		return 0;

	/* アロケートしたメモリの解放 */
	Free(buf->kanaBuf);
	Free(buf->displayBuf);
	Free(buf->clauseInfo);

	/* savedic が 0 でなければ、環境にロードされている全てのファイルを
	 * save する
	 */
	if (savedic && jl_dic_save_all(buf->wnn) < 0) {
		jcErrno = JE_WNNERROR;
		return -1;
	}

	Free(buf);

	return 0;
}

static uim_lisp
uim_wnn_jcDestroyBuffer(uim_lisp buf_, uim_lisp savedic_)
{
  return MAKE_BOOL(jcDestroyBuffer(C_PTR(buf_), C_BOOL(savedic_)) == 0);
}

/* jcClear -- wnnlib の初期化 (新たな変換を始める毎に呼ばなければならない) */
int
jcClear(jcConvBuf *buf)
{
	TRACE("jcClear", "Enter")

	/* 初期値の設定 */
	buf->nClause = buf->curClause = buf->curLCStart = 0;
	buf->curLCEnd = 1;
	buf->candClause = buf->candClauseEnd = -1;
	buf->kanaEnd = buf->kanaBuf;
	buf->displayEnd = buf->displayBuf;
	buf->clauseInfo[0].kanap = buf->kanaBuf;
	buf->clauseInfo[0].dispp = buf->displayBuf;
	buf->clauseInfo[0].conv = 0;
	buf->clauseInfo[0].ltop = 1;
	buf->dot = buf->kanaBuf;
	buf->fixed = 0;
	jcErrno = JE_NOERROR;

	if (jl_bun_suu(buf->wnn) > 0)
		jl_kill(buf->wnn, 0, -1);

	return 0;
}

static uim_lisp
uim_wnn_jcClear(uim_lisp buf_)
{
  return MAKE_BOOL(jcClear(C_PTR(buf_)) == 0);
}

/* jcConvert -- カレント文節以降をかな漢字変換する */
int
jcConvert(jcConvBuf *buf, int small, int tan, int jump)
{
	int	ret;

	TRACE("jcConvert", "Enter")

	CHECKFIXED(buf);

	if (buf->curClause == buf->nClause) {
		/* カレント文節が最後の文節でしかも空 */
		jcErrno = JE_CLAUSEEMPTY;
		return -1;
	}

	/* 全候補文節がカレント大文節かそれ以降にあれば無効にする */
	checkCandidates(buf,
			small ? buf->curClause : buf->curLCStart,
			buf->nClause);

	if (tan) {
		ret = tanConvert(buf, small);
	} else {
		ret = renConvert(buf, small);
	}

	if (ret < 0)
		return ret;

	if (jump) {
		/* ドットとカレント文節を文の最後に移動させる */
		buf->curClause = buf->curLCStart = buf->nClause;
		buf->curLCEnd = buf->nClause + 1;
		buf->dot = buf->kanaEnd;
	}
	return 0;
}

static uim_lisp
uim_wnn_jcConvert(uim_lisp buf_, uim_lisp small_, uim_lisp tan_, uim_lisp jump_)
{
  return MAKE_BOOL(jcConvert(C_PTR(buf_), C_BOOL(small_), C_BOOL(tan_), C_BOOL(jump_)) == 0);
}

/* jcUnconvert -- カレント大文節を無変換の状態に戻す */
int
jcUnconvert(jcConvBuf *buf)
{
	jcClause	*clp = buf->clauseInfo + buf->curClause;

	TRACE("jcUnconvert", "Enter")

	CHECKFIXED(buf);

	if (buf->curClause == buf->nClause) {
		/* カレント文節が最後の文節でしかも空 */
		jcErrno = JE_CLAUSEEMPTY;
		return -1;
	}

	if (!clp->conv) {
		/* カレント文節は変換されていない */
		/* 無変換の文節は wnnlib 内部では常に大文節として
		 * 扱われるので、カレント小文節の変換状態を見て、
		 * それが変換状態ならカレント大文節内の
		 * 全ての小文節は変換状態、そうでなければ無変換状態、
		 * になる
		 */
		jcErrno = JE_NOTCONVERTED;
		return -1;
	}

	/* 全候補文節がカレント大文節かそれ以降にあれば無効にする */
	checkCandidates(buf, buf->curLCStart, buf->nClause);

	/* 無変換状態にする */
	if (unconvert(buf, buf->curLCStart, buf->curLCEnd) < 0)
		return -1;

	/* 大文節の設定 */
	clp = buf->clauseInfo + buf->curLCStart;
	clp->ltop = 1;
	(++clp)->ltop = 1;

	/* カレント文節の再設定 */
	buf->curClause = buf->curLCStart;
	buf->curLCEnd = buf->curLCStart + 1;

	/* ドットの設定 */
	DotSet(buf);

	return 0;
}

static uim_lisp
uim_wnn_jcUnconvert(uim_lisp buf_)
{
  return MAKE_BOOL(jcUnconvert(C_PTR(buf_)) == 0);
}

/* jcCancel -- 入力中の全文節を無変換状態にする */
int
jcCancel(jcConvBuf *buf)
{
	TRACE("jcCancel", "Enter")

	CHECKFIXED(buf);

	if (buf->nClause <= 0)
		return 0;

	/*
	 * 表示バッファの内容をかなバッファの内容で置換える
	 * この際、バッファの大きさは気にする必要が無い。なぜなら、表
	 * 示バッファとかなバッファの大きさは常に同じだから
	 */
	bcopy(buf->kanaBuf, buf->displayBuf, buf->bufferSize * sizeof (wchar));

	/*
	 * 今ある全文節を一つの無変換状態の大文節にする
	 * この際も、文節数を気にする必要はない。なぜなら、少くとも一つ
	 * の文節はあったはずだから
	 */
	buf->curClause = buf->curLCStart = 0;
	buf->nClause = buf->curLCEnd = 1;
	buf->displayEnd = buf->displayBuf + (buf->kanaEnd - buf->kanaBuf);
	buf->clauseInfo[0].conv = 0;
	buf->clauseInfo[0].ltop = 1;
	buf->clauseInfo[1].kanap = buf->kanaEnd;
	buf->clauseInfo[1].dispp = buf->displayEnd;
	buf->clauseInfo[1].conv = 0;
	buf->clauseInfo[1].ltop = 1;

	/* 全候補文節も無効にする */
	buf->candClause = buf->candClauseEnd = -1;

	/* jllib の変換状態も無効にする */
	if (jl_bun_suu(buf->wnn) > 0)
		jl_kill(buf->wnn, 0, -1);

	return 0;
}

static uim_lisp
uim_wnn_jcCancel(uim_lisp buf_)
{
  return MAKE_BOOL(jcCancel(C_PTR(buf_)) == 0);
}

/* jcExpand -- カレント文節を１文字広げる */
int
jcExpand(jcConvBuf *buf, int small, int convf)
{
	TRACE("jcExpand", "Enter")

	CHECKFIXED(buf);

	return expandOrShrink(buf, small, 1, convf);
}

static uim_lisp
uim_wnn_jcExpand(uim_lisp buf_, uim_lisp small_, uim_lisp convf_)
{
  return MAKE_BOOL(jcExpand(C_PTR(buf_), C_BOOL(small_), C_BOOL(convf_)) == 0);
}

/* jcShrink -- カレント文節を１文字縮める */
int
jcShrink(jcConvBuf *buf, int small, int convf)
{
	TRACE("jcShrink", "Enter")

	CHECKFIXED(buf);

	return expandOrShrink(buf, small, 0, convf);
}

static uim_lisp
uim_wnn_jcShrink(uim_lisp buf_, uim_lisp small_, uim_lisp convf_)
{
  return MAKE_BOOL(jcShrink(C_PTR(buf_), C_BOOL(small_), C_BOOL(convf_)) == 0);
}

/* jcKana -- カレント文節をかなにする */
int
jcKana(jcConvBuf *buf, int small, int kind)
{
	jcClause	*clp;
	wchar		*kanap, *kanaendp, *dispp;
	int		start, end;
	int		conv;
	int		c;

	TRACE("jcKana", "Enter")

	CHECKFIXED(buf);

	/* 文節番号のチェック */
	if (buf->curClause >= buf->nClause) {
		/* カレント文節が最後の文節でしかも空だった場合
		 * この場合エラーにしてもよいが...
		 */
		return 0;
	}

	/*
	 * カレント文節が変換されていればいったん無変換にする
	 */

	/* あとで変換状態をもとに戻すため、変換状態をセーブしておく */
	conv = buf->clauseInfo[buf->curClause].conv;

	if (small) {
		start = buf->curClause;
		end = start + 1;
	} else {
		start = buf->curLCStart;
		end = buf->curLCEnd;
	}

	/* 全候補文節のチェック */
	checkCandidates(buf, start, end);

	/* 無変換状態にする */
	if (unconvert(buf, start, end) < 0) {
		return -1;
	}

	/*
	 * small が 0、つまりカレント文節として大文節を選択した場合、
	 * その中の小文節は一つにまとめられるので、curClause と
	 * curLCEnd を変える必要がある
	 */
	if (!small) {
		buf->curClause = buf->curLCStart;
		buf->curLCEnd = buf->curLCStart + 1;
	}

	/*
	 * かな変換する
	 *
	 * 表示バッファだけではなく、かなバッファも変換する
	 *
	 * これにはさしたる理由はないが、まあ、Ver3 版の jclib が
	 * そうだったので…
	 */
	clp = buf->clauseInfo + buf->curClause;
	kanap = clp->kanap;
	kanaendp = (clp + 1)->kanap;
	dispp = clp->dispp;

	if (kind == JC_HIRAGANA) {	/* カタカナ→ひらがな */
		/* カタカナをひらがなに変換する際にはひらがなにない字
		 * "ヴヵヶ" があるのでいきおいで変換してしまわないように
		 * 気を付けなければならない
		 * (まあ実際は気をつけるというほどのものではないが)
		 */
		while (kanap < kanaendp) {
			c = *kanap;
			if ((KANABEG + KATAOFFSET) <= c &&
					c <= (KANAEND + KATAOFFSET)) {
				*kanap = *dispp = c - KATAOFFSET;
			}
			kanap++, dispp++;
		}
	} else {	/* ひらがな→カタカナ */
		while (kanap < kanaendp) {
			c = *kanap;
			if (KANABEG <= c && c <= KANAEND) {
				*kanap = *dispp = c + KATAOFFSET;
			}
			kanap++, dispp++;
		}
	}

	/*
	 * 変換状態をもとに戻しておく
	 */

	/* とはいっても既に変換された文節の場合、これの頻度情報を
	 * サーバに送るとまずいので、あとでかな変換したことがわかるように
	 * jcClause.conv は -1 にセットする
	 */
	clp->conv = conv ? -1 : 0;

	return 0;
}

static uim_lisp
uim_wnn_jcKana(uim_lisp buf_, uim_lisp small_, uim_lisp kind_)
{
  char *kind_str = C_SYM(kind_);
  int kind;

  if (strcmp(kind_str, "hiragana") == 0)
    kind = JC_HIRAGANA;
  else if (strcmp(kind_str, "katakana") == 0)
    kind = JC_KATAKANA;
  else {
    free(kind_str);
    return uim_scm_f();
  }
  free(kind_str);
  return MAKE_BOOL(jcKana(C_PTR(buf_), C_BOOL(small_), kind) == 0);
}

/* jcFix -- 確定する */
int
jcFix(jcConvBuf *buf)
{
	TRACE("jcFix", "Enter")

	if (buf->fixed) {
		/* 既に確定されている
		 * エラーにしてもよいが…
		 */
		return 0;
	}

	if (forceStudy(buf, buf->nClause) < 0)
		return -1;

	/* 確定フラグを立てる */
	buf->fixed = 1;

	return 0;
}

static uim_lisp
uim_wnn_jcFix(uim_lisp buf_)
{
  return MAKE_BOOL(jcFix(C_PTR(buf_)) == 0);
}

/* jcFix1 -- 最初の一文字だけを確定する */
int
jcFix1(jcConvBuf *buf)
{
	TRACE("jcFix1", "Enter")

	if (buf->fixed) {
		/* 既に確定されている
		 * エラーにしてもよいが…
		 */
		return 0;
	}

	if (buf->nClause >= 1) {
		/* 最初の文節だけを学習する */
		if (forceStudy(buf, 1) < 0)
			return -1;

		/* 最初の文節の一文字だけにする */
		buf->nClause = 1;
		buf->curClause = buf->curLCStart = 0;
		buf->curLCEnd = 1;
		buf->kanaEnd = buf->kanaBuf + 1; /* ダミー */
		buf->displayEnd = buf->displayBuf + 1;
		buf->clauseInfo[0].kanap = buf->kanaBuf;
		buf->clauseInfo[0].dispp = buf->displayBuf;
		buf->clauseInfo[0].ltop = 1;
		buf->clauseInfo[1].kanap = buf->kanaBuf + 1;  /* ダミー */
		buf->clauseInfo[1].dispp = buf->displayBuf + 1;
		buf->clauseInfo[1].ltop = 1;
		buf->dot = buf->kanaBuf + 1;
		buf->candClause = buf->candClauseEnd = -1;
	}


	/* 確定フラグを立てる */
	buf->fixed = 1;

	return 0;
}

static uim_lisp
uim_wnn_jcFix1(uim_lisp buf_)
{
  return MAKE_BOOL(jcFix1(C_PTR(buf_)) == 0);
}

/* jcNext -- カレント文節を次候補/前候補で置き換える */
int
jcNext(jcConvBuf *buf, int small, int prev)
{
	int	n;

	TRACE("jcNext", "Enter")

	CHECKFIXED(buf);

	if (!buf->clauseInfo[buf->curClause].conv) {
		/* まだ変換されていない */
		jcErrno = JE_NOTCONVERTED;
		return -1;
	}

	/* 全候補が得られていなければ、全候補を得る */
	if (getCandidates(buf, small) < 0)
		return -1;

	n = jl_zenkouho_suu(buf->wnn);
	if (n <= 1) {
		/* 次候補がない */
		jcErrno = n < 0 ? JE_WNNERROR : JE_NOCANDIDATE;
		return -1;
	}

	/* 次候補番号を得る */
	n = jl_c_zenkouho(buf->wnn) + (prev ? -1 : 1);
	if (n < 0) {
		n = jl_zenkouho_suu(buf->wnn) - 1;
	} else if (n >= jl_zenkouho_suu(buf->wnn)) {
		n = 0;
	}

	if (setCandidate(buf, n) < 0) {
		/* 次候補が得られなかった */
		jcErrno = JE_WNNERROR;
		return -1;
	}

	return 0;
}

static uim_lisp
uim_wnn_jcNext(uim_lisp buf_, uim_lisp small_, uim_lisp prev_)
{
  return MAKE_BOOL(jcNext(C_PTR(buf_), C_BOOL(small_), C_BOOL(prev_)) == 0);
}

/* jcCandidateInfo -- 次候補の数と現在の候補番号を調べる
 *		      もし次候補がまだバッファに入っていなければ用意する
 */
int
jcCandidateInfo(jcConvBuf *buf, int small, int *ncandp, int *curcandp)
{
	int 	cand, ncand;

	TRACE("jcCandidateInfo", "Enter")

	CHECKFIXED(buf);

	if (!buf->clauseInfo[buf->curClause].conv) {
		/* まだ変換されていない */
		jcErrno = JE_NOTCONVERTED;
		return -1;
	}

	/* 全候補が得られていなければ、全候補を得る */
	if (getCandidates(buf, small) < 0)
		return -1;

	ncand = jl_zenkouho_suu(buf->wnn);
	if (ncand <= 1) {
		/* 候補がない */
		jcErrno = (ncand < 0) ? JE_WNNERROR : JE_NOCANDIDATE;
		return -1;
	}

	/* 現在の候補番号を得る */
	cand = jl_c_zenkouho(buf->wnn);
	if (cand < 0) {
		/* 候補が得られない */
		jcErrno = JE_WNNERROR;
		return -1;
	}

	if (ncandp != NULL) *ncandp = ncand;
	if (curcandp != NULL) *curcandp = cand;

	return 0;
}

static uim_lisp
uim_wnn_jcCandidateInfo(uim_lisp buf_, uim_lisp small_)
{
  int ncand, curcand;

  if (jcCandidateInfo(C_PTR(buf_), C_BOOL(small_), &ncand, &curcand) != 0)
    return uim_scm_f();

  return LIST2(CONS(MAKE_SYM("ncand"), MAKE_INT(ncand)),
   CONS(MAKE_SYM("carcand"), MAKE_INT(curcand)));
}

/* jcGetCandidate -- 指定された番号の候補を取り出す */
int
jcGetCandidate(jcConvBuf *buf, int n, wchar *candstr, int len)
{
	wchar	tmp[CL_BUFSZ];

	TRACE("jcGetCandidate", "Enter")

	CHECKFIXED(buf);

	/* 文節のチェック */
	if (buf->candClause < 0) {
		jcErrno = JE_NOCANDIDATE;
		return -1;
	}

	/* 候補番号のチェック */
	if (n < 0 || n >= jl_zenkouho_suu(buf->wnn)) {
		jcErrno = JE_NOSUCHCANDIDATE;
		return -1;
	}

	/* 文字列をコピー */
	ki2_jl_get_zenkouho_kanji(buf->wnn, n, tmp, CL_BUFSZ);
	tmp[CL_BUFSZ - 1] = 0;
	wstrncpy(candstr, tmp, len / sizeof(wchar));

	return 0;
}

static uim_lisp
uim_wnn_jcGetCandidate(uim_lisp buf_, uim_lisp n_)
{
  wchar buf[BUFSIZ];
  char str[BUFSIZ * sizeof(wchar)];

  if (jcGetCandidate(C_PTR(buf_), C_INT(n_), buf, sizeof(buf)) != 0)
    return uim_scm_f();
  if (sizeof(str) < wstrlen(buf) * sizeof(wchar))
    return uim_scm_f();
  wstoeuc(str, buf, sizeof(str));
  return MAKE_STR(str);
}

/* jcSelect -- 表示バッファを指定された候補と置き換える */
int
jcSelect(jcConvBuf *buf, int n)
{
	TRACE("jcSelect", "Enter")

	CHECKFIXED(buf);

#ifdef DEBUG_WNNLIB
	fprintf(stderr,
		"Select: %d [%s for %d - %d]\n",
		n,
		buf->candKind == CAND_SMALL ? "small" : "large",
		buf->candClause,
		buf->candClauseEnd);
#endif

	/* 文節のチェック */
	if (buf->candClause < 0) {
		jcErrno = JE_NOCANDIDATE;
		return -1;
	}

	/* 候補番号のチェック */
	if (n < 0 || n >= jl_zenkouho_suu(buf->wnn)) {
		jcErrno = JE_NOSUCHCANDIDATE;
		return -1;
	}

	/* 候補がセットされていなければ、セットする */
	if (jl_c_zenkouho(buf->wnn) != n && setCandidate(buf, n) < 0)
		return -1;

	return 0;
}

static uim_lisp
uim_wnn_jcSelect(uim_lisp buf_, uim_lisp n_)
{
  return MAKE_BOOL(jcSelect(C_PTR(buf_), C_INT(n_)) == 0);
}

/* jcDotOffset -- 大文節の先頭からのドットのオフセットを返す */
int
jcDotOffset(jcConvBuf *buf)
{
	TRACE("jcDotOffset", "Enter")

	return buf->dot - buf->clauseInfo[buf->curLCStart].kanap;
}

static uim_lisp
uim_wnn_jcDotOffset(uim_lisp buf_)
{
  return MAKE_INT(jcDotOffset(C_PTR(buf_)));
}

/* jcIsConverted -- 指定された文節が変換されているかどうかを返す */
int
jcIsConverted(jcConvBuf *buf, int cl)
{
	TRACE("jcIsConverted", "Enter")

	if (cl < 0 || cl > buf->nClause) {
		/* cl == jcNClause のときをエラーにしてもいいのだけれど
		 * カレント文節が jcNClause のときがあるので
		 * エラーとはしないことにした
		 */
		return -1;
	}
	return (buf->clauseInfo[cl].conv != 0);
}

static uim_lisp
uim_wnn_jcIsConverted(uim_lisp buf_, uim_lisp cl_)
{
  int ret = jcIsConverted(C_PTR(buf_), C_INT(cl_));

  if (ret == 0)
    return MAKE_SYM("muhenkan");
  else if (ret == 1)
    return MAKE_SYM("henkan");
  else
    return uim_scm_f();
}

/* jcMove -- ドット・カレント文節を移動する */
int
jcMove(jcConvBuf *buf, int small, int dir)
{
	jcClause	*clp = buf->clauseInfo + buf->curClause;
	int		i;

	TRACE("jcMove", "Enter")

	if (!clp->conv) {
		/* カレント文節が変換されていないので、ドットの移動になる */
		if (dir == JC_FORWARD) {
			if (buf->curClause == buf->nClause) {
				/* すでに一番最後にいる */
				jcErrno = JE_CANTMOVE;
				return -1;
			} else if (buf->dot == (clp + 1)->kanap) {
				/* ドットがカレント文節の最後にあるので
				 * 文節移動する
				 */
				goto clausemove;
			} else {
				buf->dot++;
			}
		} else {
			if (buf->dot == clp->kanap) {
				/* ドットがカレント文節の先頭にあるので
				 * 文節移動する
				 */
				goto clausemove;
			} else
				buf->dot--;
		}
		return 0;
	}

clausemove:	/* 文節移動 */
	clp = buf->clauseInfo;

	if (small) {
		/* 小文節単位の移動 */
		if (dir == JC_FORWARD) {
			if (buf->curClause == buf->nClause) {
				jcErrno = JE_CANTMOVE;
				return -1;
			}
			buf->curClause++;
			if (buf->curClause >= buf->curLCEnd) {
				/* 大文節も移動する */
				buf->curLCStart = buf->curLCEnd;
				for (i = buf->curLCStart + 1;
				     i <= buf->nClause && !clp[i].ltop; i++)
					;
				buf->curLCEnd = i;
			}
		} else {	/* JC_BACKWARD */
			if (buf->curClause == 0) {
				jcErrno = JE_CANTMOVE;
				return -1;
			}
			buf->curClause--;
			if (buf->curClause < buf->curLCStart) {
				/* 大文節も移動する */
				buf->curLCEnd = buf->curLCStart;
				for (i = buf->curClause; !clp[i].ltop; i--)
					;
				buf->curLCStart = i;
			}
		}
	} else {
		/* 大文節単位の移動 */
		if (dir == JC_FORWARD) {
			if (buf->curLCStart == buf->nClause) {
				jcErrno = JE_CANTMOVE;
				return -1;
			}
			buf->curLCStart = buf->curClause = buf->curLCEnd;
			for (i = buf->curLCStart + 1;
			     i <= buf->nClause && !clp[i].ltop; i++)
				;
			buf->curLCEnd = i;
		} else {
			if (buf->curLCStart == 0) {
				jcErrno = JE_CANTMOVE;
				return -1;
			}
			buf->curLCEnd = buf->curLCStart;
			for (i = buf->curLCEnd - 1; !clp[i].ltop; i--)
				;
			buf->curLCStart = buf->curClause = i;
		}
	}

	/* 文節移動したらドットはその文節の先頭に移動する */
	buf->dot = clp[buf->curClause].kanap;

	return 0;
}

static uim_lisp
uim_wnn_jcMove(uim_lisp buf_, uim_lisp small_, uim_lisp dir_)
{
  char *dir_str = C_SYM(dir_);
  int dir;

  if (strcmp("forward", dir_str) == 0)
    dir = JC_FORWARD;
  else if (strcmp("backward", dir_str) == 0)
    dir = JC_BACKWARD;
  else {
    free(dir_str);
    return uim_scm_f();
  }
  free(dir_str);
  return MAKE_BOOL(jcMove(C_PTR(buf_), C_BOOL(small_), dir) == 0);
}

/* jcTop -- ドット・カレント文節を文の先頭に移動する */
int
jcTop(jcConvBuf *buf)
{
	TRACE("jcTop", "Enter")

	/* カレント文節を 0 にしてドットを先頭に持ってくる */
	setCurClause(buf, 0);
	buf->dot = buf->kanaBuf;

	return 0;
}

static uim_lisp
uim_wnn_jcTop(uim_lisp buf_)
{
  return MAKE_BOOL(jcTop(C_PTR(buf_)) == 0);
}

/* jcBottom -- ドット・カレント文節を文の最後に移動する */
int
jcBottom(jcConvBuf *buf)
{
	TRACE("jcBottom", "Enter")

	/*
	 * Ver3 対応の jclib では、カレント文節を jcNClause にして
	 * ドットを最後に持ってくるだけだった
	 * これだと、最後の文節にかなを入れていて、カーソルを動かして
	 * jcBottom() で元に戻って再びかなを入れると、別の文節に
	 * なってしまう
	 * そこで、最後の文節が無変換状態の時には、カレント文節は
	 * buf->nClause ではなく、buf->nClause - 1 にすることにする
	 */
	if (buf->nClause > 0 && !buf->clauseInfo[buf->nClause - 1].conv) {
		buf->curClause = buf->curLCStart = buf->nClause - 1;
		buf->curLCEnd = buf->nClause;
	} else {
		buf->curClause = buf->curLCStart = buf->nClause;
		buf->curLCEnd = buf->nClause + 1;
	}
	buf->dot = buf->kanaEnd;
	return 0;
}

static uim_lisp
uim_wnn_jcBottom(uim_lisp buf_)
{
  return MAKE_BOOL(jcBottom(C_PTR(buf_)) == 0);
}

/* jcInsertChar -- ドットの位置に一文字挿入する */
int
jcInsertChar(jcConvBuf *buf, int c)
{
	jcClause	*clp;
	wchar	*dot, *dispdot;
	int	ksizenew, dsizenew;

	TRACE("jcInsertChar", "Enter")

	CHECKFIXED(buf);

	/* 全候補文節がカレント大文節にあれば無効にする */
	checkCandidates(buf, buf->curLCStart, buf->curLCEnd);

	/*
	 * ・カレント文節番号が buf->nClause である場合
	 *	- これはドットが最後の文節の次にあるということなので
	 *	  新しい文節を作る
	 * ・変換済みの文節の場合
	 *	- 無変換の状態に戻してから挿入
	 * ・その他
	 *	- 単に挿入すればよい
	 */
	clp = buf->clauseInfo + buf->curLCStart;
	if (buf->curLCStart == buf->nClause) {
		/* 新たに文節を作る */
		/* clauseInfo のサイズのチェック */
		if (buf->nClause >= buf->clauseSize &&
		    resizeCInfo(buf, buf->nClause + 1) < 0) {
			return -1;
		}
		/* buf->nClause のアップデートと clauseInfo の設定 */
		buf->nClause += 1;
		clp = buf->clauseInfo + buf->nClause;
		clp->conv = 0;
		clp->ltop = 1;
		clp->kanap = buf->kanaEnd;
		clp->dispp = buf->displayEnd;
	} else if (clp->conv) {
		/* 無変換状態にする */
		if (unconvert(buf, buf->curLCStart, buf->curLCEnd) < 0)
			return -1;
		buf->curClause = buf->curLCStart;
		buf->curLCEnd = buf->curLCStart + 1;
		DotSet(buf);
	}

	clp = buf->clauseInfo + buf->curLCStart;

	/* バッファの大きさのチェック */
	ksizenew = (buf->kanaEnd - buf->kanaBuf) + 1;
	dsizenew = (buf->displayEnd - buf->displayBuf) + 1;
	if ((ksizenew > buf->bufferSize || dsizenew > buf->bufferSize) &&
	    resizeBuffer(buf, ksizenew > dsizenew ? ksizenew : dsizenew) < 0) {
		    return -1;
	}

	/* かなバッファをアップデート */
	dot = buf->dot;
	/* カレント文節の後ろを一文字ずらす */
	moveKBuf(buf, buf->curLCStart + 1, 1);
	/* カレント文節内のドット以降を一文字ずらす */
	(void)bcopy((char *)dot, (char *)(dot + 1),
		    ((clp + 1)->kanap - dot) * sizeof(wchar));
	/* 挿入 */
	*dot = c;

	/* 表示バッファをアップデート */
	dispdot = clp->dispp + (dot - clp->kanap);
	/* カレント文節の後ろを一文字ずらす */
	moveDBuf(buf, buf->curLCStart + 1, 1);
	/* カレント文節内のドット以降を一文字ずらす */
	(void)bcopy((char *)dispdot, (char *)(dispdot + 1),
	      ((clp + 1)->dispp - dispdot) * sizeof(wchar));
	/* 挿入 */
	*dispdot = c;

	/* ドットを更新 */
	buf->dot++;

	return 0;
}

static uim_lisp
uim_wnn_jcInsertChar(uim_lisp buf_, uim_lisp c_)
{
  if (INTP(c_))
    return MAKE_BOOL(jcInsertChar(C_PTR(buf_), C_INT(c_)) == 0);
  else if (STRP(c_)) {
    const char *euc = REFER_C_STR(c_);
    wchar wc;

    euctows(&wc, euc, 1);

    return MAKE_BOOL(jcInsertChar(C_PTR(buf_), wc) == 0);
  }
  uim_notify_fatal("In uim_wnn_jcInsertChar, argument must be string or integer.");
  return uim_scm_f();
}

/* jcDeleteChar -- ドットの前または後ろの一文字を削除する */
int
jcDeleteChar(jcConvBuf *buf, int prev)
{
	jcClause	*clp;
	wchar		*dot, *dispdot;

	TRACE("jcDeleteChar", "Enter")

	CHECKFIXED(buf);

	clp = buf->clauseInfo;
	if (buf->nClause == 0) {
		/* 文節数が 0、つまり何も入っていない時:
		 *	- エラー
		 */
		jcErrno = JE_CANTDELETE;
		return -1;
	} else if (buf->curClause >= buf->nClause) {
		/* カレント文節が最後の文節の次にある時:
		 *	- prev であれば、前の文節の最後の文字を削除
		 *	  カレント文節は前の文節に移動する
		 *	  必要ならば前の文節を無変換に戻してから削除する
		 *	  前の文節がないことはあり得ない
		 *	- !prev ならばエラー
		 */
		if (!prev) {
			jcErrno = JE_CANTDELETE;
			return -1;
		}
		(void)jcMove(buf, 0, JC_BACKWARD);
	} else if (clp[buf->curLCStart].conv) {
		/* カレント文節が変換されている時:
		 *	- prev であれば前の文節の最後の文字を削除
		 *	  カレント文節は前の文節に移動する
		 *	  必要ならば前の文節を無変換に戻してから削除する
		 *	  カレント文節が先頭ならばエラー
		 *	- !prev ならカレント文節を無変換に戻して、文節の
		 *	  最初の文字を削除
		 */
		if (prev) {
			if (buf->curLCStart == 0) {
				jcErrno = JE_CANTDELETE;
				return -1;
			}
			(void)jcMove(buf, 0, JC_BACKWARD);
		}
	} else {
		/* カレント文節が変換されていない時:
		 *	- prev であればドットの前の文字を削除
		 *	  ただしドットが文節の先頭にあれば前の文節の
		 *	  最後の文字を削除
		 *	  その時にはカレント文節は前の文節に移動する
		 *	  必要ならば前の文節を無変換に戻してから削除する
		 *	  カレント文節が先頭ならばエラー
		 *	- !prev ならドットの次の文字を削除
		 *	  ドットが文節の最後の文字の次にあればエラー
		 */
		if (prev) {
			if (buf->dot == clp[buf->curLCStart].kanap) {
				if (buf->curLCStart == 0) {
					jcErrno = JE_CANTDELETE;
					return -1;
				}
				(void)jcMove(buf, 0, JC_BACKWARD);
			}
		} else {
			if (buf->dot == clp[buf->curLCEnd].kanap) {
				jcErrno = JE_CANTDELETE;
				return -1;
			}
		}
	}

	if (buf->clauseInfo[buf->curLCStart].conv) {
		/* カレント文節が変換済みであれば無変換に戻す */
		if (jcUnconvert(buf) < 0)
			return -1;
		/* prev であれば文節の最後の文字、そうでなければ文節の
		 * 先頭の文字を削除する
		 */
		if (prev) {
			buf->dot = buf->clauseInfo[buf->curLCEnd].kanap - 1;
		} else {
			buf->dot = buf->clauseInfo[buf->curLCStart].kanap;
		}
	} else {
		/* prev ならドットを１文字戻しておく
		 * こうすればドットの後ろの文字を削除することになる
		 * 削除し終わったときにドットを動かす必要もない
		 */
		if (prev)
			buf->dot--;
	}

	clp = buf->clauseInfo + buf->curLCStart;

	/* かなバッファをアップデート */
	dot = buf->dot;
	/* カレント文節内のドット以降を一文字ずらす */
	(void)bcopy((char *)(dot + 1), (char *)dot,
		    ((clp + 1)->kanap - (dot + 1)) * sizeof(wchar));
	/* カレント文節の後ろを一文字ずらす */
	moveKBuf(buf, buf->curLCEnd, -1);

	/* 表示バッファをアップデート */
	dispdot = clp->dispp + (dot - clp->kanap);
	/* カレント文節内のドット以降を一文字ずらす */
	(void)bcopy((char *)(dispdot + 1), (char *)dispdot,
		   ((clp + 1)->dispp - (dispdot + 1)) * sizeof(wchar));
	/* カレント文節の後ろを一文字ずらす */
	moveDBuf(buf, buf->curLCEnd, -1);

	/* カレント文節の長さが１だった場合には文節が１減ることになる */
	if (clp->kanap == (clp + 1)->kanap) {
		/* 文節がなくなってしまった */
		moveCInfo(buf, buf->curLCEnd, -1);
		setCurClause(buf, buf->curLCStart);
		DotSet(buf);
	}

	return 0;
}

static uim_lisp
uim_wnn_jcDeleteChar(uim_lisp buf_, uim_lisp prev_)
{
  return MAKE_BOOL(jcDeleteChar(C_PTR(buf_), C_BOOL(prev_)) == 0);
}

/* jcKillLine -- ドット以降を削除する */
int
jcKillLine(jcConvBuf *buf)
{
	int cc = buf->curClause;

	TRACE("jcKillLine", "Enter")

	CHECKFIXED(buf);

	/* 入力中の文節がないか、ドットが最後の文節の次にあれば、エラー */
	if (buf->nClause <= 0 || cc >= buf->nClause) {
		jcErrno = JE_CANTDELETE;
		return -1;
	}

#ifdef DEBUG_WNNLIB
        showBuffers(buf, "before jcKillLine");
#endif

	/* ドットが入力の先頭であれば、jcClear を呼出して終り */
	if (buf->dot == buf->kanaBuf)
		return jcClear(buf);

	/*
	 * ドット以降を削除する
	 * といっても、単に文節情報とポインタを変更すれば良い
	 */
	checkCandidates(buf, cc, buf->nClause);
	if (buf->clauseInfo[cc].conv) {
		/* 変換されていれば、カレント文節を含めて削除 */
		buf->kanaEnd = buf->dot = buf->clauseInfo[cc].kanap;
		buf->displayEnd = buf->clauseInfo[cc].dispp;

		/* カレント文節を末尾文節に移す */
		buf->nClause = buf->curClause = buf->curLCStart = cc;
		buf->curLCEnd = cc + 1;
	} else {
		/* 未変換ならば、ドット以降を削除 */
		buf->kanaEnd = buf->dot;
		buf->displayEnd = buf->clauseInfo[cc].dispp
				+ (buf->dot - buf->clauseInfo[cc].kanap);

		/* カレント文節はそのままで、末尾だけを気にすればよい */
		cc++;
		buf->nClause = buf->curLCEnd = cc;
	}

	/* 空の末尾文節の設定をする */
	buf->clauseInfo[cc].kanap = buf->kanaEnd;
	buf->clauseInfo[cc].dispp = buf->displayEnd;
	buf->clauseInfo[cc].conv = 0;
	buf->clauseInfo[cc].ltop = 1;

	/* カレント文節とそれ以降の jllib の文節情報も無効にする */
	if (jl_bun_suu(buf->wnn) > cc)
		jl_kill(buf->wnn, cc, -1);

#ifdef DEBUG_WNNLIB
        showBuffers(buf, "after jcKillLine");
#endif
	return 0;
}

static uim_lisp
uim_wnn_jcKillLine(uim_lisp buf_)
{
  return MAKE_BOOL(jcKillLine(C_PTR(buf_)) == 0);
}

/* jcChangeClause -- カレント大文節を指定された文字列で置き換える */
int
jcChangeClause(jcConvBuf *buf, wchar *str)
{
	jcClause	*clps, *clpe;
	wchar	*p;
	int	newlen;
	int	oklen, odlen;
	int	ksize, dsize;

	TRACE("jcChangeClause", "Enter")

	CHECKFIXED(buf);

	clps = buf->clauseInfo + buf->curLCStart;
	clpe = buf->clauseInfo + buf->curLCEnd;

	newlen = 0;
	p = str;
	while (*p++)
		newlen++;

	/* かなバッファと表示バッファのサイズを調べて、
	 * 入らなかったら大きくする
	 */
	if (buf->curLCStart < buf->nClause) {
		oklen = clpe->kanap - clps->kanap;
		odlen = clpe->dispp - clps->dispp;
	} else {
		oklen = odlen = 0;
	}
	ksize = (buf->kanaEnd - buf->kanaBuf) + newlen - oklen;
	dsize = (buf->displayEnd - buf->displayBuf) + newlen - odlen;
	if (ksize > buf->bufferSize || dsize > buf->bufferSize) {
		if (resizeBuffer(buf, ksize > dsize ? ksize : dsize) < 0)
			return -1;
	}

	/* curLCStart が nClause に等しい時だけ、新たに文節が作られる */
	if (buf->curLCStart == buf->nClause) {
		/* clauseInfo の大きさを調べる*/
		if (buf->nClause + 1 > buf->clauseSize) {
			if (resizeCInfo(buf, buf->nClause + 1) < 0)
				return -1;
		}
		/* 新たにできた clauseInfo には、nClause 番目
		 * (つまり最後の clauseInfo) の内容をコピーしておく
		 */
		clpe = buf->clauseInfo + buf->nClause + 1;
		*clpe = *(clpe - 1);

		buf->nClause++;
	}

	clps = buf->clauseInfo + buf->curLCStart;
	clpe = buf->clauseInfo + buf->curLCEnd;

	/* かなバッファの変更 */
	/* まずは後ろを移動させる */
	moveKBuf(buf, buf->curLCEnd, newlen - oklen);
	/* str をコピー */
	(void)bcopy((char *)str, (char *)clps->kanap,
		    newlen * sizeof(wchar));
	/* 表示バッファの変更 */
	/* まずは後ろを移動させる */
	moveDBuf(buf, buf->curLCEnd, newlen - odlen);
	/* str をコピー */
	(void)bcopy((char *)str, (char *)clps->dispp,
		    newlen * sizeof(wchar));

	/* clauseInfo の変更 */
	/* まずは後ろを移動させる */
	if (clpe > clps + 1) {
		(void)bcopy((char *)clpe, (char *)(clps + 1),
			    (buf->nClause + 1 - buf->curLCEnd) *
			    sizeof(jcClause));
	}
	clps->conv = 0;
	clps->ltop = 1;
	(clps + 1)->ltop = 1;

	return 0;
}

static uim_lisp
uim_wnn_jcChangeClause(uim_lisp buf_, uim_lisp str_)
{
  wchar wstr[BUFSIZ];
  const char *str = REFER_C_STR(str_);

  if (BUFSIZ * 2 < strlen(str))
    return uim_scm_f();

  euctows(wstr, str, sizeof(wstr));
  return MAKE_BOOL(jcChangeClause(C_PTR(buf_), wstr) == 0);
}

/* jcSaveDic -- 辞書・頻度ファイルをセーブする */
int
jcSaveDic(jcConvBuf *buf)
{
	TRACE("jcSaveDic", "Enter")

	return jl_dic_save_all(buf->wnn);
}

static uim_lisp
uim_wnn_jcSaveDic(uim_lisp buf_)
{
  return MAKE_BOOL(jcSaveDic(C_PTR(buf_)) == 0);
}

/* サーバとの接続のための関数群 */

struct wnn_buf *
jcOpen(char *server, char *envname, int override, char *rcfile, void (*errmsg)(), int (*confirm)(), int timeout)
{
    return jcOpen2(server, envname, override, rcfile, rcfile, errmsg, confirm, timeout);
}

struct wnn_buf *
jcOpen2(char *server, char *envname, int override, char *rcfile4, char *rcfile6, void (*errmsg)(), int (*confirm)(), int timeout)
{
    struct wnn_buf *wnnbuf;
    struct wnn_env *wnnenv;
    char *rcfile;
    int env_exists;
    int wnn_version;

    TRACE("jcOpen2", "Enter")

    /* サーバ名が NULL または空文字列だった場合は環境変数 JSERVER を使用する */
    if (server == NULL || server[0] == '\0') {
	server = getenv("JSERVER");
    }

    /* 環境名が空文字列だった場合は、ユーザ名を使用する */
    if (envname != NULL && *envname == 0) {
        struct passwd *p = getpwuid(getuid());

	if (p != NULL) envname = p->pw_name;
    }

    /*
     * jserver のバージョンによって wnnrc を変えたいのだが、
     * バージョンを調べるためにはまず接続しなくてはならない。
     * そこで wnnrc 引数を NULL にして接続する。
     */
#if JSERVER_VERSION > 0x4030
    wnnbuf = jl_open_lang(envname, server, "ja_JP",
    				NULL, confirm, errmsg, timeout);
#else
    wnnbuf = jl_open(envname, server, NULL, confirm, errmsg, timeout);
#endif

    /*
     * ・バッファが作れなかった
     * ・jserver に接続できなかった
     * ・wnnrc ファイルの指定がない (つまり初期化しない)
     * 場合にはこれで終り。
     */
    if (wnnbuf == NULL ||
	!jl_isconnect(wnnbuf) ||
	(rcfile4 == NULL && rcfile6 == NULL)) {
	return wnnbuf;
    }

    wnnenv = jl_env_get(wnnbuf);

    /*
     * 以前から環境が存在していたかどうかと、サーバのバージョンを調べる。
     * 環境が存在していたかどうかは jl_fuzokugo_get で (つまり付属語
     * 辞書が設定されているかどうかで) 判断する。jl_open_lang は環境が
     * なければ作ってしまうため、js_env_exist は使えない。
     */
    {
	char fzk[1024];
	int serv_ver, lib_ver;

	if (ki2_jl_fuzokugo_get(wnnbuf, fzk, 1024) != -1) {
	    env_exists = 1;
	    TRACE("jcOpen2", "env exists");
	} else {
	    env_exists = 0;
	    TRACE("jcOpen2", "no env");
	}
	if (js_version(wnnenv->js_id, &serv_ver, &lib_ver) != -1 &&
	    serv_ver >= 0x4f00) {
	    wnn_version = 6;
	    TRACE("jcOpen2", "Wnn6");
	} else {
	    wnn_version = 4;
	    TRACE("jcOpen2", "Wnn4");
	}
    }

    /* wnnrc の選択 */
    rcfile = (wnn_version == 4) ? rcfile4 : rcfile6;

    /*
     * 環境がすでに存在しかつ環境の上書きが指定されていない、あるいは
     * rcfile が NULL の場合にはこれで終り。
     */
    if ((env_exists && !override) || rcfile == NULL) return wnnbuf;

    /*
     * wnnrc が空文字列だった場合は、デフォルトを使用する。
     * 1. 環境変数 WNNENVRC4 または WNNENVRC6
     * 2. 環境変数 WNNENVRC
     * 3. システムのデフォルト
     * の順で検索する。最後のはちょっといいかげん。
     */
    if (*rcfile == '\0') {
	rcfile = getenv((wnn_version == 4) ? "WNNENVRC4" : "WNNENVRC6");
	if (rcfile == NULL || access(rcfile, R_OK) != 0) {
	    rcfile = getenv("WNNENVRC");
	}
	if (rcfile == NULL || access(rcfile, R_OK) != 0) {
	    if (wnn_version == 6) {
#ifdef WNN6
		rcfile = "@DEFAULT";
#else
		rcfile = "wnnenvrc";
		if (access(rcfile, R_OK) != 0)
			uim_notify_fatal(_("uim-wnn: \"%s\" is not exist. Please run uim-pref and set \"Wnn resource file\" (maybe file name is \"wnnenvrc\")."),
			    rcfile);
#endif
	    } else {
#if defined(WNNENVDIR) && JSERVER_VERSION > 0x4030
		static char envrc[256];
		rcfile = envrc;
		(void)snprintf(rcfile, sizeof(envrc), "%s/ja_JP/wnnenvrc", WNNENVDIR);
		if (access(rcfile, R_OK) != 0)
		    (void) snprintf(rcfile, sizeof(envrc), "%s/wnnenvrc", WNNENVDIR);
#else
		rcfile = "wnnenvrc";
#endif
		if (access(rcfile, R_OK) != 0)
			uim_notify_fatal(_("uim-wnn: \"%s\" is not exist. Please run uim-pref and set \"Wnn resource file\" (maybe file name is \"wnnenvrc\")."),
				rcfile);
	    }
        }
    }

    /* 環境設定する */
    (void)jl_set_env_wnnrc(wnnenv, rcfile, confirm, errmsg);

    return wnnbuf;
}

static int
uim_wnn_confirm(char *msg)
{
  /* XXX */
  fprintf(stderr, "wnn confirm: %s\n", msg);
  return 1; /* return Y */
}

static uim_lisp
uim_wnn_jcOpen(uim_lisp server_, uim_lisp envname_, uim_lisp rcfile_, uim_lisp timeout_)
{
  struct wnn_buf *wnnbuf;
  char *server  = uim_strdup(REFER_C_STR(server_));
  char *envname = uim_strdup(REFER_C_STR(envname_));
  char *rcfile  = uim_strdup(REFER_C_STR(rcfile_));

  /* XXX: no errmsg, no conform */
  wnnbuf = jcOpen(server, envname, 0, rcfile, (void (*)())uim_notify_info, uim_wnn_confirm, C_INT(timeout_));

  free(server);
  free(envname);
  free(rcfile);

  if (!wnnbuf)
    return uim_scm_f();
  return MAKE_PTR(wnnbuf);
}

void
jcClose(struct wnn_buf *wnnbuf)
{
    TRACE("jcClose", "Enter")

    if (wnnbuf != NULL)
	jl_close(wnnbuf);
}

static uim_lisp
uim_wnn_jcClose(uim_lisp wnnbuf_)
{
  jcClose(C_PTR(wnnbuf_));
  return uim_scm_t();
}

int
jcIsConnect(struct wnn_buf *wnnbuf)
{
    TRACE("jcIsConnect", "Enter")

    if (wnnbuf == NULL)
        return 0;
    return jl_isconnect(wnnbuf);
}

static uim_lisp
uim_wnn_jcIsConnect(uim_lisp wnnbuf_)
{
  return MAKE_BOOL(jcIsConnect(C_PTR(wnnbuf_)) != 0);
}


#ifdef DEBUG_WNNLIB
static void
printBuffer(wchar *start, wchar *end)
{
	wchar wc;

	while (start < end) {
		wc = *start++;
		if (wc >= 0200) {
			putc((wc >> 8) & 0xff, stderr);
			wc &= 0xff;
		} else if (wc < 040 || wc == 0177) {
			putc('^', stderr);
			wc ^= 0100;
		} else if (wc == '^' || wc == '\\') {
			putc('\\', stderr);
		}
		putc(wc, stderr);
	}
}

static void
showBuffers(jcConvBuf *buf, char *tag)
{
	int i;
	jcClause *clp = buf->clauseInfo;
	wchar ws[512];

	fprintf(stderr, "Buffer Info [%s]\n", tag);
	fprintf(stderr, "nClause = %d, curClause = %d [%d, %d], ",
		 buf->nClause, buf->curClause, buf->curLCStart, buf->curLCEnd);

	if (buf->dot < buf->kanaBuf) {
		fprintf(stderr, "dot < 0\n");
	} else if (buf->dot > buf->kanaEnd) {
		fprintf(stderr, "dot > 0\n");
	} else if (buf->nClause == 0) {
		fprintf(stderr, "dot == 0\n");
	} else {
		for (i = 0; i < buf->nClause; i++) {
			if (buf->dot <= clp[i].kanap)
				break;
		}
		if (buf->dot < clp[i].kanap)
			i--;
		fprintf(stderr, "dot = %d.%d\n", i, buf->dot - clp[i].kanap);
	}

	for (i = 0; i < buf->nClause; i++) {
		fprintf(stderr,
			"clause[%d]: conv = %d, ltop = %d",
			i, clp->conv, clp->ltop);
		if (clp->conv == 1) {
			fprintf(stderr, " [%d]", jl_dai_top(buf->wnn, i));
		}
		fprintf(stderr, "\n");
		fprintf(stderr, "clause[%d]: Kana = '", i);
		printBuffer(clp->kanap, (clp + 1)->kanap);
		fprintf(stderr, "'\n");
		if (clp->conv == 1) {
			fprintf(stderr, "clause[%d]: Yomi = '", i);
			(void)ki2_jl_get_yomi(buf->wnn, i, i + 1, ws, sizeof(ws));
			printBuffer(ws, ws + jl_yomi_len(buf->wnn, i, i + 1));
			fprintf(stderr, "'\n");
		}
		fprintf(stderr, "clause[%d]: Disp = '", i);
		printBuffer(clp->dispp, (clp + 1)->dispp);
		fprintf(stderr, "'\n");
		if (clp->conv == 1) {
			fprintf(stderr, "clause[%d]: Conv = '", i);
			(void)ki2_jl_get_kanji(buf->wnn, i, i + 1, ws, sizeof(ws));
			printBuffer(ws, ws + jl_kanji_len(buf->wnn, i, i + 1));
			fprintf(stderr, "'\n");
		}
		clp++;
	}
}
#endif /* DEBUG_WNNLIB */

static uim_lisp
uim_wnn_jc_dump_jconvbuf(uim_lisp buf_)
{
  jcConvBuf *buf = C_PTR(buf_);
  uim_lisp ret_ = uim_scm_null();
  int len;
  wchar *wstr;
  char *str;
  uim_lisp kana_ = MAKE_STR(""), disp_ = MAKE_STR("");
  int clause;
  jcClause *clauseinfo;

  ret_ = CONS(CONS(MAKE_SYM("cur-n-clause"), MAKE_INT(buf->nClause)), ret_);
  ret_ = CONS(CONS(MAKE_SYM("cur-clause"),   MAKE_INT(buf->curClause)), ret_);
  ret_ = CONS(CONS(MAKE_SYM("cur-lc-start"), MAKE_INT(buf->curLCStart)), ret_);
  ret_ = CONS(CONS(MAKE_SYM("cur-lc-end"),   MAKE_INT(buf->curLCEnd)), ret_);

  len = buf->kanaEnd - buf->kanaBuf;
  if (0 < len) {
    wstr = uim_malloc(sizeof(wchar) * (len + 1));
    str  = uim_malloc(sizeof(wchar) * (len + 1));
    memcpy(wstr, (char *)buf->kanaBuf, sizeof(wchar) * (len + 1));
    wstr[len] = '\0';
    wstoeuc(str, wstr, sizeof(wchar) * (len + 1));
    ret_ = CONS(CONS(MAKE_SYM("kana-buf"), MAKE_STR(str)), ret_);
    free(str);
    free(wstr);
  } else
    ret_ = CONS(CONS(MAKE_SYM("kana-buf"), MAKE_STR("")), ret_);

  len = buf->displayEnd - buf->displayBuf;
  if (0 < len) {
    wstr = uim_malloc(sizeof(wchar) * (len + 1));
    str  = uim_malloc(sizeof(wchar) * (len + 1));
    memcpy(wstr, (char *)buf->displayBuf, sizeof(wchar) * (len + 1));
    wstr[len] = '\0';
    wstoeuc(str, wstr, sizeof(wchar) * (len + 1));
    ret_ = CONS(CONS(MAKE_SYM("display-buf"), MAKE_STR(str)), ret_);
    free(str);
    free(wstr);
  } else {
    ret_ = CONS(CONS(MAKE_SYM("display-buf"), MAKE_STR("")), ret_);
  }

  clause = buf->curClause;
  clauseinfo = buf->clauseInfo;

  if (clause != buf->nClause) {
    len = clauseinfo[clause + 1].kanap - clauseinfo[clause].kanap;
    wstr = uim_malloc(sizeof(wchar) * (len + 1));
    str  = uim_malloc(sizeof(wchar) * (len + 1));
    memcpy(wstr, clauseinfo[clause].kanap, len * sizeof(wchar));
    wstr[len] = '\0';
    wstoeuc(str, wstr, sizeof(wchar) * (len + 1));
    kana_ = MAKE_STR(str);
    free(str);
    free(wstr);

    len = clauseinfo[clause + 1].dispp - clauseinfo[clause].dispp;
    wstr = uim_malloc(sizeof(wchar) * (len + 1));
    str  = uim_malloc(sizeof(wchar) * (len + 1));
    memcpy(wstr, clauseinfo[clause].dispp, len * sizeof(wchar));
    wstr[len] = '\0';
    wstoeuc(str, wstr, sizeof(wchar) * (len + 1));
    disp_ = MAKE_STR(str);
    free(str);
    free(wstr);
  }

  ret_ = CONS(CONS(MAKE_SYM("clause-info"),
    LIST4(CONS(MAKE_SYM("kanap"), kana_),
     CONS(MAKE_SYM("dispp"), disp_),
     CONS(MAKE_SYM("conv"), MAKE_INT((int)clauseinfo->conv)),
     CONS(MAKE_SYM("ltop"), MAKE_INT((int)clauseinfo->ltop)))),
   ret_);

  ret_ = CONS(CONS(MAKE_SYM("wnn"), MAKE_PTR(buf->wnn)), ret_);

  return uim_scm_callf("reverse", "o", ret_);
}

void
uim_plugin_instance_init(void)
{
  uim_scm_init_proc3("wnn-lib-create-buffer", uim_wnn_jcCreateBuffer);
  uim_scm_init_proc2("wnn-lib-destroy-buffer", uim_wnn_jcDestroyBuffer);
  uim_scm_init_proc1("wnn-lib-clear", uim_wnn_jcClear);
  uim_scm_init_proc4("wnn-lib-convert", uim_wnn_jcConvert);
  uim_scm_init_proc1("wnn-lib-unconvert", uim_wnn_jcUnconvert);
  uim_scm_init_proc1("wnn-lib-cancel", uim_wnn_jcCancel);
  uim_scm_init_proc3("wnn-lib-expand", uim_wnn_jcExpand);
  uim_scm_init_proc3("wnn-lib-shrink", uim_wnn_jcShrink);
  uim_scm_init_proc3("wnn-lib-kana", uim_wnn_jcKana);
  uim_scm_init_proc1("wnn-lib-fix", uim_wnn_jcFix);
  uim_scm_init_proc1("wnn-lib-fix1", uim_wnn_jcFix1);
  uim_scm_init_proc3("wnn-lib-next", uim_wnn_jcNext);
  uim_scm_init_proc2("wnn-lib-candidate-info", uim_wnn_jcCandidateInfo);
  uim_scm_init_proc2("wnn-lib-get-candidate", uim_wnn_jcGetCandidate);
  uim_scm_init_proc2("wnn-lib-select", uim_wnn_jcSelect);
  uim_scm_init_proc1("wnn-lib-dot-offset", uim_wnn_jcDotOffset);
  uim_scm_init_proc2("wnn-lib-converted?", uim_wnn_jcIsConverted);
  uim_scm_init_proc3("wnn-lib-move", uim_wnn_jcMove);
  uim_scm_init_proc1("wnn-lib-top", uim_wnn_jcTop);
  uim_scm_init_proc1("wnn-lib-bottom", uim_wnn_jcBottom);
  uim_scm_init_proc2("wnn-lib-insert-char", uim_wnn_jcInsertChar);
  uim_scm_init_proc2("wnn-lib-delete-char", uim_wnn_jcDeleteChar);
  uim_scm_init_proc1("wnn-lib-kill-line", uim_wnn_jcKillLine);
  uim_scm_init_proc2("wnn-lib-change-clause", uim_wnn_jcChangeClause);
  uim_scm_init_proc1("wnn-lib-save-dic", uim_wnn_jcSaveDic);
  uim_scm_init_proc4("wnn-lib-open", uim_wnn_jcOpen);
  uim_scm_init_proc1("wnn-lib-close", uim_wnn_jcClose);
  uim_scm_init_proc1("wnn-lib-connect?", uim_wnn_jcIsConnect);
  uim_scm_init_proc1("wnn-lib-get-jconvbuf", uim_wnn_jc_dump_jconvbuf);
}

void
uim_plugin_instance_quit(void)
{
}

