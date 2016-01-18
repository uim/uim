;;;
;;; Copyright (c) 2010-2013 uim Project https://github.com/uim/uim
;;;
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;; 3. Neither the name of authors nor the names of its contributors
;;;    may be used to endorse or promote products derived from this software
;;;    without specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS IS'' AND
;;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE
;;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
;;; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;;; SUCH DAMAGE.
;;;;

;;; tutcode-bushu.scm: 対話的な部首合成変換
;;;
;;; tc-2.3.1のtc-bushu.elを移植(sortでの打ちやすさの考慮は未対応)。
;;; (参考:部首合成アルゴリズムは[tcode-ml:1942]あたり。以下抜粋)
;;; ●定義
;;; ○Σを全文字の集合とする。
;;; ○Πを全部首の集合とする。(Π ⊂ Σでなくてもよいはず。)
;;; ○各文字c ∈ Σは部首p ∈ Πの集合から成る。
;;; ○以下の集合を文字aと文字bの合成集合と呼ぶ。
;;; 
;;;   {c | c ∈ Σ, c ⊇ a' ∪ b', a' ⊆ a (a' ≠ 空集合), b' ⊆ b (b' ≠ 空集合)
;;;        c ≠ a, c ≠ b }
;;; 
;;;   特に、 a' = a かつ b' = b の場合を強合成集合と呼ぶ。
;;;   強合成集合でない合成集合を弱合成集合と呼ぶ。
;;; 
;;; ○以下の集合を文字aと文字bの差合成集合と呼ぶ。
;;; 
;;;   {c | c ∈ Σ, c ⊆ a- ∪ b-}
;;; 
;;;   ここで、a-、b-の定義は次とおりである。
;;;         a- = a \ (a ∩ b)
;;;         b- = b \ (a ∩ b)
;;; 
;;;   特に、(a- = 空集合) または (b- = 空集合)の場合を強差合成集合と呼ぶ。
;;;   強差合成集合でない差合成集合を弱差合成集合と呼ぶ。
;;; --------------------------------------------------------
;;; ●部首合成変換の優先順位
;;; (1) 強合成集合
;;;       複数ある場合は、部首の集合の要素数が小さい方が優先?
;;; (2) 強差合成集合
;;; (3) 弱合成集合
;;; (4) 弱差合成集合
;;; --------------------------------------------------------
;;; 
;;; 例:
;;; 
;;; 漸 ≡ {シ, 車, 斤}
;;; 斬 ≡ {車, 斤}
;;; 
;;; 稿 ≡ {禾, 高}
;;; 私 ≡ {禾, ム}
;;; --------
;;; 
;;; (i) a = シ ≡ {シ}、 b = 斬の場合
;;; 
;;;   a ∪ b = {シ, 車, 斤} および「漸 ⊇ a ∪ b」より、
;;;   「漸」は強合成集合の要素になる。
;;; 
;;; (ii) a = シ、 b = 車 ≡ {車} の場合
;;; 
;;;   a ∪ b = {シ, 車} および「漸 ⊇ a ∪ b」より、
;;;   「漸」は強合成集合の要素になる。
;;; 
;;; (iii) a = 私、b = 高 ≡ {高} の場合
;;; 
;;;   a' = {禾}, b' = {高} とすると、
;;;   a' ∪ b' = {禾, 高} であり、かつ「稿 ⊇ a' ∪ b'」より
;;;   「稿」は弱合成集合の要素になる。
;;; 
;;; (iv) a = 私、b = ム の場合
;;; 
;;;   a- = {ム}、b- = 空集合 より、
;;;   「ム」は強差合成集合の要素になる。
;;; 
;;; (v) a = 私、b = 稿 の場合
;;; 
;;;   a- = {ム}、b- = {高}より、
;;;   「ム」および「高」は弱差合成集合の要素になる。

(require-extension (srfi 1 2 8 69 95))
(require "util.scm")
(require-dynlib "look")

;;; #tの場合、部首の並べ方によって合成される文字の優先度が変わる
(define tutcode-bushu-sequence-sensitive? #t)

;;; 優先度が同じ場合に優先される文字のリスト
(define tutcode-bushu-prioritized-chars ())

;;; 部首合成出力には入れない文字のリスト (tc-2.3.1-22.6より)
(define tutcode-bushu-inhibited-output-chars
  '("え" "し" "へ" "ア" "イ" "ウ" "エ" "オ" "カ" "ク" "ケ" "サ" "シ"
    "タ" "チ" "テ" "ト" "ニ" "ヌ" "ネ" "ノ" "ハ" "ヒ" "ホ" "ム" "メ"
    "ヨ" "リ" "ル" "レ" "ロ" "ワ" "ン"))

;;; sortを入れると遅すぎる環境でも、対話的な部首合成変換は使いたい場合向け。
;;; ~/.uimに以下を追加するとsortを省略可能。
;;; (set! tutcode-bushu-sort! (lambda (seq less?) seq))
(define tutcode-bushu-sort! sort!)

;;; bushu.helpファイルを読んで生成したtutcode-bushudic形式のリスト
(define tutcode-bushu-help ())

;;; tutcode-bushu-for-charのキャッシュ用hash-table
(define tutcode-bushu-for-char-hash-table (make-hash-table =))

;;; 文字のリストとして返す。
(define (tutcode-bushu-parse-entry str)
  (reverse! (string-to-list str)))

;;; STR で始まる行のうち、最初のものを見つける。
;;; @param str 検索文字列
;;; @param file 対象ファイル名
;;; @return 見つけた文字列(strは含まない)。見つからなかった場合は#f
(define (tutcode-bushu-search str file)
  (let ((looked (look-lib-look #f #f 1 file str)))
    (and (pair? looked)
         (car looked)))) ; 1行ぶんの文字列だけ取得

;;; CHARを構成する部首のリストを返す。
(define (tutcode-bushu-for-char char)
  (let*
    ((i (tutcode-euc-jp-string->ichar char))
     (cache
      (and i (hash-table-ref/default tutcode-bushu-for-char-hash-table i #f))))
    (if cache
      (list-copy cache)
      (let*
        ((looked (tutcode-bushu-search char tutcode-bushu-expand-filename))
         (res
          (if looked
            (tutcode-bushu-parse-entry looked)
            (list char))))
        (if i
          (hash-table-set! tutcode-bushu-for-char-hash-table i (list-copy res)))
        res))))

(define (tutcode-bushu-lookup-index2-entry-internal str)
  (let
    ((looked (tutcode-bushu-search (string-append str " ")
              tutcode-bushu-index2-filename)))
    (if looked
      (tutcode-bushu-parse-entry looked)
      ())))

;;; CHARを部首として持つ文字のリストを返す。
;;; 返すリストにはCHARも含まれる。
(define (tutcode-bushu-lookup-index2-entry-1 char)
  (cons char (tutcode-bushu-lookup-index2-entry-internal char)))

;;; CHARとCHAR2を部首として持つ文字のリストを返す。
(define (tutcode-bushu-lookup-index2-entry-2 char char2)
  (let
    ((str (if (string<? char char2)
              (string-append char char2)
              (string-append char2 char))))
    (tutcode-bushu-lookup-index2-entry-internal str)))

;;; CHARをN個以上部首として持つ文字のリストを返す。
(define (tutcode-bushu-lookup-index2-entry-many char n)
  (if (= n 1)
    (tutcode-bushu-lookup-index2-entry-1 char)
    (tutcode-bushu-lookup-index2-entry-internal
      (apply string-append (make-list n char)))))

;;; LIST中のELTの数を返す。
(define (tutcode-bushu-count elt list)
  (count (lambda (elem) (string=? elt elem)) list))

;;; BUSHU を N 個以上含む文字のリストを返す。
(define (tutcode-bushu-included-char-list bushu n)
  (tutcode-bushu-lookup-index2-entry-many bushu n))

;;; LIST1がLIST2に含まれる集合かどうかを表す述語。
;;; 同じ要素が複数ある場合は、LIST2に含まれる数の方が少なければ#fを返す。
(define (tutcode-bushu-included-set? list1 list2)
  (if (null? list1)
    #t
    (let ((x (car list1)))
      (if (> (tutcode-bushu-count x list1) (tutcode-bushu-count x list2))
        #f
        (tutcode-bushu-included-set? (cdr list1) list2)))))

;;; LIST1とLIST2が同じ集合かどうかを表す述語。
;;; 同じ要素が複数ある場合は、同じ数だけ含まれていないと等しいとはみなさない。
(define (tutcode-bushu-same-set? list1 list2)
  (and (= (length list1) (length list2))
       (tutcode-bushu-included-set? list1 list2)))

;;; BUSHU-LISTで構成される字の集合を求める。
(define (tutcode-bushu-char-list-for-bushu bushu-list)
  (cond
    ((null? bushu-list) ())
    ((null? (cdr bushu-list)) ; 1文字
      (let*
        ((bushu (car bushu-list))
         (included (tutcode-bushu-included-char-list bushu 1))
         (ret
          (filter-map
            (lambda (elem)
              (let ((l (tutcode-bushu-for-char elem)))
                ;; 等価文字
                (and (string=? bushu (car l))
                     (null? (cdr l))
                     elem)))
            included)))
        ret))
    ((null? (cddr bushu-list)) ; 2文字
      (let*
        ((bushu1 (car bushu-list))
         (bushu2 (cadr bushu-list))
         (included (tutcode-bushu-lookup-index2-entry-2 bushu1 bushu2))
         (ret
          (filter-map
            (lambda (elem)
              (let*
                ((l (tutcode-bushu-for-char elem))
                 (len2? (= (length l) 2))
                 (l1 (and len2? (car l)))
                 (l2 (and len2? (cadr l))))
                (and
                  len2?
                  (or (and (string=? bushu1 l1) (string=? bushu2 l2))
                      (and (string=? bushu2 l1) (string=? bushu1 l2)))
                  elem)))
            included)))
        ret))
    (else ; 3文字以上
      (let*
        ((bushu1 (car bushu-list))
         (bushu2 (cadr bushu-list))
         (included (tutcode-bushu-lookup-index2-entry-2 bushu1 bushu2))
         (ret
          (filter-map
            (lambda (elem)
              (and
                (tutcode-bushu-same-set?
                  (tutcode-bushu-for-char elem) bushu-list)
                elem))
            included)))
        ret))))

;;; LIST1とLIST2との集合積を返す。
;;; 同じ要素が複数ある場合は区別する。
;;; 返り値における要素の並び方はLIST1の方に基づく。
(define (tutcode-bushu-intersection list1 list2)
  (let loop
    ((l1 list1)
     (l2 list2)
     (intersection ()))
    (if (or (null? l1) (null? l2))
      (reverse! intersection)
      (let*
        ((elt (car l1))
         (l2mem (member elt l2))
         (new-intersection (if l2mem (cons elt intersection) intersection))
         (l2-deleted-first-elt
          (if l2mem
            (append (drop-right l2 (length l2mem)) (cdr l2mem))
            l2)))
        (loop (cdr l1) l2-deleted-first-elt new-intersection)))))

(define (tutcode-bushu-complement-intersection list1 list2)
  (if (null? list2)
    list1
    (let loop
      ((l1 list1)
       (l2 list2)
       (ci ()))
      (if (or (null? l1) (null? l2))
        (append ci l1 l2)
        (let*
          ((e (car l1))
           (c1 (+ 1 (tutcode-bushu-count e (cdr l1))))
           (c2 (tutcode-bushu-count e l2))
           (diff (abs (- c1 c2))))
          (loop
            (if (> c1 1)
              (delete e (cdr l1))
              (cdr l1))
            (if (> c2 0)
              (delete e l2)
              l2)
            (if (> diff 0)
              (append! ci (make-list diff e))
              ci)))))))

(define (tutcode-bushu-subtract-set list1 list2)
  (if (null? list2)
    list1
    (let loop
      ((l1 list1)
       (l2 list2)
       (ci ()))
      (if (or (null? l1) (null? l2))
        (append l1 ci)
        (let*
          ((e (car l1))
           (c1 (+ 1 (tutcode-bushu-count e (cdr l1))))
           (c2 (tutcode-bushu-count e l2))
           (diff (- c1 c2)))
          (loop
            (if (> c1 1)
              (delete e (cdr l1))
              (cdr l1))
            (if (> c2 0)
              (delete e l2)
              l2)
            (if (> diff 0)
              (append! ci (make-list diff e))
              ci)))))))

;;; 部首の部分集合がBUSHU-LISTである字の集合を求める。
(define (tutcode-bushu-superset bushu-list)
  (cond
    ((null? bushu-list) ())
    ((null? (cdr bushu-list)) ; 1文字
      (tutcode-bushu-included-char-list (car bushu-list) 1))
    ((null? (cddr bushu-list)) ; 2文字
      (tutcode-bushu-lookup-index2-entry-2 (car bushu-list) (cadr bushu-list)))
    (else ; 3文字以上
      (let*
        ((bushu (car bushu-list))
         (n (tutcode-bushu-count bushu bushu-list))
         (bushu-list-wo-bushu
          (if (> n 1)
            (delete bushu (cdr bushu-list))
            (cdr bushu-list)))
         (included
          (if (> n 1)
            (tutcode-bushu-included-char-list bushu n)
            (tutcode-bushu-lookup-index2-entry-2 bushu
              (list-ref bushu-list-wo-bushu 1))))
         (ret
          (filter-map
            (lambda (elem)
              (and
                (tutcode-bushu-included-set? bushu-list-wo-bushu
                  (tutcode-bushu-for-char elem))
                elem))
            included)))
        ret))))

;;; CHARが変数`tutcode-bushu-prioritized-chars'の何番目にあるかを返す。
;;; なければ #f を返す。
(define (tutcode-bushu-priority-level char)
  (and (pair? tutcode-bushu-prioritized-chars)
    (let ((char-list (member char tutcode-bushu-prioritized-chars)))
      (and char-list
        (- (length tutcode-bushu-prioritized-chars) (length char-list) -1)))))

;;; REFを基準として、BUSHU1の方がBUSHU2よりも並び方が基準に近いかどうか。
;;; 判断できなかったり、する必要がない場合はDEFAULTを返す。
(define (tutcode-bushu-higher-priority? bushu1 bushu2 ref default)
  (if tutcode-bushu-sequence-sensitive?
    (let loop
      ((bushu1 bushu1)
       (bushu2 bushu2)
       (ref ref))
      (if (or (null? ref) (null? bushu1) (null? bushu2))
        default
        (let*
          ((b1 (car bushu1))
           (b2 (car bushu2))
           (r (car ref))
           (r=b1? (string=? r b1))
           (r=b2? (string=? r b2)))
          (if (and r=b1? r=b2?)
            (loop (cdr bushu1) (cdr bushu2) (cdr ref))
            (cond
              ((and r=b1? (not r=b2?))
                #t)
              ((and (not r=b1?) r=b2?)
                #f)
              ((and (not r=b1?) (not r=b2?))
                default))))))
    default))

;;; CHAR1がCHAR2より優先度が高いか?
;;; BUSHU-LISTで指定された部首リストを基準とする。
;;; MANY?が#fの場合、同じ優先度では、BUSHU-LISTに含まれない
;;; 部首の数が少ない方が優先される。
;;; #tの場合は多い方が優先される。
(define (tutcode-bushu-less? char1 char2 bushu-list many?)
  (let*
    ((bushu1 (tutcode-bushu-for-char char1))
     (bushu2 (tutcode-bushu-for-char char2))
     (i1 (tutcode-bushu-intersection bushu1 bushu-list))
     (i2 (tutcode-bushu-intersection bushu2 bushu-list))
     (il1 (length i1))
     (il2 (length i2))
     (l1 (length bushu1))
     (l2 (length bushu2)))
    (if (= il1 il2)
      (if (= l1 l2)
        (let ((p1 (tutcode-bushu-priority-level char1))
              (p2 (tutcode-bushu-priority-level char2)))
          (cond
            (p1
              (if p2
                (< p1 p2)
                #t))
            (p2
              #f)
            (else
              (let
                ((val (tutcode-bushu-higher-priority? i1 i2
                        (tutcode-bushu-intersection bushu-list (append! i1 i2))
                        'default)))
                (if (not (eq? val 'default))
                  val
                  (let
                    ((s1 (tutcode-reverse-find-seq char1 tutcode-rule))
                     (s2 (tutcode-reverse-find-seq char2 tutcode-rule)))
                    (cond 
                      ((and s1 s2)
                        (let
                          ((sl1 (length s1))
                           (sl2 (length s2)))
                          (if (= sl1 sl2)
                            ;;XXX:打ちやすさでの比較は省略
                            (string<? char1 char2)
                            (< sl1 sl2))))
                      (s1
                        #t)
                      (s2
                        #f)
                      (else
                        (string<? char1 char2)))))))))
        (if many?
          (> l1 l2)
          (< l1 l2)))
      (> il1 il2))))

(define (tutcode-bushu-less-against-sequence? char1 char2 bushu-list)
  (let ((p1 (tutcode-bushu-priority-level char1))
        (p2 (tutcode-bushu-priority-level char2)))
    (cond
      (p1
        (if p2
          (< p1 p2)
          #t))
      (p2
        #f)
      (else
        (tutcode-bushu-higher-priority?
          (tutcode-bushu-for-char char1)
          (tutcode-bushu-for-char char2)
          bushu-list
          (string<? char1 char2))))))

(define (tutcode-bushu-complete-compose-set char-list bushu-list)
  (tutcode-bushu-sort!
    (tutcode-bushu-subtract-set
      (tutcode-bushu-char-list-for-bushu bushu-list) char-list)
    (lambda (a b)
      (tutcode-bushu-less-against-sequence? a b bushu-list))))

(define (tutcode-bushu-strong-compose-set char-list bushu-list)
  (let*
    ((r (tutcode-bushu-superset bushu-list))
     (r2
      (let loop
        ((lis char-list)
         (r r))
        (if (null? lis)
          r
          (loop (cdr lis) (delete! (car lis) r))))))
    (tutcode-bushu-sort! r2
      (lambda (a b) (tutcode-bushu-less? a b bushu-list #f)))))

(define (tutcode-bushu-include-all-chars-bushu? char char-list)
  (let*
    ((bushu0 (tutcode-bushu-for-char char))
     (new-bushu
      (let loop
        ((char-list char-list)
         (new-bushu bushu0))
        (if (null? char-list)
          new-bushu
          (loop
            (cdr char-list)
            (tutcode-bushu-subtract-set
              new-bushu (tutcode-bushu-for-char (car char-list)))))))
     (bushu (tutcode-bushu-subtract-set bushu0 new-bushu)))
    (let loop
      ((cl char-list))
      (cond
        ((null? cl)
          #t)
        ((null?
          (tutcode-bushu-subtract-set bushu
            (append-map!
              (lambda (char)
                (tutcode-bushu-for-char char))
              (tutcode-bushu-subtract-set char-list (list (car cl))))))
          #f)
        (else
          (loop (cdr cl)))))))

(define (tutcode-bushu-all-compose-set char-list bushu-list)
  (let*
    ((char (car char-list))
     (rest (cdr char-list))
     (all-list
      (delete-duplicates!
        (delete! char
          (append-map!
            (if (pair? rest)
              (lambda (bushu)
                (tutcode-bushu-all-compose-set rest (cons bushu bushu-list)))
              (lambda (bushu)
                (tutcode-bushu-superset (cons bushu bushu-list))))
            (tutcode-bushu-for-char char))))))
    (filter!
      (lambda (char)
        (tutcode-bushu-include-all-chars-bushu? char char-list))
      all-list)))

(define (tutcode-bushu-weak-compose-set char-list bushu-list strong-compose-set)
  (if (null? (cdr char-list)) ; char-list が一文字だけの時は何もしない
    ()
    (tutcode-bushu-sort!
      (tutcode-bushu-subtract-set
        (tutcode-bushu-all-compose-set char-list ())
        strong-compose-set)
      (lambda (a b)
        (tutcode-bushu-less? a b bushu-list #f)))))

(define (tutcode-bushu-subset bushu-list)
  ;;XXX:長いリストに対するdelete-duplicates!は遅いので、filter後に行う
  (delete-duplicates!
    (filter!
      (lambda (char)
        (null? 
          (tutcode-bushu-subtract-set
            (tutcode-bushu-for-char char) bushu-list)))
      (append-map!
        (lambda (elem)
          (tutcode-bushu-included-char-list elem 1))
        (delete-duplicates bushu-list)))))

(define (tutcode-bushu-strong-diff-set char-list . args)
  (let-optionals* args ((bushu-list ()) (complete? #f))
    (let*
      ((char (car char-list))
       (rest (cdr char-list))
       (bushu (tutcode-bushu-for-char char))
       (i
        (if (pair? bushu-list)
          (tutcode-bushu-intersection bushu bushu-list)
          bushu)))
      (if (null? i)
        ()
        (let*
          ((d1 (tutcode-bushu-complement-intersection bushu i))
           (d2 (tutcode-bushu-complement-intersection bushu-list i))
           (d1-or-d2 (if (pair? d1) d1 d2)))
          (if
            (or (and (pair? d1) (pair? d2))
                (and (null? d1) (null? d2)))
            ()
            (if (pair? rest)
              (delete! char
                (tutcode-bushu-strong-diff-set rest d1-or-d2 complete?))
              (tutcode-bushu-sort!
                (delete! char
                  (if complete?
                    (tutcode-bushu-char-list-for-bushu d1-or-d2)
                    (tutcode-bushu-subset d1-or-d2)))
                (lambda (a b)
                  (tutcode-bushu-less? a b bushu-list #t))))))))))

(define (tutcode-bushu-complete-diff-set char-list)
  (tutcode-bushu-strong-diff-set char-list () #t))

(define (tutcode-bushu-all-diff-set char-list bushu-list common-list)
  (let*
    ((char (car char-list))
     (rest (cdr char-list))
     (bushu (tutcode-bushu-for-char char))
     (new-common-list
      (if (pair? common-list)
        (tutcode-bushu-intersection bushu common-list)
        bushu)))
    (if (null? new-common-list)
      ()
      (let*
        ((new-bushu-list
          (if (null? common-list)
            ()
            (append bushu-list
              (tutcode-bushu-complement-intersection
                bushu new-common-list)
              (tutcode-bushu-complement-intersection
                common-list new-common-list)))))
        (if (pair? rest)
          (delete! char
            (tutcode-bushu-all-diff-set rest new-bushu-list new-common-list))
          (delete-duplicates!
            (delete! char
              (append-map!
                (lambda (bushu)
                  (tutcode-bushu-subset
                    (append new-bushu-list (delete bushu new-common-list))))
                new-common-list))))))))

(define (tutcode-bushu-weak-diff-set char-list strong-diff-set)
  (let*
    ((bushu-list (tutcode-bushu-for-char (car char-list)))
     (diff-set
      (tutcode-bushu-subtract-set
        (tutcode-bushu-all-diff-set char-list () ())
        strong-diff-set))
     (less-or-many? (lambda (a b) (tutcode-bushu-less? a b bushu-list #t)))
     (res
       (receive
        (true-diff-set rest-diff-set)
        (partition!
          (lambda (char)
            (null?
              (tutcode-bushu-subtract-set
                (tutcode-bushu-for-char char) bushu-list)))
          diff-set)
        (append! (tutcode-bushu-sort! true-diff-set less-or-many?)
                 (tutcode-bushu-sort! rest-diff-set less-or-many?)))))
    (delete-duplicates! res)))

;;; bushu.helpファイルを読んでtutcode-bushudic形式のリストを生成する
;;; @return tutcode-bushudic形式のリスト。読み込めなかった場合は#f
(define (tutcode-bushu-help-load)
  (define parse
    (lambda (line)
      ;; 例: "傳イ専* 伝・"
      ;; →(((("イ" "専"))("傳"))((("専" "イ"))("傳"))((("伝" "・"))("傳")))
      (let*
          ((comps (string-split line " "))
           (kanji-lcomps (map tutcode-bushu-parse-entry comps))
           (kanji (and (pair? (car kanji-lcomps)) (caar kanji-lcomps)))
           ;; 行頭の合成後の漢字を除いたリスト。例:(("イ" "専" "*")("伝" "・"))
           (lcomps
            (if kanji
                (cons (cdar kanji-lcomps) (cdr kanji-lcomps))
                ())))
        (append-map!
         (lambda (elem)
           (let ((len (length elem)))
             (if (< len 2)
                 ()
                 (let*
                     ((bushu1 (list-ref elem 0))
                      (bushu2 (list-ref elem 1))
                      (rule (list (list (list bushu1 bushu2)) (list kanji)))
                      (rev
                       (and
                        (and (>= len 3) (string=? (list-ref elem 2) "*"))
                        (list (list (list bushu2 bushu1)) (list kanji)))))
                   (if rev
                       (list rule rev)
                       (list rule))))))
         lcomps))))
  (and
    (file-readable? tutcode-bushu-help-filename)
    (call-with-input-file tutcode-bushu-help-filename
      (lambda (port)
        (let loop ((line (read-line port))
                   (rules ()))
          (if (or (not line)
                  (eof-object? line))
              rules
              (loop (read-line port)
                    (append! rules (parse line)))))))))


;;; bushu.helpファイルに基づく部首合成を行う
(define (tutcode-bushu-compose-explicitly char-list)
  (if (null? tutcode-bushu-help)
    (set! tutcode-bushu-help (tutcode-bushu-help-load)))
  (if (not tutcode-bushu-help)
    ()
    (cond
      ((null? char-list)
        ())
      ((null? (cdr char-list)) ; 1文字
        (map (lambda (elem) (caadr elem))
          (rk-lib-find-partial-seqs char-list tutcode-bushu-help)))
      ((pair? (cddr char-list)) ; 3文字以上
        ())
      (else ; 2文字
        (let ((seq (rk-lib-find-seq char-list tutcode-bushu-help)))
          (if seq
            (cadr seq)
            ()))))))

;;; 対話的な部首合成変換用に、指定された部首のリストから部首合成可能な
;;; 漢字のリストを返す。
;;; @param char-list 入力された部首のリスト
;;; @param exit-on-found? 漢字が1文字でも合成できたらそれ以上の合成は中止する
;;; @return 合成可能な漢字のリスト
(define (tutcode-bushu-compose-tc23 char-list exit-on-found?)
  (let*
    ((bushu-list (append-map! tutcode-bushu-for-char char-list))
     (update-res!
      (lambda (res lst)
        (append! res
          (filter!
            (lambda (elem)
              (not (member elem tutcode-bushu-inhibited-output-chars)))
            lst))))
     (resall
      (let
        ((r0 (update-res! () (tutcode-bushu-compose-explicitly char-list))))
        (if (and exit-on-found? (pair? r0))
          r0
          (let
            ((r1 (update-res! r0
                  (tutcode-bushu-complete-compose-set char-list bushu-list))))
            (if (and exit-on-found? (pair? r1))
              r1
              (let
                ((r2 (update-res! r1
                      (tutcode-bushu-complete-diff-set char-list))))
                (if (and exit-on-found? (pair? r2))
                  r2
                  (let*
                    ((strong-compose-set
                      (tutcode-bushu-strong-compose-set char-list bushu-list))
                     (r3 (update-res! r2 strong-compose-set)))
                    (if (and exit-on-found? (pair? r3))
                      r3
                      (let*
                        ((strong-diff-set
                          (tutcode-bushu-strong-diff-set char-list))
                         (r4 (update-res! r3 strong-diff-set)))
                        (if (and exit-on-found? (pair? r4))
                          r4
                          (let
                            ((r5 (update-res! r4
                                  (tutcode-bushu-weak-diff-set char-list
                                    strong-diff-set))))
                            (if (and exit-on-found? (pair? r5))
                              r5
                              (let
                                ((r6 (update-res! r5
                                      (tutcode-bushu-weak-compose-set char-list
                                        bushu-list strong-compose-set))))
                                r6)))))))))))))))
    (delete-duplicates! resall)))

;;; 対話的な部首合成変換用に、指定された部首のリストから部首合成可能な
;;; 漢字のリストを返す。
;;; @param char-list 入力された部首のリスト
;;; @return 合成可能な漢字のリスト
(define (tutcode-bushu-compose-interactively char-list)
  (tutcode-bushu-compose-tc23 char-list #f))

;;; 部首合成変換を行う。
;;; tc-2.3.1-22.6の部首合成アルゴリズムを使用。
;;; @param c1 1番目の部首
;;; @param c2 2番目の部首
;;; @return 合成後の文字。合成できなかったときは#f
(define (tutcode-bushu-convert-tc23 c1 c2)
  (let ((res (tutcode-bushu-compose-tc23 (list c1 c2) #t)))
    (if (null? res)
      #f
      (car res))))

;; tc-2.3.1のtc-help.elからの移植
(define (tutcode-bushu-decompose-to-two-char char)
  (let ((b1 (tutcode-bushu-for-char char)))
    (let loop
      ((b1 (cdr b1))
       (b2 (list (car b1))))
      (if (null? b1)
        #f
        (let*
          ((cl1t (tutcode-bushu-char-list-for-bushu b2))
           (cl1
            (if (pair? cl1t)
              cl1t
              (if (= (length b2) 1)
                b2
                ())))
           (cl2t (tutcode-bushu-char-list-for-bushu b1))
           (cl2
            (if (pair? cl2t)
              cl2t
              (if (= (length b1) 1)
                b1
                ()))))
          (let c1loop
            ((cl1 cl1))
            (if (null? cl1)
              (loop (cdr b1) (append b2 (list (car b1))))
              (let c2loop
                ((cl2 cl2))
                (if (null? cl2)
                  (c1loop (cdr cl1))
                  (if
                    (equal?
                      (tutcode-bushu-convert-tc23 (car cl1) (car cl2))
                      char)
                    (cons (car cl1) (car cl2))
                    (c2loop (cdr cl2))))))))))))

;;; CHARが直接入力可能なBUSHU1とBUSHU2で合成できる場合、
;;; BUSHU1とBUSHU2のストロークを含むリストを返す。
;;; 例: "繋" => (((("," "o"))("撃")) ((("f" "q"))("糸")))
;;; @param char 合成後の文字
;;; @param bushu1 部首1
;;; @param bushu2 部首2
;;; @param rule tutcode-rule
;;; @return 対象文字の部首合成に必要な2つの文字とストロークのリスト。
;;;  合成できない場合は#f
(define (tutcode-bushu-composed char bushu1 bushu2 rule)
  (and-let*
    ((seq1 (tutcode-auto-help-get-stroke bushu1 rule))
     (seq2 (tutcode-auto-help-get-stroke bushu2 rule))
     (composed (tutcode-bushu-convert-tc23 bushu1 bushu2)))
    (and
      (string=? composed char)
      (list seq1 seq2))))

;;; 自動ヘルプ:対象文字を部首合成するのに必要となる、
;;; 外字でない2つの文字のリストを返す
;;; 例: "繋" => (((("," "o"))("撃")) ((("f" "q"))("糸")))
;;; @param kanji 対象文字
;;; @param rule tutcode-rule
;;; @param stime 開始日時
;;; @return 対象文字の部首合成に必要な2つの文字とストロークのリスト。
;;;  見つからなかった場合は#f
(define (tutcode-auto-help-bushu-decompose-tc23 kanji rule stime)
  (if (> (string->number (difftime (time) stime)) tutcode-auto-help-time-limit)
    #f
    (let ((decomposed (tutcode-bushu-decompose-to-two-char kanji)))
      (if decomposed
        (let*
          ((char1 (car decomposed))
           (char2 (cdr decomposed))
           (seq1 (tutcode-auto-help-get-stroke char1 rule))
           (seq2 (tutcode-auto-help-get-stroke char2 rule)))
          (cond
            (seq1
              (if seq2
                (list seq1 seq2)
                (let*
                  ((bushu-list (tutcode-bushu-for-char char2))
                   (find-loop
                    (lambda (set)
                      (let loop
                        ((lis
                          (tutcode-bushu-sort! set
                            (lambda (a b)
                              (tutcode-bushu-less? a b bushu-list #f)))))
                        (if (null? lis)
                          #f
                          (let
                            ((res (tutcode-bushu-composed
                                    kanji char1 (car lis) rule)))
                            (or res
                              (loop (cdr lis)))))))))
                  (or
                    ;; 強合成集合を探す
                    (find-loop (tutcode-bushu-subset bushu-list))
                    ;; 弱合成集合を探す
                    (find-loop (tutcode-bushu-superset bushu-list))
                    ;; 再帰的に探す
                    (let
                      ((dec2
                        (tutcode-auto-help-bushu-decompose-tc23 char2
                          rule stime)))
                      (and dec2
                        (list seq1 dec2)))))))
            (seq2
              (let*
                ((bushu-list (tutcode-bushu-for-char char1))
                 (find-loop
                  (lambda (set)
                    (let loop
                      ((lis
                        (tutcode-bushu-sort! set
                          (lambda (a b)
                            (tutcode-bushu-less? a b bushu-list #f)))))
                      (if (null? lis)
                        #f
                        (let
                          ((res (tutcode-bushu-composed
                                  kanji (car lis) char2 rule)))
                          (or res
                            (loop (cdr lis)))))))))
                (or
                  ;; 強合成集合を探す
                  (find-loop (tutcode-bushu-subset bushu-list))
                  ;; 弱合成集合を探す
                  (find-loop (tutcode-bushu-superset bushu-list))
                  ;; 再帰的に探す
                  (let
                    ((dec1
                      (tutcode-auto-help-bushu-decompose-tc23 char1
                        rule stime)))
                    (and dec1
                      (list dec1 seq2))))))
            (else
              (let*
                ((bushu1 (tutcode-bushu-for-char char1))
                 (bushu2 (tutcode-bushu-for-char char2))
                 (bushu-list (append bushu1 bushu2))
                 (mkcl
                  (lambda (bushu)
                    (tutcode-bushu-sort!
                      (delete-duplicates!
                        (append!
                          (tutcode-bushu-subset bushu)
                          (tutcode-bushu-superset bushu)))
                      (lambda (a b)
                        (tutcode-bushu-less? a b bushu-list #f)))))
                 (cl1 (mkcl bushu1))
                 (cl2 (mkcl bushu2)))
                (let loop1
                  ((cl1 cl1))
                  (if (null? cl1)
                    #f
                    (let loop2
                      ((cl2 cl2))
                      (if (null? cl2)
                        (loop1 (cdr cl1))
                        (let
                          ((res (tutcode-bushu-composed
                                  kanji (car cl1) (car cl2) rule)))
                          (or res
                            (loop2 (cdr cl2))))))))))))
        ;; 二つに分割できない場合
        ;; 強差合成集合を探す
        (let*
          ((bushu-list (tutcode-bushu-for-char kanji))
           (superset
            (tutcode-bushu-sort!
              (tutcode-bushu-superset bushu-list)
              (lambda (a b)
                (tutcode-bushu-less? a b bushu-list #f)))))
          (let loop1
            ((lis superset))
            (if (null? lis)
              #f
              (let*
                ((seq1 (tutcode-auto-help-get-stroke (car lis) rule))
                 (diff (if seq1
                        (tutcode-bushu-subtract-set
                          (tutcode-bushu-for-char (car lis)) bushu-list)
                        ())))
                (if seq1
                  (let loop2
                    ((lis2 (tutcode-bushu-subset diff)))
                    (if (null? lis2)
                      (loop1 (cdr lis))
                      (let
                        ((res (tutcode-bushu-composed
                                kanji (car lis) (car lis2) rule)))
                        (or res
                          (loop2 (cdr lis2))))))
                  (loop1 (cdr lis)))))))))))
