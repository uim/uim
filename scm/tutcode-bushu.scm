;;;
;;; Copyright (c) 2010-2011 uim Project http://code.google.com/p/uim/
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
;;; tc-2.3.1のtc-bushu.elを移植(bushu.helpファイルやsortには未対応)。
;;; (参考:部首合成アルゴリズムは[tcode-ml:1942]あたり)

(require-extension (srfi 1 8))
(require-dynlib "look")

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
  (let ((looked (tutcode-bushu-search char tutcode-bushu-expand-filename)))
    (if looked
      (tutcode-bushu-parse-entry looked)
      (list char))))

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
    ((str (if (string<=? char char2)
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

(define (tutcode-bushu-complete-compose-set char-list)
  (let ((bushu-list (append-map! tutcode-bushu-for-char char-list)))
    (tutcode-bushu-subtract-set
      (tutcode-bushu-char-list-for-bushu bushu-list) char-list)))

(define (tutcode-bushu-strong-compose-set char-list)
  (let*
    ((bushu-list (append-map! tutcode-bushu-for-char char-list))
     (r (tutcode-bushu-superset bushu-list))
     (r2
      (let loop
        ((lis char-list)
         (r r))
        (if (null? lis)
          r
          (loop (cdr lis) (delete! (car lis) r))))))
    r2))

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

(define (tutcode-bushu-weak-compose-set char-list strong-compose-set)
  (if (null? (cdr char-list)) ; char-list が一文字だけの時は何もしない
    ()
    (tutcode-bushu-subtract-set
      (tutcode-bushu-all-compose-set char-list ())
      strong-compose-set)))

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
            (delete! char
              (if (pair? rest)
                (tutcode-bushu-strong-diff-set rest d1-or-d2 complete?)
                (if complete?
                  (tutcode-bushu-char-list-for-bushu d1-or-d2)
                  (tutcode-bushu-subset d1-or-d2))))))))))

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
     (res
       (receive
        (true-diff-set rest-diff-set)
        (partition!
          (lambda (char)
            (null?
              (tutcode-bushu-subtract-set
                (tutcode-bushu-for-char char) bushu-list)))
          diff-set)
        (append! true-diff-set rest-diff-set))))
    (delete-duplicates! res)))

;;; 対話的な部首合成変換用に、指定された部首のリストから部首合成可能な
;;; 漢字のリストを返す。
;;; @param char-list 入力された部首のリスト
;;; @return 合成可能な漢字のリスト
(define (tutcode-bushu-compose-interactively char-list)
  (let*
    ((complete-compose-set (tutcode-bushu-complete-compose-set char-list))
     (complete-diff-set (tutcode-bushu-complete-diff-set char-list))
     (strong-compose-set (tutcode-bushu-strong-compose-set char-list))
     (strong-diff-set (tutcode-bushu-strong-diff-set char-list))
     (weak-diff-set (tutcode-bushu-weak-diff-set char-list strong-diff-set))
     (weak-compose-set (tutcode-bushu-weak-compose-set char-list
                        strong-compose-set)))
  (delete-duplicates!
    (append!
      complete-compose-set
      complete-diff-set
      strong-compose-set
      strong-diff-set
      weak-diff-set
      weak-compose-set))))
