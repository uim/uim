#! /usr/bin/env sscm -C EUC-JP
;; -*- buffer-file-coding-system: euc-jisx0213 -*-

;;  FileName : test-enc-eucjp.scm
;;  About    : unit test for EUC-JP string
;;
;;  Copyright (C) 2005 Kazuki Ohta <mover AT hct.zaq.ne.jp>
;;
;;  All rights reserved.
;;
;;  Redistribution and use in source and binary forms, with or without
;;  modification, are permitted provided that the following conditions
;;  are met:
;;
;;  1. Redistributions of source code must retain the above copyright
;;     notice, this list of conditions and the following disclaimer.
;;  2. Redistributions in binary form must reproduce the above copyright
;;     notice, this list of conditions and the following disclaimer in the
;;     documentation and/or other materials provided with the distribution.
;;  3. Neither the name of authors nor the names of its contributors
;;     may be used to endorse or promote products derived from this software
;;     without specific prior written permission.
;;
;;  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS
;;  IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
;;  THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;;  PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR
;;  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;;  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;;  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(load "./test/unittest.scm")

(define tn test-name)

;; string?
(assert-true "string? check" (string? "あいうえお"))

;; make-string
(assert-true "alphabet make-string check" (string=? "aaa" (make-string 3 #\a)))
(assert-true "hiragana make-string check" (string=? "あああ" (make-string 3 #\あ)))
(assert-equal? "make-string 1" "歩歩歩歩歩"   (make-string 5 #\歩))

;; string-ref
(assert-equal? "hiragana string-ref check" #\お (string-ref "あいうえお" 4))
(assert-equal? "mixed string-ref check"    #\お (string-ref "あiueお" 4))
(assert-equal? "alphabet string-ref 0 check" #\a  (string-ref "aiueo" 0))
(assert-equal? "hiragena string-ref 0 check" #\あ (string-ref "あいうえお" 0))
(assert-equal? "string-ref 1" #\歩  (string-ref "歯hi歩ﾍ歩" 3))


;; string-set!
(assert-true"hiragana string-set! check" (string=? "あいかえお"
						   (begin
						     (define str (string-copy "あいうえお"))
						     (string-set! str 2 #\か)
						     str)))
(assert-true"mixed string-set! check" (string=? "aiueo"
						(begin
						  (define str (string-copy "aiうeo"))
						  (string-set! str 2 #\u)
						  str)))
(assert-equal? "string-set! 1"     "金桂玉"   (string-set!
                                               (string-copy "金桂と")
                                               2
                                               #\玉))


;; string-length
(assert-equal? "hiragana string-length check" 5 (string-length "あいうえお"))

;; string=?
(assert-equal? "hiragana string=? check" #t (string=? "あいうえお" "あいうえお"))
(assert-equal? "mixed string=? check"    #t (string=? "aいうえo" "aいうえo"))

;; substring
(assert-true"hiragana substring check" (string=? "いう" (substring (string-copy "あいうえお") 1 3)))
(assert-true"mixed substring check"    (string=? "いu"  (substring (string-copy "aいuえo") 1 3)))

;; string-append
(assert-true "hiragana 1 string-append check" (string=? "あ"     (string-append "あ")))
(assert-true "hiragana 2 string-append check" (string=? "あい"   (string-append "あ" "い")))
(assert-true "hiragana 3 string-append check" (string=? "あいう" (string-append "あ" "い" "う")))
(assert-true "mixed 2 string-append check" (string=? "あi"   (string-append "あ" "i")))
(assert-true "mixed 3 string-append check" (string=? "あiう" (string-append "あ" "i" "う")))


;; string->list
(assert-true "string->list check" (equal? '(#\あ #\i #\う #\e #\お) (string->list "あiうeお")))
(assert-equal? "string->list 1" '(#\ぁ #\き #\る) (string->list "ぁきる"))

;; list->string
(assert-equal? "list->string check" "あaい" (list->string '(#\あ #\a #\い)))
(assert-equal? "list->string 1" "3日で" (list->string '(#\3 #\日 #\で)))

;; string-fill!
(assert-true"hiragana string-fill! check" (string=? "あああああ" (begin
								   (define str (string-copy "aiueo"))
								   (string-fill! str #\あ)
								   str)))
(assert-true"mixed string-fill! by alphabet check" (string=? "aaaaa" (begin
								       (define str (string-copy "aいうえo"))
								       (string-fill! str #\a)
								       str)))
(assert-true"mixed string-fill! by hiragana check" (string=? "いいいいい" (begin
									    (define str (string-copy "aいうえo"))
									    (string-fill! str #\い)
									    str)))

;; string
(assert-equal? "string 1" "美人には" (string #\美 #\人 #\に #\は))

;; string-copy
(assert-equal? "string-copy 1"     "金銀香"   (string-copy "金銀香"))


;; The character after ☆ is from JIS X 0212.  The one after ◎ is
;; from JIS X 0213 plane 2.  This violates all known standards, but
;; souldn't be a real problem.
(define str1 "あﾋャah暴\\暴n!☆錳◎�、!")
(define str1-list '(#\あ #\ﾋ #\ャ #\a #\h #\暴 #\\ #\暴 #\n #\! #\☆ #\錳 #\◎ #\�、 #\!))

(assert-equal? "string 2" str1 (apply string str1-list))
(assert-equal? "list->string 2" str1-list (string->list str1))

;; SRFI-75
(tn "SRFI-75")
(assert-parseable   (tn) "#\\x63")
(assert-parse-error (tn) "#\\u0063")
(assert-parse-error (tn) "#\\U00000063")

(assert-parseable   (tn) "\"\\x63\"")
(assert-parse-error (tn) "\"\\u0063\"")
(assert-parse-error (tn) "\"\\U00000063\"")

(assert-parseable   (tn) "'a")
(assert-parse-error (tn) "'あ")

(total-report)
