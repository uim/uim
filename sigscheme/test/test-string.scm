;;  FileName : test-string.scm
;;  About    : unit test for R5RS string
;;
;;  Copyright (C) 2005      by Kazuki Ohta (mover@hct.zaq.ne.jp)
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

;; check string?
(assert-true"string? check" (string? "aiueo"))

;; check make-string
(assert-true"null make-string" (string? (make-string 6)))
(assert-true"alphabet make-string check" (string=? "aaa" (make-string 3 #\a)))
(assert-true"hiragana make-string check" (string=? "あああ" (make-string 3 #\あ)))

;; check string-ref
(assert-equal? "alphabet string-ref check" #\o  (string-ref "aiueo" 4))
(assert-equal? "hiragana string-ref check" #\お (string-ref "あいうえお" 4))
(assert-equal? "mixed string-ref check"    #\お (string-ref "あiueお" 4))
(assert-equal? "alphabet string-ref 0 check" #\a  (string-ref "aiueo" 0))
(assert-equal? "hiragena string-ref 0 check" #\あ (string-ref "あいうえお" 0))

;; check string-set!
(assert-true"alphabet string-set! check" (string=? "aikeo"
					       (begin
						 (define str "aiueo")
						 (string-set! str 2 #\k)
						 str)))
(assert-true"hiragana string-set! check" (string=? "あいかえお"
					       (begin
						 (define str "あいうえお")
						 (string-set! str 2 #\か)
						 str)))
(assert-true"mixed string-set! check" (string=? "aiueo"
					    (begin
					      (define str "aiうeo")
					      (string-set! str 2 #\u)
					      str)))

;; immutable strings: See "3.4 Storage model" of R5RS
(assert-error "string-set! on constant string #1"
              (lambda ()
                (string-set! "foo" 0 #\b)))
(assert-error "string-set! on constant string #2"
              (lambda ()
                (string-set! (symbol->string 'foo) 0 #\b)))

;; check string-length
(assert-equal? "alphabet string-length check" 5 (string-length "aiueo"))
(assert-equal? "hiragana string-length check" 5 (string-length "あいうえお"))
(assert-equal? "backslash string-length check" 1 (string-length "\\"))
(assert-equal? "backslash string-length check" 2 (string-length "\\\\"))
(assert-equal? "backslash string-length check" 3 (string-length "\\\\\\"))

;; string=? check
(assert-equal? "alphabet string=? check" #t (string=? "aiueo" "aiueo"))
(assert-equal? "hiragana string=? check" #t (string=? "あいうえお" "あいうえお"))
(assert-equal? "mixed string=? check"    #t (string=? "aいうえo" "aいうえo"))

;; substring check
(assert-true"alphabet substring check" (string=? "iu"   (substring "aiueo" 1 3)))
(assert-true"hiragana substring check" (string=? "いう" (substring "あいうえお" 1 3)))
(assert-true"mixed substring check"    (string=? "いu"  (substring "aいuえo" 1 3)))


;; string-append check
(assert-true"alphabet 1 string-append check" (string=? "a"   (string-append "a")))
(assert-true"alphabet 2 string-append check" (string=? "ai"  (string-append "a" "i")))
(assert-true"alphabet 3 string-append check" (string=? "aiu" (string-append "a" "i" "u")))
(assert-true"hiragana 1 string-append check" (string=? "あ"     (string-append "あ")))
(assert-true"hiragana 2 string-append check" (string=? "あい"   (string-append "あ" "い")))
(assert-true"hiragana 3 string-append check" (string=? "あいう" (string-append "あ" "い" "う")))
(assert-true"mixed 2 string-append check" (string=? "あi"   (string-append "あ" "i")))
(assert-true"mixed 3 string-append check" (string=? "あiう" (string-append "あ" "i" "う")))

;; string->list
(assert-true"string->list check" (equal? '(#\あ #\i #\う #\e #\お) (string->list "あiうeお")))
(assert-equal? "string->list check" '(#\\)             (string->list "\\"))
(assert-equal? "string->list check" '(#\\ #\\)         (string->list "\\\\"))
(assert-equal? "string->list check" '(#\\ #\\ #\\)     (string->list "\\\\\\"))
;;(assert-equal? "string->list check" '(#\tab)           (string->list "\t"))
(assert-equal? "string->list check" '(#\	)      (string->list "\t"))
;;(assert-equal? "string->list check" '(#\return)        (string->list "\r"))
(assert-equal? "string->list check" '(#\)            (string->list "\r"))
(assert-equal? "string->list check" '(#\ #\)       (string->list "\r\r"))
(assert-equal? "string->list check" '(#\newline)           (string->list "\n"))
(assert-equal? "string->list check" '(#\newline #\newline) (string->list "\n\n"))
(assert-equal? "string->list check" '(#\space)         (string->list " "))
(assert-equal? "string->list check" '(#\space #\space) (string->list "  "))
(assert-equal? "string->list check" '(#\")             (string->list "\""))
(assert-equal? "string->list check" '(#\" #\")         (string->list "\"\""))

;; list->string
(assert-equal? "list->string check" "あaい" (list->string '(#\あ #\a #\い)))
(assert-equal? "list->string check" "\\"     (list->string '(#\\)))
(assert-equal? "list->string check" "\\\\"   (list->string '(#\\ #\\)))
(assert-equal? "list->string check" "\\\\\\" (list->string '(#\\ #\\ #\\)))
(assert-equal? "list->string check" "\t" (list->string '(#\	)))
;;(assert-equal? "list->string check" "\t" (list->string '(#\tab)))
(assert-equal? "list->string check" "\r" (list->string '(#\)))
;;(assert-equal? "list->string check" "\r" (list->string '(#\return)))
(assert-equal? "list->string check" "\n" (list->string '(#\
)))
(assert-equal? "list->string check" "\n" (list->string '(#\newline)))
(assert-equal? "list->string check" " " (list->string '(#\ )))
(assert-equal? "list->string check" " " (list->string '(#\space)))
(assert-equal? "list->string check" " " (list->string '(#\ )))
(assert-equal? "list->string check" "\"" (list->string '(#\")))
(assert-equal? "list->string check" "\"a\"" (list->string '(#\" #\a #\")))

;; string-fill!
(assert-true"alphabet string-fill! check" (string=? "jjjjj" (begin
							  (define str "aiueo")
							  (string-fill! str #\j)
							  str)))
(assert-true"hiragana string-fill! check" (string=? "あああああ" (begin
							       (define str "aiueo")
							       (string-fill! str #\あ)
							       str)))
(assert-true"mixed string-fill! by alphabet check" (string=? "aaaaa" (begin
								   (define str "aいうえo")
								   (string-fill! str #\a)
								   str)))
(assert-true"mixed string-fill! by hiragana check" (string=? "いいいいい" (begin
									(define str "aいうえo")
									(string-fill! str #\い)
									str)))

(total-report)
