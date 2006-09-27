#! /usr/bin/env sscm -C UTF-8
;; -*- buffer-file-coding-system: utf-8 -*-

;;  Filename : test-symbol.scm
;;  About    : unit tests for symbols
;;
;;  Copyright (C) 2006 YAMAMOTO Kengo <yamaken AT bp.iij4u.or.jp>
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

;; See also test-syntax.scm for valid identifiers based on "7.1 Formal syntax"
;; of R5RS.

(load "test/unittest.scm")

(define tn test-name)

(tn "symbol comparisons")
(assert-true   (tn) (eq? 'symbol 'symbol))
(assert-true   (tn) (eq? 'SYMBOL 'SYMBOL))
(assert-true   (tn) (eq? 'syMBol 'syMBol))
(if (provided? "sigscheme")
    (begin
      ;; SigScheme distinguishes case of identifiers.
      (assert-false  (tn) (eq? 'symbol 'SYMBOL))
      (assert-false  (tn) (eq? 'symbol 'syMBol))
      (assert-false  (tn) (eq? 'SYMBOL 'syMBol)))
    (begin
      ;; R5RS-conformant implementations normalize the cases.
      (assert-true   (tn) (eq? 'symbol 'SYMBOL))
      (assert-true   (tn) (eq? 'symbol 'syMBol))
      (assert-true   (tn) (eq? 'SYMBOL 'syMBol))))
(assert-true   (tn) (eq? '日本語シンボル '日本語シンボル))
(assert-false  (tn) (eq? '日本語しんぼる '日本語シンボル))
(assert-false  (tn) (eq? 'sym1 'sym2))
(assert-false  (tn) (eq? 'sym 'mys))
(assert-false  (tn) (eq? 'sym 'symb))

(tn "case normalization of literal symbols")
(assert-true   (tn) (eq? 'symbol
                         (string->symbol (symbol->string 'symbol))))
(assert-true   (tn) (eq? 'SYMBOL
                         (string->symbol (symbol->string 'SYMBOL))))
(assert-true   (tn) (eq? 'syMBol
                         (string->symbol (symbol->string 'syMBol))))
(if (provided? "sigscheme")
    (begin
      ;; SigScheme distinguishes case of identifiers.
      (assert-false  (tn) (eq? 'symbol
                               (string->symbol (symbol->string 'SYMBOL))))
      (assert-false  (tn) (eq? 'symbol
                               (string->symbol (symbol->string 'syMBol))))
      (assert-false  (tn) (eq? 'SYMBOL
                               (string->symbol (symbol->string 'symbol))))
      (assert-false  (tn) (eq? 'SYMBOL
                               (string->symbol (symbol->string 'syMBol))))
      (assert-false  (tn) (eq? 'syMBol
                               (string->symbol (symbol->string 'symbol))))
      (assert-false  (tn) (eq? 'syMBol
                               (string->symbol (symbol->string 'SYMBOL)))))
    (begin
      ;; R5RS-conformant implementations normalize the cases.
      (assert-false  (tn) (eq? 'symbol
                               (string->symbol (symbol->string 'SYMBOL))))
      (assert-false  (tn) (eq? 'symbol
                               (string->symbol (symbol->string 'syMBol))))
      (assert-false  (tn) (eq? 'SYMBOL
                               (string->symbol (symbol->string 'symbol))))
      (assert-false  (tn) (eq? 'SYMBOL
                               (string->symbol (symbol->string 'syMBol))))
      (assert-false  (tn) (eq? 'syMBol
                               (string->symbol (symbol->string 'symbol))))
      (assert-false  (tn) (eq? 'syMBol
                               (string->symbol (symbol->string 'SYMBOL))))))

(tn "case preservation of non-literal symbols")
(assert-true   (tn) (string=? "symbol"
                              (symbol->string (string->symbol "symbol"))))
(assert-true   (tn) (string=? "SYMBOL"
                              (symbol->string (string->symbol "SYMBOL"))))
(assert-true   (tn) (string=? "syMBol"
                              (symbol->string (string->symbol "syMBol"))))
(assert-true   (tn) (string=? "Yes, this is a valid symbol."
                              (symbol->string (string->symbol "Yes, this is a valid symbol."))))

;;
;; symbol?
;;

(tn "symbol?")
(assert-false  (tn) (symbol? #f))
(assert-false  (tn) (symbol? #t))
(assert-false  (tn) (symbol? '#f))
(assert-false  (tn) (symbol? '#t))
(assert-false  (tn) (symbol? '()))
(if (provided? "sigscheme")
    (begin
      (assert-false  (tn) (symbol? (eof)))
      (assert-false  (tn) (symbol? (undef)))))
(assert-false  (tn) (symbol? 0))
(assert-false  (tn) (symbol? 1))
(assert-false  (tn) (symbol? 3))
(assert-false  (tn) (symbol? -1))
(assert-false  (tn) (symbol? -3))
(assert-true   (tn) (symbol? 'symbol))
(assert-true   (tn) (symbol? 'SYMBOL))
;; FIXME: provide "utf-8"
(if (and (provided? "sigscheme")
         (provided? "utf-8"))
    (assert-true   (tn) (symbol? (string-read "日本語シンボル"))))
(let ((s 'symbol))
  (assert-true   (tn) (symbol? s)))
(assert-true   (tn) (symbol? (car '(a b))))
(assert-false  (tn) (symbol? #\a))
(assert-false  (tn) (symbol? #\あ))
(assert-false  (tn) (symbol? ""))
(assert-false  (tn) (symbol? " "))
(assert-false  (tn) (symbol? "a"))
(assert-false  (tn) (symbol? "A"))
(assert-false  (tn) (symbol? "aBc12!"))
(assert-false  (tn) (symbol? "あ"))
(assert-false  (tn) (symbol? "あ0イう12!"))
(assert-false  (tn) (symbol? +))
(assert-false  (tn) (symbol? (lambda () #t)))

;; syntactic keywords should not be appeared as operand

(call-with-current-continuation
 (lambda (k)
   (assert-false  (tn) (symbol? k))))
(assert-false  (tn) (symbol? (current-output-port)))
(assert-false  (tn) (symbol? '(#t . #t)))
(assert-false  (tn) (symbol? (cons #t #t)))
(assert-false  (tn) (symbol? '(0 1 2)))
(assert-false  (tn) (symbol? (list 0 1 2)))
(assert-false  (tn) (symbol? '#()))
(assert-false  (tn) (symbol? (vector)))
(assert-false  (tn) (symbol? '#(0 1 2)))
(assert-false  (tn) (symbol? (vector 0 1 2)))


;;
;; symbol->string?
;;

(tn "symbol->string")
(assert-equal? (tn) "symbol" (symbol->string 'symbol))
(if (provided? "sigscheme")
    (begin
      ;; SigScheme distinguishes case of identifiers and so does not conforms
      ;; to R5RS.
      (assert-equal? (tn) "symbol" (symbol->string 'symbol))
      (assert-equal? (tn) "SYMBOL" (symbol->string 'SYMBOL))
      (assert-equal? (tn) "syMBol" (symbol->string 'syMBol)))
    ;; R5RS-conformant implementations normalize the cases of LITERAL symbols
    ;; to implementation-specific ones. Non-literal symbols is not (see
    ;; the string->symbol tests).
    (if (string-equal? "a" (symbol->string "A"))
        (begin
          (assert-equal? (tn) "symbol" (symbol->string 'symbol))
          (assert-equal? (tn) "symbol" (symbol->string 'SYMBOL))
          (assert-equal? (tn) "symbol" (symbol->string 'syMBol)))
        (begin
          (assert-equal? (tn) "SYMBOL" (symbol->string 'symbol))
          (assert-equal? (tn) "SYMBOL" (symbol->string 'SYMBOL))
          (assert-equal? (tn) "SYMBOL" (symbol->string 'syMBol)))))

(tn "symbol->string special initial identifier")
(assert-equal? (tn) "!" (symbol->string '!))
(assert-equal? (tn) "$" (symbol->string '$))
(assert-equal? (tn) "%" (symbol->string '%))
(assert-equal? (tn) "&" (symbol->string '&))
(assert-equal? (tn) "*" (symbol->string '*))
(assert-equal? (tn) "/" (symbol->string '/))
(assert-equal? (tn) ":" (symbol->string ':))
(assert-equal? (tn) "<" (symbol->string '<))
(assert-equal? (tn) "=" (symbol->string '=))
(assert-equal? (tn) ">" (symbol->string '>))
(assert-equal? (tn) "?" (symbol->string '?))
(assert-equal? (tn) "^" (symbol->string '^))
(assert-equal? (tn) "_" (symbol->string '_))
(assert-equal? (tn) "~" (symbol->string '~))

(tn "symbol->string special initial identifier + number")
(assert-equal? (tn) "!1" (symbol->string '!1))
(assert-equal? (tn) "$1" (symbol->string '$1))
(assert-equal? (tn) "%1" (symbol->string '%1))
(assert-equal? (tn) "&1" (symbol->string '&1))
(assert-equal? (tn) "*1" (symbol->string '*1))
(assert-equal? (tn) "/1" (symbol->string '/1))
(assert-equal? (tn) ":1" (symbol->string ':1))
(assert-equal? (tn) "<1" (symbol->string '<1))
(assert-equal? (tn) "=1" (symbol->string '=1))
(assert-equal? (tn) ">1" (symbol->string '>1))
(assert-equal? (tn) "?1" (symbol->string '?1))
(assert-equal? (tn) "^1" (symbol->string '^1))
(assert-equal? (tn) "_1" (symbol->string '_1))
(assert-equal? (tn) "~1" (symbol->string '~1))

(tn "symbol->string special initial identifier + letter")
(assert-equal? (tn) "!a" (symbol->string '!a))
(assert-equal? (tn) "$a" (symbol->string '$a))
(assert-equal? (tn) "%a" (symbol->string '%a))
(assert-equal? (tn) "&a" (symbol->string '&a))
(assert-equal? (tn) "*a" (symbol->string '*a))
(assert-equal? (tn) "/a" (symbol->string '/a))
(assert-equal? (tn) ":a" (symbol->string ':a))
(assert-equal? (tn) "<a" (symbol->string '<a))
(assert-equal? (tn) "=a" (symbol->string '=a))
(assert-equal? (tn) ">a" (symbol->string '>a))
(assert-equal? (tn) "?a" (symbol->string '?a))
(assert-equal? (tn) "^a" (symbol->string '^a))
(assert-equal? (tn) "_a" (symbol->string '_a))
(assert-equal? (tn) "~a" (symbol->string '~a))

(tn "symbol->string identifiers")
(assert-equal? (tn) "..." (symbol->string '...))
(assert-equal? (tn) "+"   (symbol->string '+))
(assert-equal? (tn) "-"   (symbol->string '-))
(assert-equal? (tn) "a."  (symbol->string 'a.))
(assert-equal? (tn) "a+"  (symbol->string 'a+))
(assert-equal? (tn) "a-"  (symbol->string 'a-))
(assert-equal? (tn) "a@"  (symbol->string 'a@))
(assert-equal? (tn) "a1"  (symbol->string 'a1))
;; SigScheme allows initial hyphen by default.
(if (and (provided? "sigscheme")
         (not (provided? "strict-r5rs")))
    (assert-equal? (tn) "-a" (symbol->string '-a)))

;;
;; string->symbol
;;

(tn "string->symbol")
(if (provided? "sigscheme")
    (begin
      (assert-true   (tn) (eq? 'symbol (string->symbol "symbol")))
      (assert-true   (tn) (eq? 'SYMBOL (string->symbol "SYMBOL")))
      (assert-true   (tn) (eq? 'syMBol (string->symbol "syMBol"))))
    (begin
      (if (string-equal? "a" (symbol->string "A"))
          (begin
            (assert-true   (tn) (eq? 'symbol (string->symbol "symbol")))
            (assert-false  (tn) (eq? 'SYMBOL (string->symbol "SYMBOL"))))
          (begin
            (assert-false  (tn) (eq? 'symbol (string->symbol "symbol")))
            (assert-true   (tn) (eq? 'SYMBOL (string->symbol "SYMBOL")))))
      ;; Literal symbols (e.g. 'syMBol) is normalized in R5RS-conformant
      ;; implementations, but symbols that created by string->symbol is not
      ;; (i.e. returns 'syMBol).
      (assert-false  (tn) (eq? 'syMBol (string->symbol "syMBol")))))
(assert-true   (tn) (eq? '日本語シンボル (string->symbol "日本語シンボル")))

(tn "string->symbol non-literal symbols")
(assert-true   (tn) (symbol? (string->symbol "")))
(assert-true   (tn) (symbol? (string->symbol "Yes, this is a valid symbol.")))

(assert-true   (tn) (symbol? (string->symbol "#f")))
(assert-true   (tn) (symbol? (string->symbol "#t")))
(assert-true   (tn) (symbol? (string->symbol "()")))
(assert-true   (tn) (symbol? (string->symbol "0")))
(assert-true   (tn) (symbol? (string->symbol "1")))
(assert-true   (tn) (symbol? (string->symbol "3")))
(assert-true   (tn) (symbol? (string->symbol "-1")))
(assert-true   (tn) (symbol? (string->symbol "-3")))
(assert-true   (tn) (symbol? (string->symbol "#\\a")))
(assert-true   (tn) (symbol? (string->symbol "#\\あ")))
(assert-true   (tn) (symbol? (string->symbol "\"\"")))
(assert-true   (tn) (symbol? (string->symbol "\" \"")))
(assert-true   (tn) (symbol? (string->symbol "\"a\"")))
(assert-true   (tn) (symbol? (string->symbol "\"A\"")))
(assert-true   (tn) (symbol? (string->symbol "\"aBc12!\"")))
(assert-true   (tn) (symbol? (string->symbol "\"あ\"")))
(assert-true   (tn) (symbol? (string->symbol "\"あ0イう12!\"")))
(assert-true   (tn) (symbol? (string->symbol "+")))
(assert-true   (tn) (symbol? (string->symbol "(lambda () #t)")))
(assert-true   (tn) (symbol? (string->symbol "(#t . #t)")))
(assert-true   (tn) (symbol? (string->symbol "(0 1 2)")))
(assert-true   (tn) (symbol? (string->symbol "#()")))
(assert-true   (tn) (symbol? (string->symbol "#(0 1 2)")))

(assert-true   (tn) (symbol? (string->symbol "#")))
(assert-true   (tn) (symbol? (string->symbol ".")))
(assert-true   (tn) (symbol? (string->symbol "..")))
(assert-true   (tn) (symbol? (string->symbol "....")))
(assert-true   (tn) (symbol? (string->symbol ".a")))
(assert-true   (tn) (symbol? (string->symbol "+a")))
(assert-true   (tn) (symbol? (string->symbol "++")))
(assert-true   (tn) (symbol? (string->symbol "--")))
(assert-true   (tn) (symbol? (string->symbol "-=")))
(assert-true   (tn) (symbol? (string->symbol "-$")))
(assert-true   (tn) (symbol? (string->symbol "-.")))
(assert-true   (tn) (symbol? (string->symbol "-@")))
(assert-true   (tn) (symbol? (string->symbol "@")))
(assert-true   (tn) (symbol? (string->symbol "1a")))

(assert-true   (tn) (symbol? (string->symbol ";")))
(assert-true   (tn) (symbol? (string->symbol "'")))
(assert-true   (tn) (symbol? (string->symbol "`")))
(assert-true   (tn) (symbol? (string->symbol ",")))
(assert-true   (tn) (symbol? (string->symbol ",@")))
(assert-true   (tn) (symbol? (string->symbol "\"")))
(assert-true   (tn) (symbol? (string->symbol "\\")))
(assert-true   (tn) (symbol? (string->symbol "(")))
(assert-true   (tn) (symbol? (string->symbol ")")))
(assert-true   (tn) (symbol? (string->symbol "[")))
(assert-true   (tn) (symbol? (string->symbol "]")))
(assert-true   (tn) (symbol? (string->symbol "{")))
(assert-true   (tn) (symbol? (string->symbol "}")))
(assert-true   (tn) (symbol? (string->symbol "|")))


(total-report)
