#! /usr/bin/env sscm -C UTF-8
;; -*- buffer-file-coding-system: utf-8 -*-

;;  FileName : test-syntax.scm
;;  About    : unit test for R5RS syntax
;;
;;  Copyright (C) 2005-2006 YamaKen <yamaken AT bp.iij4u.or.jp>
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

;; All tests in this file are passed against r2302 (new repository)

;; See "7.1 Formal syntax" of R5RS

(assert-parse-error "invalid boolean" "#F")
(assert-parse-error "invalid boolean" "#T")

(assert-true "boolean" (boolean? (string-read "#f")))
(assert-true "boolean" (boolean? (string-read "#t")))

(assert-parse-error "invalid identifier" "#")
(assert-parse-error "invalid identifier" ".")
(assert-parse-error "invalid identifier" "..")
(assert-parse-error "invalid identifier" "....")
(assert-parse-error "invalid identifier" ".a")
(assert-parse-error "invalid identifier" "+a")
(assert-parse-error "invalid identifier" "++")
(assert-parse-error "invalid identifier" "--")
(assert-parse-error "invalid identifier" "-=")
(assert-parse-error "invalid identifier" "-$")
(assert-parse-error "invalid identifier" "-.")
(assert-parse-error "invalid identifier" "-@")
(assert-parse-error "invalid identifier" "@")
(assert-parse-error "invalid identifier" "1a")

(assert-true "special initial identifier" (symbol? (string-read "!")))
(assert-true "special initial identifier" (symbol? (string-read "$")))
(assert-true "special initial identifier" (symbol? (string-read "%")))
(assert-true "special initial identifier" (symbol? (string-read "&")))
(assert-true "special initial identifier" (symbol? (string-read "*")))
(assert-true "special initial identifier" (symbol? (string-read "/")))
(assert-true "special initial identifier" (symbol? (string-read ":")))
(assert-true "special initial identifier" (symbol? (string-read "<")))
(assert-true "special initial identifier" (symbol? (string-read "=")))
(assert-true "special initial identifier" (symbol? (string-read ">")))
(assert-true "special initial identifier" (symbol? (string-read "?")))
(assert-true "special initial identifier" (symbol? (string-read "^")))
(assert-true "special initial identifier" (symbol? (string-read "_")))
(assert-true "special initial identifier" (symbol? (string-read "~")))

(assert-true "special initial identifier #2" (symbol? (string-read "!1")))
(assert-true "special initial identifier #2" (symbol? (string-read "$1")))
(assert-true "special initial identifier #2" (symbol? (string-read "%1")))
(assert-true "special initial identifier #2" (symbol? (string-read "&1")))
(assert-true "special initial identifier #2" (symbol? (string-read "*1")))
(assert-true "special initial identifier #2" (symbol? (string-read "/1")))
(assert-true "special initial identifier #2" (symbol? (string-read ":1")))
(assert-true "special initial identifier #2" (symbol? (string-read "<1")))
(assert-true "special initial identifier #2" (symbol? (string-read "=1")))
(assert-true "special initial identifier #2" (symbol? (string-read ">1")))
(assert-true "special initial identifier #2" (symbol? (string-read "?1")))
(assert-true "special initial identifier #2" (symbol? (string-read "^1")))
(assert-true "special initial identifier #2" (symbol? (string-read "_1")))
(assert-true "special initial identifier #2" (symbol? (string-read "~1")))

(assert-true "special initial identifier #2" (symbol? (string-read "!a")))
(assert-true "special initial identifier #2" (symbol? (string-read "$a")))
(assert-true "special initial identifier #2" (symbol? (string-read "%a")))
(assert-true "special initial identifier #2" (symbol? (string-read "&a")))
(assert-true "special initial identifier #2" (symbol? (string-read "*a")))
(assert-true "special initial identifier #2" (symbol? (string-read "/a")))
(assert-true "special initial identifier #2" (symbol? (string-read ":a")))
(assert-true "special initial identifier #2" (symbol? (string-read "<a")))
(assert-true "special initial identifier #2" (symbol? (string-read "=a")))
(assert-true "special initial identifier #2" (symbol? (string-read ">a")))
(assert-true "special initial identifier #2" (symbol? (string-read "?a")))
(assert-true "special initial identifier #2" (symbol? (string-read "^a")))
(assert-true "special initial identifier #2" (symbol? (string-read "_a")))
(assert-true "special initial identifier #2" (symbol? (string-read "~a")))

(assert-true "identifier" (symbol? (string-read "...")))
(assert-true "identifier" (symbol? (string-read "+")))
(assert-true "identifier" (symbol? (string-read "-")))
(assert-true "identifier" (symbol? (string-read "a.")))
(assert-true "identifier" (symbol? (string-read "a+")))
(assert-true "identifier" (symbol? (string-read "a-")))
(assert-true "identifier" (symbol? (string-read "a@")))
(assert-true "identifier" (symbol? (string-read "a1")))
(if (and (provided? "sigscheme")
         (not (provided? "strict-r5rs")))
    (assert-true "identifier" (symbol? (string-read "-a"))))

(assert-true "integer" (integer? (string-read "1")))
(assert-true "integer" (integer? (string-read "1")))
(assert-true "integer" (integer? (string-read "+1")))
(assert-true "integer" (integer? (string-read "-1")))
(assert-true "integer" (integer? (string-eval "'+1")))
(assert-true "integer" (integer? (string-eval "'-1")))
(assert-true "integer" (integer? (string-read "#d1")))
(assert-true "integer" (integer? (string-read "#d1")))
(assert-true "integer" (integer? (string-read "#d+1")))
(assert-true "integer" (integer? (string-read "#d-1")))
(assert-true "integer" (integer? (string-eval "'#d+1")))
(assert-true "integer" (integer? (string-eval "'#d-1")))

(assert-parse-error "invalid dot pair" "( . )")
(assert-parse-error "invalid dot pair" "( . \"foo\")")
(assert-parse-error "invalid dot pair" "( . \"foo\" \"bar\")")
(assert-parse-error "invalid dot pair" "(\"foo\" . )")
(assert-parse-error "invalid dot pair" "(\"foo\" \"bar\" . )")
(assert-parse-error "invalid dot pair" "(\"foo\" . \"bar\" \"baz\")")
(assert-parse-error "invalid dot pair" "(\"foo\" \"bar\" . \"baz\" \"quux\")")

(assert-parse-error "invalid dot pair without left space" "(. )")
(assert-parse-error "invalid dot pair without left space" "(. \"foo\")")
(assert-parse-error "invalid dot pair without left space" "(. \"foo\" \"bar\")")
(assert-parse-error "invalid dot pair without left space" "(\"foo\". )")
(assert-parse-error "invalid dot pair without left space" "(\"foo\" \"bar\". )")
(assert-parse-error "invalid dot pair without left space" "(\"foo\". \"bar\" \"baz\")")
(assert-parse-error "invalid dot pair without left space" "(\"foo\" \"bar\". \"baz\" \"quux\")")

(assert-parseable "dot pair" "(\"foo\" . \"bar\")")
(assert-parseable "dot pair" "(\"foo\" \"bar\" . \"baz\")")

(assert-parseable "dot pair without left space" "(\"foo\". \"bar\")")
(assert-parseable "dot pair without left space" "(\"foo\" \"bar\". \"baz\")")

(let ((assert (if (and (provided? "sigscheme")
                       (not (provided? "strict-r5rs")))
                  assert-parse-error
                  assert-parseable)))
  (assert "invalid dot pair without right space" "( .)")
  (assert "invalid dot pair without right space" "( .\"foo\")")
  (assert "invalid dot pair without right space" "( .\"foo\" \"bar\")")
  (assert "invalid dot pair without right space" "(\"foo\" .)")
  (assert "invalid dot pair without right space" "(\"foo\" \"bar\" .)")
  (assert "invalid dot pair without right space" "(\"foo\" .\"bar\" \"baz\")")
  (assert "invalid dot pair without right space" "(\"foo\" \"bar\" .\"baz\" \"quux\")")

  (assert "invalid dot pair without both space" "(.)")
  (assert "invalid dot pair without both space" "(.\"foo\")")
  (assert "invalid dot pair without both space" "(.\"foo\" \"bar\")")
  (assert "invalid dot pair without both space" "(\"foo\".)")
  (assert "invalid dot pair without both space" "(\"foo\" \"bar\".)")
  (assert "invalid dot pair without both space" "(\"foo\".\"bar\" \"baz\")")
  (assert "invalid dot pair without both space" "(\"foo\" \"bar\".\"baz\" \"quux\")")

  (assert "dot pair without right space" "(\"foo\" .\"bar\")")
  (assert "dot pair without right space" "(\"foo\" \"bar\" .\"baz\")")

  (assert "dot pair without both space" "(\"foo\".\"bar\")")
  (assert "dot pair without both space" "(\"foo\" \"bar\".\"baz\")"))

(tn "function calling fixed_0")
(define f (lambda () #t))
(assert-equal? (tn) #t         (f))
(assert-error  (tn) (lambda () (f . #t)))
(assert-error  (tn) (lambda () (f #t)))
(assert-error  (tn) (lambda () (f #t . #t)))
(assert-error  (tn) (lambda () (f #t #t)))
(assert-error  (tn) (lambda () (f #t #t . #t)))
(assert-error  (tn) (lambda () (f #t #t #t)))
(assert-error  (tn) (lambda () (f #t #t #t . #t)))
(tn "function calling variadic_0")
(define f (lambda args args))
(assert-equal? (tn) '()         (f))
(assert-error  (tn) (lambda ()  (f . #t)))
(assert-equal? (tn) '(#t)       (f #t))
(assert-error  (tn) (lambda ()  (f #t . #t)))
(assert-equal? (tn) '(#t #t)    (f #t #t))
(assert-error  (tn) (lambda ()  (f #t #t . #t)))
(assert-equal? (tn) '(#t #t #t) (f #t #t #t))
(assert-error  (tn) (lambda ()  (f #t #t #t . #t)))
(tn "function calling fixed_1")
(define f (lambda (x) x))
(assert-error  (tn) (lambda () (f)))
(assert-error  (tn) (lambda () (f . #t)))
(assert-equal? (tn) #t         (f #t))
(assert-error  (tn) (lambda () (f #t . #t)))
(assert-error  (tn) (lambda () (f #t #t)))
(assert-error  (tn) (lambda () (f #t #t . #t)))
(assert-error  (tn) (lambda () (f #t #t #t)))
(assert-error  (tn) (lambda () (f #t #t #t . #t)))
(tn "function calling variadic_1")
(define f (lambda (x . rest) (list x rest)))
(assert-error  (tn) (lambda ()    (f)))
(assert-error  (tn) (lambda ()    (f . #t)))
(assert-equal? (tn) '(#t ())      (f #t))
(assert-error  (tn) (lambda ()    (f #t . #t)))
(assert-equal? (tn) '(#t (#t))    (f #t #t))
(assert-error  (tn) (lambda ()    (f #t #t . #t)))
(assert-equal? (tn) '(#t (#t #t)) (f #t #t #t))
(assert-error  (tn) (lambda ()    (f #t #t #t . #t)))
(tn "function calling fixed_2")
(define f (lambda (x y) (list x y)))
(assert-error  (tn) (lambda ()    (f)))
(assert-error  (tn) (lambda ()    (f . #t)))
(assert-error  (tn) (lambda ()    (f #t)))
(assert-error  (tn) (lambda ()    (f #t . #t)))
(assert-equal? (tn) '(#t #t)      (f #t #t))
(assert-error  (tn) (lambda ()    (f #t #t . #t)))
(assert-error  (tn) (lambda ()    (f #t #t #t)))
(assert-error  (tn) (lambda ()    (f #t #t #t . #t)))
(tn "function calling variadic_2")
(define f (lambda (x y . rest) (list x y rest)))
(assert-error  (tn) (lambda ()    (f)))
(assert-error  (tn) (lambda ()    (f . #t)))
(assert-error  (tn) (lambda ()    (f #t)))
(assert-error  (tn) (lambda ()    (f #t . #t)))
(assert-equal? (tn) '(#t #t ())   (f #t #t))
(assert-error  (tn) (lambda ()    (f #t #t . #t)))
(assert-equal? (tn) '(#t #t (#t)) (f #t #t #t))
(assert-error  (tn) (lambda ()    (f #t #t #t . #t)))

;; Although SigScheme's eval facility itself does not ensure properness of
;; syntax args, each syntax implementation must check it. These tests only
;; indicate what should be done.
(tn "syntax application fixed_0")
(define s (lambda () #t))  ;; FIXME: no syntax with syntax_fixed_0
(assert-equal? (tn) #t         (s))
(assert-error  (tn) (lambda () (s . #t)))
(assert-error  (tn) (lambda () (s #t)))
(assert-error  (tn) (lambda () (s #t . #t)))
(assert-error  (tn) (lambda () (s #t #t)))
(assert-error  (tn) (lambda () (s #t #t . #t)))
(assert-error  (tn) (lambda () (s #t #t #t)))
(assert-error  (tn) (lambda () (s #t #t #t . #t)))
(tn "syntax application variadic_0")
(define s and)
(assert-equal? (tn) #t          (s))
(assert-error  (tn) (lambda ()  (s . #t)))
(assert-equal? (tn) #t          (s #t))
(assert-error  (tn) (lambda ()  (s #t . #t)))
(assert-equal? (tn) #t          (s #t #t))
(assert-error  (tn) (lambda ()  (s #t #t . #t)))
(assert-equal? (tn) #t          (s #t #t #t))
(assert-error  (tn) (lambda ()  (s #t #t #t . #t)))
(tn "syntax application fixed_1")
(define s quote)
(assert-error  (tn) (lambda () (s)))
(assert-error  (tn) (lambda () (s . #t)))
(assert-equal? (tn) #t         (s #t))
(assert-error  (tn) (lambda () (s #t . #t)))
(assert-error  (tn) (lambda () (s #t #t)))
(assert-error  (tn) (lambda () (s #t #t . #t)))
(assert-error  (tn) (lambda () (s #t #t #t)))
(assert-error  (tn) (lambda () (s #t #t #t . #t)))
(tn "syntax application variadic_1")
(define s let*)
(assert-error  (tn) (lambda ()    (s)))
(assert-error  (tn) (lambda ()    (s . #t)))
(assert-equal? (tn) (undef)       (s ()))
(assert-error  (tn) (lambda ()    (s #t . #t)))
(assert-equal? (tn) #t            (s () #t))
(assert-error  (tn) (lambda ()    (s #t #t . #t)))
(assert-equal? (tn) #t            (s () #t #t))
(assert-error  (tn) (lambda ()    (s #t #t #t . #t)))
(tn "syntax application fixed_2")
(define s set!)
(define foo #f)
(assert-error  (tn) (lambda ()    (s)))
(assert-error  (tn) (lambda ()    (s . #t)))
(assert-error  (tn) (lambda ()    (s #t)))
(assert-error  (tn) (lambda ()    (s #t . #t)))
(if (and (provided? "sigscheme")
         (provided? "strict-r5rs"))
    (assert-equal? (tn) (undef)   (s foo #t))
    (assert-equal? (tn) #t        (s foo #t)))
(assert-error  (tn) (lambda ()    (s #t #t . #t)))
(assert-error  (tn) (lambda ()    (s #t #t #t)))
(assert-error  (tn) (lambda ()    (s #t #t #t . #t)))
(tn "syntax application variadic_2")
(define s if)
(assert-error  (tn) (lambda ()    (s)))
(assert-error  (tn) (lambda ()    (s . #t)))
(assert-error  (tn) (lambda ()    (s #t)))
(assert-error  (tn) (lambda ()    (s #t . #t)))
(assert-equal? (tn) #t            (s #t #t))
(assert-error  (tn) (lambda ()    (s #t #t . #t)))
(assert-equal? (tn) #t            (s #t #t #t))
(assert-error  (tn) (lambda ()    (s #t #t #t . #t)))

(total-report)
