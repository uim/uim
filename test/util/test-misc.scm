;;; Copyright (c) 2003-2013 uim Project https://github.com/uim/uim
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
;;;

;; These tests are passed at revision 6605 (new repository)

(define-module test.util.test-misc
  (use test.unit.test-case)
  (use test.uim-test))
(select-module test.util.test-misc)

(define (setup)
  (uim-test-setup))

(define (teardown)
  (uim-test-teardown))

(define (test-gauche-supported-escape-sequences)
  "R6RS(SRFI-75) and C89 compliant escape sequences of Gauche"
  (assert-not-equal "t" "\t")  ;; #\tab
  (assert-not-equal "n" "\n")  ;; #\linefeed
  (assert-not-equal "f" "\f")  ;; #\page
  (assert-not-equal "r" "\r")  ;; #\return
  #f)

(define (test-gauche-unsupported-escape-sequences)
  "This test will fail after Gauche supports all of "
  "R6RS(SRFI-75) and C89 compliant escape sequences"
  (assert-equal "a" "\a")  ;; #\alarm
  (assert-equal "b" "\b")  ;; #\backspace
  (assert-equal "v" "\v")  ;; #\vtab
  #f)

(define (test-string-escape)
  ;; empty string
  (assert-uim-equal "\"\""
                    '(string-escape ""))
  ;; single character
  ;; R5RS
  (assert-uim-equal "\"\\\"\""
                    '(string-escape "\""))
  (assert-uim-equal "\"\\\\\""
                    '(string-escape "\\"))

  ;; R6RS(SRFI-75) and C89 (uim-sh)
  (assert-uim-equal "\"\\t\""
                    '(string-escape "\t")) ;; #\tab
  (assert-uim-equal "\"\\n\""
                    '(string-escape "\n")) ;; #\linefeed
  (assert-uim-equal "\"\\f\""
                    '(string-escape "\f")) ;; #\page
  (assert-uim-equal "\"\\r\""
                    '(string-escape "\r")) ;; #\return

  ;; R6RS(SRFI-75) and C89 (uim-sh), but Gauche isn't
  ;; supported escape sequences. We can replace uim-raw with
  ;; uim after "test-gauche-unsupported-escape-sequences"
  ;; test fails which means Gauche supports them.
  (assert-uim-equal-raw "\"\\a\""
                        "(string-escape \"\\a\")")  ;; #\alarm
  (assert-uim-equal-raw "\"\\b\""
                        "(string-escape \"\\b\")")  ;; #\backspace
  (assert-uim-equal-raw "\"\\v\""
                        "(string-escape \"\\v\")")  ;; #\vtab

  ;; R5RS
  (assert-uim-equal "\"a\""
                    '(string-escape "a"))
  (assert-uim-equal "\"b\""
                    '(string-escape "b"))
  (assert-uim-equal "\"c\""
                    '(string-escape "c"))
  (assert-uim-equal "\"a\""
                    '(string-escape "\a"))
  (assert-uim-equal "\"b\""
                    '(string-escape "\b"))
  (assert-uim-equal "\"c\""
                    '(string-escape "\c"))
  (assert-uim-equal "\"A\""
                    '(string-escape "A"))
  (assert-uim-equal "\"B\""
                    '(string-escape "B"))
  (assert-uim-equal "\"C\""
                    '(string-escape "C"))
  (assert-uim-equal "\"A\""
                    '(string-escape "\A"))
  (assert-uim-equal "\"B\""
                    '(string-escape "\B"))
  (assert-uim-equal "\"C\""
                    '(string-escape "\C"))
  ;; 2 characters
  (assert-uim-equal "\"\\\"\\\"\""
                    '(string-escape "\"\""))
  (assert-uim-equal "\"\\\\\\\"\""
                    '(string-escape "\\\""))
  (assert-uim-equal "\"\\\\\\\\\""
                    '(string-escape "\\\\"))
  (assert-uim-equal "\"\\r\\n\""
                    '(string-escape "\r\n"))
  (assert-uim-equal "\"aB\""
                    '(string-escape "aB"))
  (assert-uim-equal "\"aB\""
                    '(string-escape "a\B"))
  (assert-uim-equal "\"aB\""
                    '(string-escape "\a\B"))
  ;; complex
  (assert-uim-equal "\"\\\"a string\\\" in two-line\\nstring\\n\""
                    '(string-escape "\"a string\" in two-line\nstring\n"))
  #f)

(define (test-compose)
  (uim '(define test-list '(0 1 2 3 4 5)))
  (assert-uim-true  '(procedure? (compose)))
  (assert-uim-true  '(procedure? (compose car)))
  (assert-uim-true  '(procedure? (compose car cdr)))
  (assert-uim-true  '(procedure? (compose car cdr list)))
  (assert-uim-equal '(0 1 2 3 4 5)
                    '((compose) test-list))
  (assert-uim-equal 0
                    '((compose car) test-list))
  (assert-uim-equal 1
                    '((compose car cdr) test-list))
  (assert-uim-equal 2
                    '((compose car cdr cdr) test-list))
  (assert-uim-equal 4
                    '((compose car cdr reverse) test-list))
  (assert-uim-equal 3
                    '((compose car cdr cdr reverse) test-list))
  #f)

(define (test-safe-car)
  (assert-uim-equal 1
                    '(safe-car '(1 2)))
  (assert-uim-equal 1
                    '(safe-car '(1 . 2)))
  (assert-uim-false '(safe-car '()))
  (assert-uim-false '(safe-car 1))
  #f)

(define (test-safe-cdr)
  (assert-uim-equal '(2)
                    '(safe-cdr '(1 2)))
  (assert-uim-equal 2
                    '(safe-cdr '(1 . 2)))
  (assert-uim-false '(safe-cdr '()))
  (assert-uim-false '(safe-cdr 1))
  #f)

(define (test-assq-cdr)
  (assert-uim-equal '(2)
                    '(assq-cdr 1 '((1 2))))
  (assert-uim-equal 2
                    '(assq-cdr 1 '((1 . 2))))
  (assert-uim-false '(assq-cdr 2 '((1 2))))
  (assert-uim-false '(assq-cdr 2 '((1 . 2))))
  (assert-uim-equal '(2)
                    '(assq-cdr 1 '((3 4) (1 2))))
  (assert-uim-equal 2
                    '(assq-cdr 1 '((3 . 4) (1 . 2))))
  (assert-uim-equal '(4)
                    '(assq-cdr 3 '((3 4) (1 2))))
  (assert-uim-equal 4
                    '(assq-cdr 3 '((3 . 4) (1 . 2))))
  (assert-uim-false '(assq-cdr 1 '()))
  (assert-uim-error '(assq-cdr 1 1))
  #f)

(define (test-clamp)
  (assert-uim-equal 0  '(clamp -2 0 -1))
  (assert-uim-equal 0  '(clamp -1 0 -1))
  (assert-uim-equal 0  '(clamp 0  0 -1))
  (assert-uim-equal 0  '(clamp 1  0 -1))
  (assert-uim-equal 0  '(clamp 2  0 -1))
  (assert-uim-equal 0  '(clamp 10 0 -1))

  (assert-uim-equal -2 '(clamp -2 -2 0))
  (assert-uim-equal -1 '(clamp -1 -2 0))
  (assert-uim-equal 0  '(clamp 0  -2 0))
  (assert-uim-equal 0  '(clamp 1  -2 0))
  (assert-uim-equal 0  '(clamp 2  -2 0))
  (assert-uim-equal 0  '(clamp 10 -2 0))

  (assert-uim-equal -1 '(clamp -2 -1 0))
  (assert-uim-equal -1 '(clamp -1 -1 0))
  (assert-uim-equal 0  '(clamp 0  -1 0))
  (assert-uim-equal 0  '(clamp 1  -1 0))
  (assert-uim-equal 0  '(clamp 2  -1 0))
  (assert-uim-equal 0  '(clamp 10 -1 0))

  (assert-uim-equal 0  '(clamp -2 0 0))
  (assert-uim-equal 0  '(clamp -1 0 0))
  (assert-uim-equal 0  '(clamp 0  0 0))
  (assert-uim-equal 0  '(clamp 1  0 0))
  (assert-uim-equal 0  '(clamp 2  0 0))
  (assert-uim-equal 0  '(clamp 10 0 0))

  (assert-uim-equal 0  '(clamp -2 0 1))
  (assert-uim-equal 0  '(clamp -1 0 1))
  (assert-uim-equal 0  '(clamp 0  0 1))
  (assert-uim-equal 1  '(clamp 1  0 1))
  (assert-uim-equal 1  '(clamp 2  0 1))
  (assert-uim-equal 1  '(clamp 10 0 1))

  (assert-uim-equal 0  '(clamp -2 0 2))
  (assert-uim-equal 0  '(clamp -1 0 2))
  (assert-uim-equal 0  '(clamp 0  0 2))
  (assert-uim-equal 1  '(clamp 1  0 2))
  (assert-uim-equal 2  '(clamp 2  0 2))
  (assert-uim-equal 2  '(clamp 10 0 2))

  (assert-uim-equal 0  '(clamp -2 0 3))
  (assert-uim-equal 0  '(clamp -1 0 3))
  (assert-uim-equal 0  '(clamp 0  0 3))
  (assert-uim-equal 1  '(clamp 1  0 3))
  (assert-uim-equal 2  '(clamp 2  0 3))
  (assert-uim-equal 3  '(clamp 10 0 3))

  (assert-uim-equal 1  '(clamp -2 1 3))
  (assert-uim-equal 1  '(clamp -1 1 3))
  (assert-uim-equal 1  '(clamp 0  1 3))
  (assert-uim-equal 1  '(clamp 1  1 3))
  (assert-uim-equal 2  '(clamp 2  1 3))
  (assert-uim-equal 3  '(clamp 10 1 3))

  (assert-uim-equal -1 '(clamp -2 -1 3))
  (assert-uim-equal -1 '(clamp -1 -1 3))
  (assert-uim-equal 0  '(clamp 0  -1 3))
  (assert-uim-equal 1  '(clamp 1  -1 3))
  (assert-uim-equal 2  '(clamp 2  -1 3))
  (assert-uim-equal 3  '(clamp 10 -1 3))

  (assert-uim-equal -2 '(clamp -2 -5 5))
  (assert-uim-equal -1 '(clamp -1 -5 5))
  (assert-uim-equal 0  '(clamp 0  -5 5))
  (assert-uim-equal 1  '(clamp 1  -5 5))
  (assert-uim-equal 2  '(clamp 2  -5 5))
  (assert-uim-equal 5  '(clamp 10 -5 5))
  #f)

(define (test-try-load)
  (assert-uim-true-value '(try-load "anthy.scm"))
  (assert-uim-false '(try-load "nonexistent.scm"))
  #f)

(define (test-try-require)
  (assert-uim-true-value '(try-require "anthy.scm"))
  (assert-uim-false '(try-require "nonexistent.scm"))
  #f)

(provide "test/util/test-misc")
