;;; Copyright (c) 2003-2008 uim Project http://code.google.com/p/uim/
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

(define-module test.util.test-misc
  (use test.unit.test-case)
  (use test.uim-test-utils-new))
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
  (assert-equal "\"\""
                (uim '(string-escape "")))
  ;; single character
  ;; R5RS
  (assert-equal "\"\\\"\""
                (uim '(string-escape "\"")))
  (assert-equal "\"\\\\\""
                (uim '(string-escape "\\")))

  ;; R6RS(SRFI-75) and C89 (uim-sh)
  (assert-equal "\"\\t\""
                (uim '(string-escape "\t"))) ;; #\tab
  (assert-equal "\"\\n\""
                (uim '(string-escape "\n"))) ;; #\linefeed
  (assert-equal "\"\\f\""
                (uim '(string-escape "\f"))) ;; #\page
  (assert-equal "\"\\r\""
                (uim '(string-escape "\r"))) ;; #\return

  ;; R6RS(SRFI-75) and C89 (uim-sh), but Gauche isn't
  ;; supported escape sequences. We can replace uim-raw with
  ;; uim after "test-gauche-unsupported-escape-sequences"
  ;; test fails which means Gauche supports them.
  (assert-equal "\"\\a\""
                (uim-raw "(string-escape \"\\a\")"))  ;; #\alarm
  (assert-equal "\"\\b\""
                (uim-raw "(string-escape \"\\b\")"))  ;; #\backspace
  (assert-equal "\"\\v\""
                (uim-raw "(string-escape \"\\v\")"))  ;; #\vtab

  ;; R5RS
  (assert-equal "\"a\""
                (uim '(string-escape "a")))
  (assert-equal "\"b\""
                (uim '(string-escape "b")))
  (assert-equal "\"c\""
                (uim '(string-escape "c")))
  (assert-equal "\"a\""
                (uim '(string-escape "\a")))
  (assert-equal "\"b\""
                (uim '(string-escape "\b")))
  (assert-equal "\"c\""
                (uim '(string-escape "\c")))
  (assert-equal "\"A\""
                (uim '(string-escape "A")))
  (assert-equal "\"B\""
                (uim '(string-escape "B")))
  (assert-equal "\"C\""
                (uim '(string-escape "C")))
  (assert-equal "\"A\""
                (uim '(string-escape "\A")))
  (assert-equal "\"B\""
                (uim '(string-escape "\B")))
  (assert-equal "\"C\""
                (uim '(string-escape "\C")))
  ;; 2 characters
  (assert-equal "\"\\\"\\\"\""
                (uim '(string-escape "\"\"")))
  (assert-equal "\"\\\\\\\"\""
                (uim '(string-escape "\\\"")))
  (assert-equal "\"\\\\\\\\\""
                (uim '(string-escape "\\\\")))
  (assert-equal "\"\\r\\n\""
                (uim '(string-escape "\r\n")))
  (assert-equal "\"aB\""
                (uim '(string-escape "aB")))
  (assert-equal "\"aB\""
                (uim '(string-escape "a\B")))
  (assert-equal "\"aB\""
                (uim '(string-escape "\a\B")))
  ;; complex
  (assert-equal "\"\\\"a string\\\" in two-line\\nstring\\n\""
                (uim '(string-escape "\"a string\" in two-line\nstring\n")))
  #f)

(define (test-compose)
  (uim '(define test-list '(0 1 2 3 4 5)))
  (assert-true  (uim-bool '(procedure? (compose))))
  (assert-true  (uim-bool '(procedure? (compose car))))
  (assert-true  (uim-bool '(procedure? (compose car cdr))))
  (assert-true  (uim-bool '(procedure? (compose car cdr list))))
  (assert-equal '(0 1 2 3 4 5)
                (uim '((compose) test-list)))
  (assert-equal 0
                (uim '((compose car) test-list)))
  (assert-equal 1
                (uim '((compose car cdr) test-list)))
  (assert-equal 2
                (uim '((compose car cdr cdr) test-list)))
  (assert-equal 4
                (uim '((compose car cdr reverse) test-list)))
  (assert-equal 3
                (uim '((compose car cdr cdr reverse) test-list)))
  #f)

(define (test-safe-car)
  (assert-equal 1
                (uim '(safe-car '(1 2))))
  (assert-equal 1
                (uim '(safe-car '(1 . 2))))
  (assert-false (uim '(safe-car '())))
  (assert-false (uim '(safe-car 1)))
  #f)

(define (test-safe-cdr)
  (assert-equal '(2)
                (uim '(safe-cdr '(1 2))))
  (assert-equal 2
                (uim '(safe-cdr '(1 . 2))))
  (assert-false (uim '(safe-cdr '())))
  (assert-false (uim '(safe-cdr 1)))
  #f)

(define (test-assq-cdr)
  (assert-equal '(2)
                (uim '(assq-cdr 1 '((1 2)))))
  (assert-equal 2
                (uim '(assq-cdr 1 '((1 . 2)))))
  (assert-false (uim '(assq-cdr 2 '((1 2)))))
  (assert-false (uim '(assq-cdr 2 '((1 . 2)))))
  (assert-equal '(2)
                (uim '(assq-cdr 1 '((3 4) (1 2)))))
  (assert-equal 2
                (uim '(assq-cdr 1 '((3 . 4) (1 . 2)))))
  (assert-equal '(4)
                (uim '(assq-cdr 3 '((3 4) (1 2)))))
  (assert-equal 4
                (uim '(assq-cdr 3 '((3 . 4) (1 . 2)))))
  (assert-false (uim '(assq-cdr 1 '())))
  (assert-error (lambda () (uim '(assq-cdr 1 1))))
  #f)

(define (test-clamp)
  (assert-equal 0 (uim '(clamp -2 0 -1)))
  (assert-equal 0 (uim '(clamp -1 0 -1)))
  (assert-equal 0 (uim '(clamp 0  0 -1)))
  (assert-equal 0 (uim '(clamp 1  0 -1)))
  (assert-equal 0 (uim '(clamp 2  0 -1)))
  (assert-equal 0 (uim '(clamp 10 0 -1)))

  (assert-equal -2 (uim '(clamp -2 -2 0)))
  (assert-equal -1 (uim '(clamp -1 -2 0)))
  (assert-equal 0  (uim '(clamp 0  -2 0)))
  (assert-equal 0  (uim '(clamp 1  -2 0)))
  (assert-equal 0  (uim '(clamp 2  -2 0)))
  (assert-equal 0  (uim '(clamp 10 -2 0)))

  (assert-equal -1 (uim '(clamp -2 -1 0)))
  (assert-equal -1 (uim '(clamp -1 -1 0)))
  (assert-equal 0  (uim '(clamp 0  -1 0)))
  (assert-equal 0  (uim '(clamp 1  -1 0)))
  (assert-equal 0  (uim '(clamp 2  -1 0)))
  (assert-equal 0  (uim '(clamp 10 -1 0)))

  (assert-equal 0 (uim '(clamp -2 0 0)))
  (assert-equal 0 (uim '(clamp -1 0 0)))
  (assert-equal 0 (uim '(clamp 0  0 0)))
  (assert-equal 0 (uim '(clamp 1  0 0)))
  (assert-equal 0 (uim '(clamp 2  0 0)))
  (assert-equal 0 (uim '(clamp 10 0 0)))

  (assert-equal 0 (uim '(clamp -2 0 1)))
  (assert-equal 0 (uim '(clamp -1 0 1)))
  (assert-equal 0 (uim '(clamp 0  0 1)))
  (assert-equal 1 (uim '(clamp 1  0 1)))
  (assert-equal 1 (uim '(clamp 2  0 1)))
  (assert-equal 1 (uim '(clamp 10 0 1)))

  (assert-equal 0 (uim '(clamp -2 0 2)))
  (assert-equal 0 (uim '(clamp -1 0 2)))
  (assert-equal 0 (uim '(clamp 0  0 2)))
  (assert-equal 1 (uim '(clamp 1  0 2)))
  (assert-equal 2 (uim '(clamp 2  0 2)))
  (assert-equal 2 (uim '(clamp 10 0 2)))

  (assert-equal 0 (uim '(clamp -2 0 3)))
  (assert-equal 0 (uim '(clamp -1 0 3)))
  (assert-equal 0 (uim '(clamp 0  0 3)))
  (assert-equal 1 (uim '(clamp 1  0 3)))
  (assert-equal 2 (uim '(clamp 2  0 3)))
  (assert-equal 3 (uim '(clamp 10 0 3)))

  (assert-equal 1 (uim '(clamp -2 1 3)))
  (assert-equal 1 (uim '(clamp -1 1 3)))
  (assert-equal 1 (uim '(clamp 0  1 3)))
  (assert-equal 1 (uim '(clamp 1  1 3)))
  (assert-equal 2 (uim '(clamp 2  1 3)))
  (assert-equal 3 (uim '(clamp 10 1 3)))

  (assert-equal -1 (uim '(clamp -2 -1 3)))
  (assert-equal -1 (uim '(clamp -1 -1 3)))
  (assert-equal 0  (uim '(clamp 0  -1 3)))
  (assert-equal 1  (uim '(clamp 1  -1 3)))
  (assert-equal 2  (uim '(clamp 2  -1 3)))
  (assert-equal 3  (uim '(clamp 10 -1 3)))

  (assert-equal -2 (uim '(clamp -2 -5 5)))
  (assert-equal -1 (uim '(clamp -1 -5 5)))
  (assert-equal 0  (uim '(clamp 0  -5 5)))
  (assert-equal 1  (uim '(clamp 1  -5 5)))
  (assert-equal 2  (uim '(clamp 2  -5 5)))
  (assert-equal 5  (uim '(clamp 10 -5 5)))
  #f)

(provide "test/util/test-list")
