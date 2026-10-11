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

(require-extension (unittest))

(test-begin "string-escape")
;; empty string
(test-equal "\"\""
            (string-escape ""))
;; single character
;; R5RS
(test-equal "\"\\\"\""
            (string-escape "\""))
(test-equal "\"\\\\\""
            (string-escape "\\"))

;; R6RS(SRFI-75) and C89 (uim-sh)
(test-equal "\"\\t\""
            (string-escape "\t")) ;; #\tab
(test-equal "\"\\n\""
            (string-escape "\n")) ;; #\linefeed
(test-equal "\"\\f\""
            (string-escape "\f")) ;; #\page
(test-equal "\"\\r\""
            (string-escape "\r")) ;; #\return
(test-equal "\"\\a\""
            (string-escape "\a"))  ;; #\alarm
(test-equal "\"\\b\""
            (string-escape "\b"))  ;; #\backspace
(test-equal "\"\\v\""
            (string-escape "\v"))  ;; #\vtab

;; R5RS
(test-equal "\"a\""
            (string-escape "a"))
(test-equal "\"b\""
            (string-escape "b"))
(test-equal "\"c\""
            (string-escape "c"))
(test-equal "\"A\""
            (string-escape "A"))
(test-equal "\"B\""
            (string-escape "B"))
(test-equal "\"C\""
            (string-escape "C"))
;; 2 characters
(test-equal "\"\\\"\\\"\""
            (string-escape "\"\""))
(test-equal "\"\\\\\\\"\""
            (string-escape "\\\""))
(test-equal "\"\\\\\\\\\""
            (string-escape "\\\\"))
(test-equal "\"\\r\\n\""
            (string-escape "\r\n"))
(test-equal "\"aB\""
            (string-escape "aB"))
;; complex
(test-equal "\"\\\"a string\\\" in two-line\\nstring\\n\""
            (string-escape "\"a string\" in two-line\nstring\n"))
(test-end)

(test-begin "compose")
(define test-list '(0 1 2 3 4 5))
(test-eq #t (procedure? (compose)))
(test-eq #t (procedure? (compose car)))
(test-eq #t (procedure? (compose car cdr)))
(test-eq #t (procedure? (compose car cdr list)))
(test-equal '(0 1 2 3 4 5)
            ((compose) test-list))
(test-equal 0
            ((compose car) test-list))
(test-equal 1
            ((compose car cdr) test-list))
(test-equal 2
            ((compose car cdr cdr) test-list))
(test-equal 4
            ((compose car cdr reverse) test-list))
(test-equal 3
            ((compose car cdr cdr reverse) test-list))
(test-end)

(test-begin "safe-car")
(test-equal 1
            (safe-car '(1 2)))
(test-equal 1
            (safe-car '(1 . 2)))
(test-false (safe-car '()))
(test-false (safe-car 1))
(test-end)

(test-begin "safe-cdr")
(test-equal '(2)
            (safe-cdr '(1 2)))
(test-equal 2
            (safe-cdr '(1 . 2)))
(test-false (safe-cdr '()))
(test-false (safe-cdr 1))
(test-end)

(test-begin "assq-cdr")
(test-equal '(2)
            (assq-cdr 1 '((1 2))))
(test-equal 2
            (assq-cdr 1 '((1 . 2))))
(test-false (assq-cdr 2 '((1 2))))
(test-false (assq-cdr 2 '((1 . 2))))
(test-equal '(2)
            (assq-cdr 1 '((3 4) (1 2))))
(test-equal 2
            (assq-cdr 1 '((3 . 4) (1 . 2))))
(test-equal '(4)
            (assq-cdr 3 '((3 4) (1 2))))
(test-equal 4
            (assq-cdr 3 '((3 . 4) (1 . 2))))
(test-false (assq-cdr 1 '()))
(test-error (assq-cdr 1 1))
(test-end)

(test-begin "clamp")
(test-equal 0  (clamp -2 0 -1))
(test-equal 0  (clamp -1 0 -1))
(test-equal 0  (clamp 0  0 -1))
(test-equal 0  (clamp 1  0 -1))
(test-equal 0  (clamp 2  0 -1))
(test-equal 0  (clamp 10 0 -1))

(test-equal -2 (clamp -2 -2 0))
(test-equal -1 (clamp -1 -2 0))
(test-equal 0  (clamp 0  -2 0))
(test-equal 0  (clamp 1  -2 0))
(test-equal 0  (clamp 2  -2 0))
(test-equal 0  (clamp 10 -2 0))

(test-equal -1 (clamp -2 -1 0))
(test-equal -1 (clamp -1 -1 0))
(test-equal 0  (clamp 0  -1 0))
(test-equal 0  (clamp 1  -1 0))
(test-equal 0  (clamp 2  -1 0))
(test-equal 0  (clamp 10 -1 0))

(test-equal 0  (clamp -2 0 0))
(test-equal 0  (clamp -1 0 0))
(test-equal 0  (clamp 0  0 0))
(test-equal 0  (clamp 1  0 0))
(test-equal 0  (clamp 2  0 0))
(test-equal 0  (clamp 10 0 0))

(test-equal 0  (clamp -2 0 1))
(test-equal 0  (clamp -1 0 1))
(test-equal 0  (clamp 0  0 1))
(test-equal 1  (clamp 1  0 1))
(test-equal 1  (clamp 2  0 1))
(test-equal 1  (clamp 10 0 1))

(test-equal 0  (clamp -2 0 2))
(test-equal 0  (clamp -1 0 2))
(test-equal 0  (clamp 0  0 2))
(test-equal 1  (clamp 1  0 2))
(test-equal 2  (clamp 2  0 2))
(test-equal 2  (clamp 10 0 2))

(test-equal 0  (clamp -2 0 3))
(test-equal 0  (clamp -1 0 3))
(test-equal 0  (clamp 0  0 3))
(test-equal 1  (clamp 1  0 3))
(test-equal 2  (clamp 2  0 3))
(test-equal 3  (clamp 10 0 3))

(test-equal 1  (clamp -2 1 3))
(test-equal 1  (clamp -1 1 3))
(test-equal 1  (clamp 0  1 3))
(test-equal 1  (clamp 1  1 3))
(test-equal 2  (clamp 2  1 3))
(test-equal 3  (clamp 10 1 3))

(test-equal -1 (clamp -2 -1 3))
(test-equal -1 (clamp -1 -1 3))
(test-equal 0  (clamp 0  -1 3))
(test-equal 1  (clamp 1  -1 3))
(test-equal 2  (clamp 2  -1 3))
(test-equal 3  (clamp 10 -1 3))

(test-equal -2 (clamp -2 -5 5))
(test-equal -1 (clamp -1 -5 5))
(test-equal 0  (clamp 0  -5 5))
(test-equal 1  (clamp 1  -5 5))
(test-equal 2  (clamp 2  -5 5))
(test-equal 5  (clamp 10 -5 5))
(test-end)

(test-begin "try-load")
(test-true (try-load "anthy.scm"))
(test-false (try-load "nonexistent.scm"))
(test-end)

(test-begin "try-require")
(test-true (try-require "anthy.scm"))
(test-false (try-require "nonexistent.scm"))
(test-end)

(test-report-result)
