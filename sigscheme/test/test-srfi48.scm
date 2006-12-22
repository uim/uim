#! /usr/bin/env sscm -C UTF-8

;;  Filename : test-srfi48.scm
;;  About    : unit test for SRFI-48
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

;; All tests in this file are passed against r3170 (new repository)

(load "./test/unittest.scm")

(if (not (provided? "srfi-48"))
    (test-skip "SRFI-48 is not enabled"))

(use srfi-6)
(use srfi-38)
(use srfi-48)

;; test SRFI-28 compatible part of SRFI-48
(load "./test/test-srfi28.scm")
(newline)

(define tn test-name)

(define testing-format+? (and (symbol-bound? 'format+)
                              (eq? format format+)))

(define cl (list 0 1))
(set-cdr! (cdr cl) cl)

(tn "SRFI-48 format invalid form")
(assert-error  (tn) (lambda () (format 0       "~~")))
(assert-error  (tn) (lambda () (format #\a     "~~")))
(assert-error  (tn) (lambda () (format "a"     "~~")))
(assert-error  (tn) (lambda () (format '(0 1)  "~~")))
(assert-error  (tn) (lambda () (format '#(0 1) "~~")))
(assert-error  (tn) (lambda () (format 0       "~s" 0)))
(assert-error  (tn) (lambda () (format #\a     "~s" #\a)))
(assert-error  (tn) (lambda () (format "a"     "~s" "aBc")))
(assert-error  (tn) (lambda () (format '(0 1)  "~s" '(0 1))))
(assert-error  (tn) (lambda () (format '#(0 1) "~s" '#(0 1))))
(assert-error  (tn) (lambda () (format "~")))
(assert-error  (tn) (lambda () (format "a~")))

(tn "SRFI-48 format explicit port")
(assert-equal? (tn)
               "\"aBc\""
               (format #f "~s" "aBc"))
(display "expected output: \"aBc\"")
(newline)
(display "actual output:   ")
(assert-equal? (tn)
               (undef)
               (format #t "~s" "aBc"))
(newline)
(let ((port (open-output-string)))
  (assert-equal? (tn)
                 (undef)
                 (format port "~s" "aBc"))
  (assert-equal? (tn)
                 "\"aBc\""
                 (get-output-string port)))

(tn "format ~w")
(assert-error  (tn) (lambda () (format "~w")))
(assert-error  (tn) (lambda () (format "~w" 0 1)))
(assert-error  (tn) (lambda () (format "~1w" 1)))
(assert-equal? (tn)
               (if (and (provided? "sigscheme")
                        (provided? "siod-bugs"))
                   "()"
                   "#f")
               (format "~w" #f))
(assert-equal? (tn)
               "#t"
               (format "~w" #t))
(assert-equal? (tn)
               "123"
               (format "~w" 123))
(assert-equal? (tn)
               "#\\a"
               (format "~w" #\a))
(assert-equal? (tn)
               "\"\""
               (format "~w" ""))
(assert-equal? (tn)
               "\"\\\"\""
               (format "~w" "\""))
(assert-equal? (tn)
               "\"aBc\""
               (format "~w" "aBc"))
(assert-equal? (tn)
               "(#t 123 #\\a \"aBc\" (0))"
               (format "~w" '(#t 123 #\a "aBc" (0))))
(assert-equal? (tn)
               "#(#t 123 #\\a \"aBc\" (0))"
               (format "~w" '#(#t 123 #\a "aBc" (0))))
(assert-equal? (tn)
               (if (provided? "sigscheme")
                   "#1=(0 1 . #1#)"  ;; SigScheme starts the index with 1
                   "#0=(0 1 . #0#)")
               (format "~w" cl))
(assert-equal? (tn)
               "#t"
               (format "~W" #t))

(tn "format ~d")
(assert-error  (tn) (lambda () (format "~d")))
(assert-error  (tn) (lambda () (format "~d" 0 1)))
(assert-error  (tn) (lambda () (format "~d" #t)))
(assert-error  (tn) (lambda () (format "~d" #\a)))
(assert-error  (tn) (lambda () (format "~d" "aBc")))
(assert-error  (tn) (lambda () (format "~d" '(0 1))))
(assert-error  (tn) (lambda () (format "~d" '#(0 1))))
(if (not testing-format+?)
    (assert-error  (tn) (lambda () (format "~1d" 1))))
(assert-equal? (tn) "-100" (format "~d" -100))
(assert-equal? (tn) "-10"  (format "~d" -10))
(assert-equal? (tn) "-1"   (format "~d" -1))
(assert-equal? (tn) "0"    (format "~d" 0))
(assert-equal? (tn) "1"    (format "~d" 1))
(assert-equal? (tn) "10"   (format "~d" 10))
(assert-equal? (tn) "100"  (format "~d" 100))
(assert-equal? (tn) "10"   (format "~D" 10))

(tn "format ~x")
(assert-error  (tn) (lambda () (format "~x")))
(assert-error  (tn) (lambda () (format "~x" 0 1)))
(assert-error  (tn) (lambda () (format "~x" #t)))
(assert-error  (tn) (lambda () (format "~x" #\a)))
(assert-error  (tn) (lambda () (format "~x" "aBc")))
(assert-error  (tn) (lambda () (format "~x" '(0 1))))
(assert-error  (tn) (lambda () (format "~x" '#(0 1))))
(if (not testing-format+?)
    (assert-error  (tn) (lambda () (format "~1x" 1))))
(assert-equal? (tn) "-64"  (format "~x" -100))
(assert-equal? (tn) "-a"   (format "~x" -10))
(assert-equal? (tn) "-1"   (format "~x" -1))
(assert-equal? (tn) "0"    (format "~x" 0))
(assert-equal? (tn) "1"    (format "~x" 1))
(assert-equal? (tn) "a"    (format "~x" 10))
(assert-equal? (tn) "64"   (format "~x" 100))
(assert-equal? (tn) "a"    (format "~X" 10))

(tn "format ~o")
(assert-error  (tn) (lambda () (format "~o")))
(assert-error  (tn) (lambda () (format "~o" 0 1)))
(assert-error  (tn) (lambda () (format "~o" #t)))
(assert-error  (tn) (lambda () (format "~o" #\a)))
(assert-error  (tn) (lambda () (format "~o" "aBc")))
(assert-error  (tn) (lambda () (format "~o" '(0 1))))
(assert-error  (tn) (lambda () (format "~o" '#(0 1))))
(if (not testing-format+?)
    (assert-error  (tn) (lambda () (format "~1o" 1))))
(assert-equal? (tn) "-144" (format "~o" -100))
(assert-equal? (tn) "-12"  (format "~o" -10))
(assert-equal? (tn) "-1"   (format "~o" -1))
(assert-equal? (tn) "0"    (format "~o" 0))
(assert-equal? (tn) "1"    (format "~o" 1))
(assert-equal? (tn) "12"   (format "~o" 10))
(assert-equal? (tn) "144"  (format "~o" 100))
(assert-equal? (tn) "12"   (format "~O" 10))

(tn "format ~b")
(assert-error  (tn) (lambda () (format "~b")))
(assert-error  (tn) (lambda () (format "~b" 0 1)))
(assert-error  (tn) (lambda () (format "~b" #t)))
(assert-error  (tn) (lambda () (format "~b" #\a)))
(assert-error  (tn) (lambda () (format "~b" "aBc")))
(assert-error  (tn) (lambda () (format "~b" '(0 1))))
(assert-error  (tn) (lambda () (format "~b" '#(0 1))))
(if (not testing-format+?)
    (assert-error  (tn) (lambda () (format "~1b" 1))))
(assert-equal? (tn) "-1100100" (format "~b" -100))
(assert-equal? (tn) "-1010"    (format "~b" -10))
(assert-equal? (tn) "-1"       (format "~b" -1))
(assert-equal? (tn) "0"        (format "~b" 0))
(assert-equal? (tn) "1"        (format "~b" 1))
(assert-equal? (tn) "1010"     (format "~b" 10))
(assert-equal? (tn) "1100100"  (format "~b" 100))
(assert-equal? (tn) "1010"     (format "~B" 10))

(if (and (symbol-bound? 'greatest-fixnum)
         (symbol-bound? 'least-fixnum))
    (case fixnum-bits
      ((28)
       (tn "format 28bit fixnum")
       (assert-equal? (tn)
                      "-134217728"
                      (format "~d" (least-fixnum)))
       (assert-equal? (tn)
                      "111111111111111111111111111"
                      (format "~b" (greatest-fixnum)))
       (assert-equal? (tn)
                      "-1000000000000000000000000000"
                      (format "~b" (least-fixnum)))
       (assert-equal? (tn)
                      "-111111111111111111111111111"
                      (format "~b" (+ (least-fixnum) 1))))

      ((32)
       (tn "format 32bit fixnum")
       (assert-equal? (tn)
                      "-2147483648"
                      (format "~d" (least-fixnum)))
       (assert-equal? (tn)
                      "1111111111111111111111111111111"
                      (format "~b" (greatest-fixnum)))
       (assert-equal? (tn)
                      "-10000000000000000000000000000000"
                      (format "~b" (least-fixnum)))
       (assert-equal? (tn)
                      "-1111111111111111111111111111111"
                      (format "~b" (+ (least-fixnum) 1))))

      ((60)
       (tn "format 60bit fixnum")
       (assert-equal? (tn)
                      "-576460752303423488"
                      (format "~d" (least-fixnum)))
       (assert-equal? (tn)
                      "11111111111111111111111111111111111111111111111111111111111"
                      (format "~b" (greatest-fixnum)))
       (assert-equal? (tn)
                      "-100000000000000000000000000000000000000000000000000000000000"
                      (format "~b" (least-fixnum)))
       (assert-equal? (tn)
                      "-11111111111111111111111111111111111111111111111111111111111"
                      (format "~b" (+ (least-fixnum) 1))))

      ((64)
       (tn "format 64bit fixnum")
       (assert-equal? (tn)
                      "-9223372036854775808"
                      (format "~d" (least-fixnum)))
       (assert-equal? (tn)
                      "111111111111111111111111111111111111111111111111111111111111111"
                      (format "~b" (greatest-fixnum)))
       (assert-equal? (tn)
                      "-1000000000000000000000000000000000000000000000000000000000000000"
                      (format "~b" (least-fixnum)))
       (assert-equal? (tn)
                      "-111111111111111111111111111111111111111111111111111111111111111"
                      (format "~b" (+ (least-fixnum) 1))))

      (else
       (error "unknown int bitwidth"))))

(tn "format ~c")
(assert-error  (tn) (lambda () (format "~c")))
(assert-error  (tn) (lambda () (format "~c" #\a #\b)))
(assert-error  (tn) (lambda () (format "~c" #t)))
(assert-error  (tn) (lambda () (format "~c" 0)))
(assert-error  (tn) (lambda () (format "~c" "aBc")))
(assert-error  (tn) (lambda () (format "~c" '(#\a #\b))))
(assert-error  (tn) (lambda () (format "~c" '#(#\a #\b))))
(assert-error  (tn) (lambda () (format "~1c" #\a)))
(assert-equal? (tn) "a"  (format "~c" #\a))
(assert-equal? (tn) "\"" (format "~c" #\"))
(assert-equal? (tn) "あ" (format "~c" #\あ))

(tn "format ~f (number)")
(assert-error  (tn) (lambda () (format "~f")))
(assert-error  (tn) (lambda () (format "~f" 0 1)))
(assert-error  (tn) (lambda () (format "~f" #t)))
(assert-error  (tn) (lambda () (format "~f" #\a)))
(assert-error  (tn) (lambda () (format "~f" '(0 1))))
(assert-error  (tn) (lambda () (format "~f" '#(0 1))))
(assert-error  (tn) (lambda () (format "0128f"    1)))
(assert-error  (tn) (lambda () (format "0128,1f"  1)))
(assert-error  (tn) (lambda () (format "1,0128f"  1)))
(assert-error  (tn) (lambda () (format "01024f"   1)))
(assert-error  (tn) (lambda () (format "01024,1f" 1)))
(assert-error  (tn) (lambda () (format "1,01024f" 1)))
(assert-error  (tn) (lambda () (format "~-1f"    1)))
(assert-error  (tn) (lambda () (format "~-0f"    1)))
(assert-error  (tn) (lambda () (format "~0,-0f"  1)))
(assert-error  (tn) (lambda () (format "~0,-1f"  1)))
(assert-error  (tn) (lambda () (format "~1,-0f"  1)))
(assert-error  (tn) (lambda () (format "~1,-1f"  1)))
(assert-error  (tn) (lambda () (format "~-0,0f"  1)))
(assert-error  (tn) (lambda () (format "~-0,1f"  1)))
(assert-error  (tn) (lambda () (format "~-1,0f"  1)))
(assert-error  (tn) (lambda () (format "~-1,1f"  1)))
(assert-error  (tn) (lambda () (format "~-0,-0f" 1)))
(assert-error  (tn) (lambda () (format "~-0,-1f" 1)))
(assert-error  (tn) (lambda () (format "~-1,-0f" 1)))
(assert-error  (tn) (lambda () (format "~-1,-1f" 1)))
(assert-error  (tn) (lambda () (format "~,f"     1)))
(assert-error  (tn) (lambda () (format "~,1f"    1)))
(assert-error  (tn) (lambda () (format "~1,f"    1)))
(assert-equal? (tn) "-100" (format "~f" -100))
(assert-equal? (tn) "-10"  (format "~f" -10))
(assert-equal? (tn) "-1"   (format "~f" -1))
(assert-equal? (tn) "0"    (format "~f" 0))
(assert-equal? (tn) "1"    (format "~f" 1))
(assert-equal? (tn) "10"   (format "~f" 10))
(assert-equal? (tn) "100"  (format "~f" 100))

(if (not testing-format+?)
    (begin
      (assert-equal? (tn) "-100" (format "~0f" -100))
      (assert-equal? (tn) "-10"  (format "~0f" -10))
      (assert-equal? (tn) "-1"   (format "~0f" -1))
      (assert-equal? (tn) "0"    (format "~0f" 0))
      (assert-equal? (tn) "1"    (format "~0f" 1))
      (assert-equal? (tn) "10"   (format "~0f" 10))
      (assert-equal? (tn) "100"  (format "~0f" 100))))

(assert-equal? (tn) "-100" (format "~1f" -100))
(assert-equal? (tn) "-10"  (format "~1f" -10))
(assert-equal? (tn) "-1"   (format "~1f" -1))
(assert-equal? (tn) "0"    (format "~1f" 0))
(assert-equal? (tn) "1"    (format "~1f" 1))
(assert-equal? (tn) "10"   (format "~1f" 10))
(assert-equal? (tn) "100"  (format "~1f" 100))

(assert-equal? (tn) "-100" (format "~2f" -100))
(assert-equal? (tn) "-10"  (format "~2f" -10))
(assert-equal? (tn) "-1"   (format "~2f" -1))
(assert-equal? (tn) " 0"   (format "~2f" 0))
(assert-equal? (tn) " 1"   (format "~2f" 1))
(assert-equal? (tn) "10"   (format "~2f" 10))
(assert-equal? (tn) "100"  (format "~2f" 100))

(assert-equal? (tn) "-100" (format "~3f" -100))
(assert-equal? (tn) "-10"  (format "~3f" -10))
(assert-equal? (tn) " -1"  (format "~3f" -1))
(assert-equal? (tn) "  0"  (format "~3f" 0))
(assert-equal? (tn) "  1"  (format "~3f" 1))
(assert-equal? (tn) " 10"  (format "~3f" 10))
(assert-equal? (tn) "100"  (format "~3f" 100))

(assert-equal? (tn) "-100" (format "~4f" -100))
(assert-equal? (tn) " -10" (format "~4f" -10))
(assert-equal? (tn) "  -1" (format "~4f" -1))
(assert-equal? (tn) "   0" (format "~4f" 0))
(assert-equal? (tn) "   1" (format "~4f" 1))
(assert-equal? (tn) "  10" (format "~4f" 10))
(assert-equal? (tn) " 100" (format "~4f" 100))

(assert-equal? (tn) " -100" (format "~5f" -100))
(assert-equal? (tn) "  -10" (format "~5f" -10))
(assert-equal? (tn) "   -1" (format "~5f" -1))
(assert-equal? (tn) "    0" (format "~5f" 0))
(assert-equal? (tn) "    1" (format "~5f" 1))
(assert-equal? (tn) "   10" (format "~5f" 10))
(assert-equal? (tn) "  100" (format "~5f" 100))

(if (not testing-format+?)
    (begin
      (assert-equal? (tn) "-100" (format "~0,0f" -100))
      (assert-equal? (tn) "-10"  (format "~0,0f" -10))
      (assert-equal? (tn) "-1"   (format "~0,0f" -1))
      (assert-equal? (tn) "0"    (format "~0,0f" 0))
      (assert-equal? (tn) "1"    (format "~0,0f" 1))
      (assert-equal? (tn) "10"   (format "~0,0f" 10))
      (assert-equal? (tn) "100"  (format "~0,0f" 100))))

(if (not testing-format+?)
    (begin
      (assert-equal? (tn) " -100" (format "~05f" -100))
      (assert-equal? (tn) "  -10" (format "~05f" -10))
      (assert-equal? (tn) "   -1" (format "~05f" -1))
      (assert-equal? (tn) "    0" (format "~05f" 0))
      (assert-equal? (tn) "    1" (format "~05f" 1))
      (assert-equal? (tn) "   10" (format "~05f" 10))
      (assert-equal? (tn) "  100" (format "~05f" 100))))

(if (symbol-bound? 'exact->inexact)
    (begin
      (assert-equal? (tn) "-100.0" (format "~5,0f" -100))
      (assert-equal? (tn) "-10.0"  (format "~5,0f" -10))
      (assert-equal? (tn) " -1.0"  (format "~5,0f" -1))
      (assert-equal? (tn) "  0.0"  (format "~5,0f" 0))
      (assert-equal? (tn) "  1.0"  (format "~5,0f" 1))
      (assert-equal? (tn) " 10.0"  (format "~5,0f" 10))
      (assert-equal? (tn) "100.0"  (format "~5,0f" 100))

      (assert-equal? (tn) "-100.0" (format "~5,1f" -100))
      (assert-equal? (tn) "-10.0"  (format "~5,1f" -10))
      (assert-equal? (tn) " -1.0"  (format "~5,1f" -1))
      (assert-equal? (tn) "  0.0"  (format "~5,1f" 0))
      (assert-equal? (tn) "  1.0"  (format "~5,1f" 1))
      (assert-equal? (tn) " 10.0"  (format "~5,1f" 10))
      (assert-equal? (tn) "100.0"  (format "~5,1f" 100))

      (assert-equal? (tn) "-100.00" (format "~5,2f" -100))
      (assert-equal? (tn) "-10.00"  (format "~5,2f" -10))
      (assert-equal? (tn) "-1.00"   (format "~5,2f" -1))
      (assert-equal? (tn) " 0.00"   (format "~5,2f" 0))
      (assert-equal? (tn) " 1.00"   (format "~5,2f" 1))
      (assert-equal? (tn) "10.00"   (format "~5,2f" 10))
      (assert-equal? (tn) "100.00"  (format "~5,2f" 100))

      (if (not testing-format+?)
          (begin
            (assert-equal? (tn) "-100.00" (format "~05,02f" -100))
            (assert-equal? (tn) "-10.00"  (format "~05,02f" -10))
            (assert-equal? (tn) "-1.00"   (format "~05,02f" -1))
            (assert-equal? (tn) " 0.00"   (format "~05,02f" 0))
            (assert-equal? (tn) " 1.00"   (format "~05,02f" 1))
            (assert-equal? (tn) "10.00"   (format "~05,02f" 10))
            (assert-equal? (tn) "100.00"  (format "~05,02f" 100))))

      (assert-equal? (tn) "100.0"  (format "~5,1F" 100))))

(assert-equal? (tn)
               "                                                                                                                            123"
               (format "~127f" 123))
(if (not testing-format+?)
    (assert-equal? (tn)
                   "                                                                                                                            123"
                   (format "~0127f" 123)))

(assert-equal? (tn) "10"    (format "~F" 10))
(assert-equal? (tn) "  100" (format "~5F" 100))

(tn "format ~f (string)")
(assert-error  (tn) (lambda () (format "~f" "a" "b")))
(assert-error  (tn) (lambda () (format "~f" '("a" "b"))))
(assert-error  (tn) (lambda () (format "~f" '#("a" "b"))))
(assert-error  (tn) (lambda () (format "0100f"   "a")))
(assert-error  (tn) (lambda () (format "0100,1f" "a")))
(assert-error  (tn) (lambda () (format "1,0100f" "a")))
(assert-error  (tn) (lambda () (format "~-1f"    "a")))
(assert-error  (tn) (lambda () (format "~-0f"    "a")))
(assert-error  (tn) (lambda () (format "~0,-0f"  "a")))
(assert-error  (tn) (lambda () (format "~0,-1f"  "a")))
(assert-error  (tn) (lambda () (format "~1,-0f"  "a")))
(assert-error  (tn) (lambda () (format "~1,-1f"  "a")))
(assert-error  (tn) (lambda () (format "~-0,0f"  "a")))
(assert-error  (tn) (lambda () (format "~-0,1f"  "a")))
(assert-error  (tn) (lambda () (format "~-1,0f"  "a")))
(assert-error  (tn) (lambda () (format "~-1,1f"  "a")))
(assert-error  (tn) (lambda () (format "~-0,-0f" "a")))
(assert-error  (tn) (lambda () (format "~-0,-1f" "a")))
(assert-error  (tn) (lambda () (format "~-1,-0f" "a")))
(assert-error  (tn) (lambda () (format "~-1,-1f" "a")))
(assert-equal? (tn) ""        (format "~f"   ""))
(assert-equal? (tn) "\""      (format "~f"   "\""))
(assert-equal? (tn) "aBc"     (format "~f"   "aBc"))
(assert-equal? (tn) "あbう"   (format "~f"   "あbう"))

(assert-equal? (tn) ""        (format "~0f"  ""))
(assert-equal? (tn) "\""      (format "~0f"  "\""))
(assert-equal? (tn) "aBc"     (format "~0f"  "aBc"))
(assert-equal? (tn) "あbう"   (format "~0f"  "あbう"))

(assert-equal? (tn) " "       (format "~1f"  ""))
(assert-equal? (tn) "\""      (format "~1f"  "\""))
(assert-equal? (tn) "aBc"     (format "~1f"  "aBc"))
(assert-equal? (tn) "あbう"   (format "~1f"  "あbう"))

(assert-equal? (tn) "  "      (format "~2f"  ""))
(assert-equal? (tn) " \""     (format "~2f"  "\""))
(assert-equal? (tn) "aBc"     (format "~2f"  "aBc"))
(assert-equal? (tn) "あbう"   (format "~2f"  "あbう"))

(assert-equal? (tn) "   "     (format "~3f"  ""))
(assert-equal? (tn) "  \""    (format "~3f"  "\""))
(assert-equal? (tn) "aBc"     (format "~3f"  "aBc"))
(assert-equal? (tn) "あbう"   (format "~3f"  "あbう"))

(assert-equal? (tn) "    "    (format "~4f"  ""))
(assert-equal? (tn) "   \""   (format "~4f"  "\""))
(assert-equal? (tn) " aBc"    (format "~4f"  "aBc"))
(assert-equal? (tn) " あbう"  (format "~4f"  "あbう"))

(assert-equal? (tn) "     "   (format "~5f"  ""))
(assert-equal? (tn) "    \""  (format "~5f"  "\""))
(assert-equal? (tn) "  aBc"   (format "~5f"  "aBc"))
(assert-equal? (tn) "  あbう" (format "~5f"  "あbう"))

(assert-equal? (tn) "     "   (format "~05f" ""))
(assert-equal? (tn) "    \""  (format "~05f" "\""))
(assert-equal? (tn) "  aBc"   (format "~05f" "aBc"))
(assert-equal? (tn) "  あbう" (format "~05f" "あbう"))

(assert-equal? (tn) "     "   (format "~5,2f"   ""))
(assert-equal? (tn) "    \""  (format "~5,2f"   "\""))
(assert-equal? (tn) "  aBc"   (format "~5,2f"   "aBc"))
(assert-equal? (tn) "  あbう" (format "~5,2f"   "あbう"))

(assert-equal? (tn) "     "   (format "~05,02f" ""))
(assert-equal? (tn) "    \""  (format "~05,02f" "\""))
(assert-equal? (tn) "  aBc"   (format "~05,02f" "aBc"))
(assert-equal? (tn) "  あbう" (format "~05,02f" "あbう"))

(assert-equal? (tn)
               "                                                                                                                            aBc"
               (format "~127f" "aBc"))
(assert-equal? (tn)
               "                                                                                                                            aBc"
               (format "~0127f" "aBc"))

(assert-equal? (tn) "aBc"     (format "~F"      "aBc"))
(assert-equal? (tn) "  aBc"   (format "~5F"     "aBc"))
(assert-equal? (tn) "  aBc"   (format "~05F"    "aBc"))
(assert-equal? (tn) "  aBc"   (format "~5,2F"   "aBc"))

(tn "format ~?")
(assert-error  (tn) (lambda () (format "~?")))
(assert-error  (tn) (lambda () (format "~?" "~~")))
(assert-error  (tn) (lambda () (format "~?" "a")))
(assert-error  (tn) (lambda () (format "~?" "a" '() "b")))
(assert-error  (tn) (lambda () (format "~1?" "a" '())))
(assert-error  (tn) (lambda () (format "~?" "~a" '())))
(assert-error  (tn) (lambda () (format "~?" "~a" '(0 1))))
(assert-error  (tn) (lambda () (format "~?" "~?" '("~a"))))
(assert-error  (tn) (lambda () (format "~?" "~?" '("~a" (0 1)))))
(assert-error  (tn) (lambda () (format "~?" #t      '())))
(assert-error  (tn) (lambda () (format "~?" 0       '())))
(assert-error  (tn) (lambda () (format "~?" #\a     '())))
(assert-error  (tn) (lambda () (format "~?" '(0 1)  '())))
(assert-error  (tn) (lambda () (format "~?" '#(0 1) '())))
(assert-equal? (tn) "~"       (format "~?" "~~" '()))
(assert-equal? (tn) " "       (format "~?" "~_" '()))
(assert-equal? (tn) "\n"      (format "~?" "~%" '()))
(assert-equal? (tn) "\n"      (format "~?" "~&" '()))
;; hard to be this on current port implementation
;;(assert-equal? (tn) "\n"      (format "~?" "~%~?"  '("~&" ())))
(assert-equal? (tn) "\n\n"    (format "~?" "~%~?"  '("~&" ())))
(assert-equal? (tn) "\n \n"   (format "~?" "~% ~?" '("~&" ())))
(assert-equal? (tn) "\n \n"   (format "~?" "~%~?"  '(" ~&" ())))
(assert-equal? (tn) "aBc"     (format "~?" "aBc" '()))
(assert-equal? (tn) "0aBc1"   (format "~?" "0~a1" '("aBc")))
(assert-equal? (tn) "02aBc31" (format "~?" "0~?1" '("2~a3" ("aBc"))))
(assert-equal? (tn) "024aBc531"
               (format "~?" "0~?1" '("2~?3" ("4~a5" ("aBc")))))
(assert-equal? (tn)
               (if (and (provided? "sigscheme")
                        (provided? "siod-bugs"))
                   "()"
                   "#f")
               (format "~?" "~w" '(#f)))
(assert-equal? (tn)
               "#t"
               (format "~?" "~w" '(#t)))
(assert-equal? (tn)
               "123"
               (format "~?" "~w" '(123)))
(assert-equal? (tn)
               "#\\a"
               (format "~?" "~w" '(#\a)))
(assert-equal? (tn)
               "\"\""
               (format "~?" "~w" '("")))
(assert-equal? (tn)
               "\"\\\"\""
               (format "~?" "~w" '("\"")))
(assert-equal? (tn)
               "\"aBc\""
               (format "~?" "~w" '("aBc")))
(assert-equal? (tn)
               "(#t 123 #\\a \"aBc\" (0))"
               (format "~?" "~w" '((#t 123 #\a "aBc" (0)))))
(assert-equal? (tn)
               "#(#t 123 #\\a \"aBc\" (0))"
               (format "~?" "~w" '(#(#t 123 #\a "aBc" (0)))))
(assert-equal? (tn)
               (if (provided? "sigscheme")
                   "#1=(0 1 . #1#)"  ;; SigScheme starts the index with 1
                   "#0=(0 1 . #0#)")
               (format "~?" "~w" (list cl)))

;; alias of ~?
(tn "format ~k")
(assert-error  (tn) (lambda () (format "~k")))
(assert-error  (tn) (lambda () (format "~k" "~~")))
(assert-error  (tn) (lambda () (format "~k" "a")))
(assert-error  (tn) (lambda () (format "~k" "a" '() "b")))
(assert-error  (tn) (lambda () (format "~1k" "a" '())))
(assert-error  (tn) (lambda () (format "~k" "~a" '())))
(assert-error  (tn) (lambda () (format "~k" "~a" '(0 1))))
(assert-error  (tn) (lambda () (format "~k" "~k" '("~a"))))
(assert-error  (tn) (lambda () (format "~k" "~k" '("~a" (0 1)))))
(assert-error  (tn) (lambda () (format "~k" #t      '())))
(assert-error  (tn) (lambda () (format "~k" 0       '())))
(assert-error  (tn) (lambda () (format "~k" #\a     '())))
(assert-error  (tn) (lambda () (format "~k" '(0 1)  '())))
(assert-error  (tn) (lambda () (format "~k" '#(0 1) '())))
(assert-equal? (tn) "~"       (format "~k" "~~" '()))
(assert-equal? (tn) "02aBc31" (format "~k" "0~k1" '("2~a3" ("aBc"))))
(assert-error  (tn) (lambda () (format "~K")))
(assert-error  (tn) (lambda () (format "~K" "~~")))
(assert-error  (tn) (lambda () (format "~K" "a")))
(assert-error  (tn) (lambda () (format "~K" "a" '() "b")))
(assert-error  (tn) (lambda () (format "~1K" "a" '())))
(assert-error  (tn) (lambda () (format "~K" "~a" '())))
(assert-error  (tn) (lambda () (format "~K" "~a" '(0 1))))
(assert-error  (tn) (lambda () (format "~K" "~K" '("~a"))))
(assert-error  (tn) (lambda () (format "~K" "~K" '("~a" (0 1)))))
(assert-error  (tn) (lambda () (format "~K" #t      '())))
(assert-error  (tn) (lambda () (format "~K" 0       '())))
(assert-error  (tn) (lambda () (format "~K" #\a     '())))
(assert-error  (tn) (lambda () (format "~K" '(0 1)  '())))
(assert-error  (tn) (lambda () (format "~K" '#(0 1) '())))
(assert-equal? (tn) "~"       (format "~K" "~~" '()))
(assert-equal? (tn) "02aBc31" (format "~K" "0~K1" '("2~a3" ("aBc"))))

(tn "format ~y")
(assert-error  (tn) (lambda () (format "~y")))
(assert-error  (tn) (lambda () (format "~y" 0 1)))
(assert-error  (tn) (lambda () (format "~1y" 1)))
(assert-equal? (tn)
               (if (and (provided? "sigscheme")
                        (provided? "siod-bugs"))
                   "()"
                   "#f")
               (format "~y" #f))
(assert-equal? (tn)
               "#t"
               (format "~y" #t))
(assert-equal? (tn)
               "123"
               (format "~y" 123))
(assert-equal? (tn)
               "#\\a"
               (format "~y" #\a))
(assert-equal? (tn)
               "\"\""
               (format "~y" ""))
(assert-equal? (tn)
               "\"\\\"\""
               (format "~y" "\""))
(assert-equal? (tn)
               "\"aBc\""
               (format "~y" "aBc"))
;; no pretty-print procedure
(assert-equal? (tn)
               "(#t 123 #\\a \"aBc\" (0))"
               (format "~y" '(#t 123 #\a "aBc" (0))))
(assert-equal? (tn)
               "#(#t 123 #\\a \"aBc\" (0))"
               (format "~y" '#(#t 123 #\a "aBc" (0))))
(tn "format ~y with explicit port to pretty-print")
(let ((p (open-output-string)))
  (format p "~y" 123)
  (assert-equal? (tn) "123" (get-output-string p)))
(define pretty-print write)
(let ((p (open-output-string)))
  (format p "~y" 123)
  (assert-equal? (tn) "123" (get-output-string p)))
(define pretty-print #f)
(let ((p (open-output-string)))
  (assert-error  (tn) (lambda () (format p "~y" 123))))

(tn "format ~t")
(assert-error  (tn) (lambda () (format "~t" #t)))
(assert-error  (tn) (lambda () (format "~t" 0)))
(assert-error  (tn) (lambda () (format "~t" #\a)))
(assert-error  (tn) (lambda () (format "~t" "aBc")))
(assert-error  (tn) (lambda () (format "~t" '(0 1))))
(assert-error  (tn) (lambda () (format "~t" '#(0 1))))
(assert-error  (tn) (lambda () (format "~1t")))
(assert-equal? (tn) "	" (format "~t"))
(assert-equal? (tn) "\t"  (format "~t"))
(assert-equal? (tn) "\t"  (format "~T"))

(tn "format ~_")
(assert-error  (tn) (lambda () (format "~_" #t)))
(assert-error  (tn) (lambda () (format "~_" 0)))
(assert-error  (tn) (lambda () (format "~_" #\a)))
(assert-error  (tn) (lambda () (format "~_" "aBc")))
(assert-error  (tn) (lambda () (format "~_" '(0 1))))
(assert-error  (tn) (lambda () (format "~_" '#(0 1))))
(assert-error  (tn) (lambda () (format "~1_")))
(assert-equal? (tn) " " (format "~_"))

(tn "format ~&")
(assert-error  (tn) (lambda () (format "~&" #t)))
(assert-error  (tn) (lambda () (format "~&" 0)))
(assert-error  (tn) (lambda () (format "~&" #\a)))
(assert-error  (tn) (lambda () (format "~&" "aBc")))
(assert-error  (tn) (lambda () (format "~&" '(0 1))))
(assert-error  (tn) (lambda () (format "~&" '#(0 1))))
(assert-error  (tn) (lambda () (format "~1&")))
(assert-equal? (tn) "
" (format "~&"))
(assert-equal? (tn) "\n"   (format "~&"))
(assert-equal? (tn) "\n"   (format "~&~&"))
(assert-equal? (tn) "\n"   (format "~&~&~&"))
(assert-equal? (tn) "\n"   (format "~%~&"))
(assert-equal? (tn) "\n"   (format "~%~&~&"))
(assert-equal? (tn) "\n\n" (format "~&~%"))
(assert-equal? (tn) "\n\n" (format "~&~%~&"))
(assert-equal? (tn) "\n"   (format "\n~&"))
(assert-equal? (tn) "\n\n" (format "~&\n"))
(assert-equal? (tn) "\n\n" (format "~&\n~&"))
(assert-equal? (tn) " \n"  (format " ~&"))
(assert-equal? (tn) "\n \n \n" (format "\n ~& ~&"))

(tn "format ~h")
(define help-str
"(format [<port>] <format-string> [<arg>...])
  - <port> is #t, #f or an output-port
  - any escape sequence is case insensitive

SEQ   MNEMONIC        DESCRIPTION
~H    [Help]          output this text
~A    [Any]           (display arg) for humans
~S    [Slashified]    (write arg) for parsers
~W    [WriteCircular] like ~s but outputs with write/ss
~~    [Tilde]         output a tilde
~T    [Tab]           output a tab character
~%    [Newline]       output a newline character
~&    [Freshline]     output a newline if the previous output was not a newline
~D    [Decimal]       the arg is a number which is output in decimal radix
~X    [heXadecimal]   the arg is a number which is output in hexdecimal radix
~O    [Octal]         the arg is a number which is output in octal radix
~B    [Binary]        the arg is a number which is output in binary radix
~F
~wF   [Fixed]         the arg is a string or number which has width w and
~w,dF                 d digits after the decimal
~C    [Character]     charater arg is output by write-char
~_    [Space]         a single space character is output
~Y    [Yuppify]       the list arg is pretty-printed to the output
~?    [Indirection]   recursive format: next 2 args are format-string and list
                      of arguments
~K    [Indirection]   same as ~?
")
(if (not testing-format+?)
    (begin
      (assert-error  (tn) (lambda () (format "~h" #t)))
      (assert-error  (tn) (lambda () (format "~h" 0)))
      (assert-error  (tn) (lambda () (format "~h" #\a)))
      (assert-error  (tn) (lambda () (format "~h" "aBc")))
      (assert-error  (tn) (lambda () (format "~h" '(0 1))))
      (assert-error  (tn) (lambda () (format "~h" '#(0 1))))
      (assert-error  (tn) (lambda () (format "~1h")))
      (assert-equal? (tn) help-str (format "~h"))
      (assert-equal? (tn) help-str (format "~H"))))

(if (not testing-format+?)
    (total-report))
