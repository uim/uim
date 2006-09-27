;;  Filename : test-num.scm
;;  About    : unit test for R5RS numbers
;;
;;  Copyright (C) 2005-2006 Kazuki Ohta <mover AT hct.zaq.ne.jp>
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

(use srfi-23)

(define tn test-name)

(tn "=")
(assert-true  (tn) (= 1 1))
(assert-false (tn) (= 1 2))
(assert-true  (tn) (= 1 1 1))
(assert-false (tn) (= 1 2 1))
(assert-false (tn) (= 1 1 2))
(assert-true  (tn) (= -1 -1))
(assert-false (tn) (= -1 -2))
(assert-true  (tn) (= -1 -1 -1))
(assert-false (tn) (= -1 -2 -1))
(assert-false (tn) (= -1 -1 -2))
(assert-false (tn) (= -1 1))
(assert-false (tn) (= 1 -1))

(tn ">")
(assert-true  (tn) (> 1 0))
(assert-false (tn) (> 1 1))
(assert-false (tn) (> 1 2))
(assert-false (tn) (> 1 0 0))
(assert-true  (tn) (> 1 0 -1))
(assert-true  (tn) (> 1 0 -1))
(assert-false (tn) (> 1 0 1))
(assert-false (tn) (> 1 1 0))
(assert-false (tn) (> 1 1 1))
(assert-false (tn) (> 1 2 1))
(assert-false (tn) (> 1 2 0))
(assert-true  (tn) (> 1 -1))
(assert-true  (tn) (> 1 0 -1))
(assert-true  (tn) (> -1 -2))
(assert-true  (tn) (> -1 -2 -3))
(assert-false (tn) (> -2 -1))
(assert-false (tn) (> -3 -2 -1))

(tn "<")
(assert-true  (tn) (< 0 1))
(assert-false (tn) (< 1 1))
(assert-false (tn) (< 2 1))
(assert-false (tn) (< 0 0 1))
(assert-true  (tn) (< -1 0 1))
(assert-true  (tn) (< -1 0 1))
(assert-false (tn) (< 1 0 1))
(assert-false (tn) (< 1 0 1))
(assert-false (tn) (< 1 1 1))
(assert-false (tn) (< 1 2 1))
(assert-false (tn) (< 0 2 1))
(assert-true  (tn) (< -1 1))
(assert-true  (tn) (< -1 0 1))
(assert-true  (tn) (< -2 -1))
(assert-true  (tn) (< -3 -2 -1))
(assert-false (tn) (< -1 -2))
(assert-false (tn) (< -1 -2 -3))

(tn ">=")
(assert-true  (tn) (>= 1 0))
(assert-true  (tn) (>= 1 1))
(assert-false (tn) (>= 1 2))
(assert-true  (tn) (>= 1 0 0))
(assert-true  (tn) (>= 1 0 -1))
(assert-true  (tn) (>= 1 0 -1))
(assert-false (tn) (>= 1 0 1))
(assert-true  (tn) (>= 1 1 0))
(assert-true  (tn) (>= 1 1 1))
(assert-false (tn) (>= 1 2 1))
(assert-false (tn) (>= 1 2 0))
(assert-true  (tn) (>= 1 -1))
(assert-true  (tn) (>= 1 0 -1))
(assert-true  (tn) (>= -1 -2))
(assert-true  (tn) (>= -1 -2 -3))
(assert-false (tn) (>= -2 -1))
(assert-false (tn) (>= -3 -2 -1))
(assert-true  (tn) (>= -1 -1 -2))

(tn "<=")
(assert-true  (tn) (<= 0 1))
(assert-true  (tn) (<= 1 1))
(assert-false (tn) (<= 2 1))
(assert-true  (tn) (<= 0 0 1))
(assert-true  (tn) (<= -1 0 1))
(assert-true  (tn) (<= -1 0 1))
(assert-false (tn) (<= 1 0 1))
(assert-false (tn) (<= 1 0 1))
(assert-true  (tn) (<= 1 1 1))
(assert-false (tn) (<= 1 2 1))
(assert-false (tn) (<= 0 2 1))
(assert-true  (tn) (<= -1 1))
(assert-true  (tn) (<= -1 0 1))
(assert-true  (tn) (<= -2 -1))
(assert-true  (tn) (<= -3 -2 -1))
(assert-false (tn) (<= -1 -2))
(assert-false (tn) (<= -1 -2 -3))
(assert-true  (tn) (<= -2 -1 -1))

(tn "max")
(assert-equal? (tn) 0 (max 0))
(assert-equal? (tn) 1 (max 0 1))
(assert-equal? (tn) 2 (max 0 2 1))
(assert-equal? (tn) 7 (max -1 -10 7))

(tn "min")
(assert-equal? (tn) 0   (min 0))
(assert-equal? (tn) 0   (min 0 1))
(assert-equal? (tn) 0   (min 2 0 1))
(assert-equal? (tn) -10 (min -1 -10 7))

(tn "+")
(assert-equal? (tn) 0  (+))
(assert-equal? (tn) 3  (+ 3))
(assert-equal? (tn) 3  (+ 1 2))
(assert-equal? (tn) 6  (+ 1 2 3))

(tn "-")
(assert-equal? (tn) -3 (- 3))
(assert-equal? (tn) -1 (- 1 2))
(assert-equal? (tn) -4 (- 1 2 3))

(tn "*")
(assert-equal? (tn) 1  (*))
(assert-equal? (tn) 2  (* 2))
(assert-equal? (tn) 24 (* 2 3 4))

(tn "/")
(assert-equal? (tn) 0  (/ 1 2))
(assert-equal? (tn) -1 (/ -2 2))

(tn "abs")
(assert-equal? (tn) 7 (abs -7))
(assert-equal? (tn) 7 (abs 7))
(assert-equal? (tn) 0 (abs 0))

(tn "quotient")
(assert-equal? (tn) 0  (/ 1 2))
(assert-equal? (tn) -1 (/ -2 2))

(tn "modulo")
(assert-equal? (tn) 1  (modulo 13 4))
(assert-equal? (tn) 3  (modulo -13 4))
(assert-equal? (tn) -3 (modulo 13 -4))
(assert-equal? (tn) -1 (modulo -13 -4))

(tn "remainder")
(assert-equal? (tn) 1  (remainder 13 4))
(assert-equal? (tn) -1 (remainder -13 4))
(assert-equal? (tn) 1  (remainder 13 -4))
(assert-equal? (tn) -1 (remainder -13 -4))

;;
;; number->string
;;
(tn "number->string invalid radix")
(assert-error  (tn) (lambda () (number->string 0 -16)))
(assert-error  (tn) (lambda () (number->string 0 -10)))
(assert-error  (tn) (lambda () (number->string 0 -8)))
(assert-error  (tn) (lambda () (number->string 0 -2)))
(assert-error  (tn) (lambda () (number->string 0 -1)))
(assert-error  (tn) (lambda () (number->string 0 0)))
(assert-error  (tn) (lambda () (number->string 0 1)))
(assert-error  (tn) (lambda () (number->string 0 3)))
(assert-error  (tn) (lambda () (number->string 0 4)))
(assert-error  (tn) (lambda () (number->string 0 7)))
(assert-error  (tn) (lambda () (number->string 0 9)))
(assert-error  (tn) (lambda () (number->string 0 11)))
(assert-error  (tn) (lambda () (number->string 0 15)))
(assert-error  (tn) (lambda () (number->string 0 17)))
(tn "number->string implicit decimal")
(assert-equal? (tn) "-100" (number->string -100))
(assert-equal? (tn) "-10"  (number->string -10))
(assert-equal? (tn) "-1"   (number->string -1))
(assert-equal? (tn) "0"    (number->string 0))
(assert-equal? (tn) "1"    (number->string 1))
(assert-equal? (tn) "10"   (number->string 10))
(assert-equal? (tn) "100"  (number->string 100))
(tn "number->string explicit decimal")
(assert-equal? (tn) "-100" (number->string -100 10))
(assert-equal? (tn) "-10"  (number->string -10  10))
(assert-equal? (tn) "-1"   (number->string -1   10))
(assert-equal? (tn) "0"    (number->string 0    10))
(assert-equal? (tn) "1"    (number->string 1    10))
(assert-equal? (tn) "10"   (number->string 10   10))
(assert-equal? (tn) "100"  (number->string 100  10))
(tn "number->string hexadecimal")
(assert-equal? (tn) "-64"  (number->string -100 16))
(assert-equal? (tn) "-a"   (number->string -10  16))
(assert-equal? (tn) "-1"   (number->string -1   16))
(assert-equal? (tn) "0"    (number->string 0    16))
(assert-equal? (tn) "1"    (number->string 1    16))
(assert-equal? (tn) "a"    (number->string 10   16))
(assert-equal? (tn) "64"   (number->string 100  16))
(tn "number->string octal")
(assert-equal? (tn) "-144" (number->string -100 8))
(assert-equal? (tn) "-12"  (number->string -10  8))
(assert-equal? (tn) "-1"   (number->string -1   8))
(assert-equal? (tn) "0"    (number->string 0    8))
(assert-equal? (tn) "1"    (number->string 1    8))
(assert-equal? (tn) "12"   (number->string 10   8))
(assert-equal? (tn) "144"  (number->string 100  8))
(tn "number->string binary")
(assert-equal? (tn) "-1100100" (number->string -100 2))
(assert-equal? (tn) "-1010"    (number->string -10  2))
(assert-equal? (tn) "-1"       (number->string -1   2))
(assert-equal? (tn) "0"        (number->string 0    2))
(assert-equal? (tn) "1"        (number->string 1    2))
(assert-equal? (tn) "1010"     (number->string 10   2))
(assert-equal? (tn) "1100100"  (number->string 100  2))
(if (and (symbol-bound? 'greatest-fixnum)
         (symbol-bound? 'least-fixnum))
    (let ((greatest (number->string (greatest-fixnum))))
      (cond
       ((string=? greatest "134217727")
        (tn "number->string 28bit fixnum")
        (assert-equal? (tn)
                       "-134217728"
                       (number->string (least-fixnum)))
        (assert-equal? (tn)
                       "111111111111111111111111111"
                       (number->string (greatest-fixnum) 2))
        (assert-equal? (tn)
                       "-1000000000000000000000000000"
                       (number->string (least-fixnum) 2))
        (assert-equal? (tn)
                       "-111111111111111111111111111"
                       (number->string (+ (least-fixnum) 1) 2)))

       ((string=? greatest "2147483647")
        (tn "number->string 32bit fixnum")
        (assert-equal? (tn)
                       "-2147483648"
                       (number->string (least-fixnum)))
        (assert-equal? (tn)
                       "1111111111111111111111111111111"
                       (number->string (greatest-fixnum) 2))
        (assert-equal? (tn)
                       "-10000000000000000000000000000000"
                       (number->string (least-fixnum) 2))
        (assert-equal? (tn)
                       "-1111111111111111111111111111111"
                       (number->string (+ (least-fixnum) 1) 2)))

       ((string=? greatest "576460752303423487")
        (tn "number->string 60bit fixnum")
        (assert-equal? (tn)
                       "-576460752303423488"
                       (number->string (least-fixnum)))
        (assert-equal? (tn)
                       "11111111111111111111111111111111111111111111111111111111111"
                       (number->string (greatest-fixnum) 2))
        (assert-equal? (tn)
                       "-100000000000000000000000000000000000000000000000000000000000"
                       (number->string (least-fixnum) 2))
        (assert-equal? (tn)
                       "-11111111111111111111111111111111111111111111111111111111111"
                       (number->string (+ (least-fixnum) 1) 2)))

       ((string=? greatest "9223372036854775807")
        (tn "number->string 64bit fixnum")
        (assert-equal? (tn)
                       "-9223372036854775808"
                       (number->string (least-fixnum)))
        (assert-equal? (tn)
                       "111111111111111111111111111111111111111111111111111111111111111"
                       (number->string (greatest-fixnum) 2))
        (assert-equal? (tn)
                       "-1000000000000000000000000000000000000000000000000000000000000000"
                       (number->string (least-fixnum) 2))
        (assert-equal? (tn)
                       "-111111111111111111111111111111111111111111111111111111111111111"
                       (number->string (+ (least-fixnum) 1) 2)))

       (else
        (error "unknown int bitwidth")))))


(tn "string->number")
(assert-equal? (tn) 1   (string->number "1"))
(assert-equal? (tn) 10  (string->number "10"))
(assert-equal? (tn) 100 (string->number "100"))
(assert-equal? (tn) 1   (string->number "1"   2))
(assert-equal? (tn) 2   (string->number "10"  2))
(assert-equal? (tn) 4   (string->number "100" 2))
(assert-equal? (tn) 1   (string->number "1"   8))
(assert-equal? (tn) 8   (string->number "10"  8))
(assert-equal? (tn) 64  (string->number "100" 8))
(assert-equal? (tn) 1   (string->number "1"   10))
(assert-equal? (tn) 10  (string->number "10"  10))
(assert-equal? (tn) 100 (string->number "100" 10))
(assert-equal? (tn) 1   (string->number "1"   16))
(assert-equal? (tn) 16  (string->number "10"  16))
(assert-equal? (tn) 256 (string->number "100" 16))


(total-report)
