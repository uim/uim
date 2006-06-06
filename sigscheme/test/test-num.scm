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

; check =
(assert-true  "= #1"  (= 1 1))
(assert-false "= #2"  (= 1 2))
(assert-true  "= #3"  (= 1 1 1))
(assert-false "= #4"  (= 1 2 1))
(assert-false "= #5"  (= 1 1 2))
(assert-true  "= #6"  (= -1 -1))
(assert-false "= #7"  (= -1 -2))
(assert-true  "= #8"  (= -1 -1 -1))
(assert-false "= #9"  (= -1 -2 -1))
(assert-false "= #10" (= -1 -1 -2))
(assert-false "= #11" (= -1 1))
(assert-false "= #12" (= 1 -1))

; check >
(assert-true  "> #1"  (> 1 0))
(assert-false "> #2"  (> 1 1))
(assert-false "> #3"  (> 1 2))
(assert-false "> #4"  (> 1 0 0))
(assert-true  "> #5"  (> 1 0 -1))
(assert-true  "> #6"  (> 1 0 -1))
(assert-false "> #7"  (> 1 0 1))
(assert-false "> #8"  (> 1 1 0))
(assert-false "> #9"  (> 1 1 1))
(assert-false "> #10" (> 1 2 1))
(assert-false "> #11" (> 1 2 0))
(assert-true  "> #12" (> 1 -1))
(assert-true  "> #13" (> 1 0 -1))
(assert-true  "> #14" (> -1 -2))
(assert-true  "> #15" (> -1 -2 -3))
(assert-false "> #16" (> -2 -1))
(assert-false "> #17" (> -3 -2 -1))

; check <
(assert-true  "< #1"  (< 0 1))
(assert-false "< #2"  (< 1 1))
(assert-false "< #3"  (< 2 1))
(assert-false "< #4"  (< 0 0 1))
(assert-true  "< #5"  (< -1 0 1))
(assert-true  "< #6"  (< -1 0 1))
(assert-false "< #7"  (< 1 0 1))
(assert-false "< #8"  (< 1 0 1))
(assert-false "< #9"  (< 1 1 1))
(assert-false "< #10" (< 1 2 1))
(assert-false "< #11" (< 0 2 1))
(assert-true  "< #12" (< -1 1))
(assert-true  "< #13" (< -1 0 1))
(assert-true  "< #14" (< -2 -1))
(assert-true  "< #15" (< -3 -2 -1))
(assert-false "< #16" (< -1 -2))
(assert-false "< #17" (< -1 -2 -3))

; check >=
(assert-true  ">= #1"  (>= 1 0))
(assert-true  ">= #2"  (>= 1 1))
(assert-false ">= #3"  (>= 1 2))
(assert-true  ">= #4"  (>= 1 0 0))
(assert-true  ">= #5"  (>= 1 0 -1))
(assert-true  ">= #6"  (>= 1 0 -1))
(assert-false ">= #7"  (>= 1 0 1))
(assert-true  ">= #8"  (>= 1 1 0))
(assert-true  ">= #9"  (>= 1 1 1))
(assert-false ">= #10" (>= 1 2 1))
(assert-false ">= #11" (>= 1 2 0))
(assert-true  ">= #12" (>= 1 -1))
(assert-true  ">= #13" (>= 1 0 -1))
(assert-true  ">= #14" (>= -1 -2))
(assert-true  ">= #15" (>= -1 -2 -3))
(assert-false ">= #16" (>= -2 -1))
(assert-false ">= #17" (>= -3 -2 -1))
(assert-true  ">= #18" (>= -1 -1 -2))

; check <=
(assert-true  "<= #1"  (<= 0 1))
(assert-true  "<= #2"  (<= 1 1))
(assert-false "<= #3"  (<= 2 1))
(assert-true  "<= #4"  (<= 0 0 1))
(assert-true  "<= #5"  (<= -1 0 1))
(assert-true  "<= #6"  (<= -1 0 1))
(assert-false "<= #7"  (<= 1 0 1))
(assert-false "<= #8"  (<= 1 0 1))
(assert-true  "<= #9"  (<= 1 1 1))
(assert-false "<= #10" (<= 1 2 1))
(assert-false "<= #11" (<= 0 2 1))
(assert-true  "<= #12" (<= -1 1))
(assert-true  "<= #13" (<= -1 0 1))
(assert-true  "<= #14" (<= -2 -1))
(assert-true  "<= #15" (<= -3 -2 -1))
(assert-false "<= #16" (<= -1 -2))
(assert-false "<= #17" (<= -1 -2 -3))
(assert-true  "<= #18" (<= -2 -1 -1))

; check max
(assert-equal? "max test1" 0 (max 0))
(assert-equal? "max test2" 1 (max 0 1))
(assert-equal? "max test3" 2 (max 0 2 1))
(assert-equal? "max test4" 7 (max -1 -10 7))

; check min
(assert-equal? "min test1" 0 (min 0))
(assert-equal? "min test2" 0 (min 0 1))
(assert-equal? "min test3" 0 (min 2 0 1))
(assert-equal? "min test4" -10 (min -1 -10 7))

; check +
(assert-equal? "+ test1" 0  (+))
(assert-equal? "+ test2" 3  (+ 3))
(assert-equal? "+ test3" 3  (+ 1 2))
(assert-equal? "+ test4" 6  (+ 1 2 3))

; check -
(assert-equal? "- test1" -3 (- 3))
(assert-equal? "- test2" -1 (- 1 2))
(assert-equal? "- test3" -4 (- 1 2 3))

; check *
(assert-equal? "* test1" 1  (*))
(assert-equal? "* test2" 2  (* 2))
(assert-equal? "* test3" 24 (* 2 3 4))

; check /
(assert-equal? "/ test1" 0  (/ 1 2))
(assert-equal? "/ test2" -1 (/ -2 2))

; check abs
(assert-equal? "abs test1" 7 (abs -7))
(assert-equal? "abs test2" 7 (abs 7))
(assert-equal? "abs test3" 0 (abs 0))

; check quotient
(assert-equal? "quotient test1" 0  (/ 1 2))
(assert-equal? "quotient test2" -1 (/ -2 2))

; check modulo
(assert-equal? "modulo test1" 1 (modulo 13 4))
(assert-equal? "modulo test2" 3 (modulo -13 4))
(assert-equal? "modulo test3" -3 (modulo 13 -4))
(assert-equal? "modulo test4" -1 (modulo -13 -4))

; check remainder
(assert-equal? "remainder test1" 1 (remainder 13 4))
(assert-equal? "remainder test2" -1 (remainder -13 4))
(assert-equal? "remainder test3" 1 (remainder 13 -4))
(assert-equal? "remainder test4" -1 (remainder -13 -4))

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


; check string->number
(assert-equal? "string->number test1"  1   (string->number "1"))
(assert-equal? "string->number test2"  10  (string->number "10"))
(assert-equal? "string->number test3"  100 (string->number "100"))
(assert-equal? "string->number test4"  1   (string->number "1"   2))
(assert-equal? "string->number test5"  2   (string->number "10"  2))
(assert-equal? "string->number test6"  4   (string->number "100" 2))
(assert-equal? "string->number test7"  1   (string->number "1"   8))
(assert-equal? "string->number test8"  8   (string->number "10"  8))
(assert-equal? "string->number test9"  64  (string->number "100" 8))
(assert-equal? "string->number test10" 1   (string->number "1"   10))
(assert-equal? "string->number test11" 10  (string->number "10"  10))
(assert-equal? "string->number test12" 100 (string->number "100" 10))
(assert-equal? "string->number test13" 1   (string->number "1"   16))
(assert-equal? "string->number test14" 16  (string->number "10"  16))
(assert-equal? "string->number test15" 256 (string->number "100" 16))

; numbers in various radices
(assert-true "binary number test1" (= #b1111 15))
(assert-true "binary number test2" (= #b010  2))
(assert-true "binary number test3" (= #b0    0))
(assert-true "binary number test4" (= #b-1   -1))
(assert-true "binary number test5" (= #b-10  -2))
(assert-true "binary number test6" (= #b-010 -2))
(assert-true "octal number test1"  (= #o077  63))
(assert-true "octal number test2"  (= #o361  241))
(assert-true "decimal number test1" (= #d3900 3900))
(assert-true "decimal number test2" (= #d18782 18782))
(assert-true "hexadecimal test1" (= #xffff 65535))
(assert-true "hexadecimal test2" (= #x0A7b 2683))

(total-report)
