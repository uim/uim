;;  Filename : test-number-io.scm
;;  About    : unit test for R5RS number<->string conversion procedures
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

(define tn test-name)


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
(tn "number->string invalid forms")
(assert-error  (tn) (lambda () (number->string)))
(assert-error  (tn) (lambda () (number->string "0")))
(assert-error  (tn) (lambda () (number->string #\0)))
(assert-error  (tn) (lambda () (number->string #f)))
(assert-error  (tn) (lambda () (number->string '())))
(assert-error  (tn) (lambda () (number->string "0" 2)))
(assert-error  (tn) (lambda () (number->string 0 "2")))
(assert-error  (tn) (lambda () (number->string 0 2 #f)))
(tn "number->string implicit decimal")
(assert-equal? (tn) "-100" (number->string -100))
(assert-equal? (tn) "-10"  (number->string -10))
(assert-equal? (tn) "-1"   (number->string -1))
(assert-equal? (tn) "0"    (number->string 0))
(assert-equal? (tn) "1"    (number->string 1))
(assert-equal? (tn) "10"   (number->string 10))
(assert-equal? (tn) "100"  (number->string 100))
(if (>= fixnum-bits 60)
    (begin
      (tn "number->string implicit decimal 64-bit")
      (string-eval "(assert-equal? (tn)  \"956397711204\"  (number->string  956397711204))")
      (string-eval "(assert-equal? (tn) \"-956397711204\"  (number->string -956397711204))")))
(tn "number->string explicit decimal")
(assert-equal? (tn) "-100" (number->string -100 10))
(assert-equal? (tn) "-10"  (number->string -10  10))
(assert-equal? (tn) "-1"   (number->string -1   10))
(assert-equal? (tn) "0"    (number->string 0    10))
(assert-equal? (tn) "1"    (number->string 1    10))
(assert-equal? (tn) "10"   (number->string 10   10))
(assert-equal? (tn) "100"  (number->string 100  10))
(if (>= fixnum-bits 60)
    (begin
      (tn "number->string explicit decimal 64-bit")
      (string-eval "(assert-equal? (tn)  \"956397711204\"  (number->string  956397711204 10))")
      (string-eval "(assert-equal? (tn) \"-956397711204\"  (number->string -956397711204 10))")))
(tn "number->string hexadecimal")
(assert-equal? (tn) "-64"  (number->string -100 16))
(assert-equal? (tn) "-a"   (number->string -10  16))
(assert-equal? (tn) "-1"   (number->string -1   16))
(assert-equal? (tn) "0"    (number->string 0    16))
(assert-equal? (tn) "1"    (number->string 1    16))
(assert-equal? (tn) "a"    (number->string 10   16))
(assert-equal? (tn) "64"   (number->string 100  16))
(if (>= fixnum-bits 60)
    (begin
      (tn "number->string hexadecimal 64-bit")
      (string-eval "(assert-equal? (tn)  \"deadbeef64\"  (number->string  956397711204 16))")
      (string-eval "(assert-equal? (tn) \"-deadbeef64\"  (number->string -956397711204 16))")))
(tn "number->string octal")
(assert-equal? (tn) "-144" (number->string -100 8))
(assert-equal? (tn) "-12"  (number->string -10  8))
(assert-equal? (tn) "-1"   (number->string -1   8))
(assert-equal? (tn) "0"    (number->string 0    8))
(assert-equal? (tn) "1"    (number->string 1    8))
(assert-equal? (tn) "12"   (number->string 10   8))
(assert-equal? (tn) "144"  (number->string 100  8))
(if (>= fixnum-bits 60)
    (begin
      (tn "number->string octal 64-bit")
      (string-eval "(assert-equal? (tn)  \"15725557567544\"  (number->string  956397711204 8))")
      (string-eval "(assert-equal? (tn) \"-15725557567544\"  (number->string -956397711204 8))")))
(tn "number->string binary")
(assert-equal? (tn) "-1100100" (number->string -100 2))
(assert-equal? (tn) "-1010"    (number->string -10  2))
(assert-equal? (tn) "-1"       (number->string -1   2))
(assert-equal? (tn) "0"        (number->string 0    2))
(assert-equal? (tn) "1"        (number->string 1    2))
(assert-equal? (tn) "1010"     (number->string 10   2))
(assert-equal? (tn) "1100100"  (number->string 100  2))
(if (>= fixnum-bits 60)
    (begin
      (tn "number->string binary 64-bit")
      (string-eval "(assert-equal? (tn)  \"1101111010101101101111101110111101100100\"  (number->string  956397711204 2))")
      (string-eval "(assert-equal? (tn) \"-1101111010101101101111101110111101100100\"  (number->string -956397711204 2))")))
(tn "number->string boundary numbers")
(if (and (symbol-bound? 'greatest-fixnum)
         (symbol-bound? 'least-fixnum))
    (case fixnum-bits
      ((28)
       (assert-equal? (tn)
                      "134217727"
                      (number->string (greatest-fixnum)))
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

      ((32)
       (assert-equal? (tn)
                      "2147483647"
                      (number->string (greatest-fixnum)))
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

      ((60)
       (assert-equal? (tn)
                      "576460752303423487"
                      (number->string (greatest-fixnum)))
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

      ((64)
       (assert-equal? (tn)
                      "9223372036854775807"
                      (number->string (greatest-fixnum)))
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
       (assert-fail (tn) "unknown int bitwidth"))))


;;
;; string->number
;;

(tn "string->number invalid radix")
(assert-error  (tn) (lambda () (string->number "0" -16)))
(assert-error  (tn) (lambda () (string->number "0" -10)))
(assert-error  (tn) (lambda () (string->number "0" -8)))
(assert-error  (tn) (lambda () (string->number "0" -2)))
(assert-error  (tn) (lambda () (string->number "0" -1)))
(assert-error  (tn) (lambda () (string->number "0" 0)))
(assert-error  (tn) (lambda () (string->number "0" 1)))
(assert-error  (tn) (lambda () (string->number "0" 3)))
(assert-error  (tn) (lambda () (string->number "0" 4)))
(assert-error  (tn) (lambda () (string->number "0" 7)))
(assert-error  (tn) (lambda () (string->number "0" 9)))
(assert-error  (tn) (lambda () (string->number "0" 11)))
(assert-error  (tn) (lambda () (string->number "0" 15)))
(assert-error  (tn) (lambda () (string->number "0" 17)))
(tn "string->number invalid forms")
(assert-error  (tn) (lambda () (string->number)))
(assert-error  (tn) (lambda () (string->number 0)))
(assert-error  (tn) (lambda () (string->number 0 2)))
(assert-error  (tn) (lambda () (string->number "0" "2")))
(assert-error  (tn) (lambda () (string->number "0" 2 #f)))
(tn "string->number invalid strings")
(assert-eq?    (tn) #f  (string->number ""))
(assert-eq?    (tn) #f  (string->number "a"))
(assert-eq?    (tn) #f  (string->number "a" 10))
(assert-eq?    (tn) #f  (string->number "g" 16))
(assert-eq?    (tn) #f  (string->number "0xf" 16))
(assert-eq?    (tn) #f  (string->number "8" 8))
(assert-eq?    (tn) #f  (string->number "2" 2))
(assert-eq?    (tn) #f  (string->number "- 9"))
(assert-eq?    (tn) #f  (string->number "- 9" 10))
(assert-eq?    (tn) #f  (string->number "- f" 16))
(assert-eq?    (tn) #f  (string->number "- 0xf" 16))
(assert-eq?    (tn) #f  (string->number "- 7" 8))
(assert-eq?    (tn) #f  (string->number "- 1" 2))
(assert-eq?    (tn) #f  (string->number "-a"))
(assert-eq?    (tn) #f  (string->number "-a" 10))
(assert-eq?    (tn) #f  (string->number "-g" 16))
(assert-eq?    (tn) #f  (string->number "-0xf" 16))
(assert-eq?    (tn) #f  (string->number "-8" 8))
(assert-eq?    (tn) #f  (string->number "-2" 2))
(assert-eq?    (tn) #f  (string->number "+a"))
(assert-eq?    (tn) #f  (string->number "+a" 10))
(assert-eq?    (tn) #f  (string->number "+g" 16))
(assert-eq?    (tn) #f  (string->number "+0xf" 16))
(assert-eq?    (tn) #f  (string->number "+8" 8))
(assert-eq?    (tn) #f  (string->number "+2" 2))
(assert-eq?    (tn) #f  (string->number " 1"))
(assert-eq?    (tn) #f  (string->number " -1"))
(assert-eq?    (tn) #f  (string->number " +1"))
(assert-eq?    (tn) #f  (string->number " 01"))
(assert-eq?    (tn) #f  (string->number "1 "))
(assert-eq?    (tn) #f  (string->number "-"))
(assert-eq?    (tn) #f  (string->number "+"))
(assert-eq?    (tn) #f  (string->number "+-0"))
(assert-eq?    (tn) #f  (string->number "-+0"))
(assert-eq?    (tn) #f  (string->number "++0"))
(assert-eq?    (tn) #f  (string->number "--0"))
(tn "string->number implicit decimal")
(assert-equal? (tn) -100 (string->number "-100"))
(assert-equal? (tn) -10  (string->number "-10"))
(assert-equal? (tn) -1   (string->number "-1"))
(assert-equal? (tn) 0    (string->number "-0"))
(assert-equal? (tn) 0    (string->number  "0"))
(assert-equal? (tn) 0    (string->number "+0"))
(assert-equal? (tn) 1    (string->number  "1"))
(assert-equal? (tn) 1    (string->number "+1"))
(assert-equal? (tn) 10   (string->number  "10"))
(assert-equal? (tn) 10   (string->number "+10"))
(assert-equal? (tn) 100  (string->number  "100"))
(assert-equal? (tn) 100  (string->number "+100"))
(assert-equal? (tn)  238975 (string->number  "0238975"))
(assert-equal? (tn)  238975 (string->number   "238975"))
(assert-equal? (tn)  238975 (string->number  "+238975"))
(assert-equal? (tn)  238975 (string->number "+0238975"))
(assert-equal? (tn) -238975 (string->number  "-238975"))
(assert-equal? (tn) -238975 (string->number "-0238975"))
(if (>= fixnum-bits 60)
    (begin
      (tn "string->number implicit decimal 64-bit")
      (string-eval "(assert-equal? (tn)  956397711204 (string->number  \"0956397711204\"))")
      (string-eval "(assert-equal? (tn)  956397711204 (string->number   \"956397711204\"))")
      (string-eval "(assert-equal? (tn)  956397711204 (string->number  \"+956397711204\"))")
      (string-eval "(assert-equal? (tn)  956397711204 (string->number \"+0956397711204\"))")
      (string-eval "(assert-equal? (tn) -956397711204 (string->number  \"-956397711204\"))")
      (string-eval "(assert-equal? (tn) -956397711204 (string->number \"-0956397711204\"))")))
(tn "string->number explicit decimal")
(assert-equal? (tn) -100 (string->number "-100" 10))
(assert-equal? (tn) -10  (string->number "-10"  10))
(assert-equal? (tn) -1   (string->number "-1"   10))
(assert-equal? (tn) 0    (string->number "-0"   10))
(assert-equal? (tn) 0    (string->number  "0"   10))
(assert-equal? (tn) 0    (string->number "+0"   10))
(assert-equal? (tn) 1    (string->number  "1"   10))
(assert-equal? (tn) 1    (string->number "+1"   10))
(assert-equal? (tn) 10   (string->number  "10"  10))
(assert-equal? (tn) 10   (string->number "+10"  10))
(assert-equal? (tn) 100  (string->number  "100" 10))
(assert-equal? (tn) 100  (string->number "+100" 10))
(assert-equal? (tn)  238975 (string->number  "0238975" 10))
(assert-equal? (tn)  238975 (string->number   "238975" 10))
(assert-equal? (tn)  238975 (string->number  "+238975" 10))
(assert-equal? (tn)  238975 (string->number "+0238975" 10))
(assert-equal? (tn) -238975 (string->number  "-238975" 10))
(assert-equal? (tn) -238975 (string->number "-0238975" 10))
(if (>= fixnum-bits 60)
    (begin
      (tn "string->number explicit decimal 64-bit")
      (string-eval "(assert-equal? (tn)  956397711204 (string->number  \"0956397711204\" 10))")
      (string-eval "(assert-equal? (tn)  956397711204 (string->number   \"956397711204\" 10))")
      (string-eval "(assert-equal? (tn)  956397711204 (string->number  \"+956397711204\" 10))")
      (string-eval "(assert-equal? (tn)  956397711204 (string->number \"+0956397711204\" 10))")
      (string-eval "(assert-equal? (tn) -956397711204 (string->number  \"-956397711204\" 10))")
      (string-eval "(assert-equal? (tn) -956397711204 (string->number \"-0956397711204\" 10))")))
(tn "string->number hexadecimal")
(assert-equal? (tn) -256 (string->number "-100" 16))
(assert-equal? (tn) -16  (string->number "-10"  16))
(assert-equal? (tn) -1   (string->number "-1"   16))
(assert-equal? (tn) 0    (string->number "-0"   16))
(assert-equal? (tn) 0    (string->number  "0"   16))
(assert-equal? (tn) 0    (string->number "+0"   16))
(assert-equal? (tn) 1    (string->number  "1"   16))
(assert-equal? (tn) 1    (string->number "+1"   16))
(assert-equal? (tn) 16   (string->number  "10"  16))
(assert-equal? (tn) 16   (string->number "+10"  16))
(assert-equal? (tn) 256  (string->number  "100" 16))
(assert-equal? (tn) 256  (string->number "+100" 16))
(assert-equal? (tn) -10  (string->number "-a"   16))
(assert-equal? (tn) -10  (string->number "-A"   16))
(assert-equal? (tn) 10   (string->number  "a"   16))
(assert-equal? (tn) 10   (string->number "+a"   16))
(assert-equal? (tn) 10   (string->number  "A"   16))
(assert-equal? (tn) 10   (string->number "+A"   16))
(assert-equal? (tn)  14593330 (string->number  "0deAd32" 16))
(assert-equal? (tn)  14593330 (string->number   "deAd32" 16))
(assert-equal? (tn)  14593330 (string->number  "+dEad32" 16))
(assert-equal? (tn)  14593330 (string->number "+0dEad32" 16))
(assert-equal? (tn) -14593330 (string->number  "-deaD32" 16))
(assert-equal? (tn) -14593330 (string->number "-0deaD32" 16))
(assert-equal? (tn)   3333805 (string->number   "32deAd" 16))
(assert-equal? (tn)   3333805 (string->number  "+32dEad" 16))
(assert-equal? (tn)  -3333805 (string->number  "-32deaD" 16))
(if (>= fixnum-bits 60)
    (begin
      (tn "string->number hexadecimal 64-bit")
      (string-eval "(assert-equal? (tn)  956397711204 (string->number  \"0deAdbeef64\" 16))")
      (string-eval "(assert-equal? (tn)  956397711204 (string->number   \"deAdbeef64\" 16))")
      (string-eval "(assert-equal? (tn)  956397711204 (string->number  \"+dEadBeef64\" 16))")
      (string-eval "(assert-equal? (tn)  956397711204 (string->number \"+0dEadBeef64\" 16))")
      (string-eval "(assert-equal? (tn) -956397711204 (string->number  \"-dEadBeef64\" 16))")
      (string-eval "(assert-equal? (tn) -956397711204 (string->number \"-0deaDbeeF64\" 16))")))
(tn "string->number octal")
(assert-equal? (tn) -64 (string->number "-100" 8))
(assert-equal? (tn) -8  (string->number "-10"  8))
(assert-equal? (tn) -1  (string->number "-1"   8))
(assert-equal? (tn) 0   (string->number "-0"   8))
(assert-equal? (tn) 0   (string->number  "0"   8))
(assert-equal? (tn) 0   (string->number "+0"   8))
(assert-equal? (tn) 1   (string->number  "1"   8))
(assert-equal? (tn) 1   (string->number "+1"   8))
(assert-equal? (tn) 8   (string->number  "10"  8))
(assert-equal? (tn) 8   (string->number "+10"  8))
(assert-equal? (tn) 64  (string->number  "100" 8))
(assert-equal? (tn) 64  (string->number "+100" 8))
(assert-equal? (tn)  1556392 (string->number  "05737650" 8))
(assert-equal? (tn)  1556392 (string->number   "5737650" 8))
(assert-equal? (tn) +1556392 (string->number  "+5737650" 8))
(assert-equal? (tn) +1556392 (string->number "+05737650" 8))
(assert-equal? (tn) -1556392 (string->number  "-5737650" 8))
(assert-equal? (tn) -1556392 (string->number "-05737650" 8))
(if (>= fixnum-bits 60)
    (begin
      (tn "string->number octal 64-bit")
      (string-eval "(assert-equal? (tn)  956397711204 (string->number  \"015725557567544\" 8))")
      (string-eval "(assert-equal? (tn)  956397711204 (string->number   \"15725557567544\" 8))")
      (string-eval "(assert-equal? (tn)  956397711204 (string->number  \"+15725557567544\" 8))")
      (string-eval "(assert-equal? (tn)  956397711204 (string->number \"+015725557567544\" 8))")
      (string-eval "(assert-equal? (tn) -956397711204 (string->number  \"-15725557567544\" 8))")
      (string-eval "(assert-equal? (tn) -956397711204 (string->number \"-015725557567544\" 8))")))
(tn "string->number binary")
(assert-equal? (tn) -4  (string->number "-100" 2))
(assert-equal? (tn) -2  (string->number "-10"  2))
(assert-equal? (tn) -1  (string->number "-1"   2))
(assert-equal? (tn) 0   (string->number "-0"   2))
(assert-equal? (tn) 0   (string->number  "0"   2))
(assert-equal? (tn) 0   (string->number "+0"   2))
(assert-equal? (tn) 1   (string->number  "1"   2))
(assert-equal? (tn) 1   (string->number "+1"   2))
(assert-equal? (tn) 2   (string->number  "10"  2))
(assert-equal? (tn) 2   (string->number "+10"  2))
(assert-equal? (tn) 4   (string->number  "100" 2))
(assert-equal? (tn) 4   (string->number "+100" 2))
(assert-equal? (tn)  2990842 (string->number  "01011011010001011111010" 2))
(assert-equal? (tn)  2990842 (string->number   "1011011010001011111010" 2))
(assert-equal? (tn) +2990842 (string->number  "+1011011010001011111010" 2))
(assert-equal? (tn) +2990842 (string->number "+01011011010001011111010" 2))
(assert-equal? (tn) -2990842 (string->number  "-1011011010001011111010" 2))
(assert-equal? (tn) -2990842 (string->number "-01011011010001011111010" 2))
(if (>= fixnum-bits 60)
    (begin
      (tn "string->number binary 64-bit")
      (string-eval "(assert-equal? (tn)  956397711204 (string->number  \"01101111010101101101111101110111101100100\" 2))")
      (string-eval "(assert-equal? (tn)  956397711204 (string->number   \"1101111010101101101111101110111101100100\" 2))")
      (string-eval "(assert-equal? (tn)  956397711204 (string->number  \"+1101111010101101101111101110111101100100\" 2))")
      (string-eval "(assert-equal? (tn)  956397711204 (string->number \"+01101111010101101101111101110111101100100\" 2))")
      (string-eval "(assert-equal? (tn) -956397711204 (string->number  \"-1101111010101101101111101110111101100100\" 2))")
      (string-eval "(assert-equal? (tn) -956397711204 (string->number \"-01101111010101101101111101110111101100100\" 2))")))
(tn "string->number boundary numbers")
(case fixnum-bits
  ((28)
   (string-eval
    "(assert-equal? (tn)  134217727 (string->number  \"134217727\"))")
   (string-eval
    "(assert-equal? (tn) -134217728 (string->number \"-134217728\"))")
   (assert-error  (tn) (lambda () (string->number  "134217728")))
   (assert-error  (tn) (lambda () (string->number "-134217729"))))
  ((32)
   (string-eval
    "(assert-equal? (tn)  2147483647 (string->number  \"2147483647\"))")
   (string-eval
    "(assert-equal? (tn) -2147483648 (string->number \"-2147483648\"))")
   (assert-error  (tn) (lambda () (string->number   "2147483648")))
   (assert-error  (tn) (lambda () (string->number  "-2147483649"))))
  ((60)
   (string-eval
    "(assert-equal? (tn)
                    576460752303423487
                    (string->number \"576460752303423487\"))")
   (string-eval
    "(assert-equal? (tn)
                    -576460752303423488
                    (string->number \"-576460752303423488\")))")
   (assert-error  (tn) (lambda () (string->number  "576460752303423488")))
   (assert-error  (tn) (lambda () (string->number "-576460752303423489"))))
  ((64)
   (string-eval
    "(assert-equal? (tn)
                    9223372036854775807
                    (string->number \"9223372036854775807\"))")
   (string-eval
    "(assert-equal? (tn)
                    -9223372036854775808
                    (string->number \"-9223372036854775808\")))")
   (assert-error  (tn) (lambda () (string->number  "9223372036854775808")))
   (assert-error  (tn) (lambda () (string->number "-9223372036854775809"))))
  (else
   (assert-fail (tn) "unknown int bitwidth")))


(total-report)
