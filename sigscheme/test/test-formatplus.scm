;;  Filename : test-formatplus.scm
;;  About    : unit test for SigScheme-specific procedure format+
;;
;;  Copyright (C) 2006 YamaKen <yamaken AT bp.iij4u.or.jp>
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

(use srfi-48)

;; test SRFI-48 compatible part of format+
(define format format+)
(load "./test/test-srfi48.scm")
(newline)

(define tn test-name)

(tn "format+ ~d")
(assert-error  (tn) (lambda () (format+ "0128d"    1)))
(assert-error  (tn) (lambda () (format+ "0128,1d"  1)))
(assert-error  (tn) (lambda () (format+ "1,0128d"  1)))
(assert-error  (tn) (lambda () (format+ "01024d"   1)))
(assert-error  (tn) (lambda () (format+ "01024,1d" 1)))
(assert-error  (tn) (lambda () (format+ "1,01024d" 1)))
(assert-equal? (tn) "-100" (format+ "~0d" -100))
(assert-equal? (tn) "-10"  (format+ "~0d" -10))
(assert-equal? (tn) "-1"   (format+ "~0d" -1))
(assert-equal? (tn) "0"    (format+ "~0d" 0))
(assert-equal? (tn) "1"    (format+ "~0d" 1))
(assert-equal? (tn) "10"   (format+ "~0d" 10))
(assert-equal? (tn) "100"  (format+ "~0d" 100))

(assert-equal? (tn) "-100" (format+ "~03d" -100))
(assert-equal? (tn) "-10"  (format+ "~03d" -10))
(assert-equal? (tn) "-01"  (format+ "~03d" -1))
(assert-equal? (tn) "000"  (format+ "~03d" 0))
(assert-equal? (tn) "001"  (format+ "~03d" 1))
(assert-equal? (tn) "010"  (format+ "~03d" 10))
(assert-equal? (tn) "100"  (format+ "~03d" 100))

(assert-equal? (tn)
               "                                                                                                                            123"
               (format+ "~127d" 123))
(assert-equal? (tn)
               "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000123"
               (format+ "~0127d" 123))

(tn "format+ ~x")
(assert-error  (tn) (lambda () (format+ "0128x"    1)))
(assert-error  (tn) (lambda () (format+ "0128,1x"  1)))
(assert-error  (tn) (lambda () (format+ "1,0128x"  1)))
(assert-error  (tn) (lambda () (format+ "01024x"   1)))
(assert-error  (tn) (lambda () (format+ "01024,1x" 1)))
(assert-error  (tn) (lambda () (format+ "1,01024x" 1)))
(assert-equal? (tn) "-64"  (format+ "~0x" -100))
(assert-equal? (tn) "-a"   (format+ "~0x" -10))
(assert-equal? (tn) "-1"   (format+ "~0x" -1))
(assert-equal? (tn) "0"    (format+ "~0x" 0))
(assert-equal? (tn) "1"    (format+ "~0x" 1))
(assert-equal? (tn) "a"    (format+ "~0x" 10))
(assert-equal? (tn) "64"   (format+ "~0x" 100))

(assert-equal? (tn) "-64"  (format+ "~03x" -100))
(assert-equal? (tn) "-0a"  (format+ "~03x" -10))
(assert-equal? (tn) "-01"  (format+ "~03x" -1))
(assert-equal? (tn) "000"  (format+ "~03x" 0))
(assert-equal? (tn) "001"  (format+ "~03x" 1))
(assert-equal? (tn) "00a"  (format+ "~03x" 10))
(assert-equal? (tn) "064"  (format+ "~03x" 100))

(assert-equal? (tn)
               "                                                                                                                            1ac"
               (format+ "~127x" #x1ac))
(assert-equal? (tn)
               "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001ac"
               (format+ "~0127x" #x1ac))

(tn "format+ ~o")
(assert-error  (tn) (lambda () (format+ "0128o"    1)))
(assert-error  (tn) (lambda () (format+ "0128,1o"  1)))
(assert-error  (tn) (lambda () (format+ "1,0128o"  1)))
(assert-error  (tn) (lambda () (format+ "01024o"   1)))
(assert-error  (tn) (lambda () (format+ "01024,1o" 1)))
(assert-error  (tn) (lambda () (format+ "1,01024o" 1)))
(assert-equal? (tn) "-144" (format+ "~0o" -100))
(assert-equal? (tn) "-12"  (format+ "~0o" -10))
(assert-equal? (tn) "-1"   (format+ "~0o" -1))
(assert-equal? (tn) "0"    (format+ "~0o" 0))
(assert-equal? (tn) "1"    (format+ "~0o" 1))
(assert-equal? (tn) "12"   (format+ "~0o" 10))
(assert-equal? (tn) "144"  (format+ "~0o" 100))

(assert-equal? (tn) "-144" (format+ "~03o" -100))
(assert-equal? (tn) "-12"  (format+ "~03o" -10))
(assert-equal? (tn) "-01"  (format+ "~03o" -1))
(assert-equal? (tn) "000"  (format+ "~03o" 0))
(assert-equal? (tn) "001"  (format+ "~03o" 1))
(assert-equal? (tn) "012"  (format+ "~03o" 10))
(assert-equal? (tn) "144"  (format+ "~03o" 100))

(assert-equal? (tn)
               "                                                                                                                            123"
               (format+ "~127o" #o123))
(assert-equal? (tn)
               "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000123"
               (format+ "~0127o" #o123))

(tn "format+ ~b")
(assert-error  (tn) (lambda () (format+ "0128b"    1)))
(assert-error  (tn) (lambda () (format+ "0128,1b"  1)))
(assert-error  (tn) (lambda () (format+ "1,0128b"  1)))
(assert-error  (tn) (lambda () (format+ "01024b"   1)))
(assert-error  (tn) (lambda () (format+ "01024,1b" 1)))
(assert-error  (tn) (lambda () (format+ "1,01024b" 1)))
(assert-equal? (tn) "-1100100" (format+ "~0b" -100))
(assert-equal? (tn) "-1010"    (format+ "~0b" -10))
(assert-equal? (tn) "-1"       (format+ "~0b" -1))
(assert-equal? (tn) "0"        (format+ "~0b" 0))
(assert-equal? (tn) "1"        (format+ "~0b" 1))
(assert-equal? (tn) "1010"     (format+ "~0b" 10))
(assert-equal? (tn) "1100100"  (format+ "~0b" 100))

(assert-equal? (tn) "-1100100" (format+ "~05b" -100))
(assert-equal? (tn) "-1010"    (format+ "~05b" -10))
(assert-equal? (tn) "-0001"    (format+ "~05b" -1))
(assert-equal? (tn) "00000"    (format+ "~05b" 0))
(assert-equal? (tn) "00001"    (format+ "~05b" 1))
(assert-equal? (tn) "01010"    (format+ "~05b" 10))
(assert-equal? (tn) "1100100"  (format+ "~05b" 100))

(assert-equal? (tn)
               "                                                                                                                            101"
               (format+ "~127b" #b101))
(assert-equal? (tn)
               "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000101"
               (format+ "~0127b" #b101))

(tn "format+ ~f (number)")
(assert-equal? (tn) "-100" (format+ "~0f" -100))
(assert-equal? (tn) "-10"  (format+ "~0f" -10))
(assert-equal? (tn) "-1"   (format+ "~0f" -1))
(assert-equal? (tn) "0"    (format+ "~0f" 0))
(assert-equal? (tn) "1"    (format+ "~0f" 1))
(assert-equal? (tn) "10"   (format+ "~0f" 10))
(assert-equal? (tn) "100"  (format+ "~0f" 100))

(assert-equal? (tn) "-100" (format "~03f" -100))
(assert-equal? (tn) "-10"  (format "~03f" -10))
(assert-equal? (tn) "-01"  (format "~03f" -1))
(assert-equal? (tn) "000"  (format "~03f" 0))
(assert-equal? (tn) "001"  (format "~03f" 1))
(assert-equal? (tn) "010"  (format "~03f" 10))
(assert-equal? (tn) "100"  (format "~03f" 100))

(if (symbol-bound? 'exact->inexact)
    (begin
      (assert-equal? (tn) "-100.00" (format+ "~06,02f" -100))
      (assert-equal? (tn) "-10.00"  (format+ "~06,02f" -10))
      (assert-equal? (tn) "-01.00"  (format+ "~06,02f" -1))
      (assert-equal? (tn) "000.00"  (format+ "~06,02f" 0))
      (assert-equal? (tn) "001.00"  (format+ "~06,02f" 1))
      (assert-equal? (tn) "010.00"  (format+ "~06,02f" 10))
      (assert-equal? (tn) "100.00"  (format+ "~06,02f" 100))))

(assert-equal? (tn)
               "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000123"
               (format+ "~0127f" 123))

(tn "format ~h")
(define help-str
"(format+ [<port>] <format-string> [<arg>...])
  - <port> is #t, #f or an output-port
  - any escape sequence is case insensitive

  The format+ procedure is a SigScheme-specific superset of SRFI-48.
  Following directives accept optional width w and d digits after the decimal,
  and w accepts leading zero as zero-digit-padding specifier. All other rules
  are same as SRFI-48. See also the help message for SRFI-48.

SEQ        MNEMONIC       DESCRIPTION
~[w[,d]]D  [Decimal]      the arg is a number output in decimal radix
~[w[,d]]X  [heXadecimal]  the arg is a number output in hexdecimal radix
~[w[,d]]O  [Octal]        the arg is a number output in octal radix
~[w[,d]]B  [Binary]       the arg is a number output in binary radix
~[w[,d]]F  [Fixed]        the arg is a string or number
")
(assert-equal? (tn) help-str (format "~h"))
(assert-equal? (tn) help-str (format "~H"))

(total-report)
