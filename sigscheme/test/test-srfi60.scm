;;  FileName : test-srfi60.scm
;;  About    : unit test for SRFI-60 integers as bits
;;
;;  Copyright (C) 2005      by YamaKen
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

(load "test/unittest.scm")


;;
;; Bitwise Operations
;;

;; logand
(assert-equal? "logand" 0      (logand))
(assert-equal? "logand" 0      (logand 0))
(assert-equal? "logand" #b11   (logand #b11))
(assert-equal? "logand" #b10   (logand #b1010 #b10))
(assert-equal? "logand" 0      (logand #b1010 #b100))
(assert-equal? "logand" #b1010 (logand #b1010 #b1110))
(assert-equal? "logand" #b1000 (logand #b1010 #b1110 #b101000))
(assert-equal? "logand" 0      (logand #b1010 #b1110 #b101000 0))

;; logior
(assert-equal? "logior" 0        (logior))
(assert-equal? "logior" 0        (logior 0))
(assert-equal? "logior" #b11     (logior #b11))
(assert-equal? "logior" #b1010   (logior #b1010 #b10))
(assert-equal? "logior" #b1110   (logior #b1010 #b100))
(assert-equal? "logior" #b1110   (logior #b1010 #b1110))
(assert-equal? "logior" #b101110 (logior #b1010 #b1110 #b101000))
(assert-equal? "logior" #b101110 (logior #b1010 #b1110 #b101000 0))

;; logxor
(assert-equal? "logxor" 0        (logxor))
(assert-equal? "logxor" 0        (logxor 0))
(assert-equal? "logxor" #b11     (logxor #b11))
(assert-equal? "logxor" #b1000   (logxor #b1010 #b10))
(assert-equal? "logxor" #b1110   (logxor #b1010 #b100))
(assert-equal? "logxor" #b0100   (logxor #b1010 #b1110))
(assert-equal? "logxor" #b101100 (logxor #b1010 #b1110 #b101000))
(assert-equal? "logxor" #b101100 (logxor #b1010 #b1110 #b101000 0))

;; lognot
(assert-equal? "lognot" -1            (lognot 0))
(assert-equal? "lognot" 0             (lognot -1))
(assert-equal? "lognot" -2            (lognot 1))
(assert-equal? "lognot" 1             (lognot -2))
(assert-equal? "lognot" (- -1 #b1010) (lognot #b1010))
(assert-equal? "lognot" (- -1 #b0101) (lognot #b0101))

;; bitwise-if
(assert-equal? "bitwise-if" 0 (bitwise-if 0 0 0))
(assert-equal? "bitwise-if" 0 (bitwise-if 0 1 0))
(assert-equal? "bitwise-if" 1 (bitwise-if 0 0 1))
(assert-equal? "bitwise-if" 1 (bitwise-if 0 1 1))
(assert-equal? "bitwise-if" 0 (bitwise-if 1 0 0))
(assert-equal? "bitwise-if" 1 (bitwise-if 1 1 0))
(assert-equal? "bitwise-if" 0 (bitwise-if 1 0 1))
(assert-equal? "bitwise-if" 1 (bitwise-if 1 1 1))
(assert-equal? "bitwise-if" #b0010100 (bitwise-if #b11100 #b1010101 #b0000000))
(assert-equal? "bitwise-if" #b0110110 (bitwise-if #b11100 #b1010101 #b0101010))
(assert-equal? "bitwise-if" #b0100010 (bitwise-if #b11100 #b0000000 #b0101010))

;; logtest
(assert-false "logtest" (logtest 0 0))
(assert-false "logtest" (logtest 1 0))
(assert-false "logtest" (logtest 0 1))
(assert-true  "logtest" (logtest 1 1))
(assert-true  "logtest" (logtest #b1010 #b10))
(assert-false "logtest" (logtest #b1010 #b100))
(assert-true  "logtest" (logtest #b1010 #b1110))

;; aliases
(assert-eq? "bitwise-and"   bitwise-and   logand)
(assert-eq? "bitwise-ior"   bitwise-ior   logior)
(assert-eq? "bitwise-xor"   bitwise-xor   logxor)
(assert-eq? "bitwise-not"   bitwise-not   lognot)
(assert-eq? "bitwise-merge" bitwise-merge bitwise-if)
(assert-eq? "any-bits-set?" any-bits-set? logtest)

(total-report)
