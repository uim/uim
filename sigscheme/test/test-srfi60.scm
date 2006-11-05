;;  Filename : test-srfi60.scm
;;  About    : unit test for SRFI-60 integers as bits
;;
;;  Copyright (C) 2005-2006 YAMAMOTO Kengo <yamaken AT bp.iij4u.or.jp>
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

(use srfi-60)


;;
;; Bitwise Operations
;;

(tn "logand invalid form")
(assert-error (tn) (lambda () (logand #\a)))
(assert-error (tn) (lambda () (logand #\a 1)))
(assert-error (tn) (lambda () (logand 1 #\a)))
(assert-error (tn) (lambda () (logand 1 1 #\a)))
(tn "logand")
(assert-equal? (tn) -1     (logand))
(assert-equal? (tn) 0      (logand 0))
(assert-equal? (tn) -1     (logand -1))
(assert-equal? (tn) 1      (logand 1))
(assert-equal? (tn) #b11   (logand #b11))
(assert-equal? (tn) #b10   (logand #b1010 #b10))
(assert-equal? (tn) 0      (logand #b1010 #b100))
(assert-equal? (tn) #b1010 (logand #b1010 #b1110))
(assert-equal? (tn) #b1000 (logand #b1010 #b1110 #b101000))
(assert-equal? (tn) 0      (logand #b1010 #b1110 #b101000 0))
(if (and (symbol-bound? 'least-fixnum)
         (symbol-bound? 'greatest-fixnum))
    (begin
      (assert-equal? (tn) (least-fixnum)    (logior 0  (least-fixnum)))
      (assert-equal? (tn) (greatest-fixnum) (logior 0  (greatest-fixnum)))
      (assert-equal? (tn) (least-fixnum)    (logand -1 (least-fixnum)))
      (assert-equal? (tn) (greatest-fixnum) (logand -1 (greatest-fixnum)))
      (assert-equal? (tn) 0 (logand (least-fixnum) (greatest-fixnum)))))

(tn "logior invalid form")
(assert-error (tn) (lambda () (logior #\a)))
(assert-error (tn) (lambda () (logior #\a 1)))
(assert-error (tn) (lambda () (logior 1 #\a)))
(assert-error (tn) (lambda () (logior 1 1 #\a)))
(tn "logior")
(assert-equal? (tn) 0        (logior))
(assert-equal? (tn) 0        (logior 0))
(assert-equal? (tn) #b11     (logior #b11))
(assert-equal? (tn) #b1010   (logior #b1010 #b10))
(assert-equal? (tn) #b1110   (logior #b1010 #b100))
(assert-equal? (tn) #b1110   (logior #b1010 #b1110))
(assert-equal? (tn) #b101110 (logior #b1010 #b1110 #b101000))
(assert-equal? (tn) #b101110 (logior #b1010 #b1110 #b101000 0))
(if (and (symbol-bound? 'least-fixnum)
         (symbol-bound? 'greatest-fixnum))
    (begin
      (assert-equal? (tn) (least-fixnum)    (logior 0  (least-fixnum)))
      (assert-equal? (tn) (greatest-fixnum) (logior 0  (greatest-fixnum)))
      (assert-equal? (tn) -1                (logior -1 (least-fixnum)))
      (assert-equal? (tn) -1                (logior -1 (greatest-fixnum)))
      (assert-equal? (tn) -1 (logior (least-fixnum) (greatest-fixnum)))))

(tn "logxor invalid form")
(assert-error (tn) (lambda () (logxor #\a)))
(assert-error (tn) (lambda () (logxor #\a 1)))
(assert-error (tn) (lambda () (logxor 1 #\a)))
(assert-error (tn) (lambda () (logxor 1 1 #\a)))
(tn "logxor")
(assert-equal? (tn) 0        (logxor))
(assert-equal? (tn) 0        (logxor 0))
(assert-equal? (tn) #b11     (logxor #b11))
(assert-equal? (tn) #b1000   (logxor #b1010 #b10))
(assert-equal? (tn) #b1110   (logxor #b1010 #b100))
(assert-equal? (tn) #b0100   (logxor #b1010 #b1110))
(assert-equal? (tn) #b101100 (logxor #b1010 #b1110 #b101000))
(assert-equal? (tn) #b101100 (logxor #b1010 #b1110 #b101000 0))
(if (and (symbol-bound? 'least-fixnum)
         (symbol-bound? 'greatest-fixnum))
    (begin
      (assert-equal? (tn) (least-fixnum)    (logxor 0 (least-fixnum)))
      (assert-equal? (tn) (greatest-fixnum) (logxor 0 (greatest-fixnum)))
      (assert-equal? (tn) (greatest-fixnum) (logxor -1 (least-fixnum)))
      (assert-equal? (tn) (least-fixnum)    (logxor -1 (greatest-fixnum)))
      (assert-equal? (tn) -1 (logxor (least-fixnum) (greatest-fixnum)))))

(tn "lognot invalid forms")
(assert-error  (tn) (lambda ()    (lognot)))
(assert-error  (tn) (lambda ()    (lognot 0 0)))
(tn "lognot")
(assert-equal? (tn) -1            (lognot 0))
(assert-equal? (tn) 0             (lognot -1))
(assert-equal? (tn) -2            (lognot 1))
(assert-equal? (tn) 1             (lognot -2))
(assert-equal? (tn) (- -1 #b1010) (lognot #b1010))
(assert-equal? (tn) (- -1 #b0101) (lognot #b0101))
(if (and (symbol-bound? 'least-fixnum)
         (symbol-bound? 'greatest-fixnum))
    (begin
      (assert-equal? (tn) -1 (lognot 0))
      (assert-equal? (tn) 0  (lognot -1))
      (assert-equal? (tn) (greatest-fixnum) (lognot (least-fixnum)))
      (assert-equal? (tn) (least-fixnum)    (lognot (greatest-fixnum)))))

(tn "bitwise-if invalid forms")
(assert-error  (tn) (lambda () (bitwise-if)))
(assert-error  (tn) (lambda () (bitwise-if 0)))
(assert-error  (tn) (lambda () (bitwise-if 0 0)))
(tn "bitwise-if")
(assert-equal? (tn) 0 (bitwise-if 0 0 0))
(assert-equal? (tn) 0 (bitwise-if 0 1 0))
(assert-equal? (tn) 1 (bitwise-if 0 0 1))
(assert-equal? (tn) 1 (bitwise-if 0 1 1))
(assert-equal? (tn) 0 (bitwise-if 1 0 0))
(assert-equal? (tn) 1 (bitwise-if 1 1 0))
(assert-equal? (tn) 0 (bitwise-if 1 0 1))
(assert-equal? (tn) 1 (bitwise-if 1 1 1))
(assert-equal? (tn) #b0010100 (bitwise-if #b11100 #b1010101 #b0000000))
(assert-equal? (tn) #b0110110 (bitwise-if #b11100 #b1010101 #b0101010))
(assert-equal? (tn) #b0100010 (bitwise-if #b11100 #b0000000 #b0101010))

(tn "logtest")
(assert-eq? (tn) #f (logtest 0 0))
(assert-eq? (tn) #f (logtest 1 0))
(assert-eq? (tn) #f (logtest 0 1))
(assert-eq? (tn) #t (logtest 1 1))
(assert-eq? (tn) #t (logtest #b1010 #b10))
(assert-eq? (tn) #f (logtest #b1010 #b100))
(assert-eq? (tn) #t (logtest #b1010 #b1110))

;; SRFI-33 aliases
(assert-eq? "bitwise-and"   bitwise-and   logand)
(assert-eq? "bitwise-ior"   bitwise-ior   logior)
(assert-eq? "bitwise-xor"   bitwise-xor   logxor)
(assert-eq? "bitwise-not"   bitwise-not   lognot)
(assert-eq? "bitwise-merge" bitwise-merge bitwise-if)
(assert-eq? "any-bits-set?" any-bits-set? logtest)


(total-report)
