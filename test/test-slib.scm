#!/usr/bin/env gosh

;;; Copyright (c) 2003-2006 uim Project http://uim.freedesktop.org/
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
;;;;

(use test.unit)

(require "test/uim-test-utils")

(define-uim-test-case "testcase macro"
  ("test normal-let"
   (assert-equal '(-1 "b" (c) d)
                 (uim '(let ((a -1)
                             (b "b")
                             (c '(c))
                             (d 'd))
                         (list a b c d))))
   (assert-equal '() (uim '(let ((a)) a))))
  ("test named-let"
   (assert-equal -1 (uim '(let - ((x -)) (x 1))))
   (assert-equal '(4 3 2 1 0)
                 (uim '(let count-down ((lst '())
                                        (i 0))
                         (if (< i 5)
                           (count-down (cons i lst) (+ i 1))
                           lst))))
   (assert-equal '((6 1 3) (-5 -2))
                 (uim '(let loop ((numbers '(3 -2 1 6 -5))
                                  (nonneg '())
                                  (neg '()))
                         (cond ((null? numbers) (list nonneg neg))
                               ((> 0 (car numbers))
                                (loop (cdr numbers)
                                      nonneg
                                      (cons (car numbers) neg)))
                               (else
                                (loop (cdr numbers)
                                      (cons (car numbers) nonneg)
                                      neg))))))))

;; The following tests will fail due to bug #617 (uim treats false as nil)
(define-uim-test-case "testcase slib extensions"
  ("test case"
   (assert-false (uim-bool '(case 'symbol
			      ((0 #t #f () (symbol) "symbol") 0)
			      ((different-symbol symbol) #f)
			      (else 1))))
   (assert-false (uim-bool '(case (+ 5 5)
			      ((#t #f () (symbol) "10") 0)
			      (((+ 5 5) else) 1)
			      ((10) (case "string"
				      (("string") 7)
				      (else #f))))))
   (assert-false (uim-bool '(case 2
			      ((2) #f))))))

(define-uim-test-case "testcase procedures"
  ("test precedure?"
   (assert-true  (uim-bool '(procedure? eof-val)))            ;; 0
   (assert-true  (uim-bool '(procedure? car)))                ;; 1
   (assert-true  (uim-bool '(procedure? cons)))               ;; 2
   (assert-true  (uim-bool '(procedure? set-symbol-value!)))  ;; 3
   (assert-true  (uim-bool '(procedure? im-register-im)))     ;; 4
   (assert-true  (uim-bool '(procedure? dcngettext)))         ;; 5
   (assert-true  (uim-bool '(procedure? +)))                  ;; 2n
   (assert-true  (uim-bool '(procedure? append)))             ;; lsubr

   ;; SIOD
   ;;(assert-true  (uim-bool '(procedure? define)))             ;; fsubr
   ;;(assert-true  (uim-bool '(procedure? cond)))               ;; msubr
   ;; SigScheme
   (assert-false (uim-bool '(procedure? define)))             ;; fsubr
   (assert-false (uim-bool '(procedure? cond)))               ;; msubr

   (assert-true  (uim-bool '(procedure? (lambda (x) x))))     ;; closure

   (assert-false (uim-bool '(procedure? 0)))
   (assert-false (uim-bool '(procedure? "str")))
   (assert-false (uim-bool '(procedure? 'sym)))
   (assert-false (uim-bool '(procedure? '(foo bar))))
   (assert-false (uim-bool '(procedure? #t)))
   (assert-false (uim-bool '(procedure? #f)))
   (assert-false (uim-bool '(procedure? ())))))
