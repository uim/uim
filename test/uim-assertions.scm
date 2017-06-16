;;; Copyright (c) 2004-2013 uim Project https://github.com/uim/uim
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
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS
;;; IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
;;; THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;;; PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR
;;; CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;;; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
;;; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
;;; OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
;;; ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

(define-module test.uim-assertions
  (use test.unit)
  (use test.uim-test-utils-new)
  (export-all))
(select-module test.uim-assertions)

(define (assert-uim-equal expected uim-expression)
  (assert-equal expected (uim uim-expression)))

(define (assert-uim-true uim-expression)
  (assert-true (uim uim-expression)))

(define (assert-uim-false uim-expression)
  (assert-false (uim uim-expression)))

(define (assert-uim-true-value uim-expression)
  (assert (lambda (expected actual) actual)
          #t
          (uim uim-expression)))

(define (assert-uim-equal-raw expected uim-expression-string)
  (assert-equal expected (uim-raw uim-expression-string)))

(define (assert-uim-true-raw uim-expression-string)
  (assert-true (uim-raw uim-expression-string)))

(define (assert-uim-false-raw uim-expression-string)
  (assert-false (uim-raw uim-expression-string)))

(define (assert-uim-equal-ces expected uim-expression ces)
  (assert-equal expected (uim-ces uim-expression ces)))

(define (assert-uim-equal-euc-jp expected uim-expression)
  (assert-uim-equal-ces expected uim-expression "euc-jp"))

(define (assert-uim-error uim-expression)
  (assert-error (lambda () (uim uim-expression))))

(provide "test/uim-assertions")
