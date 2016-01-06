#!/usr/bin/env gosh

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
;;;;

;; These tests are passed at revision 6605 (new repository)

(define-module test.util.test-character-predicate
  (use test.unit.test-case)
  (use test.uim-test))
(select-module test.util.test-character-predicate)

(define (setup)
  (uim-test-setup))

(define (teardown)
  (uim-test-teardown))

(define (test-ichar-control?)
  (assert-uim-false '(ichar-control? 'symbol))
  (assert-uim-false '(ichar-control? "string"))
  (assert-uim-false '(ichar-control? '(0 1 2)))
  (assert-uim-false '(ichar-control? car))
  (assert-uim-true  '(ichar-control? 0))    ; NUL
  (assert-uim-true  '(ichar-control? 1))    ; SOH
  (assert-uim-true  '(ichar-control? 31))   ; US
  (assert-uim-false '(ichar-control? 32))   ; SPACE
  (assert-uim-false '(ichar-control? 33))   ; !
  (assert-uim-false '(ichar-control? 47))   ; /
  (assert-uim-false '(ichar-control? 48))   ; 0
  (assert-uim-false '(ichar-control? 57))   ; 9
  (assert-uim-false '(ichar-control? 58))   ; :
  (assert-uim-false '(ichar-control? 64))   ; @
  (assert-uim-false '(ichar-control? 65))   ; A
  (assert-uim-false '(ichar-control? 90))   ; Z
  (assert-uim-false '(ichar-control? 91))   ; [
  (assert-uim-false '(ichar-control? 96))   ; `
  (assert-uim-false '(ichar-control? 97))   ; a
  (assert-uim-false '(ichar-control? 122))  ; z
  (assert-uim-false '(ichar-control? 123))  ; {
  (assert-uim-false '(ichar-control? 126))  ; ~
  (assert-uim-true  '(ichar-control? 127))  ; DEL
  #f)

(define (test-ichar-upper-case?)
  (assert-uim-false '(ichar-upper-case? 'symbol))
  (assert-uim-false '(ichar-upper-case? "string"))
  (assert-uim-false '(ichar-upper-case? '(0 1 2)))
  (assert-uim-false '(ichar-upper-case? car))
  (assert-uim-false '(ichar-upper-case? 0))   ; NUL
  (assert-uim-false '(ichar-upper-case? 1))   ; SOH
  (assert-uim-false '(ichar-upper-case? 31))  ; US
  (assert-uim-false '(ichar-upper-case? 32))  ; SPACE
  (assert-uim-false '(ichar-upper-case? 33))  ; !
  (assert-uim-false '(ichar-upper-case? 47))  ; /
  (assert-uim-false '(ichar-upper-case? 48))  ; 0
  (assert-uim-false '(ichar-upper-case? 57))  ; 9
  (assert-uim-false '(ichar-upper-case? 58))  ; :
  (assert-uim-false '(ichar-upper-case? 64))  ; @
  (assert-uim-true  '(ichar-upper-case? 65))  ; A
  (assert-uim-true  '(ichar-upper-case? 90))  ; Z
  (assert-uim-false '(ichar-upper-case? 91))  ; [
  (assert-uim-false '(ichar-upper-case? 96))  ; `
  (assert-uim-false '(ichar-upper-case? 97))  ; a
  (assert-uim-false '(ichar-upper-case? 122)) ; z
  (assert-uim-false '(ichar-upper-case? 123)) ; {
  (assert-uim-false '(ichar-upper-case? 126)) ; ~
  (assert-uim-false '(ichar-upper-case? 127)) ; DEL
  #f)

(define (test-ichar-lower-case?)
  (assert-uim-false '(ichar-lower-case? 'symbol))
  (assert-uim-false '(ichar-lower-case? "string"))
  (assert-uim-false '(ichar-lower-case? '(0 1 2)))
  (assert-uim-false '(ichar-lower-case? car))
  (assert-uim-false '(ichar-lower-case? 0))   ; NUL
  (assert-uim-false '(ichar-lower-case? 1))   ; SOH
  (assert-uim-false '(ichar-lower-case? 31))  ; US
  (assert-uim-false '(ichar-lower-case? 32))  ; SPACE
  (assert-uim-false '(ichar-lower-case? 33))  ; !
  (assert-uim-false '(ichar-lower-case? 47))  ; /
  (assert-uim-false '(ichar-lower-case? 48))  ; 0
  (assert-uim-false '(ichar-lower-case? 57))  ; 9
  (assert-uim-false '(ichar-lower-case? 58))  ; :
  (assert-uim-false '(ichar-lower-case? 64))  ; @
  (assert-uim-false '(ichar-lower-case? 65))  ; A
  (assert-uim-false '(ichar-lower-case? 90))  ; Z
  (assert-uim-false '(ichar-lower-case? 91))  ; [
  (assert-uim-false '(ichar-lower-case? 96))  ; `
  (assert-uim-true  '(ichar-lower-case? 97))  ; a
  (assert-uim-true  '(ichar-lower-case? 122)) ; z
  (assert-uim-false '(ichar-lower-case? 123)) ; {
  (assert-uim-false '(ichar-lower-case? 126)) ; ~
  (assert-uim-false '(ichar-lower-case? 127)) ; DEL
  #f)

(define (test-ichar-alphabetic?)
  (assert-uim-false '(ichar-alphabetic? 'symbol))
  (assert-uim-false '(ichar-alphabetic? "string"))
  (assert-uim-false '(ichar-alphabetic? '(0 1 2)))
  (assert-uim-false '(ichar-alphabetic? car))
  (assert-uim-false '(ichar-alphabetic? 0))   ; NUL
  (assert-uim-false '(ichar-alphabetic? 1))   ; SOH
  (assert-uim-false '(ichar-alphabetic? 31))  ; US
  (assert-uim-false '(ichar-alphabetic? 32))  ; SPACE
  (assert-uim-false '(ichar-alphabetic? 33))  ; !
  (assert-uim-false '(ichar-alphabetic? 47))  ; /
  (assert-uim-false '(ichar-alphabetic? 48))  ; 0
  (assert-uim-false '(ichar-alphabetic? 57))  ; 9
  (assert-uim-false '(ichar-alphabetic? 58))  ; :
  (assert-uim-false '(ichar-alphabetic? 64))  ; @
  (assert-uim-true  '(ichar-alphabetic? 65))  ; A
  (assert-uim-true  '(ichar-alphabetic? 90))  ; Z
  (assert-uim-false '(ichar-alphabetic? 91))  ; [
  (assert-uim-false '(ichar-alphabetic? 96))  ; `
  (assert-uim-true  '(ichar-alphabetic? 97))  ; a
  (assert-uim-true  '(ichar-alphabetic? 122)) ; z
  (assert-uim-false '(ichar-alphabetic? 123)) ; {
  (assert-uim-false '(ichar-alphabetic? 126)) ; ~
  (assert-uim-false '(ichar-alphabetic? 127)) ; DEL
  #f)

(define (test-ichar-numeric?)
  (assert-uim-false '(ichar-numeric? 'symbol))
  (assert-uim-false '(ichar-numeric? "string"))
  (assert-uim-false '(ichar-numeric? '(0 1 2)))
  (assert-uim-false '(ichar-numeric? car))
  (assert-uim-false '(ichar-numeric? 0))        ; NUL
  (assert-uim-false '(ichar-numeric? 1))        ; SOH
  (assert-uim-false '(ichar-numeric? 31))       ; US
  (assert-uim-false '(ichar-numeric? 32)) ; SPACE
  (assert-uim-false '(ichar-numeric? 33)) ; !
  (assert-uim-false '(ichar-numeric? 47)) ; /
  (assert-uim-true  '(ichar-numeric? 48)) ; 0
  (assert-uim-true  '(ichar-numeric? 57)) ; 9
  (assert-uim-false '(ichar-numeric? 58)) ; :
  (assert-uim-false '(ichar-numeric? 64)) ; @
  (assert-uim-false '(ichar-numeric? 65)) ; A
  (assert-uim-false '(ichar-numeric? 90)) ; Z
  (assert-uim-false '(ichar-numeric? 91)) ; [
  (assert-uim-false '(ichar-numeric? 96)) ; `
  (assert-uim-false '(ichar-numeric? 97)) ; a
  (assert-uim-false '(ichar-numeric? 122))        ; z
  (assert-uim-false '(ichar-numeric? 123))        ; {
  (assert-uim-false '(ichar-numeric? 126))        ; ~
  (assert-uim-false '(ichar-numeric? 127)) ; DEL
  #f)

(define (test-ichar-printable?)
  (assert-uim-false '(ichar-printable? 'symbol))
  (assert-uim-false '(ichar-printable? "string"))
  (assert-uim-false '(ichar-printable? '(0 1 2)))
  (assert-uim-false '(ichar-printable? car))
  (assert-uim-false '(ichar-printable? 0))    ; NUL
  (assert-uim-false '(ichar-printable? 1))    ; SOH
  (assert-uim-false '(ichar-printable? 31))   ; US
  (assert-uim-true  '(ichar-printable? 32))   ; SPACE
  (assert-uim-true  '(ichar-printable? 33))   ; !
  (assert-uim-true  '(ichar-printable? 47))   ; /
  (assert-uim-true  '(ichar-printable? 48))   ; 0
  (assert-uim-true  '(ichar-printable? 57))   ; 9
  (assert-uim-true  '(ichar-printable? 58))   ; :
  (assert-uim-true  '(ichar-printable? 64))   ; @
  (assert-uim-true  '(ichar-printable? 65))   ; A
  (assert-uim-true  '(ichar-printable? 90))   ; Z
  (assert-uim-true  '(ichar-printable? 91))   ; [
  (assert-uim-true  '(ichar-printable? 96))   ; `
  (assert-uim-true  '(ichar-printable? 97))   ; a
  (assert-uim-true  '(ichar-printable? 122))  ; z
  (assert-uim-true  '(ichar-printable? 123))  ; {
  (assert-uim-true  '(ichar-printable? 126))  ; ~
  (assert-uim-false '(ichar-printable? 127))  ; DEL
  #f)

(define (test-ichar-graphic?)
  (assert-uim-false '(ichar-graphic? 'symbol))
  (assert-uim-false '(ichar-graphic? "string"))
  (assert-uim-false '(ichar-graphic? '(0 1 2)))
  (assert-uim-false '(ichar-graphic? car))
  (assert-uim-false '(ichar-graphic? 0))    ; NUL
  (assert-uim-false '(ichar-graphic? 1))    ; SOH
  (assert-uim-false '(ichar-graphic? 31))   ; US
  (assert-uim-false '(ichar-graphic? 32))   ; SPACE
  (assert-uim-true  '(ichar-graphic? 33))   ; !
  (assert-uim-true  '(ichar-graphic? 47))   ; /
  (assert-uim-true  '(ichar-graphic? 48))   ; 0
  (assert-uim-true  '(ichar-graphic? 57))   ; 9
  (assert-uim-true  '(ichar-graphic? 58))   ; :
  (assert-uim-true  '(ichar-graphic? 64))   ; @
  (assert-uim-true  '(ichar-graphic? 65))   ; A
  (assert-uim-true  '(ichar-graphic? 90))   ; Z
  (assert-uim-true  '(ichar-graphic? 91))   ; [
  (assert-uim-true  '(ichar-graphic? 96))   ; `
  (assert-uim-true  '(ichar-graphic? 97))   ; a
  (assert-uim-true  '(ichar-graphic? 122))  ; z
  (assert-uim-true  '(ichar-graphic? 123))  ; {
  (assert-uim-true  '(ichar-graphic? 126))  ; ~
  (assert-uim-false '(ichar-graphic? 127))  ; DEL
  #f)

(provide "test/util/test-character-predicate")
