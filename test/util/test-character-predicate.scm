#!/usr/bin/env gosh

;;; Copyright (c) 2003-2008 uim Project http://code.google.com/p/uim/
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

(define-module test.util.test-character-predicate
  (use test.unit.test-case)
  (use test.uim-test-utils-new))
(select-module test.util.test-character-predicate)

(define (setup)
  (uim-test-setup))

(define (teardown)
  (uim-test-teardown))

(define (test-ichar-control?)
  (assert-false (uim-bool '(ichar-control? 'symbol)))
  (assert-false (uim-bool '(ichar-control? "string")))
  (assert-false (uim-bool '(ichar-control? '(0 1 2))))
  (assert-false (uim-bool '(ichar-control? car)))
  (assert-true  (uim-bool '(ichar-control? 0)))    ; NUL
  (assert-true  (uim-bool '(ichar-control? 1)))    ; SOH
  (assert-true  (uim-bool '(ichar-control? 31)))   ; US
  (assert-false (uim-bool '(ichar-control? 32)))   ; SPACE
  (assert-false (uim-bool '(ichar-control? 33)))   ; !
  (assert-false (uim-bool '(ichar-control? 47)))   ; /
  (assert-false (uim-bool '(ichar-control? 48)))   ; 0
  (assert-false (uim-bool '(ichar-control? 57)))   ; 9
  (assert-false (uim-bool '(ichar-control? 58)))   ; :
  (assert-false (uim-bool '(ichar-control? 64)))   ; @
  (assert-false (uim-bool '(ichar-control? 65)))   ; A
  (assert-false (uim-bool '(ichar-control? 90)))   ; Z
  (assert-false (uim-bool '(ichar-control? 91)))   ; [
  (assert-false (uim-bool '(ichar-control? 96)))   ; `
  (assert-false (uim-bool '(ichar-control? 97)))   ; a
  (assert-false (uim-bool '(ichar-control? 122)))  ; z
  (assert-false (uim-bool '(ichar-control? 123)))  ; {
  (assert-false (uim-bool '(ichar-control? 126)))  ; ~
  (assert-true  (uim-bool '(ichar-control? 127)))  ; DEL
  #f)

(define (test-ichar-upper-case?)
  (assert-false (uim-bool '(ichar-upper-case? 'symbol)))
  (assert-false (uim-bool '(ichar-upper-case? "string")))
  (assert-false (uim-bool '(ichar-upper-case? '(0 1 2))))
  (assert-false (uim-bool '(ichar-upper-case? car)))
  (assert-false (uim-bool '(ichar-upper-case? 0)))   ; NUL
  (assert-false (uim-bool '(ichar-upper-case? 1)))   ; SOH
  (assert-false (uim-bool '(ichar-upper-case? 31)))  ; US
  (assert-false (uim-bool '(ichar-upper-case? 32)))  ; SPACE
  (assert-false (uim-bool '(ichar-upper-case? 33)))  ; !
  (assert-false (uim-bool '(ichar-upper-case? 47)))  ; /
  (assert-false (uim-bool '(ichar-upper-case? 48)))  ; 0
  (assert-false (uim-bool '(ichar-upper-case? 57)))  ; 9
  (assert-false (uim-bool '(ichar-upper-case? 58)))  ; :
  (assert-false (uim-bool '(ichar-upper-case? 64)))  ; @
  (assert-true  (uim-bool '(ichar-upper-case? 65)))  ; A
  (assert-true  (uim-bool '(ichar-upper-case? 90)))  ; Z
  (assert-false (uim-bool '(ichar-upper-case? 91)))  ; [
  (assert-false (uim-bool '(ichar-upper-case? 96)))  ; `
  (assert-false (uim-bool '(ichar-upper-case? 97)))  ; a
  (assert-false (uim-bool '(ichar-upper-case? 122))) ; z
  (assert-false (uim-bool '(ichar-upper-case? 123))) ; {
  (assert-false (uim-bool '(ichar-upper-case? 126))) ; ~
  (assert-false (uim-bool '(ichar-upper-case? 127))) ; DEL
  #f)

(define (test-ichar-lower-case?)
  (assert-false (uim-bool '(ichar-lower-case? 'symbol)))
  (assert-false (uim-bool '(ichar-lower-case? "string")))
  (assert-false (uim-bool '(ichar-lower-case? '(0 1 2))))
  (assert-false (uim-bool '(ichar-lower-case? car)))
  (assert-false (uim-bool '(ichar-lower-case? 0)))   ; NUL
  (assert-false (uim-bool '(ichar-lower-case? 1)))   ; SOH
  (assert-false (uim-bool '(ichar-lower-case? 31)))  ; US
  (assert-false (uim-bool '(ichar-lower-case? 32)))  ; SPACE
  (assert-false (uim-bool '(ichar-lower-case? 33)))  ; !
  (assert-false (uim-bool '(ichar-lower-case? 47)))  ; /
  (assert-false (uim-bool '(ichar-lower-case? 48)))  ; 0
  (assert-false (uim-bool '(ichar-lower-case? 57)))  ; 9
  (assert-false (uim-bool '(ichar-lower-case? 58)))  ; :
  (assert-false (uim-bool '(ichar-lower-case? 64)))  ; @
  (assert-false (uim-bool '(ichar-lower-case? 65)))  ; A
  (assert-false (uim-bool '(ichar-lower-case? 90)))  ; Z
  (assert-false (uim-bool '(ichar-lower-case? 91)))  ; [
  (assert-false (uim-bool '(ichar-lower-case? 96)))  ; `
  (assert-true  (uim-bool '(ichar-lower-case? 97)))  ; a
  (assert-true  (uim-bool '(ichar-lower-case? 122))) ; z
  (assert-false (uim-bool '(ichar-lower-case? 123))) ; {
  (assert-false (uim-bool '(ichar-lower-case? 126))) ; ~
  (assert-false (uim-bool '(ichar-lower-case? 127))) ; DEL
  #f)

(define (test-ichar-alphabetic?)
  (assert-false (uim-bool '(ichar-alphabetic? 'symbol)))
  (assert-false (uim-bool '(ichar-alphabetic? "string")))
  (assert-false (uim-bool '(ichar-alphabetic? '(0 1 2))))
  (assert-false (uim-bool '(ichar-alphabetic? car)))
  (assert-false (uim-bool '(ichar-alphabetic? 0)))   ; NUL
  (assert-false (uim-bool '(ichar-alphabetic? 1)))   ; SOH
  (assert-false (uim-bool '(ichar-alphabetic? 31)))  ; US
  (assert-false (uim-bool '(ichar-alphabetic? 32)))  ; SPACE
  (assert-false (uim-bool '(ichar-alphabetic? 33)))  ; !
  (assert-false (uim-bool '(ichar-alphabetic? 47)))  ; /
  (assert-false (uim-bool '(ichar-alphabetic? 48)))  ; 0
  (assert-false (uim-bool '(ichar-alphabetic? 57)))  ; 9
  (assert-false (uim-bool '(ichar-alphabetic? 58)))  ; :
  (assert-false (uim-bool '(ichar-alphabetic? 64)))  ; @
  (assert-true  (uim-bool '(ichar-alphabetic? 65)))  ; A
  (assert-true  (uim-bool '(ichar-alphabetic? 90)))  ; Z
  (assert-false (uim-bool '(ichar-alphabetic? 91)))  ; [
  (assert-false (uim-bool '(ichar-alphabetic? 96)))  ; `
  (assert-true  (uim-bool '(ichar-alphabetic? 97)))  ; a
  (assert-true  (uim-bool '(ichar-alphabetic? 122))) ; z
  (assert-false (uim-bool '(ichar-alphabetic? 123))) ; {
  (assert-false (uim-bool '(ichar-alphabetic? 126))) ; ~
  (assert-false (uim-bool '(ichar-alphabetic? 127))) ; DEL
  #f)

(define (test-ichar-numeric?)
  (assert-false (uim-bool '(ichar-numeric? 'symbol)))
  (assert-false (uim-bool '(ichar-numeric? "string")))
  (assert-false (uim-bool '(ichar-numeric? '(0 1 2))))
  (assert-false (uim-bool '(ichar-numeric? car)))
  (assert-false (uim-bool '(ichar-numeric? 0)))        ; NUL
  (assert-false (uim-bool '(ichar-numeric? 1)))        ; SOH
  (assert-false (uim-bool '(ichar-numeric? 31)))       ; US
  (assert-false (uim-bool '(ichar-numeric? 32))) ; SPACE
  (assert-false (uim-bool '(ichar-numeric? 33))) ; !
  (assert-false (uim-bool '(ichar-numeric? 47))) ; /
  (assert-true  (uim-bool '(ichar-numeric? 48))) ; 0
  (assert-true  (uim-bool '(ichar-numeric? 57))) ; 9
  (assert-false (uim-bool '(ichar-numeric? 58))) ; :
  (assert-false (uim-bool '(ichar-numeric? 64))) ; @
  (assert-false (uim-bool '(ichar-numeric? 65))) ; A
  (assert-false (uim-bool '(ichar-numeric? 90))) ; Z
  (assert-false (uim-bool '(ichar-numeric? 91))) ; [
  (assert-false (uim-bool '(ichar-numeric? 96))) ; `
  (assert-false (uim-bool '(ichar-numeric? 97))) ; a
  (assert-false (uim-bool '(ichar-numeric? 122)))        ; z
  (assert-false (uim-bool '(ichar-numeric? 123)))        ; {
  (assert-false (uim-bool '(ichar-numeric? 126)))        ; ~
  (assert-false (uim-bool '(ichar-numeric? 127))) ; DEL
  #f)

(define (test-ichar-printable?)
  (assert-false (uim-bool '(ichar-printable? 'symbol)))
  (assert-false (uim-bool '(ichar-printable? "string")))
  (assert-false (uim-bool '(ichar-printable? '(0 1 2))))
  (assert-false (uim-bool '(ichar-printable? car)))
  (assert-false (uim-bool '(ichar-printable? 0)))    ; NUL
  (assert-false (uim-bool '(ichar-printable? 1)))    ; SOH
  (assert-false (uim-bool '(ichar-printable? 31)))   ; US
  (assert-true  (uim-bool '(ichar-printable? 32)))   ; SPACE
  (assert-true  (uim-bool '(ichar-printable? 33)))   ; !
  (assert-true  (uim-bool '(ichar-printable? 47)))   ; /
  (assert-true  (uim-bool '(ichar-printable? 48)))   ; 0
  (assert-true  (uim-bool '(ichar-printable? 57)))   ; 9
  (assert-true  (uim-bool '(ichar-printable? 58)))   ; :
  (assert-true  (uim-bool '(ichar-printable? 64)))   ; @
  (assert-true  (uim-bool '(ichar-printable? 65)))   ; A
  (assert-true  (uim-bool '(ichar-printable? 90)))   ; Z
  (assert-true  (uim-bool '(ichar-printable? 91)))   ; [
  (assert-true  (uim-bool '(ichar-printable? 96)))   ; `
  (assert-true  (uim-bool '(ichar-printable? 97)))   ; a
  (assert-true  (uim-bool '(ichar-printable? 122)))  ; z
  (assert-true  (uim-bool '(ichar-printable? 123)))  ; {
  (assert-true  (uim-bool '(ichar-printable? 126)))  ; ~
  (assert-false (uim-bool '(ichar-printable? 127)))  ; DEL
  #f)

(define (test-ichar-graphic?)
  (assert-false (uim-bool '(ichar-graphic? 'symbol)))
  (assert-false (uim-bool '(ichar-graphic? "string")))
  (assert-false (uim-bool '(ichar-graphic? '(0 1 2))))
  (assert-false (uim-bool '(ichar-graphic? car)))
  (assert-false (uim-bool '(ichar-graphic? 0)))    ; NUL
  (assert-false (uim-bool '(ichar-graphic? 1)))    ; SOH
  (assert-false (uim-bool '(ichar-graphic? 31)))   ; US
  (assert-false (uim-bool '(ichar-graphic? 32)))   ; SPACE
  (assert-true  (uim-bool '(ichar-graphic? 33)))   ; !
  (assert-true  (uim-bool '(ichar-graphic? 47)))   ; /
  (assert-true  (uim-bool '(ichar-graphic? 48)))   ; 0
  (assert-true  (uim-bool '(ichar-graphic? 57)))   ; 9
  (assert-true  (uim-bool '(ichar-graphic? 58)))   ; :
  (assert-true  (uim-bool '(ichar-graphic? 64)))   ; @
  (assert-true  (uim-bool '(ichar-graphic? 65)))   ; A
  (assert-true  (uim-bool '(ichar-graphic? 90)))   ; Z
  (assert-true  (uim-bool '(ichar-graphic? 91)))   ; [
  (assert-true  (uim-bool '(ichar-graphic? 96)))   ; `
  (assert-true  (uim-bool '(ichar-graphic? 97)))   ; a
  (assert-true  (uim-bool '(ichar-graphic? 122)))  ; z
  (assert-true  (uim-bool '(ichar-graphic? 123)))  ; {
  (assert-true  (uim-bool '(ichar-graphic? 126)))  ; ~
  (assert-false (uim-bool '(ichar-graphic? 127)))  ; DEL
  #f)

(provide "test/util/test-character-predicate")
