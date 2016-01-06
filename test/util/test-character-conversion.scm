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
;;;

;; These tests are passed at revision 6605 (new repository)

(define-module test.util.test-character-conversion
  (use test.unit.test-case)
  (use test.uim-test))
(select-module test.util.test-character-conversion)

(define (setup)
  (uim-test-setup))

(define (teardown)
  (uim-test-teardown))

(define (test-numeric-ichar->integer)
  (assert-uim-true    '(integer? (numeric-ichar->integer 0)))    ; NUL
  (assert-uim-true    '(integer? (numeric-ichar->integer 1)))    ; SOH
  (assert-uim-true    '(integer? (numeric-ichar->integer 31)))   ; US
  (assert-uim-true    '(integer? (numeric-ichar->integer 32)))   ; SPACE
  (assert-uim-true    '(integer? (numeric-ichar->integer 33)))   ; !
  (assert-uim-true    '(integer? (numeric-ichar->integer 47)))   ; /
  (assert-uim-equal 0 '(numeric-ichar->integer 48))              ; 0
  (assert-uim-equal 1 '(numeric-ichar->integer 49))              ; 1
  (assert-uim-equal 2 '(numeric-ichar->integer 50))              ; 2
  (assert-uim-equal 3 '(numeric-ichar->integer 51))              ; 3
  (assert-uim-equal 4 '(numeric-ichar->integer 52))              ; 4
  (assert-uim-equal 5 '(numeric-ichar->integer 53))              ; 5
  (assert-uim-equal 6 '(numeric-ichar->integer 54))              ; 6
  (assert-uim-equal 7 '(numeric-ichar->integer 55))              ; 7
  (assert-uim-equal 8 '(numeric-ichar->integer 56))              ; 8
  (assert-uim-equal 9 '(numeric-ichar->integer 57))              ; 9
  (assert-uim-true    '(integer? (numeric-ichar->integer 58)))   ; :
  (assert-uim-true    '(integer? (numeric-ichar->integer 64)))   ; @
  (assert-uim-true    '(integer? (numeric-ichar->integer 65)))   ; A
  (assert-uim-true    '(integer? (numeric-ichar->integer 90)))   ; Z
  (assert-uim-true    '(integer? (numeric-ichar->integer 91)))   ; [
  (assert-uim-true    '(integer? (numeric-ichar->integer 96)))   ; `
  (assert-uim-true    '(integer? (numeric-ichar->integer 97)))   ; a
  (assert-uim-true    '(integer? (numeric-ichar->integer 122)))  ; z
  (assert-uim-true    '(integer? (numeric-ichar->integer 123)))  ; {
  (assert-uim-true    '(integer? (numeric-ichar->integer 126)))  ; ~
  (assert-uim-true    '(integer? (numeric-ichar->integer 127)))  ; DEL
  #f)

(define (test-ichar-downcase)
  (assert-uim-equal 0   '(ichar-downcase 0))     ; NUL
  (assert-uim-equal 1   '(ichar-downcase 1))     ; SOH
  (assert-uim-equal 31  '(ichar-downcase 31))    ; US
  (assert-uim-equal 32  '(ichar-downcase 32))    ; SPACE
  (assert-uim-equal 33  '(ichar-downcase 33))    ; !
  (assert-uim-equal 47  '(ichar-downcase 47))    ; /
  (assert-uim-equal 48  '(ichar-downcase 48))    ; 0
  (assert-uim-equal 57  '(ichar-downcase 57))    ; 9
  (assert-uim-equal 58  '(ichar-downcase 58))    ; :
  (assert-uim-equal 64  '(ichar-downcase 64))    ; @
  (assert-uim-equal 97  '(ichar-downcase 65))    ; A
  (assert-uim-equal 122 '(ichar-downcase 90))    ; Z
  (assert-uim-equal 91  '(ichar-downcase 91))    ; [
  (assert-uim-equal 96  '(ichar-downcase 96))    ; `
  (assert-uim-equal 97  '(ichar-downcase 97))    ; a
  (assert-uim-equal 122 '(ichar-downcase 122))   ; z
  (assert-uim-equal 123 '(ichar-downcase 123))   ; {
  (assert-uim-equal 126 '(ichar-downcase 126))   ; ~
  (assert-uim-equal 127 '(ichar-downcase 127))   ; DEL
  #f)

(define (test-ichar-upcase)
  (assert-uim-equal 0   '(ichar-upcase 0))     ; NUL
  (assert-uim-equal 1   '(ichar-upcase 1))     ; SOH
  (assert-uim-equal 31  '(ichar-upcase 31))    ; US
  (assert-uim-equal 32  '(ichar-upcase 32))    ; SPACE
  (assert-uim-equal 33  '(ichar-upcase 33))    ; !
  (assert-uim-equal 47  '(ichar-upcase 47))    ; /
  (assert-uim-equal 48  '(ichar-upcase 48))    ; 0
  (assert-uim-equal 57  '(ichar-upcase 57))    ; 9
  (assert-uim-equal 58  '(ichar-upcase 58))    ; :
  (assert-uim-equal 64  '(ichar-upcase 64))    ; @
  (assert-uim-equal 65  '(ichar-upcase 65))    ; A
  (assert-uim-equal 90  '(ichar-upcase 90))    ; Z
  (assert-uim-equal 91  '(ichar-upcase 91))    ; [
  (assert-uim-equal 96  '(ichar-upcase 96))    ; `
  (assert-uim-equal 65  '(ichar-upcase 97))    ; a
  (assert-uim-equal 90  '(ichar-upcase 122))   ; z
  (assert-uim-equal 123 '(ichar-upcase 123))   ; {
  (assert-uim-equal 126 '(ichar-upcase 126))   ; ~
  (assert-uim-equal 127 '(ichar-upcase 127))   ; DEL
  #f)

(define (test-string->alphabetic-ichar)
  (assert-uim-false     '(string->alphabetic-ichar ""))       ; NUL
  (assert-uim-false-raw "(string->alphabetic-ichar \"\")")  ; SOH
  (assert-uim-false-raw "(string->alphabetic-ichar \"\")")  ; US
  (assert-uim-false     '(string->alphabetic-ichar " "))      ; SPACE
  (assert-uim-false     '(string->alphabetic-ichar "!"))      ; !
  (assert-uim-false     '(string->alphabetic-ichar "/"))      ; /
  (assert-uim-false     '(string->alphabetic-ichar "0"))      ; 0
  (assert-uim-false     '(string->alphabetic-ichar "9"))      ; 9
  (assert-uim-false     '(string->alphabetic-ichar ":"))      ; :
  (assert-uim-false     '(string->alphabetic-ichar "@"))      ; @
  (assert-uim-false     '(string->alphabetic-ichar "AA"))     ; AA
  (assert-uim-equal 65  '(string->alphabetic-ichar "A"))      ; A
  (assert-uim-equal 90  '(string->alphabetic-ichar "Z"))      ; Z
  (assert-uim-false     '(string->alphabetic-ichar "ZZ"))     ; ZZ
  (assert-uim-false     '(string->alphabetic-ichar "["))      ; [
  (assert-uim-false     '(string->alphabetic-ichar "`"))      ; `
  (assert-uim-false     '(string->alphabetic-ichar "aa"))     ; aa
  (assert-uim-equal 97  '(string->alphabetic-ichar "a"))      ; a
  (assert-uim-equal 122 '(string->alphabetic-ichar "z"))      ; z
  (assert-uim-false     '(string->alphabetic-ichar "zz"))     ; zz
  (assert-uim-false     '(string->alphabetic-ichar "{"))      ; {
  (assert-uim-false     '(string->alphabetic-ichar "~"))      ; ~
  (assert-uim-false-raw "(string->alphabetic-ichar \"\")")  ; DEL
  #f)

(provide "test/util/test-character-conversion")
