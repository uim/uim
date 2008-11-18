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
;;;

(define-module test.util.test-character-conversion
  (use test.unit.test-case)
  (use test.uim-test-utils-new))
(select-module test.util.test-character-conversion)

(define (setup)
  (uim-test-setup))

(define (teardown)
  (uim-test-teardown))

(define (test-numeric-ichar->integer)
  (assert-true  (uim-bool '(integer? (numeric-ichar->integer 0))))    ; NUL
  (assert-true  (uim-bool '(integer? (numeric-ichar->integer 1))))    ; SOH
  (assert-true  (uim-bool '(integer? (numeric-ichar->integer 31))))   ; US
  (assert-true  (uim-bool '(integer? (numeric-ichar->integer 32))))   ; SPACE
  (assert-true  (uim-bool '(integer? (numeric-ichar->integer 33))))   ; !
  (assert-true  (uim-bool '(integer? (numeric-ichar->integer 47))))   ; /
  (assert-equal 0 (uim '(numeric-ichar->integer 48)))                 ; 0
  (assert-equal 1 (uim '(numeric-ichar->integer 49)))                 ; 1
  (assert-equal 2 (uim '(numeric-ichar->integer 50)))                 ; 2
  (assert-equal 3 (uim '(numeric-ichar->integer 51)))                 ; 3
  (assert-equal 4 (uim '(numeric-ichar->integer 52)))                 ; 4
  (assert-equal 5 (uim '(numeric-ichar->integer 53)))                 ; 5
  (assert-equal 6 (uim '(numeric-ichar->integer 54)))                 ; 6
  (assert-equal 7 (uim '(numeric-ichar->integer 55)))                 ; 7
  (assert-equal 8 (uim '(numeric-ichar->integer 56)))                 ; 8
  (assert-equal 9 (uim '(numeric-ichar->integer 57)))                 ; 9
  (assert-true  (uim-bool '(integer? (numeric-ichar->integer 58))))   ; :
  (assert-true  (uim-bool '(integer? (numeric-ichar->integer 64))))   ; @
  (assert-true  (uim-bool '(integer? (numeric-ichar->integer 65))))   ; A
  (assert-true  (uim-bool '(integer? (numeric-ichar->integer 90))))   ; Z
  (assert-true  (uim-bool '(integer? (numeric-ichar->integer 91))))   ; [
  (assert-true  (uim-bool '(integer? (numeric-ichar->integer 96))))   ; `
  (assert-true  (uim-bool '(integer? (numeric-ichar->integer 97))))   ; a
  (assert-true  (uim-bool '(integer? (numeric-ichar->integer 122))))  ; z
  (assert-true  (uim-bool '(integer? (numeric-ichar->integer 123))))  ; {
  (assert-true  (uim-bool '(integer? (numeric-ichar->integer 126))))  ; ~
  (assert-true  (uim-bool '(integer? (numeric-ichar->integer 127))))  ; DEL
  #f)

(define (test-ichar-downcase)
  (assert-equal 0   (uim '(ichar-downcase 0)))     ; NUL
  (assert-equal 1   (uim '(ichar-downcase 1)))     ; SOH
  (assert-equal 31  (uim '(ichar-downcase 31)))    ; US
  (assert-equal 32  (uim '(ichar-downcase 32)))    ; SPACE
  (assert-equal 33  (uim '(ichar-downcase 33)))    ; !
  (assert-equal 47  (uim '(ichar-downcase 47)))    ; /
  (assert-equal 48  (uim '(ichar-downcase 48)))    ; 0
  (assert-equal 57  (uim '(ichar-downcase 57)))    ; 9
  (assert-equal 58  (uim '(ichar-downcase 58)))    ; :
  (assert-equal 64  (uim '(ichar-downcase 64)))    ; @
  (assert-equal 97  (uim '(ichar-downcase 65)))    ; A
  (assert-equal 122 (uim '(ichar-downcase 90)))    ; Z
  (assert-equal 91  (uim '(ichar-downcase 91)))    ; [
  (assert-equal 96  (uim '(ichar-downcase 96)))    ; `
  (assert-equal 97  (uim '(ichar-downcase 97)))    ; a
  (assert-equal 122 (uim '(ichar-downcase 122)))   ; z
  (assert-equal 123 (uim '(ichar-downcase 123)))   ; {
  (assert-equal 126 (uim '(ichar-downcase 126)))   ; ~
  (assert-equal 127 (uim '(ichar-downcase 127)))   ; DEL
  #f)

(define (test-ichar-upcase)
  (assert-equal 0   (uim '(ichar-upcase 0)))     ; NUL
  (assert-equal 1   (uim '(ichar-upcase 1)))     ; SOH
  (assert-equal 31  (uim '(ichar-upcase 31)))    ; US
  (assert-equal 32  (uim '(ichar-upcase 32)))    ; SPACE
  (assert-equal 33  (uim '(ichar-upcase 33)))    ; !
  (assert-equal 47  (uim '(ichar-upcase 47)))    ; /
  (assert-equal 48  (uim '(ichar-upcase 48)))    ; 0
  (assert-equal 57  (uim '(ichar-upcase 57)))    ; 9
  (assert-equal 58  (uim '(ichar-upcase 58)))    ; :
  (assert-equal 64  (uim '(ichar-upcase 64)))    ; @
  (assert-equal 65  (uim '(ichar-upcase 65)))    ; A
  (assert-equal 90  (uim '(ichar-upcase 90)))    ; Z
  (assert-equal 91  (uim '(ichar-upcase 91)))    ; [
  (assert-equal 96  (uim '(ichar-upcase 96)))    ; `
  (assert-equal 65  (uim '(ichar-upcase 97)))    ; a
  (assert-equal 90  (uim '(ichar-upcase 122)))   ; z
  (assert-equal 123 (uim '(ichar-upcase 123)))   ; {
  (assert-equal 126 (uim '(ichar-upcase 126)))   ; ~
  (assert-equal 127 (uim '(ichar-upcase 127)))   ; DEL
  #f)

(define (test-string->alphabetic-ichar)
  (assert-false (uim-bool '(string->alphabetic-ichar "")))    ; NUL
  ;; FIXME: Since these control chars are normalized to "\x01" and so on by
  ;; Gauche at first, uim-sh cannot interpret them without SRFI-75 support.
  ;;(assert-false (uim-bool '(string->alphabetic-ichar "")))  ; SOH
  ;;(assert-false (uim-bool '(string->alphabetic-ichar "")))  ; US
  (assert-false (uim-bool '(string->alphabetic-ichar " ")))   ; SPACE
  (assert-false (uim-bool '(string->alphabetic-ichar "!")))   ; !
  (assert-false (uim-bool '(string->alphabetic-ichar "/")))   ; /
  (assert-false (uim-bool '(string->alphabetic-ichar "0")))   ; 0
  (assert-false (uim-bool '(string->alphabetic-ichar "9")))   ; 9
  (assert-false (uim-bool '(string->alphabetic-ichar ":")))   ; :
  (assert-false (uim-bool '(string->alphabetic-ichar "@")))   ; @
  (assert-false (uim-bool '(string->alphabetic-ichar "AA")))  ; AA
  (assert-equal 65   (uim '(string->alphabetic-ichar "A")))   ; A
  (assert-equal 90   (uim '(string->alphabetic-ichar "Z")))   ; Z
  (assert-false (uim-bool '(string->alphabetic-ichar "ZZ")))  ; ZZ
  (assert-false (uim-bool '(string->alphabetic-ichar "[")))   ; [
  (assert-false (uim-bool '(string->alphabetic-ichar "`")))   ; `
  (assert-false (uim-bool '(string->alphabetic-ichar "aa")))  ; aa
  (assert-equal 97   (uim '(string->alphabetic-ichar "a")))   ; a
  (assert-equal 122  (uim '(string->alphabetic-ichar "z")))   ; z
  (assert-false (uim-bool '(string->alphabetic-ichar "zz")))  ; zz
  (assert-false (uim-bool '(string->alphabetic-ichar "{")))   ; {
  (assert-false (uim-bool '(string->alphabetic-ichar "~")))   ; ~
  ;;(assert-false (uim-bool '(string->alphabetic-ichar ""))) ; DEL
  #f)

(provide "test/util/test-character-conversion")
