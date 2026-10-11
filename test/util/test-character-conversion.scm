;;; Copyright (c) 2003-2026 uim Project https://github.com/uim/uim
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

(require-extension (unittest))

(test-begin "numeric-ichar->integer")
(test-eq #t (integer? (numeric-ichar->integer 0)))    ; NUL
(test-eq #t (integer? (numeric-ichar->integer 1)))    ; SOH
(test-eq #t (integer? (numeric-ichar->integer 31)))   ; US
(test-eq #t (integer? (numeric-ichar->integer 32)))   ; SPACE
(test-eq #t (integer? (numeric-ichar->integer 33)))   ; !
(test-eq #t (integer? (numeric-ichar->integer 47)))   ; /
(test-equal 0 (numeric-ichar->integer 48))              ; 0
(test-equal 1 (numeric-ichar->integer 49))              ; 1
(test-equal 2 (numeric-ichar->integer 50))              ; 2
(test-equal 3 (numeric-ichar->integer 51))              ; 3
(test-equal 4 (numeric-ichar->integer 52))              ; 4
(test-equal 5 (numeric-ichar->integer 53))              ; 5
(test-equal 6 (numeric-ichar->integer 54))              ; 6
(test-equal 7 (numeric-ichar->integer 55))              ; 7
(test-equal 8 (numeric-ichar->integer 56))              ; 8
(test-equal 9 (numeric-ichar->integer 57))              ; 9
(test-eq #t (integer? (numeric-ichar->integer 58)))   ; :
(test-eq #t (integer? (numeric-ichar->integer 64)))   ; @
(test-eq #t (integer? (numeric-ichar->integer 65)))   ; A
(test-eq #t (integer? (numeric-ichar->integer 90)))   ; Z
(test-eq #t (integer? (numeric-ichar->integer 91)))   ; [
(test-eq #t (integer? (numeric-ichar->integer 96)))   ; `
(test-eq #t (integer? (numeric-ichar->integer 97)))   ; a
(test-eq #t (integer? (numeric-ichar->integer 122)))  ; z
(test-eq #t (integer? (numeric-ichar->integer 123)))  ; {
(test-eq #t (integer? (numeric-ichar->integer 126)))  ; ~
(test-eq #t (integer? (numeric-ichar->integer 127)))  ; DEL
(test-end)

(test-begin "ichar-downcase")
(test-equal 0   (ichar-downcase 0))     ; NUL
(test-equal 1   (ichar-downcase 1))     ; SOH
(test-equal 31  (ichar-downcase 31))    ; US
(test-equal 32  (ichar-downcase 32))    ; SPACE
(test-equal 33  (ichar-downcase 33))    ; !
(test-equal 47  (ichar-downcase 47))    ; /
(test-equal 48  (ichar-downcase 48))    ; 0
(test-equal 57  (ichar-downcase 57))    ; 9
(test-equal 58  (ichar-downcase 58))    ; :
(test-equal 64  (ichar-downcase 64))    ; @
(test-equal 97  (ichar-downcase 65))    ; A
(test-equal 122 (ichar-downcase 90))    ; Z
(test-equal 91  (ichar-downcase 91))    ; [
(test-equal 96  (ichar-downcase 96))    ; `
(test-equal 97  (ichar-downcase 97))    ; a
(test-equal 122 (ichar-downcase 122))   ; z
(test-equal 123 (ichar-downcase 123))   ; {
(test-equal 126 (ichar-downcase 126))   ; ~
(test-equal 127 (ichar-downcase 127))   ; DEL
(test-end)

(test-begin "ichar-upcase")
(test-equal 0   (ichar-upcase 0))     ; NUL
(test-equal 1   (ichar-upcase 1))     ; SOH
(test-equal 31  (ichar-upcase 31))    ; US
(test-equal 32  (ichar-upcase 32))    ; SPACE
(test-equal 33  (ichar-upcase 33))    ; !
(test-equal 47  (ichar-upcase 47))    ; /
(test-equal 48  (ichar-upcase 48))    ; 0
(test-equal 57  (ichar-upcase 57))    ; 9
(test-equal 58  (ichar-upcase 58))    ; :
(test-equal 64  (ichar-upcase 64))    ; @
(test-equal 65  (ichar-upcase 65))    ; A
(test-equal 90  (ichar-upcase 90))    ; Z
(test-equal 91  (ichar-upcase 91))    ; [
(test-equal 96  (ichar-upcase 96))    ; `
(test-equal 65  (ichar-upcase 97))    ; a
(test-equal 90  (ichar-upcase 122))   ; z
(test-equal 123 (ichar-upcase 123))   ; {
(test-equal 126 (ichar-upcase 126))   ; ~
(test-equal 127 (ichar-upcase 127))   ; DEL
(test-end)

(test-begin "string->alphabetic-ichar")
(test-false     (string->alphabetic-ichar ""))       ; NUL
(test-false     (string->alphabetic-ichar (string (integer->char 1))))   ; SOH
(test-false     (string->alphabetic-ichar (string (integer->char 31))))  ; US
(test-false     (string->alphabetic-ichar " "))      ; SPACE
(test-false     (string->alphabetic-ichar "!"))      ; !
(test-false     (string->alphabetic-ichar "/"))      ; /
(test-false     (string->alphabetic-ichar "0"))      ; 0
(test-false     (string->alphabetic-ichar "9"))      ; 9
(test-false     (string->alphabetic-ichar ":"))      ; :
(test-false     (string->alphabetic-ichar "@"))      ; @
(test-false     (string->alphabetic-ichar "AA"))     ; AA
(test-equal 65  (string->alphabetic-ichar "A"))      ; A
(test-equal 90  (string->alphabetic-ichar "Z"))      ; Z
(test-false     (string->alphabetic-ichar "ZZ"))     ; ZZ
(test-false     (string->alphabetic-ichar "["))      ; [
(test-false     (string->alphabetic-ichar "`"))      ; `
(test-false     (string->alphabetic-ichar "aa"))     ; aa
(test-equal 97  (string->alphabetic-ichar "a"))      ; a
(test-equal 122 (string->alphabetic-ichar "z"))      ; z
(test-false     (string->alphabetic-ichar "zz"))     ; zz
(test-false     (string->alphabetic-ichar "{"))      ; {
(test-false     (string->alphabetic-ichar "~"))      ; ~
(test-false     (string->alphabetic-ichar (string (integer->char 127)))) ; DEL
(test-end)

(test-report-result)
