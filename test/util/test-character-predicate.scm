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

(require-extension (unittest))

(test-begin "ichar-control?")
(test-false (ichar-control? 'symbol))
(test-false (ichar-control? "string"))
(test-false (ichar-control? '(0 1 2)))
(test-false (ichar-control? car))
(test-eq #t (ichar-control? 0))    ; NUL
(test-eq #t (ichar-control? 1))    ; SOH
(test-eq #t (ichar-control? 31))   ; US
(test-false (ichar-control? 32))   ; SPACE
(test-false (ichar-control? 33))   ; !
(test-false (ichar-control? 47))   ; /
(test-false (ichar-control? 48))   ; 0
(test-false (ichar-control? 57))   ; 9
(test-false (ichar-control? 58))   ; :
(test-false (ichar-control? 64))   ; @
(test-false (ichar-control? 65))   ; A
(test-false (ichar-control? 90))   ; Z
(test-false (ichar-control? 91))   ; [
(test-false (ichar-control? 96))   ; `
(test-false (ichar-control? 97))   ; a
(test-false (ichar-control? 122))  ; z
(test-false (ichar-control? 123))  ; {
(test-false (ichar-control? 126))  ; ~
(test-eq #t (ichar-control? 127))  ; DEL
(test-end)

(test-begin "ichar-upper-case?")
(test-false (ichar-upper-case? 'symbol))
(test-false (ichar-upper-case? "string"))
(test-false (ichar-upper-case? '(0 1 2)))
(test-false (ichar-upper-case? car))
(test-false (ichar-upper-case? 0))   ; NUL
(test-false (ichar-upper-case? 1))   ; SOH
(test-false (ichar-upper-case? 31))  ; US
(test-false (ichar-upper-case? 32))  ; SPACE
(test-false (ichar-upper-case? 33))  ; !
(test-false (ichar-upper-case? 47))  ; /
(test-false (ichar-upper-case? 48))  ; 0
(test-false (ichar-upper-case? 57))  ; 9
(test-false (ichar-upper-case? 58))  ; :
(test-false (ichar-upper-case? 64))  ; @
(test-eq #t (ichar-upper-case? 65))  ; A
(test-eq #t (ichar-upper-case? 90))  ; Z
(test-false (ichar-upper-case? 91))  ; [
(test-false (ichar-upper-case? 96))  ; `
(test-false (ichar-upper-case? 97))  ; a
(test-false (ichar-upper-case? 122)) ; z
(test-false (ichar-upper-case? 123)) ; {
(test-false (ichar-upper-case? 126)) ; ~
(test-false (ichar-upper-case? 127)) ; DEL
(test-end)

(test-begin "ichar-lower-case?")
(test-false (ichar-lower-case? 'symbol))
(test-false (ichar-lower-case? "string"))
(test-false (ichar-lower-case? '(0 1 2)))
(test-false (ichar-lower-case? car))
(test-false (ichar-lower-case? 0))   ; NUL
(test-false (ichar-lower-case? 1))   ; SOH
(test-false (ichar-lower-case? 31))  ; US
(test-false (ichar-lower-case? 32))  ; SPACE
(test-false (ichar-lower-case? 33))  ; !
(test-false (ichar-lower-case? 47))  ; /
(test-false (ichar-lower-case? 48))  ; 0
(test-false (ichar-lower-case? 57))  ; 9
(test-false (ichar-lower-case? 58))  ; :
(test-false (ichar-lower-case? 64))  ; @
(test-false (ichar-lower-case? 65))  ; A
(test-false (ichar-lower-case? 90))  ; Z
(test-false (ichar-lower-case? 91))  ; [
(test-false (ichar-lower-case? 96))  ; `
(test-eq #t (ichar-lower-case? 97))  ; a
(test-eq #t (ichar-lower-case? 122)) ; z
(test-false (ichar-lower-case? 123)) ; {
(test-false (ichar-lower-case? 126)) ; ~
(test-false (ichar-lower-case? 127)) ; DEL
(test-end)

(test-begin "ichar-alphabetic?")
(test-false (ichar-alphabetic? 'symbol))
(test-false (ichar-alphabetic? "string"))
(test-false (ichar-alphabetic? '(0 1 2)))
(test-false (ichar-alphabetic? car))
(test-false (ichar-alphabetic? 0))   ; NUL
(test-false (ichar-alphabetic? 1))   ; SOH
(test-false (ichar-alphabetic? 31))  ; US
(test-false (ichar-alphabetic? 32))  ; SPACE
(test-false (ichar-alphabetic? 33))  ; !
(test-false (ichar-alphabetic? 47))  ; /
(test-false (ichar-alphabetic? 48))  ; 0
(test-false (ichar-alphabetic? 57))  ; 9
(test-false (ichar-alphabetic? 58))  ; :
(test-false (ichar-alphabetic? 64))  ; @
(test-eq #t (ichar-alphabetic? 65))  ; A
(test-eq #t (ichar-alphabetic? 90))  ; Z
(test-false (ichar-alphabetic? 91))  ; [
(test-false (ichar-alphabetic? 96))  ; `
(test-eq #t (ichar-alphabetic? 97))  ; a
(test-eq #t (ichar-alphabetic? 122)) ; z
(test-false (ichar-alphabetic? 123)) ; {
(test-false (ichar-alphabetic? 126)) ; ~
(test-false (ichar-alphabetic? 127)) ; DEL
(test-end)

(test-begin "ichar-numeric?")
(test-false (ichar-numeric? 'symbol))
(test-false (ichar-numeric? "string"))
(test-false (ichar-numeric? '(0 1 2)))
(test-false (ichar-numeric? car))
(test-false (ichar-numeric? 0))        ; NUL
(test-false (ichar-numeric? 1))        ; SOH
(test-false (ichar-numeric? 31))       ; US
(test-false (ichar-numeric? 32)) ; SPACE
(test-false (ichar-numeric? 33)) ; !
(test-false (ichar-numeric? 47)) ; /
(test-eq #t (ichar-numeric? 48)) ; 0
(test-eq #t (ichar-numeric? 57)) ; 9
(test-false (ichar-numeric? 58)) ; :
(test-false (ichar-numeric? 64)) ; @
(test-false (ichar-numeric? 65)) ; A
(test-false (ichar-numeric? 90)) ; Z
(test-false (ichar-numeric? 91)) ; [
(test-false (ichar-numeric? 96)) ; `
(test-false (ichar-numeric? 97)) ; a
(test-false (ichar-numeric? 122))        ; z
(test-false (ichar-numeric? 123))        ; {
(test-false (ichar-numeric? 126))        ; ~
(test-false (ichar-numeric? 127)) ; DEL
(test-end)

(test-begin "ichar-printable?")
(test-false (ichar-printable? 'symbol))
(test-false (ichar-printable? "string"))
(test-false (ichar-printable? '(0 1 2)))
(test-false (ichar-printable? car))
(test-false (ichar-printable? 0))    ; NUL
(test-false (ichar-printable? 1))    ; SOH
(test-false (ichar-printable? 31))   ; US
(test-eq #t (ichar-printable? 32))   ; SPACE
(test-eq #t (ichar-printable? 33))   ; !
(test-eq #t (ichar-printable? 47))   ; /
(test-eq #t (ichar-printable? 48))   ; 0
(test-eq #t (ichar-printable? 57))   ; 9
(test-eq #t (ichar-printable? 58))   ; :
(test-eq #t (ichar-printable? 64))   ; @
(test-eq #t (ichar-printable? 65))   ; A
(test-eq #t (ichar-printable? 90))   ; Z
(test-eq #t (ichar-printable? 91))   ; [
(test-eq #t (ichar-printable? 96))   ; `
(test-eq #t (ichar-printable? 97))   ; a
(test-eq #t (ichar-printable? 122))  ; z
(test-eq #t (ichar-printable? 123))  ; {
(test-eq #t (ichar-printable? 126))  ; ~
(test-false (ichar-printable? 127))  ; DEL
(test-end)

(test-begin "ichar-graphic?")
(test-false (ichar-graphic? 'symbol))
(test-false (ichar-graphic? "string"))
(test-false (ichar-graphic? '(0 1 2)))
(test-false (ichar-graphic? car))
(test-false (ichar-graphic? 0))    ; NUL
(test-false (ichar-graphic? 1))    ; SOH
(test-false (ichar-graphic? 31))   ; US
(test-false (ichar-graphic? 32))   ; SPACE
(test-eq #t (ichar-graphic? 33))   ; !
(test-eq #t (ichar-graphic? 47))   ; /
(test-eq #t (ichar-graphic? 48))   ; 0
(test-eq #t (ichar-graphic? 57))   ; 9
(test-eq #t (ichar-graphic? 58))   ; :
(test-eq #t (ichar-graphic? 64))   ; @
(test-eq #t (ichar-graphic? 65))   ; A
(test-eq #t (ichar-graphic? 90))   ; Z
(test-eq #t (ichar-graphic? 91))   ; [
(test-eq #t (ichar-graphic? 96))   ; `
(test-eq #t (ichar-graphic? 97))   ; a
(test-eq #t (ichar-graphic? 122))  ; z
(test-eq #t (ichar-graphic? 123))  ; {
(test-eq #t (ichar-graphic? 126))  ; ~
(test-false (ichar-graphic? 127))  ; DEL
(test-end)

(test-report-result)
