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
;;;;

(require-extension (unittest))

(define test-shift-state (cdr (assq 'Shift_key key-state-alist)))
(define test-ignore-case
  (caadr (parse-key-str "<IgnoreCase>" () 0 0)))
(define test-ignore-shift
  (caadr (parse-key-str "<IgnoreShift>" () 0 0)))
(define test-ignore-regular-shift
  (caadr (parse-key-str "<IgnoreRegularShift>" () 0 0)))

(test-begin "IgnoreCase-translator")
(test-equal '(0   0)     (test-ignore-case 0 0)) ; NUL
(test-equal '(1   0)     (test-ignore-case 1 0)) ; SOH
(test-equal '(31  0)     (test-ignore-case 31 0)) ; US
(test-equal '(32  0)     (test-ignore-case 32 0)) ; SPACE
(test-equal '(33  0)     (test-ignore-case 33 0)) ; !
(test-equal '(47  0)     (test-ignore-case 47 0)) ; /
(test-equal '(48  0)     (test-ignore-case 48 0)) ; 0
(test-equal '(57  0)     (test-ignore-case 57 0)) ; 9
(test-equal '(58  0)     (test-ignore-case 58 0)) ; :
(test-equal '(64  0)     (test-ignore-case 64 0)) ; @
(test-equal '(97  0)     (test-ignore-case 65 0)) ; A
(test-equal '(122 0)     (test-ignore-case 90 0)) ; Z
(test-equal '(91  0)     (test-ignore-case 91 0)) ; [
(test-equal '(96  0)     (test-ignore-case 96 0)) ; `
(test-equal '(97  0)     (test-ignore-case 97 0)) ; a
(test-equal '(122 0)     (test-ignore-case 122 0)) ; z
(test-equal '(123 0)     (test-ignore-case 123 0)) ; {
(test-equal '(126 0)     (test-ignore-case 126 0)) ; ~
(test-equal '(127 0)     (test-ignore-case 127 0)) ; DEL
(test-end)

(test-begin "IgnoreShift-translator")
(test-equal '(0 0)     (test-ignore-shift 0 test-shift-state)) ; NUL
(test-equal '(1 0)     (test-ignore-shift 1 test-shift-state)) ; SOH
(test-equal '(31 0)     (test-ignore-shift 31 test-shift-state)) ; US
(test-equal '(32 0)     (test-ignore-shift 32 test-shift-state)) ; SPACE
(test-equal '(33 0)     (test-ignore-shift 33 test-shift-state)) ; !
(test-equal '(47 0)     (test-ignore-shift 47 test-shift-state)) ; /
(test-equal '(48 0)     (test-ignore-shift 48 test-shift-state)) ; 0
(test-equal '(57 0)     (test-ignore-shift 57 test-shift-state)) ; 9
(test-equal '(58 0)     (test-ignore-shift 58 test-shift-state)) ; :
(test-equal '(64 0)     (test-ignore-shift 64 test-shift-state)) ; @
(test-equal '(65 0)     (test-ignore-shift 65 test-shift-state)) ; A
(test-equal '(90 0)     (test-ignore-shift 90 test-shift-state)) ; Z
(test-equal '(91 0)     (test-ignore-shift 91 test-shift-state)) ; [
(test-equal '(96 0)     (test-ignore-shift 96 test-shift-state)) ; `
(test-equal '(97 0)     (test-ignore-shift 97 test-shift-state)) ; a
(test-equal '(122 0)     (test-ignore-shift 122 test-shift-state)) ; z
(test-equal '(123 0)     (test-ignore-shift 123 test-shift-state)) ; {
(test-equal '(126 0)     (test-ignore-shift 126 test-shift-state)) ; ~
(test-equal '(127 0)     (test-ignore-shift 127 test-shift-state)) ; DEL

(test-equal '(0   0)     (test-ignore-shift 0 0)) ; NUL
(test-equal '(1   0)     (test-ignore-shift 1 0)) ; SOH
(test-equal '(31  0)     (test-ignore-shift 31 0)) ; US
(test-equal '(32  0)     (test-ignore-shift 32 0)) ; SPACE
(test-equal '(33  0)     (test-ignore-shift 33 0)) ; !
(test-equal '(47  0)     (test-ignore-shift 47 0)) ; /
(test-equal '(48  0)     (test-ignore-shift 48 0)) ; 0
(test-equal '(57  0)     (test-ignore-shift 57 0)) ; 9
(test-equal '(58  0)     (test-ignore-shift 58 0)) ; :
(test-equal '(64  0)     (test-ignore-shift 64 0)) ; @
(test-equal '(65  0)     (test-ignore-shift 65 0)) ; A
(test-equal '(90  0)     (test-ignore-shift 90 0)) ; Z
(test-equal '(91  0)     (test-ignore-shift 91 0)) ; [
(test-equal '(96  0)     (test-ignore-shift 96 0)) ; `
(test-equal '(97  0)     (test-ignore-shift 97 0)) ; a
(test-equal '(122 0)     (test-ignore-shift 122 0)) ; z
(test-equal '(123 0)     (test-ignore-shift 123 0)) ; {
(test-equal '(126 0)     (test-ignore-shift 126 0)) ; ~
(test-equal '(127 0)     (test-ignore-shift 127 0)) ; DEL
(test-end)

(test-begin "IgnoreRegularShift-translator")
(test-equal (list 0 test-shift-state)
            (test-ignore-regular-shift 0 test-shift-state)) ; NUL
(test-equal (list 1 test-shift-state)
            (test-ignore-regular-shift 1 test-shift-state)) ; SOH
(test-equal (list 31 test-shift-state)
            (test-ignore-regular-shift 31 test-shift-state)) ; US
(test-equal (list 32 test-shift-state)
            (test-ignore-regular-shift 32 test-shift-state)) ; SPACE
(test-equal '(33 0)
            (test-ignore-regular-shift 33 test-shift-state)) ; !
(test-equal '(47 0)
            (test-ignore-regular-shift 47 test-shift-state)) ; /
(test-equal '(48 0)
            (test-ignore-regular-shift 48 test-shift-state)) ; 0
(test-equal '(57 0)
            (test-ignore-regular-shift 57 test-shift-state)) ; 9
(test-equal '(58 0)
            (test-ignore-regular-shift 58 test-shift-state)) ; :
(test-equal '(64 0)
            (test-ignore-regular-shift 64 test-shift-state)) ; @
(test-equal '(65 0)
            (test-ignore-regular-shift 65 test-shift-state)) ; A
(test-equal '(90 0)
            (test-ignore-regular-shift 90 test-shift-state)) ; Z
(test-equal '(91 0)
            (test-ignore-regular-shift 91 test-shift-state)) ; [
(test-equal '(96 0)
            (test-ignore-regular-shift 96 test-shift-state)) ; `
(test-equal '(97 0)
            (test-ignore-regular-shift 97 test-shift-state)) ; a
(test-equal '(122 0)
            (test-ignore-regular-shift 122 test-shift-state)) ; z
(test-equal '(123 0)
            (test-ignore-regular-shift 123 test-shift-state)) ; {
(test-equal '(126 0)
            (test-ignore-regular-shift 126 test-shift-state)) ; ~
(test-equal (list 127 test-shift-state)
            (test-ignore-regular-shift 127 test-shift-state)) ; DEL

(test-equal '(0   0)     (test-ignore-regular-shift 0 0)) ; NUL
(test-equal '(1   0)     (test-ignore-regular-shift 1 0)) ; SOH
(test-equal '(31  0)     (test-ignore-regular-shift 31 0)) ; US
(test-equal '(32  0)     (test-ignore-regular-shift 32 0)) ; SPACE
(test-equal '(33  0)     (test-ignore-regular-shift 33 0)) ; !
(test-equal '(47  0)     (test-ignore-regular-shift 47 0)) ; /
(test-equal '(48  0)     (test-ignore-regular-shift 48 0)) ; 0
(test-equal '(57  0)     (test-ignore-regular-shift 57 0)) ; 9
(test-equal '(58  0)     (test-ignore-regular-shift 58 0)) ; :
(test-equal '(64  0)     (test-ignore-regular-shift 64 0)) ; @
(test-equal '(65  0)     (test-ignore-regular-shift 65 0)) ; A
(test-equal '(90  0)     (test-ignore-regular-shift 90 0)) ; Z
(test-equal '(91  0)     (test-ignore-regular-shift 91 0)) ; [
(test-equal '(96  0)     (test-ignore-regular-shift 96 0)) ; `
(test-equal '(97  0)     (test-ignore-regular-shift 97 0)) ; a
(test-equal '(122 0)     (test-ignore-regular-shift 122 0)) ; z
(test-equal '(123 0)     (test-ignore-regular-shift 123 0)) ; {
(test-equal '(126 0)     (test-ignore-regular-shift 126 0)) ; ~
(test-equal '(127 0)     (test-ignore-regular-shift 127 0)) ; DEL
(test-end)

(test-begin "apply-translators")
;; apply single translator
(test-equal (list () 0 test-shift-state)
            (apply-translators (list test-ignore-regular-shift)
                                0 test-shift-state)) ; NUL
(test-equal (list () 1 test-shift-state)
            (apply-translators (list test-ignore-regular-shift)
                                1 test-shift-state)) ; SOH
(test-equal (list () 31 test-shift-state)
            (apply-translators (list test-ignore-regular-shift)
                                31 test-shift-state)) ; US
(test-equal (list () 32 test-shift-state)
            (apply-translators (list test-ignore-regular-shift)
                                32 test-shift-state)) ; SPACE
(test-equal '(() 33 0)
            (apply-translators (list test-ignore-regular-shift)
                                33 test-shift-state)) ; !
(test-equal '(() 47 0)
            (apply-translators (list test-ignore-regular-shift)
                                47 test-shift-state)) ; /
(test-equal '(() 48 0)
            (apply-translators (list test-ignore-regular-shift)
                                48 test-shift-state)) ; 0
(test-equal '(() 57 0)
            (apply-translators (list test-ignore-regular-shift)
                                57 test-shift-state)) ; 9
(test-equal '(() 58 0)
            (apply-translators (list test-ignore-regular-shift)
                                58 test-shift-state)) ; :
(test-equal '(() 64 0)
            (apply-translators (list test-ignore-regular-shift)
                                64 test-shift-state)) ; @
(test-equal '(() 65 0)
            (apply-translators (list test-ignore-regular-shift)
                                65 test-shift-state)) ; A
(test-equal '(() 90 0)
            (apply-translators (list test-ignore-regular-shift)
                                90 test-shift-state)) ; Z
(test-equal '(() 91 0)
            (apply-translators (list test-ignore-regular-shift)
                                91 test-shift-state)) ; [
(test-equal '(() 96 0)
            (apply-translators (list test-ignore-regular-shift)
                                96 test-shift-state)) ; `
(test-equal '(() 97 0)
            (apply-translators (list test-ignore-regular-shift)
                                97 test-shift-state)) ; a
(test-equal '(() 122 0)
            (apply-translators (list test-ignore-regular-shift)
                                122 test-shift-state)) ; z
(test-equal '(() 123 0)
            (apply-translators (list test-ignore-regular-shift)
                                123 test-shift-state)) ; {
(test-equal '(() 126 0)
            (apply-translators (list test-ignore-regular-shift)
                                126 test-shift-state)) ; ~
(test-equal (list () 127 test-shift-state)
            (apply-translators (list test-ignore-regular-shift)
                                127 test-shift-state)) ; DEL

;; apply multiple translator
(test-equal (list () 0 test-shift-state)
            (apply-translators (list test-ignore-regular-shift
                                      test-ignore-case)
                                0 test-shift-state)) ; NUL
(test-equal (list () 1 test-shift-state)
            (apply-translators (list test-ignore-regular-shift
                                      test-ignore-case)
                                1 test-shift-state)) ; SOH
(test-equal (list () 31 test-shift-state)
            (apply-translators (list test-ignore-regular-shift
                                      test-ignore-case)
                                31 test-shift-state)) ; US
(test-equal (list () 32 test-shift-state)
            (apply-translators (list test-ignore-regular-shift
                                      test-ignore-case)
                                32 test-shift-state)) ; SPACE
(test-equal '(() 33 0)
            (apply-translators (list test-ignore-regular-shift
                                      test-ignore-case)
                                33 test-shift-state)) ; !
(test-equal '(() 47 0)
            (apply-translators (list test-ignore-regular-shift
                                      test-ignore-case)
                                47 test-shift-state)) ; /
(test-equal '(() 48 0)
            (apply-translators (list test-ignore-regular-shift
                                      test-ignore-case)
                                48 test-shift-state)) ; 0
(test-equal '(() 57 0)
            (apply-translators (list test-ignore-regular-shift
                                      test-ignore-case)
                                57 test-shift-state)) ; 9
(test-equal '(() 58 0)
            (apply-translators (list test-ignore-regular-shift
                                      test-ignore-case)
                                58 test-shift-state)) ; :
(test-equal '(() 64 0)
            (apply-translators (list test-ignore-regular-shift
                                      test-ignore-case)
                                64 test-shift-state)) ; @
(test-equal '(() 97 0)
            (apply-translators (list test-ignore-regular-shift
                                      test-ignore-case)
                                65 test-shift-state)) ; A
(test-equal '(() 122 0)
            (apply-translators (list test-ignore-regular-shift
                                      test-ignore-case)
                                90 test-shift-state)) ; Z
(test-equal '(() 91 0)
            (apply-translators (list test-ignore-regular-shift
                                      test-ignore-case)
                                91 test-shift-state)) ; [
(test-equal '(() 96 0)
            (apply-translators (list test-ignore-regular-shift
                                      test-ignore-case)
                                96 test-shift-state)) ; `
(test-equal '(() 97 0)
            (apply-translators (list test-ignore-regular-shift
                                      test-ignore-case)
                                97 test-shift-state)) ; a
(test-equal '(() 122 0)
            (apply-translators (list test-ignore-regular-shift
                                      test-ignore-case)
                                122 test-shift-state)) ; z
(test-equal '(() 123 0)
            (apply-translators (list test-ignore-regular-shift
                                      test-ignore-case)
                                123 test-shift-state)) ; {
(test-equal '(() 126 0)
            (apply-translators (list test-ignore-regular-shift
                                      test-ignore-case)
                                126 test-shift-state)) ; ~
(test-equal (list () 127 test-shift-state)
            (apply-translators (list test-ignore-regular-shift
                                      test-ignore-case)
                                127 test-shift-state)) ; DEL
(test-end)

(test-report-result)
