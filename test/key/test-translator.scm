;;; Copyright (c) 2003-2009 uim Project http://code.google.com/p/uim/
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

(define-module test.key.test-translator
  (use test.unit.test-case)
  (use test.uim-test))
(select-module test.key.test-translator)

(define (setup)
  (uim-test-setup)
  (uim-eval
   '(begin
      (define test-shift-state (cdr (assq 'Shift_key key-state-alist)))
      (define test-ignore-case
        (caadr (parse-key-str "<IgnoreCase>" () 0 0)))
      (define test-ignore-shift
        (caadr (parse-key-str "<IgnoreShift>" () 0 0)))
      (define test-ignore-regular-shift
        (caadr (parse-key-str "<IgnoreRegularShift>" () 0 0))))))

(define (teardown)
  (uim-test-teardown))

(define (test-IgnoreCase-translator)
  (assert-equal '(0   0) (uim '(test-ignore-case 0 0))) ; NUL
  (assert-equal '(1   0) (uim '(test-ignore-case 1 0))) ; SOH
  (assert-equal '(31  0) (uim '(test-ignore-case 31 0))) ; US
  (assert-equal '(32  0) (uim '(test-ignore-case 32 0))) ; SPACE
  (assert-equal '(33  0) (uim '(test-ignore-case 33 0))) ; !
  (assert-equal '(47  0) (uim '(test-ignore-case 47 0))) ; /
  (assert-equal '(48  0) (uim '(test-ignore-case 48 0))) ; 0
  (assert-equal '(57  0) (uim '(test-ignore-case 57 0))) ; 9
  (assert-equal '(58  0) (uim '(test-ignore-case 58 0))) ; :
  (assert-equal '(64  0) (uim '(test-ignore-case 64 0))) ; @
  (assert-equal '(97  0) (uim '(test-ignore-case 65 0))) ; A
  (assert-equal '(122 0) (uim '(test-ignore-case 90 0))) ; Z
  (assert-equal '(91  0) (uim '(test-ignore-case 91 0))) ; [
  (assert-equal '(96  0) (uim '(test-ignore-case 96 0))) ; `
  (assert-equal '(97  0) (uim '(test-ignore-case 97 0))) ; a
  (assert-equal '(122 0) (uim '(test-ignore-case 122 0))) ; z
  (assert-equal '(123 0) (uim '(test-ignore-case 123 0))) ; {
  (assert-equal '(126 0) (uim '(test-ignore-case 126 0))) ; ~
  (assert-equal '(127 0) (uim '(test-ignore-case 127 0))) ; DEL
  #f)

(define (test-IgnoreShift-translator)
  (assert-equal '(0 0) (uim '(test-ignore-shift 0 test-shift-state))) ; NUL
  (assert-equal '(1 0) (uim '(test-ignore-shift 1 test-shift-state))) ; SOH
  (assert-equal '(31 0) (uim '(test-ignore-shift 31 test-shift-state))) ; US
  (assert-equal '(32 0) (uim '(test-ignore-shift 32 test-shift-state))) ; SPACE
  (assert-equal '(33 0) (uim '(test-ignore-shift 33 test-shift-state))) ; !
  (assert-equal '(47 0) (uim '(test-ignore-shift 47 test-shift-state))) ; /
  (assert-equal '(48 0) (uim '(test-ignore-shift 48 test-shift-state))) ; 0
  (assert-equal '(57 0) (uim '(test-ignore-shift 57 test-shift-state))) ; 9
  (assert-equal '(58 0) (uim '(test-ignore-shift 58 test-shift-state))) ; :
  (assert-equal '(64 0) (uim '(test-ignore-shift 64 test-shift-state))) ; @
  (assert-equal '(65 0) (uim '(test-ignore-shift 65 test-shift-state))) ; A
  (assert-equal '(90 0) (uim '(test-ignore-shift 90 test-shift-state))) ; Z
  (assert-equal '(91 0) (uim '(test-ignore-shift 91 test-shift-state))) ; [
  (assert-equal '(96 0) (uim '(test-ignore-shift 96 test-shift-state))) ; `
  (assert-equal '(97 0) (uim '(test-ignore-shift 97 test-shift-state))) ; a
  (assert-equal '(122 0) (uim '(test-ignore-shift 122 test-shift-state))) ; z
  (assert-equal '(123 0) (uim '(test-ignore-shift 123 test-shift-state))) ; {
  (assert-equal '(126 0) (uim '(test-ignore-shift 126 test-shift-state))) ; ~
  (assert-equal '(127 0) (uim '(test-ignore-shift 127 test-shift-state))) ; DEL

  (assert-equal '(0   0) (uim '(test-ignore-shift 0 0))) ; NUL
  (assert-equal '(1   0) (uim '(test-ignore-shift 1 0))) ; SOH
  (assert-equal '(31  0) (uim '(test-ignore-shift 31 0))) ; US
  (assert-equal '(32  0) (uim '(test-ignore-shift 32 0))) ; SPACE
  (assert-equal '(33  0) (uim '(test-ignore-shift 33 0))) ; !
  (assert-equal '(47  0) (uim '(test-ignore-shift 47 0))) ; /
  (assert-equal '(48  0) (uim '(test-ignore-shift 48 0))) ; 0
  (assert-equal '(57  0) (uim '(test-ignore-shift 57 0))) ; 9
  (assert-equal '(58  0) (uim '(test-ignore-shift 58 0))) ; :
  (assert-equal '(64  0) (uim '(test-ignore-shift 64 0))) ; @
  (assert-equal '(65  0) (uim '(test-ignore-shift 65 0))) ; A
  (assert-equal '(90  0) (uim '(test-ignore-shift 90 0))) ; Z
  (assert-equal '(91  0) (uim '(test-ignore-shift 91 0))) ; [
  (assert-equal '(96  0) (uim '(test-ignore-shift 96 0))) ; `
  (assert-equal '(97  0) (uim '(test-ignore-shift 97 0))) ; a
  (assert-equal '(122 0) (uim '(test-ignore-shift 122 0))) ; z
  (assert-equal '(123 0) (uim '(test-ignore-shift 123 0))) ; {
  (assert-equal '(126 0) (uim '(test-ignore-shift 126 0))) ; ~
  (assert-equal '(127 0) (uim '(test-ignore-shift 127 0))) ; DEL
  #f)

(define (test-IgnoreRegularShift-translator)
  (assert-equal (uim '(list 0 test-shift-state))
                (uim '(test-ignore-regular-shift 0 test-shift-state))) ; NUL
  (assert-equal (uim '(list 1 test-shift-state))
                (uim '(test-ignore-regular-shift 1 test-shift-state))) ; SOH
  (assert-equal (uim '(list 31 test-shift-state))
                (uim '(test-ignore-regular-shift 31 test-shift-state))) ; US
  (assert-equal (uim '(list 32 test-shift-state))
                (uim '(test-ignore-regular-shift 32 test-shift-state))) ; SPACE
  (assert-equal '(33 0)
                (uim '(test-ignore-regular-shift 33 test-shift-state))) ; !
  (assert-equal '(47 0)
                (uim '(test-ignore-regular-shift 47 test-shift-state))) ; /
  (assert-equal '(48 0)
                (uim '(test-ignore-regular-shift 48 test-shift-state))) ; 0
  (assert-equal '(57 0)
                (uim '(test-ignore-regular-shift 57 test-shift-state))) ; 9
  (assert-equal '(58 0)
                (uim '(test-ignore-regular-shift 58 test-shift-state))) ; :
  (assert-equal '(64 0)
                (uim '(test-ignore-regular-shift 64 test-shift-state))) ; @
  (assert-equal '(65 0)
                (uim '(test-ignore-regular-shift 65 test-shift-state))) ; A
  (assert-equal '(90 0)
                (uim '(test-ignore-regular-shift 90 test-shift-state))) ; Z
  (assert-equal '(91 0)
                (uim '(test-ignore-regular-shift 91 test-shift-state))) ; [
  (assert-equal '(96 0)
                (uim '(test-ignore-regular-shift 96 test-shift-state))) ; `
  (assert-equal '(97 0)
                (uim '(test-ignore-regular-shift 97 test-shift-state))) ; a
  (assert-equal '(122 0)
                (uim '(test-ignore-regular-shift 122 test-shift-state))) ; z
  (assert-equal '(123 0)
                (uim '(test-ignore-regular-shift 123 test-shift-state))) ; {
  (assert-equal '(126 0)
                (uim '(test-ignore-regular-shift 126 test-shift-state))) ; ~
  (assert-equal (uim '(list 127 test-shift-state))
                (uim '(test-ignore-regular-shift 127 test-shift-state))) ; DEL

  (assert-equal '(0   0) (uim '(test-ignore-regular-shift 0 0))) ; NUL
  (assert-equal '(1   0) (uim '(test-ignore-regular-shift 1 0))) ; SOH
  (assert-equal '(31  0) (uim '(test-ignore-regular-shift 31 0))) ; US
  (assert-equal '(32  0) (uim '(test-ignore-regular-shift 32 0))) ; SPACE
  (assert-equal '(33  0) (uim '(test-ignore-regular-shift 33 0))) ; !
  (assert-equal '(47  0) (uim '(test-ignore-regular-shift 47 0))) ; /
  (assert-equal '(48  0) (uim '(test-ignore-regular-shift 48 0))) ; 0
  (assert-equal '(57  0) (uim '(test-ignore-regular-shift 57 0))) ; 9
  (assert-equal '(58  0) (uim '(test-ignore-regular-shift 58 0))) ; :
  (assert-equal '(64  0) (uim '(test-ignore-regular-shift 64 0))) ; @
  (assert-equal '(65  0) (uim '(test-ignore-regular-shift 65 0))) ; A
  (assert-equal '(90  0) (uim '(test-ignore-regular-shift 90 0))) ; Z
  (assert-equal '(91  0) (uim '(test-ignore-regular-shift 91 0))) ; [
  (assert-equal '(96  0) (uim '(test-ignore-regular-shift 96 0))) ; `
  (assert-equal '(97  0) (uim '(test-ignore-regular-shift 97 0))) ; a
  (assert-equal '(122 0) (uim '(test-ignore-regular-shift 122 0))) ; z
  (assert-equal '(123 0) (uim '(test-ignore-regular-shift 123 0))) ; {
  (assert-equal '(126 0) (uim '(test-ignore-regular-shift 126 0))) ; ~
  (assert-equal '(127 0) (uim '(test-ignore-regular-shift 127 0))) ; DEL
  #f)

(define (test-apply-translators)
  ;; apply single translator
  (assert-equal (uim '(list () 0 test-shift-state))
                (uim '(apply-translators (list test-ignore-regular-shift)
                                         0 test-shift-state))) ; NUL
  (assert-equal (uim '(list () 1 test-shift-state))
                (uim '(apply-translators (list test-ignore-regular-shift)
                                         1 test-shift-state))) ; SOH
  (assert-equal (uim '(list () 31 test-shift-state))
                (uim '(apply-translators (list test-ignore-regular-shift)
                                         31 test-shift-state))) ; US
  (assert-equal (uim '(list () 32 test-shift-state))
                (uim '(apply-translators (list test-ignore-regular-shift)
                                         32 test-shift-state))) ; SPACE
  (assert-equal '(() 33 0)
                (uim '(apply-translators (list test-ignore-regular-shift)
                                         33 test-shift-state))) ; !
  (assert-equal '(() 47 0)
                (uim '(apply-translators (list test-ignore-regular-shift)
                                         47 test-shift-state))) ; /
  (assert-equal '(() 48 0)
                (uim '(apply-translators (list test-ignore-regular-shift)
                                         48 test-shift-state))) ; 0
  (assert-equal '(() 57 0)
                (uim '(apply-translators (list test-ignore-regular-shift)
                                         57 test-shift-state))) ; 9
  (assert-equal '(() 58 0)
                (uim '(apply-translators (list test-ignore-regular-shift)
                                         58 test-shift-state))) ; :
  (assert-equal '(() 64 0)
                (uim '(apply-translators (list test-ignore-regular-shift)
                                         64 test-shift-state))) ; @
  (assert-equal '(() 65 0)
                (uim '(apply-translators (list test-ignore-regular-shift)
                                         65 test-shift-state))) ; A
  (assert-equal '(() 90 0)
                (uim '(apply-translators (list test-ignore-regular-shift)
                                         90 test-shift-state))) ; Z
  (assert-equal '(() 91 0)
                (uim '(apply-translators (list test-ignore-regular-shift)
                                         91 test-shift-state))) ; [
  (assert-equal '(() 96 0)
                (uim '(apply-translators (list test-ignore-regular-shift)
                                         96 test-shift-state))) ; `
  (assert-equal '(() 97 0)
                (uim '(apply-translators (list test-ignore-regular-shift)
                                         97 test-shift-state))) ; a
  (assert-equal '(() 122 0)
                (uim '(apply-translators (list test-ignore-regular-shift)
                                         122 test-shift-state))) ; z
  (assert-equal '(() 123 0)
                (uim '(apply-translators (list test-ignore-regular-shift)
                                         123 test-shift-state))) ; {
  (assert-equal '(() 126 0)
                (uim '(apply-translators (list test-ignore-regular-shift)
                                         126 test-shift-state))) ; ~
  (assert-equal (uim '(list () 127 test-shift-state))
                (uim '(apply-translators (list test-ignore-regular-shift)
                                         127 test-shift-state))) ; DEL

  ;; apply multiple translator
  (assert-equal (uim '(list () 0 test-shift-state))
                (uim '(apply-translators (list test-ignore-regular-shift
                                               test-ignore-case)
                                         0 test-shift-state))) ; NUL
  (assert-equal (uim '(list () 1 test-shift-state))
                (uim '(apply-translators (list test-ignore-regular-shift
                                               test-ignore-case)
                                         1 test-shift-state))) ; SOH
  (assert-equal (uim '(list () 31 test-shift-state))
                (uim '(apply-translators (list test-ignore-regular-shift
                                               test-ignore-case)
                                         31 test-shift-state))) ; US
  (assert-equal (uim '(list () 32 test-shift-state))
                (uim '(apply-translators (list test-ignore-regular-shift
                                               test-ignore-case)
                                         32 test-shift-state))) ; SPACE
  (assert-equal '(() 33 0)
                (uim '(apply-translators (list test-ignore-regular-shift
                                               test-ignore-case)
                                         33 test-shift-state))) ; !
  (assert-equal '(() 47 0)
                (uim '(apply-translators (list test-ignore-regular-shift
                                               test-ignore-case)
                                         47 test-shift-state))) ; /
  (assert-equal '(() 48 0)
                (uim '(apply-translators (list test-ignore-regular-shift
                                               test-ignore-case)
                                         48 test-shift-state))) ; 0
  (assert-equal '(() 57 0)
                (uim '(apply-translators (list test-ignore-regular-shift
                                               test-ignore-case)
                                         57 test-shift-state))) ; 9
  (assert-equal '(() 58 0)
                (uim '(apply-translators (list test-ignore-regular-shift
                                               test-ignore-case)
                                         58 test-shift-state))) ; :
  (assert-equal '(() 64 0)
                (uim '(apply-translators (list test-ignore-regular-shift
                                               test-ignore-case)
                                         64 test-shift-state))) ; @
  (assert-equal '(() 97 0)
                (uim '(apply-translators (list test-ignore-regular-shift
                                               test-ignore-case)
                                         65 test-shift-state))) ; A
  (assert-equal '(() 122 0)
                (uim '(apply-translators (list test-ignore-regular-shift
                                               test-ignore-case)
                                         90 test-shift-state))) ; Z
  (assert-equal '(() 91 0)
                (uim '(apply-translators (list test-ignore-regular-shift
                                               test-ignore-case)
                                         91 test-shift-state))) ; [
  (assert-equal '(() 96 0)
                (uim '(apply-translators (list test-ignore-regular-shift
                                               test-ignore-case)
                                         96 test-shift-state))) ; `
  (assert-equal '(() 97 0)
                (uim '(apply-translators (list test-ignore-regular-shift
                                               test-ignore-case)
                                         97 test-shift-state))) ; a
  (assert-equal '(() 122 0)
                (uim '(apply-translators (list test-ignore-regular-shift
                                               test-ignore-case)
                                         122 test-shift-state))) ; z
  (assert-equal '(() 123 0)
                (uim '(apply-translators (list test-ignore-regular-shift
                                               test-ignore-case)
                                         123 test-shift-state))) ; {
  (assert-equal '(() 126 0)
                (uim '(apply-translators (list test-ignore-regular-shift
                                               test-ignore-case)
                                         126 test-shift-state))) ; ~
  (assert-equal (uim '(list () 127 test-shift-state))
                (uim '(apply-translators (list test-ignore-regular-shift
                                               test-ignore-case)
                                         127 test-shift-state))) ; DEL
  #f)

(provide "test/key/test-translator")
