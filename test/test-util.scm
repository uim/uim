#!/usr/bin/env gosh

;;; Copyright (c) 2003-2005 uim Project http://uim.freedesktop.org/
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

;; This file is tested with revision 816 (new repository)

(use test.unit)

(require "test/uim-test-utils")

(define-uim-test-case "testcase util character predicates"
  ("test char-control?"
   (assert-false (uim-bool '(char-control? 'symbol)))
   (assert-false (uim-bool '(char-control? "string")))
   (assert-false (uim-bool '(char-control? '(0 1 2))))
   (assert-false (uim-bool '(char-control? car)))
   (assert-true  (uim-bool '(char-control? 0)))    ; NUL
   (assert-true  (uim-bool '(char-control? 1)))    ; SOH
   (assert-true  (uim-bool '(char-control? 31)))   ; US
   (assert-false (uim-bool '(char-control? 32)))   ; SPACE
   (assert-false (uim-bool '(char-control? 33)))   ; !
   (assert-false (uim-bool '(char-control? 47)))   ; /
   (assert-false (uim-bool '(char-control? 48)))   ; 0
   (assert-false (uim-bool '(char-control? 57)))   ; 9
   (assert-false (uim-bool '(char-control? 58)))   ; :
   (assert-false (uim-bool '(char-control? 64)))   ; @
   (assert-false (uim-bool '(char-control? 65)))   ; A
   (assert-false (uim-bool '(char-control? 90)))   ; Z
   (assert-false (uim-bool '(char-control? 91)))   ; [
   (assert-false (uim-bool '(char-control? 96)))   ; `
   (assert-false (uim-bool '(char-control? 97)))   ; a
   (assert-false (uim-bool '(char-control? 122)))  ; z
   (assert-false (uim-bool '(char-control? 123)))  ; {
   (assert-false (uim-bool '(char-control? 126)))  ; ~
   (assert-true  (uim-bool '(char-control? 127)))) ; DEL
  ("test char-upper-case?"
   (assert-false (uim-bool '(char-upper-case? 'symbol)))
   (assert-false (uim-bool '(char-upper-case? "string")))
   (assert-false (uim-bool '(char-upper-case? '(0 1 2))))
   (assert-false (uim-bool '(char-upper-case? car)))
   (assert-false (uim-bool '(char-upper-case? 0)))    ; NUL
   (assert-false (uim-bool '(char-upper-case? 1)))    ; SOH
   (assert-false (uim-bool '(char-upper-case? 31)))   ; US
   (assert-false (uim-bool '(char-upper-case? 32)))   ; SPACE
   (assert-false (uim-bool '(char-upper-case? 33)))   ; !
   (assert-false (uim-bool '(char-upper-case? 47)))   ; /
   (assert-false (uim-bool '(char-upper-case? 48)))   ; 0
   (assert-false (uim-bool '(char-upper-case? 57)))   ; 9
   (assert-false (uim-bool '(char-upper-case? 58)))   ; :
   (assert-false (uim-bool '(char-upper-case? 64)))   ; @
   (assert-true  (uim-bool '(char-upper-case? 65)))   ; A
   (assert-true  (uim-bool '(char-upper-case? 90)))   ; Z
   (assert-false (uim-bool '(char-upper-case? 91)))   ; [
   (assert-false (uim-bool '(char-upper-case? 96)))   ; `
   (assert-false (uim-bool '(char-upper-case? 97)))   ; a
   (assert-false (uim-bool '(char-upper-case? 122)))  ; z
   (assert-false (uim-bool '(char-upper-case? 123)))  ; {
   (assert-false (uim-bool '(char-upper-case? 126)))  ; ~
   (assert-false (uim-bool '(char-upper-case? 127)))) ; DEL
  ("test char-lower-case?"
   (assert-false (uim-bool '(char-lower-case? 'symbol)))
   (assert-false (uim-bool '(char-lower-case? "string")))
   (assert-false (uim-bool '(char-lower-case? '(0 1 2))))
   (assert-false (uim-bool '(char-lower-case? car)))
   (assert-false (uim-bool '(char-lower-case? 0)))    ; NUL
   (assert-false (uim-bool '(char-lower-case? 1)))    ; SOH
   (assert-false (uim-bool '(char-lower-case? 31)))   ; US
   (assert-false (uim-bool '(char-lower-case? 32)))   ; SPACE
   (assert-false (uim-bool '(char-lower-case? 33)))   ; !
   (assert-false (uim-bool '(char-lower-case? 47)))   ; /
   (assert-false (uim-bool '(char-lower-case? 48)))   ; 0
   (assert-false (uim-bool '(char-lower-case? 57)))   ; 9
   (assert-false (uim-bool '(char-lower-case? 58)))   ; :
   (assert-false (uim-bool '(char-lower-case? 64)))   ; @
   (assert-false (uim-bool '(char-lower-case? 65)))   ; A
   (assert-false (uim-bool '(char-lower-case? 90)))   ; Z
   (assert-false (uim-bool '(char-lower-case? 91)))   ; [
   (assert-false (uim-bool '(char-lower-case? 96)))   ; `
   (assert-true  (uim-bool '(char-lower-case? 97)))   ; a
   (assert-true  (uim-bool '(char-lower-case? 122)))  ; z
   (assert-false (uim-bool '(char-lower-case? 123)))  ; {
   (assert-false (uim-bool '(char-lower-case? 126)))  ; ~
   (assert-false (uim-bool '(char-lower-case? 127)))) ; DEL
  ("test char-alphabetic?"
   (assert-false (uim-bool '(char-alphabetic? 'symbol)))
   (assert-false (uim-bool '(char-alphabetic? "string")))
   (assert-false (uim-bool '(char-alphabetic? '(0 1 2))))
   (assert-false (uim-bool '(char-alphabetic? car)))
   (assert-false (uim-bool '(char-alphabetic? 0)))    ; NUL
   (assert-false (uim-bool '(char-alphabetic? 1)))    ; SOH
   (assert-false (uim-bool '(char-alphabetic? 31)))   ; US
   (assert-false (uim-bool '(char-alphabetic? 32)))   ; SPACE
   (assert-false (uim-bool '(char-alphabetic? 33)))   ; !
   (assert-false (uim-bool '(char-alphabetic? 47)))   ; /
   (assert-false (uim-bool '(char-alphabetic? 48)))   ; 0
   (assert-false (uim-bool '(char-alphabetic? 57)))   ; 9
   (assert-false (uim-bool '(char-alphabetic? 58)))   ; :
   (assert-false (uim-bool '(char-alphabetic? 64)))   ; @
   (assert-true  (uim-bool '(char-alphabetic? 65)))   ; A
   (assert-true  (uim-bool '(char-alphabetic? 90)))   ; Z
   (assert-false (uim-bool '(char-alphabetic? 91)))   ; [
   (assert-false (uim-bool '(char-alphabetic? 96)))   ; `
   (assert-true  (uim-bool '(char-alphabetic? 97)))   ; a
   (assert-true  (uim-bool '(char-alphabetic? 122)))  ; z
   (assert-false (uim-bool '(char-alphabetic? 123)))  ; {
   (assert-false (uim-bool '(char-alphabetic? 126)))  ; ~
   (assert-false (uim-bool '(char-alphabetic? 127)))) ; DEL
  ("test char-numeric?"
   (assert-false (uim-bool '(char-numeric? 'symbol)))
   (assert-false (uim-bool '(char-numeric? "string")))
   (assert-false (uim-bool '(char-numeric? '(0 1 2))))
   (assert-false (uim-bool '(char-numeric? car)))
   (assert-false (uim-bool '(char-numeric? 0)))     ; NUL
   (assert-false (uim-bool '(char-numeric? 1)))     ; SOH
   (assert-false (uim-bool '(char-numeric? 31)))    ; US
   (assert-false (uim-bool '(char-numeric? 32)))    ; SPACE
   (assert-false (uim-bool '(char-numeric? 33)))    ; !
   (assert-false (uim-bool '(char-numeric? 47)))    ; /
   (assert-true  (uim-bool '(char-numeric? 48)))    ; 0
   (assert-true  (uim-bool '(char-numeric? 57)))    ; 9
   (assert-false (uim-bool '(char-numeric? 58)))    ; :
   (assert-false (uim-bool '(char-numeric? 64)))    ; @
   (assert-false (uim-bool '(char-numeric? 65)))    ; A
   (assert-false (uim-bool '(char-numeric? 90)))    ; Z
   (assert-false (uim-bool '(char-numeric? 91)))    ; [
   (assert-false (uim-bool '(char-numeric? 96)))    ; `
   (assert-false (uim-bool '(char-numeric? 97)))    ; a
   (assert-false (uim-bool '(char-numeric? 122)))   ; z
   (assert-false (uim-bool '(char-numeric? 123)))   ; {
   (assert-false (uim-bool '(char-numeric? 126)))   ; ~
   (assert-false (uim-bool '(char-numeric? 127)))) ; DEL
  ("test char-printable?"
   (assert-false (uim-bool '(char-printable? 'symbol)))
   (assert-false (uim-bool '(char-printable? "string")))
   (assert-false (uim-bool '(char-printable? '(0 1 2))))
   (assert-false (uim-bool '(char-printable? car)))
   (assert-false (uim-bool '(char-printable? 0)))    ; NUL
   (assert-false (uim-bool '(char-printable? 1)))    ; SOH
   (assert-false (uim-bool '(char-printable? 31)))   ; US
   (assert-true  (uim-bool '(char-printable? 32)))   ; SPACE
   (assert-true  (uim-bool '(char-printable? 33)))   ; !
   (assert-true  (uim-bool '(char-printable? 47)))   ; /
   (assert-true  (uim-bool '(char-printable? 48)))   ; 0
   (assert-true  (uim-bool '(char-printable? 57)))   ; 9
   (assert-true  (uim-bool '(char-printable? 58)))   ; :
   (assert-true  (uim-bool '(char-printable? 64)))   ; @
   (assert-true  (uim-bool '(char-printable? 65)))   ; A
   (assert-true  (uim-bool '(char-printable? 90)))   ; Z
   (assert-true  (uim-bool '(char-printable? 91)))   ; [
   (assert-true  (uim-bool '(char-printable? 96)))   ; `
   (assert-true  (uim-bool '(char-printable? 97)))   ; a
   (assert-true  (uim-bool '(char-printable? 122)))  ; z
   (assert-true  (uim-bool '(char-printable? 123)))  ; {
   (assert-true  (uim-bool '(char-printable? 126)))  ; ~
   (assert-false (uim-bool '(char-printable? 127)))) ; DEL
  ("test char-graphic?"
   (assert-false (uim-bool '(char-graphic? 'symbol)))
   (assert-false (uim-bool '(char-graphic? "string")))
   (assert-false (uim-bool '(char-graphic? '(0 1 2))))
   (assert-false (uim-bool '(char-graphic? car)))
   (assert-false (uim-bool '(char-graphic? 0)))    ; NUL
   (assert-false (uim-bool '(char-graphic? 1)))    ; SOH
   (assert-false (uim-bool '(char-graphic? 31)))   ; US
   (assert-false (uim-bool '(char-graphic? 32)))   ; SPACE
   (assert-true  (uim-bool '(char-graphic? 33)))   ; !
   (assert-true  (uim-bool '(char-graphic? 47)))   ; /
   (assert-true  (uim-bool '(char-graphic? 48)))   ; 0
   (assert-true  (uim-bool '(char-graphic? 57)))   ; 9
   (assert-true  (uim-bool '(char-graphic? 58)))   ; :
   (assert-true  (uim-bool '(char-graphic? 64)))   ; @
   (assert-true  (uim-bool '(char-graphic? 65)))   ; A
   (assert-true  (uim-bool '(char-graphic? 90)))   ; Z
   (assert-true  (uim-bool '(char-graphic? 91)))   ; [
   (assert-true  (uim-bool '(char-graphic? 96)))   ; `
   (assert-true  (uim-bool '(char-graphic? 97)))   ; a
   (assert-true  (uim-bool '(char-graphic? 122)))  ; z
   (assert-true  (uim-bool '(char-graphic? 123)))  ; {
   (assert-true  (uim-bool '(char-graphic? 126)))  ; ~
   (assert-false (uim-bool '(char-graphic? 127)))) ; DEL
  ("test control-char?"
   (assert-true  (uim-bool '(eq? control-char? char-control?))))
  ("test alphabet-char?"
   (assert-true  (uim-bool '(eq? alphabet-char? char-alphabetic?))))
  ("test numeral-char?"
   (assert-true  (uim-bool '(eq? numeral-char? char-numeric?))))
  ("test usual-char?"
   (assert-true  (uim-bool '(eq? usual-char? char-graphic?)))))

(define-uim-test-case "test util character conversion procedures"
  ("test numeral-char->number"
   (assert-true  (uim-bool '(integer? (numeral-char->number 0))))    ; NUL
   (assert-true  (uim-bool '(integer? (numeral-char->number 1))))    ; SOH
   (assert-true  (uim-bool '(integer? (numeral-char->number 31))))   ; US
   (assert-true  (uim-bool '(integer? (numeral-char->number 32))))   ; SPACE
   (assert-true  (uim-bool '(integer? (numeral-char->number 33))))   ; !
   (assert-true  (uim-bool '(integer? (numeral-char->number 47))))   ; /
   (assert-equal 0 (uim '(numeral-char->number 48)))                 ; 0
   (assert-equal 1 (uim '(numeral-char->number 49)))                 ; 1
   (assert-equal 2 (uim '(numeral-char->number 50)))                 ; 2
   (assert-equal 3 (uim '(numeral-char->number 51)))                 ; 3
   (assert-equal 4 (uim '(numeral-char->number 52)))                 ; 4
   (assert-equal 5 (uim '(numeral-char->number 53)))                 ; 5
   (assert-equal 6 (uim '(numeral-char->number 54)))                 ; 6
   (assert-equal 7 (uim '(numeral-char->number 55)))                 ; 7
   (assert-equal 8 (uim '(numeral-char->number 56)))                 ; 8
   (assert-equal 9 (uim '(numeral-char->number 57)))                 ; 9
   (assert-true  (uim-bool '(integer? (numeral-char->number 58))))   ; :
   (assert-true  (uim-bool '(integer? (numeral-char->number 64))))   ; @
   (assert-true  (uim-bool '(integer? (numeral-char->number 65))))   ; A
   (assert-true  (uim-bool '(integer? (numeral-char->number 90))))   ; Z
   (assert-true  (uim-bool '(integer? (numeral-char->number 91))))   ; [
   (assert-true  (uim-bool '(integer? (numeral-char->number 96))))   ; `
   (assert-true  (uim-bool '(integer? (numeral-char->number 97))))   ; a
   (assert-true  (uim-bool '(integer? (numeral-char->number 122))))  ; z
   (assert-true  (uim-bool '(integer? (numeral-char->number 123))))  ; {
   (assert-true  (uim-bool '(integer? (numeral-char->number 126))))  ; ~
   (assert-true  (uim-bool '(integer? (numeral-char->number 127))))) ; DEL
  ("test char-downcase"
   (assert-equal 0   (uim '(char-downcase 0)))     ; NUL
   (assert-equal 1   (uim '(char-downcase 1)))     ; SOH
   (assert-equal 31  (uim '(char-downcase 31)))    ; US
   (assert-equal 32  (uim '(char-downcase 32)))    ; SPACE
   (assert-equal 33  (uim '(char-downcase 33)))    ; !
   (assert-equal 47  (uim '(char-downcase 47)))    ; /
   (assert-equal 48  (uim '(char-downcase 48)))    ; 0
   (assert-equal 57  (uim '(char-downcase 57)))    ; 9
   (assert-equal 58  (uim '(char-downcase 58)))    ; :
   (assert-equal 64  (uim '(char-downcase 64)))    ; @
   (assert-equal 97  (uim '(char-downcase 65)))    ; A
   (assert-equal 122 (uim '(char-downcase 90)))    ; Z
   (assert-equal 91  (uim '(char-downcase 91)))    ; [
   (assert-equal 96  (uim '(char-downcase 96)))    ; `
   (assert-equal 97  (uim '(char-downcase 97)))    ; a
   (assert-equal 122 (uim '(char-downcase 122)))   ; z
   (assert-equal 123 (uim '(char-downcase 123)))   ; {
   (assert-equal 126 (uim '(char-downcase 126)))   ; ~
   (assert-equal 127 (uim '(char-downcase 127))))  ; DEL
  ("test char-upcase"
   (assert-equal 0   (uim '(char-upcase 0)))     ; NUL
   (assert-equal 1   (uim '(char-upcase 1)))     ; SOH
   (assert-equal 31  (uim '(char-upcase 31)))    ; US
   (assert-equal 32  (uim '(char-upcase 32)))    ; SPACE
   (assert-equal 33  (uim '(char-upcase 33)))    ; !
   (assert-equal 47  (uim '(char-upcase 47)))    ; /
   (assert-equal 48  (uim '(char-upcase 48)))    ; 0
   (assert-equal 57  (uim '(char-upcase 57)))    ; 9
   (assert-equal 58  (uim '(char-upcase 58)))    ; :
   (assert-equal 64  (uim '(char-upcase 64)))    ; @
   (assert-equal 65  (uim '(char-upcase 65)))    ; A
   (assert-equal 90  (uim '(char-upcase 90)))    ; Z
   (assert-equal 91  (uim '(char-upcase 91)))    ; [
   (assert-equal 96  (uim '(char-upcase 96)))    ; `
   (assert-equal 65  (uim '(char-upcase 97)))    ; a
   (assert-equal 90  (uim '(char-upcase 122)))   ; z
   (assert-equal 123 (uim '(char-upcase 123)))   ; {
   (assert-equal 126 (uim '(char-upcase 126)))   ; ~
   (assert-equal 127 (uim '(char-upcase 127))))  ; DEL
  ("test string->letter"
   (assert-false (uim-bool '(string->letter "")))    ; NUL
   (assert-false (uim-bool '(string->letter "")))  ; SOH
   (assert-false (uim-bool '(string->letter "")))  ; US
   (assert-false (uim-bool '(string->letter " ")))   ; SPACE
   (assert-false (uim-bool '(string->letter "!")))   ; !
   (assert-false (uim-bool '(string->letter "/")))   ; /
   (assert-false (uim-bool '(string->letter "0")))   ; 0
   (assert-false (uim-bool '(string->letter "9")))   ; 9
   (assert-false (uim-bool '(string->letter ":")))   ; :
   (assert-false (uim-bool '(string->letter "@")))   ; @
   (assert-false (uim-bool '(string->letter "AA")))  ; AA
   (assert-equal 65   (uim '(string->letter "A")))   ; A
   (assert-equal 90   (uim '(string->letter "Z")))   ; Z
   (assert-false (uim-bool '(string->letter "ZZ")))  ; ZZ
   (assert-false (uim-bool '(string->letter "[")))   ; [
   (assert-false (uim-bool '(string->letter "`")))   ; `
   (assert-false (uim-bool '(string->letter "aa")))  ; aa
   (assert-equal 97   (uim '(string->letter "a")))   ; a
   (assert-equal 122  (uim '(string->letter "z")))   ; z
   (assert-false (uim-bool '(string->letter "zz")))  ; zz
   (assert-false (uim-bool '(string->letter "{")))   ; {
   (assert-false (uim-bool '(string->letter "~")))   ; ~
   (assert-false (uim-bool '(string->letter "")))) ; DEL
  ("test to-lower-char"
   (assert-true  (uim-bool '(eq? to-lower-char char-downcase)))))

(define-uim-test-case "test util string list procedures"
  ("test string-list-concat"
   (assert-equal ""
		 (uim '(string-list-concat ())))
   (assert-equal ""
		 (uim '(string-list-concat '(""))))
   (assert-equal "foo"
		 (uim '(string-list-concat '("foo"))))
   (assert-equal "barfoo"
		 (uim '(string-list-concat '("foo" "bar"))))
   (assert-equal "bazbarfoo"
		 (uim '(string-list-concat '("foo" "bar" "baz")))))
  ("test string-find"
   (assert-false (uim-bool '(string-find () "")))
   (assert-false (uim-bool '(string-find () "quux")))
   (assert-false (uim-bool '(string-find '("foo") "")))
   (assert-true  (uim-bool '(string-find '("foo") "foo")))
   (assert-false (uim-bool '(string-find '("foo") "quux")))
   (assert-false (uim-bool '(string-find '("foo" "bar") "")))
   (assert-true  (uim-bool '(string-find '("foo" "bar") "foo")))
   (assert-true  (uim-bool '(string-find '("foo" "bar") "bar")))
   (assert-false (uim-bool '(string-find '("foo" "bar") "quux")))
   (assert-false (uim-bool '(string-find '("foo" "bar" "baz") "")))
   (assert-true  (uim-bool '(string-find '("foo" "bar" "baz") "foo")))
   (assert-true  (uim-bool '(string-find '("foo" "bar" "baz") "bar")))
   (assert-true  (uim-bool '(string-find '("foo" "bar" "baz") "baz")))
   (assert-false (uim-bool '(string-find '("foo" "bar" "baz") "quux")))))

(define-uim-test-case "testcase util list procedures"
  (setup
   (lambda ()
     (uim '(define lst '(1 "2" three (4) 5 six "7" (8 8) -9)))))

  ("test truncate-list"
   (assert-equal ()
		 (uim '(truncate-list () 0)))
   (assert-false (uim-bool '(truncate-list () 1)))
   (assert-equal ()
		 (uim '(truncate-list '("foo" "bar" "baz") 0)))
   (assert-equal '("foo")
		 (uim '(truncate-list '("foo" "bar" "baz") 1)))
   (assert-equal '("foo" "bar")
		 (uim '(truncate-list '("foo" "bar" "baz") 2)))
   (assert-equal '("foo" "bar" "baz")
		 (uim '(truncate-list '("foo" "bar" "baz") 3)))
   (assert-false (uim-bool '(truncate-list '("foo" "bar" "baz") 4))))
  ("test proc-or"
   (assert-false (uim-bool '(proc-or)))
   (assert-false (uim-bool '(proc-or #f)))
   (assert-true  (uim-bool '(proc-or #t)))
   (assert-false (uim-bool '(proc-or #f #f)))
   (assert-true  (uim-bool '(proc-or #f #t)))
   (assert-true  (uim-bool '(proc-or #t #f)))
   (assert-true  (uim-bool '(proc-or #t #t)))
   (assert-false (uim-bool '(proc-or #f #f #f)))
   (assert-true  (uim-bool '(proc-or #f #f #t)))
   (assert-true  (uim-bool '(proc-or #f #t #f)))
   (assert-true  (uim-bool '(proc-or #f #t #t)))
   (assert-true  (uim-bool '(proc-or #t #f #f)))
   (assert-true  (uim-bool '(proc-or #t #f #t)))
   (assert-true  (uim-bool '(proc-or #t #t #f)))
   (assert-true  (uim-bool '(proc-or #t #t #t)))
   (assert-false (uim-bool '(apply proc-or ())))
   (assert-false (uim-bool '(apply proc-or '(#f))))
   (assert-true  (uim-bool '(apply proc-or '(#t))))
   (assert-false (uim-bool '(apply proc-or '(#f #f))))
   (assert-true  (uim-bool '(apply proc-or '(#f #t))))
   (assert-true  (uim-bool '(apply proc-or '(#t #f))))
   (assert-true  (uim-bool '(apply proc-or '(#t #t))))
   (assert-false (uim-bool '(apply proc-or '(#f #f #f))))
   (assert-true  (uim-bool '(apply proc-or '(#f #f #t))))
   (assert-true  (uim-bool '(apply proc-or '(#f #t #f))))
   (assert-true  (uim-bool '(apply proc-or '(#f #t #t))))
   (assert-true  (uim-bool '(apply proc-or '(#t #f #f))))
   (assert-true  (uim-bool '(apply proc-or '(#t #f #t))))
   (assert-true  (uim-bool '(apply proc-or '(#t #t #f))))
   (assert-true  (uim-bool '(apply proc-or '(#t #t #t)))))
  ("test proc-and"
   (assert-true  (uim-bool '(proc-and)))
   (assert-false (uim-bool '(proc-and #f)))
   (assert-true  (uim-bool '(proc-and #t)))
   (assert-false (uim-bool '(proc-and #f #f)))
   (assert-false (uim-bool '(proc-and #f #t)))
   (assert-false (uim-bool '(proc-and #t #f)))
   (assert-true  (uim-bool '(proc-and #t #t)))
   (assert-false (uim-bool '(proc-and #f #f #f)))
   (assert-false (uim-bool '(proc-and #f #f #t)))
   (assert-false (uim-bool '(proc-and #f #t #f)))
   (assert-false (uim-bool '(proc-and #f #t #t)))
   (assert-false (uim-bool '(proc-and #t #f #f)))
   (assert-false (uim-bool '(proc-and #t #f #t)))
   (assert-false (uim-bool '(proc-and #t #t #f)))
   (assert-true  (uim-bool '(proc-and #t #t #t)))
   (assert-true  (uim-bool '(apply proc-and ())))
   (assert-false (uim-bool '(apply proc-and '(#f))))
   (assert-true  (uim-bool '(apply proc-and '(#t))))
   (assert-false (uim-bool '(apply proc-and '(#f #f))))
   (assert-false (uim-bool '(apply proc-and '(#f #t))))
   (assert-false (uim-bool '(apply proc-and '(#t #f))))
   (assert-true  (uim-bool '(apply proc-and '(#t #t))))
   (assert-false (uim-bool '(apply proc-and '(#f #f #f))))
   (assert-false (uim-bool '(apply proc-and '(#f #f #t))))
   (assert-false (uim-bool '(apply proc-and '(#f #t #f))))
   (assert-false (uim-bool '(apply proc-and '(#f #t #t))))
   (assert-false (uim-bool '(apply proc-and '(#t #f #f))))
   (assert-false (uim-bool '(apply proc-and '(#t #f #t))))
   (assert-false (uim-bool '(apply proc-and '(#t #t #f))))
   (assert-true  (uim-bool '(apply proc-and '(#t #t #t)))))
  ("test list-head"
   (assert-equal ()
		 (uim '(list-head lst 0)))
   (assert-equal '(1)
		 (uim '(list-head lst 1)))
   (assert-equal '(1 "2")
		 (uim '(list-head lst 2)))
   (assert-equal '(1 "2" three)
		 (uim '(list-head lst 3)))
   (assert-equal '(1 "2" three (4) 5 six "7" (8 8))
		 (uim '(list-head lst 8)))
   (assert-equal '(1 "2" three (4) 5 six "7" (8 8) -9)
		 (uim '(list-head lst 9)))
   (assert-error (lambda ()
		   (uim '(list-head lst 10))))
   (assert-error (lambda ()
		   (uim '(list-head lst -1)))))

  ("test alist-replace"
   (uim '(define alist ()))
   (uim '(set! alist (alist-replace '(first 1 "1") alist)))
   (assert-equal '((first 1 "1"))
		 (uim 'alist))
   (uim '(set! alist (alist-replace '(second 2 "2") alist)))
   (assert-equal '((second 2 "2") (first 1 "1"))
		 (uim 'alist))
   (uim '(set! alist (alist-replace '(third 3 "3") alist)))
   (assert-equal '((third 3 "3") (second 2 "2") (first 1 "1"))
		 (uim 'alist))
   (uim '(set! alist (alist-replace '(second two "two") alist)))
   (assert-equal '((third 3 "3") (second two "two") (first 1 "1"))
		 (uim 'alist)))

  ("test join"
   (assert-equal ()
		 (uim '(join () ())))
   (assert-equal '(())
		 (uim '(join () '(()))))
   (assert-equal '(1)
		 (uim '(join () '(1))))
   (assert-equal '(() () ())
		 (uim '(join () '(() ()))))
   (assert-equal '(1 () 2)
		 (uim '(join () '(1 2))))
   (assert-equal '(1 () 2 () 3)
		 (uim '(join () '(1 2 3))))
   (assert-equal '(one () two () three)
		 (uim '(join () '(one two three))))
   (assert-equal '("1" () "2" () "3")
		 (uim '(join () '("1" "2" "3"))))
   (assert-equal '(() () () () ())
		 (uim '(join () '(() () ()))))

   (assert-equal ()
		 (uim '(join "/" ())))
   (assert-equal '(())
		 (uim '(join "/" '(()))))
   (assert-equal '(1)
		 (uim '(join "/" '(1))))
   (assert-equal '(() "/" ())
		 (uim '(join "/" '(() ()))))
   (assert-equal '(1 "/" 2)
		 (uim '(join "/" '(1 2))))
   (assert-equal '(1 "/" 2 "/" 3)
		 (uim '(join "/" '(1 2 3))))
   (assert-equal '(one "/" two "/" three)
		 (uim '(join "/" '(one two three))))
   (assert-equal '("1" "/" "2" "/" "3")
		 (uim '(join "/" '("1" "2" "3"))))
   (assert-equal '(() "/" () "/" ())
		 (uim '(join "/" '(() () ())))))

  ("test string-join"
   (assert-equal ""
		 (uim '(string-join () ())))
   (assert-error (lambda ()
		   (uim '(string-join () '(())))))
   (assert-error (lambda ()
		   (uim '(string-join () '(1)))))
   (assert-error (lambda ()
		   (uim '(string-join () '(() ())))))
   (assert-error (lambda ()
		   (uim '(string-join () '(1 2)))))
   (assert-error (lambda ()
		   (uim '(string-join () '(1 2 3)))))
   (assert-error (lambda ()
		   (uim '(string-join () '(one two three)))))
   (assert-error (lambda ()
		   (uim '(string-join () '("1" "2" "3")))))
   (assert-error (lambda ()
		   (uim '(string-join () '(() () ())))))

   (assert-equal ""
		 (uim '(string-join "/" ())))
   (assert-equal ""
		 (uim '(string-join "/" '(""))))
   (assert-equal "1"
		 (uim '(string-join "/" '("1"))))
   (assert-equal "1/2"
		 (uim '(string-join "/" '("1" "2"))))
   (assert-equal "1/2/3"
		 (uim '(string-join "/" '("1" "2" "3"))))

   (assert-equal ""
		 (uim '(string-join "-sep-" ())))
   (assert-equal ""
		 (uim '(string-join "-sep-" '(""))))
   (assert-equal "1"
		 (uim '(string-join "-sep-" '("1"))))
   (assert-equal "1-sep-2"
		 (uim '(string-join "-sep-" '("1" "2"))))
   (assert-equal "1-sep-2-sep-3"
		 (uim '(string-join "-sep-" '("1" "2" "3")))))

  ("test string-append-map"
   (assert-equal ""
		 (uim '(string-append-map car ())))
   (assert-equal "c"
		 (uim '(string-append-map car '(("c" "C")))))
   (assert-equal "ca"
		 (uim '(string-append-map car '(("c" "C") ("a" "A")))))
   (assert-equal "car"
		 (uim '(string-append-map car '(("c" "C") ("a" "A") ("r" "R")))))))

(define-uim-test-case "testcase util misc"
  ("test compose"
   (uim '(define test-list '(0 1 2 3 4 5)))
   (assert-true  (uim-bool '(procedure? (compose))))
   (assert-true  (uim-bool '(procedure? (compose car))))
   (assert-true  (uim-bool '(procedure? (compose car cdr))))
   (assert-true  (uim-bool '(procedure? (compose car cdr list))))
   (assert-equal '(0 1 2 3 4 5)
		 (uim '((compose) test-list)))
   (assert-equal 0
		 (uim '((compose car) test-list)))
   (assert-equal 1
		 (uim '((compose car cdr) test-list)))
   (assert-equal 2
		 (uim '((compose car cdr cdr) test-list)))
   (assert-equal 4
		 (uim '((compose car cdr reverse) test-list)))
   (assert-equal 3
		 (uim '((compose car cdr cdr reverse) test-list))))
  ("test clamp"
   (assert-equal 0 (uim '(clamp -2 0 -1)))
   (assert-equal 0 (uim '(clamp -1 0 -1)))
   (assert-equal 0 (uim '(clamp 0  0 -1)))
   (assert-equal 0 (uim '(clamp 1  0 -1)))
   (assert-equal 0 (uim '(clamp 2  0 -1)))
   (assert-equal 0 (uim '(clamp 10 0 -1)))

   (assert-equal -2 (uim '(clamp -2 -2 0)))
   (assert-equal -1 (uim '(clamp -1 -2 0)))
   (assert-equal 0  (uim '(clamp 0  -2 0)))
   (assert-equal 0  (uim '(clamp 1  -2 0)))
   (assert-equal 0  (uim '(clamp 2  -2 0)))
   (assert-equal 0  (uim '(clamp 10 -2 0)))

   (assert-equal -1 (uim '(clamp -2 -1 0)))
   (assert-equal -1 (uim '(clamp -1 -1 0)))
   (assert-equal 0  (uim '(clamp 0  -1 0)))
   (assert-equal 0  (uim '(clamp 1  -1 0)))
   (assert-equal 0  (uim '(clamp 2  -1 0)))
   (assert-equal 0  (uim '(clamp 10 -1 0)))

   (assert-equal 0 (uim '(clamp -2 0 0)))
   (assert-equal 0 (uim '(clamp -1 0 0)))
   (assert-equal 0 (uim '(clamp 0  0 0)))
   (assert-equal 0 (uim '(clamp 1  0 0)))
   (assert-equal 0 (uim '(clamp 2  0 0)))
   (assert-equal 0 (uim '(clamp 10 0 0)))

   (assert-equal 0 (uim '(clamp -2 0 1)))
   (assert-equal 0 (uim '(clamp -1 0 1)))
   (assert-equal 0 (uim '(clamp 0  0 1)))
   (assert-equal 1 (uim '(clamp 1  0 1)))
   (assert-equal 1 (uim '(clamp 2  0 1)))
   (assert-equal 1 (uim '(clamp 10 0 1)))

   (assert-equal 0 (uim '(clamp -2 0 2)))
   (assert-equal 0 (uim '(clamp -1 0 2)))
   (assert-equal 0 (uim '(clamp 0  0 2)))
   (assert-equal 1 (uim '(clamp 1  0 2)))
   (assert-equal 2 (uim '(clamp 2  0 2)))
   (assert-equal 2 (uim '(clamp 10 0 2)))

   (assert-equal 0 (uim '(clamp -2 0 3)))
   (assert-equal 0 (uim '(clamp -1 0 3)))
   (assert-equal 0 (uim '(clamp 0  0 3)))
   (assert-equal 1 (uim '(clamp 1  0 3)))
   (assert-equal 2 (uim '(clamp 2  0 3)))
   (assert-equal 3 (uim '(clamp 10 0 3)))

   (assert-equal 1 (uim '(clamp -2 1 3)))
   (assert-equal 1 (uim '(clamp -1 1 3)))
   (assert-equal 1 (uim '(clamp 0  1 3)))
   (assert-equal 1 (uim '(clamp 1  1 3)))
   (assert-equal 2 (uim '(clamp 2  1 3)))
   (assert-equal 3 (uim '(clamp 10 1 3)))

   (assert-equal -1 (uim '(clamp -2 -1 3)))
   (assert-equal -1 (uim '(clamp -1 -1 3)))
   (assert-equal 0  (uim '(clamp 0  -1 3)))
   (assert-equal 1  (uim '(clamp 1  -1 3)))
   (assert-equal 2  (uim '(clamp 2  -1 3)))
   (assert-equal 3  (uim '(clamp 10 -1 3)))

   (assert-equal -2 (uim '(clamp -2 -5 5)))
   (assert-equal -1 (uim '(clamp -1 -5 5)))
   (assert-equal 0  (uim '(clamp 0  -5 5)))
   (assert-equal 1  (uim '(clamp 1  -5 5)))
   (assert-equal 2  (uim '(clamp 2  -5 5)))
   (assert-equal 5  (uim '(clamp 10 -5 5)))))

(define-uim-test-case "testcase util R5RS procedures"
  (setup
   (lambda ()
     (uim '(define lst '(1 "2" three (4) 5 six "7" (8 8) -9)))))

  ("test else"
   (assert-equal "else"
		 (uim '(cond
			((equal? 1 11)
			 1)
			((eq? 'second 'twelve)
			 2)
			((string=? "third" "thirty")
			 3)
			(else
			 "else"))))
   (assert-equal 3
		 (uim '(cond
			((equal? 1 11)
			 1)
			((eq? 'second 'twelve)
			 2)
			((string=? "third" "third")
			 3)
			(else
			 "else"))))
   (assert-false (uim-bool '(cond
			     ((equal? 1 11)
			      1)
			     ((eq? 'second 'twelve)
			      2)
			     ((string=? "third" "thirty")
			      3)
			     (else
			      #f)))))
  ("test boolean?"
   (assert-true  (uim-bool '(boolean? #f)))
   (assert-true  (uim-bool '(boolean? #t)))
   (assert-false (uim-bool '(boolean? "foo")))
   (assert-false (uim-bool '(boolean? 'foo)))
   (assert-false (uim-bool '(boolean? -1)))
   (assert-false (uim-bool '(boolean? 0)))
   (assert-true  (uim-bool '(boolean? 1)))  ; Siod specific
   (assert-false (uim-bool '(boolean? 10)))
   (assert-true  (uim-bool '(boolean? ()))) ; Siod specific
   (assert-false (uim-bool '(boolean? '(1 "2" 'three))))
   (assert-false (uim-bool '(boolean? 'nil)))
   (assert-false (uim-bool '(symbol-bound? 'nil))))
  ("test integer?"
   (assert-false (uim-bool '(integer? #f)))
   (assert-false (uim-bool '(integer? "foo")))
   (assert-false (uim-bool '(integer? 'foo)))
   (assert-true  (uim-bool '(integer? -1)))
   (assert-true  (uim-bool '(integer? 0)))
   (assert-true  (uim-bool '(integer? 1)))
   (assert-true  (uim-bool '(integer? 2)))
   (assert-true  (uim-bool '(integer? 10)))
   (assert-false (uim-bool '(integer? ())))
   (assert-false (uim-bool '(integer? '(1 "2" 'three)))))
  ("test list?"
   (assert-true  (uim-bool '(list? #f))) ; Siod specific
   (assert-false (uim-bool '(list? "foo")))
   (assert-false (uim-bool '(list? 'foo)))
   (assert-false (uim-bool '(list? -1)))
   (assert-false (uim-bool '(list? 0)))
   (assert-false (uim-bool '(list? 1)))
   (assert-false (uim-bool '(list? 2)))
   (assert-false (uim-bool '(list? 10)))
   (assert-true  (uim-bool '(list? ())))
   (assert-true  (uim-bool '(list? '(1))))
   (assert-true  (uim-bool '(list? '(1 "2"))))
   (assert-true  (uim-bool '(list? '(1 "2" 'three)))))
  ("test zero?"
   (assert-error (lambda () (uim-bool '(zero? #f))))
   (assert-error (lambda () (uim-bool '(zero? "foo"))))
   (assert-error (lambda () (uim-bool '(zero? 'foo))))
   (assert-false (uim-bool '(zero? -2)))
   (assert-false (uim-bool '(zero? -1)))
   (assert-true  (uim-bool '(zero? 0)))
   (assert-false (uim-bool '(zero? 1)))
   (assert-false (uim-bool '(zero? 2)))
   (assert-false (uim-bool '(zero? 10)))
   (assert-error (lambda () (uim-bool '(zero? ()))))
   (assert-error (lambda () (uim-bool '(zero? '(1)))))
   (assert-error (lambda () (uim-bool '(zero? '(1 "2")))))
   (assert-error (lambda () (uim-bool '(zero? '(1 "2" 'three))))))
  ("test positive?"
   (assert-error (lambda () (uim-bool '(positive? #f))))
   (assert-error (lambda () (uim-bool '(positive? "foo"))))
   (assert-error (lambda () (uim-bool '(positive? 'foo))))
   (assert-false (uim-bool '(positive? -2)))
   (assert-false (uim-bool '(positive? -1)))
   (assert-false (uim-bool '(positive? 0)))
   (assert-true  (uim-bool '(positive? 1)))
   (assert-true  (uim-bool '(positive? 2)))
   (assert-true  (uim-bool '(positive? 10)))
   (assert-error (lambda () (uim-bool '(positive? ()))))
   (assert-error (lambda () (uim-bool '(positive? '(1)))))
   (assert-error (lambda () (uim-bool '(positive? '(1 "2")))))
   (assert-error (lambda () (uim-bool '(positive? '(1 "2" 'three))))))
  ("test negative?"
   (assert-error (lambda () (uim-bool '(negative? #f))))
   (assert-error (lambda () (uim-bool '(negative? "foo"))))
   (assert-error (lambda () (uim-bool '(negative? 'foo))))
   (assert-true  (uim-bool '(negative? -2)))
   (assert-true  (uim-bool '(negative? -1)))
   (assert-false (uim-bool '(negative? 0)))
   (assert-false (uim-bool '(negative? 1)))
   (assert-false (uim-bool '(negative? 2)))
   (assert-false (uim-bool '(negative? 10)))
   (assert-error (lambda () (uim-bool '(negative? ()))))
   (assert-error (lambda () (uim-bool '(negative? '(1)))))
   (assert-error (lambda () (uim-bool '(negative? '(1 "2")))))
   (assert-error (lambda () (uim-bool '(negative? '(1 "2" 'three))))))
  ("test string->symbol"
   (assert-equal 'foo1
		 (uim '(string->symbol "foo1")))
   (assert-equal 'Foo1
		 (uim '(string->symbol "Foo1")))
   (assert-equal 'FOO1
		 (uim '(string->symbol "FOO1")))
   (assert-equal '1foo
		 (uim '(string->symbol "1foo")))
   (assert-equal '1Foo
		 (uim '(string->symbol "1Foo")))
   (assert-equal '1FOO
		 (uim '(string->symbol "1FOO"))))
  ("test map"
   (assert-equal ()
		 (uim '(map not ())))
   (assert-equal (uim '(list #f))
		 (uim '(map not '(#t))))

   ;; these two tests fail due to bug #617 'boolean value
   ;; representation is inconsistent'
;   (assert-equal (uim '(list #f #t))
;		 (uim '(map not '(#t #f))))
;   (assert-equal (uim '(list #f #t #f))
;		 (uim '(map not '(#t #f #t))))

   (assert-equal '()
		 (uim '(map +
			    ()
			    ())))
   (assert-equal '(5)
		 (uim '(map +
			    '(1)
			    '(4))))
   (assert-equal '(5 7)
		 (uim '(map +
			    '(1 2)
			    '(4 5))))
   (assert-equal '(5 7 9)
		 (uim '(map +
			    '(1 2 3)
			    '(4 5 6))))
   (assert-equal '()
		 (uim '(map +
			    '()
			    '()
			    '())))
   (assert-equal '(12)
		 (uim '(map +
			    '(1)
			    '(4)
			    '(7))))
   (assert-equal '(12 15)
		 (uim '(map +
			    '(1 2)
			    '(4 5)
			    '(7 8))))
   (assert-equal '(12 15 18)
		 (uim '(map +
			    '(1 2 3)
			    '(4 5 6)
			    '(7 8 9))))
   (assert-equal '()
		 (uim '(map +
			    '()
			    '()
			    '()
			    '())))
   (assert-equal '(22)
		 (uim '(map +
			    '(1)
			    '(4)
			    '(7)
			    '(10))))
   (assert-equal '(22 26)
		 (uim '(map +
			    '(1 2)
			    '(4 5)
			    '(7 8)
			    '(10 11))))
   (assert-equal '(22 26 30)
		 (uim '(map +
			    '(1 2 3)
			    '(4 5 6)
			    '(7 8 9)
			    '(10 11 12)))))

  ("test for-each"
   (assert-equal 3
		 (uim '(let ((i 0))
			 (for-each (lambda (x)
				     (set! i (+ i 1)))
				   '(1 2 3))
			 i)))
   (assert-equal 6
		 (uim '(let ((i 0)
			     (sum 0))
			 (for-each (lambda (x)
				     (set! i (+ i 1))
				     (set! sum (+ sum x)))
				   '(1 2 3))
			 sum)))
   (assert-equal 3
		 (uim '(let ((i 0))
			 (for-each (lambda (x y)
				     (set! i (+ i 1)))
				   '(1 2 3)
				   '(4 5 6))
			 i)))
   (assert-equal 21
		 (uim '(let ((i 0)
			     (sum 0))
			 (for-each (lambda (x y)
				     (set! i (+ i 1))
				     (set! sum (+ sum x y)))
				   '(1 2 3)
				   '(4 5 6))
			 sum))))
  ("test list-tail"
   (assert-equal '(1 "2" three (4) 5 six "7" (8 8) -9)
		 (uim '(list-tail lst 0)))
   (assert-equal '("2" three (4) 5 six "7" (8 8) -9)
		 (uim '(list-tail lst 1)))
   (assert-equal '(three (4) 5 six "7" (8 8) -9)
		 (uim '(list-tail lst 2)))
   (assert-equal '((4) 5 six "7" (8 8) -9)
		 (uim '(list-tail lst 3)))
   (assert-equal '(-9)
		 (uim '(list-tail lst 8)))
   (assert-equal '()
		 (uim '(list-tail lst 9)))
   (assert-error (lambda ()
		   (uim '(list-tail lst 10))))
   (assert-error (lambda ()
		   (uim '(list-tail lst -1))))))

(define-uim-test-case "test util SRFI procedures"
  (setup
   (lambda ()
     (uim '(define lst '(1 "2" three (4) 5 six "7" (8 8) -9)))
     (uim '(define lst2 '(1 "2" three (4) 5 six "7" (8 8) -9 #f 11 #f "13")))
     (uim '(define lst3 '("1" "2" "three")))
     (uim '(define lst4 '(1 2 3)))
     (uim '(define lst5 '(1 #f 3 "four")))
     (uim '(define lst6 '(#f #f)))
     (uim '(define lst7 '(#f #f #f)))
     (uim '(define alist-int '((23 "23" twentythree)
			       (1 "1" one)
			       (5 "5" five)
			       (3 "3" three))))
     (uim '(define alist-str '(("23" 23 twentythree)
			       ("1" 1 one)
			       ("5" 5 five)
			       ("3" 3 three))))
     (uim '(define alist-lst '((("23") 23 twentythree)
			       (("1") 1 one)
			       (("5") 5 five)
			       (("3") 3 three))))
     (uim '(define alist-sym '((twentythree "23" 23)
			       (one "1" 1)
			       (five "5" 5)
			       (three "3" 3))))))

  ("test list-tabulate"
   (assert-equal ()
		 (uim '(list-tabulate 0 (lambda (x) x))))
   (assert-equal '(0)
		 (uim '(list-tabulate 1 (lambda (x) x))))
   (assert-equal '(0 1 2 3 4)
		 (uim '(list-tabulate 5 (lambda (x) x))))
   (assert-equal '(0 1 4 9 16)
		 (uim '(list-tabulate 5 (lambda (x) (* x x)))))
   (assert-error (lambda ()
		   (uim '(list-tabulate -1 (lambda (x) x))))))

  ("test make-list"
   (assert-equal '(fill fill fill)
		 (uim '(make-list 3 'fill)))
   (assert-equal '(0 0 0)
		 (uim '(make-list 3 0)))
   (assert-equal '("string" "string" "string")
		 (uim '(make-list 3 "string")))
   (assert-equal '((fill "fill") (fill "fill") (fill "fill"))
		 (uim '(make-list 3 '(fill "fill"))))
   (assert-equal '(() () ())
		 (uim '(make-list 3 ())))
   (assert-equal '(())
		 (uim '(make-list 1 ())))
   (assert-equal '()
		 (uim '(make-list 0 ())))
   (assert-equal '()
		 (uim '(make-list 0 'fill)))
   (assert-error (lambda ()
		   (uim '(make-list -1 'fill)))))

  ("test iota"
   (assert-equal ()
		 (uim '(iota 0)))
   (assert-equal '(0)
		 (uim '(iota 1)))
   (assert-equal '(0 1 2 3 4)
		 (uim '(iota 5)))
   (assert-error (lambda ()
		   (uim '(iota -1))))

   (assert-equal ()
		 (uim '(iota 0 0)))
   (assert-equal '(0)
		 (uim '(iota 1 0)))
   (assert-equal '(0 1 2 3 4)
		 (uim '(iota 5 0)))
   (assert-error (lambda ()
		   (uim '(iota -1 0))))

   (assert-error (lambda ()
		   (uim '(iota 0 1))))
   (assert-equal '()
		 (uim '(iota 1 1)))
   (assert-equal '(1 2 3 4)
		 (uim '(iota 5 1)))
   (assert-error (lambda ()
		   (uim '(iota -1 1))))

   (assert-error (lambda ()
		   (uim '(iota 1 3))))
   (assert-equal '(3 4)
		 (uim '(iota 5 3)))
   (assert-error (lambda ()
		   (uim '(iota -1 3))))

   (assert-equal '()
		 (uim '(iota 5 5))))

  ("test zip"
   (assert-equal '((1) (2) (3) (4) (5))
		 (uim '(zip '(1 2 3 4 5))))
   (assert-equal '((1 "1" one) (2 "2" two) (3 "3" three) (4 "4" four) (5 "5" five))
		 (uim '(zip '(1 2 3 4 5)
			    '("1" "2" "3" "4" "5")
			    '(one two three four five))))
   (assert-equal '((1 "1" one) (2 "2" two) (3 "3" three))
		 (uim '(zip '(1 2 3 4 5)
			    '("1" "2" "3" "4" "5")
			    '(one two three))))
   (assert-equal ()
		 (uim '(zip '()
			    '("1" "2" "3" "4" "5")
			    '(one two three))))
   (assert-equal ()
		 (uim '(zip ()))))

  ("test append-map"
   (assert-equal '()
		 (uim '(append-map car ())))
   (assert-equal '(c)
		 (uim '(append-map car '(((c) (C))))))
   (assert-equal '(c a)
		 (uim '(append-map car '(((c) (C)) ((a) (A))))))
   (assert-equal '(c a r)
		 (uim '(append-map car '(((c) (C)) ((a) (A)) ((r) (R)))))))

  ("test append-reverse"
   (assert-equal '("5" "4" "3" "2" "1" six seven eight)
		 (uim '(append-reverse '("1" "2" "3" "4" "5")
				       '(six seven eight))))
   (assert-equal '(six seven eight)
		 (uim '(append-reverse ()
				       '(six seven eight))))
   (assert-equal '("5" "4" "3" "2" "1")
		 (uim '(append-reverse '("1" "2" "3" "4" "5")
				       ())))
   (assert-equal ()
		 (uim '(append-reverse ()
				       ()))))
  ("test find"
   (assert-equal "2"
		 (uim '(find string? lst)))
   (assert-equal 'three
		 (uim '(find symbol? lst)))
   (assert-equal ()
		 (uim '(find string? ())))
   (assert-equal -9
		 (uim '(find (lambda (x)
			       (and (integer? x)
				    (< x 0)))
			     lst))))
  ("test any"
   (assert-true  (uim-bool '(any string? lst)))
   (assert-true  (uim-bool '(any string? lst2)))
   (assert-true  (uim-bool '(any string? lst3)))
   (assert-false (uim-bool '(any string? lst4)))
   (assert-true  (uim-bool '(any proc-or lst2)))
   (assert-false (uim-bool '(any proc-or lst6)))
   (assert-true  (uim-bool '(any proc-or lst2 lst6)))
   (assert-true  (uim-bool '(any proc-or lst6 lst2)))
   (assert-false (uim-bool '(any proc-or lst6 lst7)))
   (assert-true  (uim-bool '(any proc-or lst5 lst6 lst7)))
   (assert-false (uim-bool '(any string? ()))))
  ("test every"
   (assert-false (uim-bool '(every string? lst)))
   (assert-false (uim-bool '(every string? lst2)))
   (assert-true  (uim-bool '(every string? lst3)))
   (assert-false (uim-bool '(every string? lst4)))
   (assert-true  (uim-bool '(every proc-or lst)))
   (assert-false (uim-bool '(every proc-or lst2)))
   (assert-false (uim-bool '(every proc-or lst6)))
   (assert-true  (uim-bool '(every proc-or lst2 lst6)))
   (assert-true  (uim-bool '(every proc-or lst6 lst2)))
   (assert-false (uim-bool '(every proc-or lst6 lst7)))
   (assert-true  (uim-bool '(every proc-or lst4 lst6 lst7)))
   (assert-false (uim-bool '(every proc-or lst5 lst6 lst7)))
   (assert-true  (uim-bool '(every string? ()))))
  ("test fold"
   (assert-equal ()
		 (uim '(fold cons () ())))
   (assert-equal '(5 4 3 2 1)
		 (uim '(fold cons () '(1 2 3 4 5))))
   (assert-equal '(1 2 3 4 5 6 7 8 9)
		 (uim '(fold cons '(6 7 8 9) '(5 4 3 2 1))))
   (assert-equal '(9 8 7 6 5 4 3 2 1)
		 (uim '(fold cons '(5 4 3 2 1) '(6 7 8 9))))
   (assert-equal 24
		 (uim '(fold + 0 '(1 2 3 4 5) '(1 2 1 2 3))))
   (assert-equal 9
		 (uim '(fold + 0 '(1 2 3 4 5) '(1 2 1 2 3) '(7))))
   (assert-equal 0
		 (uim '(fold + 0 '(1 2 3 4 5) () '(1 2 1 2 3))))
   (assert-equal 120
		 (uim '(fold * 1 '(1 2 3 4 5))))
   (assert-equal 14400
		 (uim '(fold * 1 '(1 2 3 4 5) '(1 2 3 4 5)))))
  ("test unfold"
   ;; immediate term
   (assert-equal '()
		 (uim '(unfold (lambda (x)
				 (= x 5))
			       (lambda (rest)
				 rest)
			       (lambda (rest)
				 (- rest 1))
			       5)))
   (assert-equal '(5)
		 (uim '(unfold (lambda (x)
				 (= x 5))
			       (lambda (rest)
				 rest)
			       (lambda (rest)
				 (- rest 1))
			       5
			       (lambda (rest)
				 (list rest)))))
   (assert-equal '(-1)
		 (uim '(unfold (lambda (x)
				 (= x 5))
			       (lambda (rest)
				 rest)
			       (lambda (rest)
				 (- rest 1))
			       5
			       (lambda (rest)
				 '(-1)))))
   ;; 5 times
   (assert-equal '(5 4 3 2 1)
		 (uim '(unfold (lambda (x)
				 (= x 0))
			       (lambda (rest)
				 rest)
			       (lambda (rest)
				 (- rest 1))
			       5)))
   (assert-equal '(5 4 3 2 1 0)
		 (uim '(unfold (lambda (x)
				 (= x 0))
			       (lambda (rest)
				 rest)
			       (lambda (rest)
				 (- rest 1))
			       5
			       (lambda (x)
				 (list x)))))
   (assert-equal '(5 4 3 2 1 -1)
		 (uim '(unfold (lambda (x)
				 (= x 0))
			       (lambda (rest)
				 rest)
			       (lambda (rest)
				 (- rest 1))
			       5
			       (lambda (x)
				 '(-1)))))
   ;; 5 times, reversed
   (assert-equal '(0 1 2 3 4)
		 (uim '(unfold (lambda (x)
				 (= x 5))
			       (lambda (rest)
				 rest)
			       (lambda (rest)
				 (+ rest 1))
			       0)))
   (assert-equal '(0 1 2 3 4 5)
		 (uim '(unfold (lambda (x)
				 (= x 5))
			       (lambda (rest)
				 rest)
			       (lambda (rest)
				 (+ rest 1))
			       0
			       (lambda (x)
				 (list x)))))
   (assert-equal '(0 1 2 3 4 -1)
		 (uim '(unfold (lambda (x)
				 (= x 5))
			       (lambda (rest)
				 rest)
			       (lambda (rest)
				 (+ rest 1))
			       0
			       (lambda (x)
				 '(-1)))))
   ;; restruct same list
   (assert-equal '(0 1 2 3 4 5)
		 (uim '(unfold null?
			       car
			       cdr
			       '(0 1 2 3 4 5))))
   (assert-equal '(0 1 2 3 4 5)
		 (uim '(unfold null?
			       car
			       cdr
			       '(0 1 2 3 4 5)
			       (lambda (x)
				 x))))
   (assert-equal '(0 1 2 3 4 5 . -1)
		 (uim '(unfold null?
			       car
			       cdr
			       '(0 1 2 3 4 5)
			       (lambda (x)
				 -1)))))
  ("test filter"
   (assert-equal ()
		 (uim '(filter not ())))
   (assert-equal '(5 6 4)
		 (uim '(filter (lambda (x)
				 (< 3 x))
			       '(3 5 2 6 4 1))))
   (assert-equal '("2" "7")
		 (uim '(filter string?
			       '(one "2" 3 #f (5) six "7" 8 (9) 10))))
   (assert-equal '(3 8 10)
		 (uim '(filter integer?
			       '(one "2" 3 #f (5) six "7" 8 (9) 10)))))

  ("test filter-map"
   ;; single list
   (assert-equal ()
		 (uim '(filter-map not ())))
   (assert-equal '(5 6 4)
		 (uim '(filter-map (lambda (x)
				     (and (< 3 x)
					  x))
				   '(3 5 2 6 4 1))))
   (assert-equal '(10 12 8)
		 (uim '(filter-map (lambda (x)
				     (and (< 3 x)
					  (* 2 x)))
				   '(3 5 2 6 4 1))))
   (assert-equal (uim '(list (string? "") (string? "")))
		 (uim '(filter-map string?
				   '(one "2" 3 #f (5) six "7" 8 (9) 10))))
   (assert-equal (uim '(list (integer? 0) (integer? 0) (integer? 0)))
		 (uim '(filter-map integer?
				   '(one "2" 3 #f (5) six "7" 8 (9) 10))))
   ;; multiple lists
   (assert-equal '()
		 (uim '(filter-map +
				   ()
				   ())))
   (assert-equal '(8 11 10 8)
		 (uim '(filter-map (lambda (x y)
				     (let ((sum (+ x y)))
				       (and (< 6 sum)
					    sum)))
				   '(3 5 2 6 4 1)
				   '(1 3 9 4 2 7))))
   (assert-equal ()
		 (uim '(filter-map (lambda (x y)
				     (let ((sum (+ x y)))
				       (and (< 6 sum)
					    sum)))
				   ()
				   '(1 3 9 4 2 7))))
   (assert-equal '(8 11 10)
		 (uim '(filter-map (lambda (x y)
				     (let ((sum (+ x y)))
				       (and (< 6 sum)
					    sum)))
				   '(3 5 2 6 4 1)
				   '(1 3 9 4))))
   (assert-equal '("aAa1" "bBb2" "cCc3" "dDd4")
		 (uim '(filter-map string-append
				   '("a" "b" "c" "d")
				   '("A" "B" "C" "D")
				   '("a" "b" "c" "d")
				   '("1" "2" "3" "4" "5")))))

  ("test remove"
   (assert-equal '(1 three (4) 5 six (8 8) -9)
		 (uim '(remove string? lst)))
   (assert-equal '("2" three (4) six "7" (8 8))
		 (uim '(remove integer? lst)))
   (assert-equal '("2" three (4) six "7" (8 8) -9)
		 (uim '(remove (lambda (x)
				 (and (integer? x)
				      (> x 0)))
			       lst)))
   (assert-equal ()
		 (uim '(remove string? ())))
   (assert-equal ()
		 (uim '(remove string? '("1" "2")))))
  ("test alist-delete"
   (assert-equal '((23 "23" twentythree)
		   (1 "1" one)
		   (5 "5" five))
		 (uim '(alist-delete 3 alist-int)))
   (assert-equal '((23 "23" twentythree)
		   (1 "1" one)
		   (5 "5" five)
		   (3 "3" three))
		 (uim '(alist-delete 0 alist-int)))
   (assert-equal '((1 "1" one)
		   (5 "5" five)
		   (3 "3" three))
		 (uim '(alist-delete 23 alist-int)))
   (assert-equal '((23 "23" twentythree)
		   (1 "1" one)
		   (5 "5" five)
		   (3 "3" three))
		 (uim '(alist-delete "3" alist-int)))
   (assert-equal '(("23" 23 twentythree)
		   ("1" 1 one)
		   ("5" 5 five)
		   ("3" 3 three))
		 (uim '(alist-delete "5" alist-str)))
   (assert-equal '(("23" 23 twentythree)
		   ("1" 1 one)
		   ("3" 3 three))
		 (uim '(alist-delete "5" alist-str string=?)))
   (assert-equal '((("23") 23 twentythree)
		   (("1") 1 one)
		   (("5") 5 five)
		   (("3") 3 three))
		 (uim '(alist-delete '("23") alist-lst)))
   (assert-equal '((("1") 1 one)
		   (("5") 5 five)
		   (("3") 3 three))
		 (uim '(alist-delete '("23") alist-lst equal?)))
   (assert-equal '((twentythree "23" 23)
		   (five "5" 5)
		   (three "3" 3))
		 (uim '(alist-delete 'one alist-sym)))
   (assert-equal '((twentythree "23" 23)
		   (one "1" 1)
		   (five "5" 5))
		 (uim '(alist-delete 'three alist-sym eq?)))))

(define-uim-test-case "test util Siod specific procedures"
  ("test toplevel-env"
   (assert-true (uim-bool '(eval '(symbol-bound? 'filter-map)
				 toplevel-env))))
  ("test enclose-another-env"
   (assert-equal 3
		 (uim '(let* ((x 1)
			      (y 2)
			      (closure (lambda ()
					 (+ x y))))
			 (closure))))
   (assert-equal 10
		 (uim '(let* ((x 1)
			      (y 2)
			      (closure (lambda ()
					 (+ x y)))
			      (another-env '((x . 4)
					     (y . 6))))
			 (set! closure
			       (enclose-another-env closure another-env))
			 (closure))))
   ;; causes error since z is not exist in the another-env
   (assert-error (lambda ()
		   (uim '(let* ((x 1)
				(y 2)
				(z 3)
				(closure (lambda ()
					   (+ x y z)))
				(another-env '((x . 4)
					       (y . 6))))
			   (set! closure
				 (enclose-another-env closure another-env))
			   (closure)))))))

(define-uim-test-case "test util define-record"
  ("test define-record record definition"
   (assert-false (uim-bool '(symbol-bound? 'test-rec-new)))
   (assert-false (uim-bool '(symbol-bound? 'test-rec-first)))
   (assert-false (uim-bool '(symbol-bound? 'test-rec-second)))
   (assert-false (uim-bool '(symbol-bound? 'test-rec-third)))
   (assert-false (uim-bool '(symbol-bound? 'test-rec-fourth)))
   (assert-false (uim-bool '(symbol-bound? 'test-rec-fifth)))
   (assert-false (uim-bool '(symbol-bound? 'test-rec-set-first!)))
   (assert-false (uim-bool '(symbol-bound? 'test-rec-set-second!)))
   (assert-false (uim-bool '(symbol-bound? 'test-rec-set-third!)))
   (assert-false (uim-bool '(symbol-bound? 'test-rec-set-fourth!)))
   (assert-false (uim-bool '(symbol-bound? 'test-rec-set-fifth!)))

   (assert-true  (uim-bool '(begin
			      (define-record 'test-rec
				'((first #f)
				  (second foo)
				  (third "bar")
				  (fourth 4)))
			      #t)))  ;; suppress closure result
   
   (assert-true  (uim-bool '(symbol-bound? 'test-rec-new)))
   (assert-true  (uim-bool '(symbol-bound? 'test-rec-first)))
   (assert-true  (uim-bool '(symbol-bound? 'test-rec-second)))
   (assert-true  (uim-bool '(symbol-bound? 'test-rec-third)))
   (assert-true  (uim-bool '(symbol-bound? 'test-rec-fourth)))
   (assert-false (uim-bool '(symbol-bound? 'test-rec-fifth)))
   (assert-true  (uim-bool '(symbol-bound? 'test-rec-set-first!)))
   (assert-true  (uim-bool '(symbol-bound? 'test-rec-set-second!)))
   (assert-true  (uim-bool '(symbol-bound? 'test-rec-set-third!)))
   (assert-true  (uim-bool '(symbol-bound? 'test-rec-set-fourth!)))
   (assert-false (uim-bool '(symbol-bound? 'test-rec-set-fifth!)))

   ;; create with default values
   (assert-equal (uim ''(#f foo "bar" 4))
		 (uim '(test-rec-new)))
   ;; create with initializer values
   (assert-equal '(one two three four)
		 (uim '(test-rec-new 'one 'two 'three 'four)))
   ;; create with partial initialization
   (assert-equal '(one foo "bar" 4)
		 (uim '(test-rec-new 'one)))
   (assert-equal '(one two "bar" 4)
		 (uim '(test-rec-new 'one 'two)))
   (assert-equal '(one two three 4)
		 (uim '(test-rec-new 'one 'two 'three)))
))

(define-uim-test-case "test util define-record accessors"
  (setup
   (lambda ()
     (uim '(begin
	     (define-record 'test-rec
	       '((first #f)
		 (second foo)
		 (third "bar")
		 (fourth 4)))
	     #t))))  ;; suppress closure result

  ("test define-record getters"
   (assert-true (uim-bool '(let ((trec (test-rec-new)))
			     (equal? (test-rec-first trec)
				     #f))))
   (assert-true (uim-bool '(let ((trec (test-rec-new)))
			     (equal? (test-rec-second trec)
				     'foo))))
   (assert-true (uim-bool '(let ((trec (test-rec-new)))
			     (equal? (test-rec-third trec)
				     "bar"))))
   (assert-true (uim-bool '(let ((trec (test-rec-new)))
			     (equal? (test-rec-fourth trec)
				     4)))))
  ("test define-record setters"
   (assert-true (uim-bool '(let ((trec (test-rec-new)))
			     (test-rec-set-first! trec #t)
			     (equal? (test-rec-first trec)
				     #t))))
   (assert-true (uim-bool '(let ((trec (test-rec-new)))
			     (test-rec-set-second! trec 'fooFoo)
			     (equal? (test-rec-second trec)
				     'fooFoo))))
   (assert-true (uim-bool '(let ((trec (test-rec-new)))
			     (test-rec-set-third! trec "barBar")
			     (equal? (test-rec-third trec)
				     "barBar"))))
   (assert-true (uim-bool '(let ((trec (test-rec-new)))
			     (test-rec-set-fourth! trec 44)
			     (equal? (test-rec-fourth trec)
				     44))))))

(define-uim-test-case "testcase util multi-segment utils"
  ("test multi-segment-make-index-list"
   (uim '(define old-lst '(0 1 2 3 4)))
   (assert-false (uim-bool '(multi-segment-make-index-list -1 old-lst)))
   (assert-equal ()
		 (uim '(multi-segment-make-index-list 0 old-lst)))
   (assert-equal '(0)
		 (uim '(multi-segment-make-index-list 1 old-lst)))
   (assert-equal '(0 1)
		 (uim '(multi-segment-make-index-list 2 old-lst)))
   (assert-equal '(0 1 2 3 4)
		 (uim '(multi-segment-make-index-list 5 old-lst)))
   (assert-equal '(0 1 2 3 4 0)
		 (uim '(multi-segment-make-index-list 6 old-lst)))
   (assert-equal '(0 1 2 3 4 0 0)
		 (uim '(multi-segment-make-index-list 7 old-lst))))

  ("test multi-segment-opposite-kana"
   (assert-equal (uim 'multi-segment-type-katakana)
		 (uim '(multi-segment-opposite-kana
			multi-segment-type-hiragana)))
   (assert-equal (uim 'multi-segment-type-hiragana)
		 (uim '(multi-segment-opposite-kana
			multi-segment-type-katakana)))
   (assert-equal (uim 'multi-segment-type-hiragana)
		 (uim '(multi-segment-opposite-kana
			multi-segment-type-hankana))))
)
