#!/usr/bin/env gosh

;;; Copyright (c) 2005 uim Project http://uim.freedesktop.org/
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
;;; THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
;;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
;;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
;;; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;;; SUCH DAMAGE.
;;;;

;; This file is tested with revision 707 of new repository

(use test.unit)

(require "test/uim-test-utils")

(define-uim-test-case "testcase key-event"
  (setup
   (lambda ()
     (uim '(require "event.scm"))))

  ("test key-event-new"
   (assert-equal (uim '(list 'key #f #f -1 #f #f #f 0 #t #f))
		 (uim '(key-event-new)))
   (assert-equal (uim '(list 'key #f #f -1 "a" #f #f 0 #t #f))
		 (uim '(key-event-new "a")))
   (assert-equal (uim '(list 'key
			     #f
			     #f
			     -1
			     "a"
			     'lkey_bracketleft
			     'pkey_qwerty_bracketleft
			     mod_Shift
			     #f
			     #t))
		 (uim '(key-event-new
			"a"
			'lkey_bracketleft
			'pkey_qwerty_bracketleft
			mod_Shift
			#f
			#t))))

  ("test key-event-char"
   ;; string->char rejects 0 length string
   (assert-false (uim-bool '(key-event-char (key-event-new ""))))    ; NUL
   ;; Since Gauche replaces control characters in string with
   ;; formatted string such as \x01, these tests cannot be evaluated
   ;; properly
;;   (assert-equal 1
;;		 (uim '(key-event-char (key-event-new ""))))  ; SOH
;;   (assert-equal 31
;;		 (uim '(key-event-char (key-event-new ""))))  ; US
   (assert-equal 32
		 (uim '(key-event-char (key-event-new " "))))   ; SPACE
   (assert-equal 33
		 (uim '(key-event-char (key-event-new "!"))))   ; !
   (assert-equal 47
		 (uim '(key-event-char (key-event-new "/"))))   ; /
   (assert-equal 48
		 (uim '(key-event-char (key-event-new "0"))))   ; 0
   (assert-equal 57
		 (uim '(key-event-char (key-event-new "9"))))   ; 9
   (assert-equal 58
		 (uim '(key-event-char (key-event-new ":"))))   ; :
   (assert-equal 64
		 (uim '(key-event-char (key-event-new "@"))))   ; @
   (assert-false (uim-bool '(key-event-char (key-event-new "AA"))))  ; AA
   (assert-equal 65
		 (uim '(key-event-char (key-event-new "A"))))   ; A
   (assert-equal 90
		 (uim '(key-event-char (key-event-new "Z"))))   ; Z
   (assert-false (uim-bool '(key-event-char (key-event-new "ZZ"))))  ; ZZ
   (assert-equal 91
		 (uim '(key-event-char (key-event-new "["))))   ; [
   (assert-equal 96
		 (uim '(key-event-char (key-event-new "`"))))   ; `
   (assert-false (uim-bool '(key-event-char (key-event-new "aa"))))  ; aa
   (assert-equal 97
		 (uim '(key-event-char (key-event-new "a"))))   ; a
   (assert-equal 122
		 (uim '(key-event-char (key-event-new "z"))))   ; z
   (assert-false (uim-bool '(key-event-char (key-event-new "zz"))))  ; zz
   (assert-equal 123
		 (uim '(key-event-char (key-event-new "{"))))   ; {
   (assert-equal 126
		 (uim '(key-event-char (key-event-new "~"))))   ; ~
;;   (assert-equal 127
;;		 (uim '(key-event-char (key-event-new ""))))  ; DEL
   )

  ("test key-event-char-upcase!"
   (uim '(define test-ev (key-event-new " ")))
   (uim '(key-event-char-upcase! test-ev))
   (assert-equal " "
		 (uim '(key-event-str test-ev)))

   (uim '(define test-ev (key-event-new "!")))
   (uim '(key-event-char-upcase! test-ev))
   (assert-equal "!"
		 (uim '(key-event-str test-ev)))

   (uim '(define test-ev (key-event-new "/")))
   (uim '(key-event-char-upcase! test-ev))
   (assert-equal "/"
		 (uim '(key-event-str test-ev)))

   (uim '(define test-ev (key-event-new "0")))
   (uim '(key-event-char-upcase! test-ev))
   (assert-equal "0"
		 (uim '(key-event-str test-ev)))

   (uim '(define test-ev (key-event-new "9")))
   (uim '(key-event-char-upcase! test-ev))
   (assert-equal "9"
		 (uim '(key-event-str test-ev)))

   (uim '(define test-ev (key-event-new ":")))
   (uim '(key-event-char-upcase! test-ev))
   (assert-equal ":"
		 (uim '(key-event-str test-ev)))

   (uim '(define test-ev (key-event-new "@")))
   (uim '(key-event-char-upcase! test-ev))
   (assert-equal "@"
		 (uim '(key-event-str test-ev)))

   (uim '(define test-ev (key-event-new "A")))
   (uim '(key-event-char-upcase! test-ev))
   (assert-equal "A"
		 (uim '(key-event-str test-ev)))

   ;; multiple characters are rejected
   (uim '(define test-ev (key-event-new "AA")))
   (uim '(key-event-char-upcase! test-ev))
   (assert-equal ""
		 (uim '(key-event-str test-ev)))

   (uim '(define test-ev (key-event-new "Z")))
   (uim '(key-event-char-upcase! test-ev))
   (assert-equal "Z"
		 (uim '(key-event-str test-ev)))

   ;; multiple characters are rejected
   (uim '(define test-ev (key-event-new "ZZ")))
   (uim '(key-event-char-upcase! test-ev))
   (assert-equal ""
		 (uim '(key-event-str test-ev)))

   (uim '(define test-ev (key-event-new "[")))
   (uim '(key-event-char-upcase! test-ev))
   (assert-equal "["
		 (uim '(key-event-str test-ev)))

   (uim '(define test-ev (key-event-new "`")))
   (uim '(key-event-char-upcase! test-ev))
   (assert-equal "`"
		 (uim '(key-event-str test-ev)))

   (uim '(define test-ev (key-event-new "a")))
   (uim '(key-event-char-upcase! test-ev))
   (assert-equal "A"
		 (uim '(key-event-str test-ev)))

   ;; multiple characters are rejected
   (uim '(define test-ev (key-event-new "aa")))
   (uim '(key-event-char-upcase! test-ev))
   (assert-equal ""
		 (uim '(key-event-str test-ev)))

   (uim '(define test-ev (key-event-new "z")))
   (uim '(key-event-char-upcase! test-ev))
   (assert-equal "Z"
		 (uim '(key-event-str test-ev)))

   ;; multiple characters are rejected
   (uim '(define test-ev (key-event-new "zz")))
   (uim '(key-event-char-upcase! test-ev))
   (assert-equal ""
		 (uim '(key-event-str test-ev)))

   (uim '(define test-ev (key-event-new "{")))
   (uim '(key-event-char-upcase! test-ev))
   (assert-equal "{"
		 (uim '(key-event-str test-ev)))

   (uim '(define test-ev (key-event-new "~")))
   (uim '(key-event-char-upcase! test-ev))
   (assert-equal "~"
		 (uim '(key-event-str test-ev))))

  ("test key-event-char-downcase!"
   (uim '(define test-ev (key-event-new " ")))
   (uim '(key-event-char-downcase! test-ev))
   (assert-equal " "
		 (uim '(key-event-str test-ev)))

   (uim '(define test-ev (key-event-new "!")))
   (uim '(key-event-char-downcase! test-ev))
   (assert-equal "!"
		 (uim '(key-event-str test-ev)))

   (uim '(define test-ev (key-event-new "/")))
   (uim '(key-event-char-downcase! test-ev))
   (assert-equal "/"
		 (uim '(key-event-str test-ev)))

   (uim '(define test-ev (key-event-new "0")))
   (uim '(key-event-char-downcase! test-ev))
   (assert-equal "0"
		 (uim '(key-event-str test-ev)))

   (uim '(define test-ev (key-event-new "9")))
   (uim '(key-event-char-downcase! test-ev))
   (assert-equal "9"
		 (uim '(key-event-str test-ev)))

   (uim '(define test-ev (key-event-new ":")))
   (uim '(key-event-char-downcase! test-ev))
   (assert-equal ":"
		 (uim '(key-event-str test-ev)))

   (uim '(define test-ev (key-event-new "@")))
   (uim '(key-event-char-downcase! test-ev))
   (assert-equal "@"
		 (uim '(key-event-str test-ev)))

   (uim '(define test-ev (key-event-new "A")))
   (uim '(key-event-char-downcase! test-ev))
   (assert-equal "a"
		 (uim '(key-event-str test-ev)))

   ;; multiple characters are rejected
   (uim '(define test-ev (key-event-new "AA")))
   (uim '(key-event-char-downcase! test-ev))
   (assert-equal ""
		 (uim '(key-event-str test-ev)))

   (uim '(define test-ev (key-event-new "Z")))
   (uim '(key-event-char-downcase! test-ev))
   (assert-equal "z"
		 (uim '(key-event-str test-ev)))

   ;; multiple characters are rejected
   (uim '(define test-ev (key-event-new "ZZ")))
   (uim '(key-event-char-downcase! test-ev))
   (assert-equal ""
		 (uim '(key-event-str test-ev)))

   (uim '(define test-ev (key-event-new "[")))
   (uim '(key-event-char-downcase! test-ev))
   (assert-equal "["
		 (uim '(key-event-str test-ev)))

   (uim '(define test-ev (key-event-new "`")))
   (uim '(key-event-char-downcase! test-ev))
   (assert-equal "`"
		 (uim '(key-event-str test-ev)))

   (uim '(define test-ev (key-event-new "a")))
   (uim '(key-event-char-downcase! test-ev))
   (assert-equal "a"
		 (uim '(key-event-str test-ev)))

   ;; multiple characters are rejected
   (uim '(define test-ev (key-event-new "aa")))
   (uim '(key-event-char-downcase! test-ev))
   (assert-equal ""
		 (uim '(key-event-str test-ev)))

   (uim '(define test-ev (key-event-new "z")))
   (uim '(key-event-char-downcase! test-ev))
   (assert-equal "z"
		 (uim '(key-event-str test-ev)))

   ;; multiple characters are rejected
   (uim '(define test-ev (key-event-new "zz")))
   (uim '(key-event-char-downcase! test-ev))
   (assert-equal ""
		 (uim '(key-event-str test-ev)))

   (uim '(define test-ev (key-event-new "{")))
   (uim '(key-event-char-downcase! test-ev))
   (assert-equal "{"
		 (uim '(key-event-str test-ev)))

   (uim '(define test-ev (key-event-new "~")))
   (uim '(key-event-char-downcase! test-ev))
   (assert-equal "~"
		 (uim '(key-event-str test-ev))))

;;  ("test key-event-covers?"
;;   )
)
