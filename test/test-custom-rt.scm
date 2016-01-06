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

;; TODO:
;;
;; custom-reload-customs

;; These tests are passed at revision 6605 (new repository)

(use test.unit)

(require "test/uim-test-utils")

(define-uim-test-case "testcase custom define-custom"
  (setup
   (lambda ()
     ;; Cancels LIBUIM_VANILLA=1. See init.scm for further details.
     (uim '(load-enabled-modules))

     (uim '(require "custom.scm"))))

  ("test define-custom (choice)"
   (assert-false (uim-bool '(symbol-bound? 'test-style)))

   (uim '(define-custom 'test-style 'test-style-ddskk
	   '(global)
	   '(choice
	     (test-style-uim "uim" "uim native")
	     (test-style-ddskk "ddskk like" "Similar to ddskk")
	     (test-style-canna "canna like" "Similar to canna"))
	   "Test style"
	   "long description will be here."))

   (assert-true (uim-bool '(symbol-bound? 'test-style)))
   (assert-equal 'test-style-ddskk
		 (uim 'test-style)))

  ("test define-custom (choice) #2"
   (uim '(define test-style 'test-style-uim))

   (uim '(define-custom 'test-style 'test-style-ddskk
	   '(global)
	   '(choice
	     (test-style-uim "uim" "uim native")
	     (test-style-ddskk "ddskk like" "Similar to ddskk")
	     (test-style-canna "canna like" "Similar to canna"))
	   "Test style"
	   "long description will be here."))

   (assert-true  (uim-bool '(symbol-bound? 'test-style)))
   ;; preexisting value is not overridden
   (assert-equal 'test-style-uim
		 (uim 'test-style)))

  ("test define-custom (key)"
   ;; single key str
   (assert-false (uim-bool '(symbol-bound? 'test-foo-key)))
   (assert-false (uim-bool '(symbol-bound? 'test-foo-key?)))

   (uim '(define-custom 'test-foo-key '("a")
	   '(global)
	   '(key)
	   "test foo key"
	   "long description will be here"))

   (assert-true  (uim-bool '(symbol-bound? 'test-foo-key)))
   (assert-equal '("a")
		 (uim 'test-foo-key))
   (assert-true  (uim-bool '(symbol-bound? 'test-foo-key?)))
   (assert-true  (uim-bool '(test-foo-key? (string->charcode "a") 0)))

   ;; key reference + key str
   (assert-false (uim-bool '(symbol-bound? 'test-bar-key)))
   (assert-false (uim-bool '(symbol-bound? 'test-bar-key?)))

   (uim '(define-custom 'test-bar-key '(test-foo-key "b")
	   '(global)
	   '(key)
	   "test bar key"
	   "long description will be here"))

   (assert-true  (uim-bool '(symbol-bound? 'test-bar-key)))
   (assert-equal '(test-foo-key "b")
		 (uim 'test-bar-key))
   (assert-true  (uim-bool '(symbol-bound? 'test-bar-key?)))
   (assert-true  (uim-bool '(test-bar-key? (string->charcode "a") 0)))
   (assert-true  (uim-bool '(test-bar-key? (string->charcode "b") 0))))

  ("test define-custom (key) #2"
   (uim '(define test-foo-key '("b")))
   (assert-false (uim-bool '(symbol-bound? 'test-foo-key?)))

   (uim '(define-custom 'test-foo-key '("a")
	   '(global)
	   '(key)
	   "test foo key"
	   "long description will be here"))

   ;; preexisting value is not overridden
   (assert-equal '("b")
		 (uim 'test-foo-key))
   ;; key predicate is not defined since custom-set-value! is not
   ;; invoked
   (assert-false (uim-bool '(symbol-bound? 'test-foo-key?)))))

(define-uim-test-case "testcase custom methods"
  (setup
   (lambda ()
     ;; Cancels LIBUIM_VANILLA=1. See init.scm for further details.
     (uim '(load-enabled-modules))

     (uim '(define-custom 'test-style 'test-style-ddskk
	     '(global)
	     '(choice
	       (test-style-uim "uim" "uim native")
	       (test-style-ddskk "ddskk like" "Similar to ddskk")
	       (test-style-canna "canna like" "Similar to canna"))
	     "Test style"
	     "long description will be here."))
     (uim '(define-custom 'test-available-ims '(anthy canna skk)
	     '(global)
	     '(ordered-list
	       (anthy "Anthy" "Anthy")
	       (canna "Cannd" "Canna")
	       (skk "SKK" "SKK"))
	     "Test avalilable IMs"
	     "long description will be here."))
     (uim '(define-custom 'test-cancel-key '("<Control>g" "escape")
	   '(global)
	   '(key)
	   "test cancel key"
	   "long description will be here."))
     (uim '(define-custom 'test-foo-key '("a" test-cancel-key)
	     '(global)
	     '(key)
	     "test foo key"
	     "long description will be here."))
     (uim '(define-custom 'test-bar-key '("b")
	   '(global)
	   '(key)
	   "test bar key"
	   "long description will be here."))
     (uim '(define-custom 'test-use-candidate-window? #t
	     '(test ui)
	     '(boolean)
	     "Use candidate window"
	     "long description will be here."))
     (uim '(define-custom 'test-nr-candidate-max 10
	     '(test advanced ui)
	     '(integer 1 20)
	     "Number of candidates in candidate window at a time"
	     "long description will be here."))
     (uim '(define-custom 'test-string "a string"
	     '(test)
	     '(string ".+")
	     "A string for testing purpose"
	     "long description will be here."))
     (uim '(define-custom 'test-dic-file-name "/usr/share/skk/SKK-JISYO.L"
	     '(test)
	     '(pathname)
	     "Dictionary file"
	     "long description will be here."))))

  ("test custom-key-exist?"
   (assert-true  (uim-bool '(custom-key-exist? 'test-cancel-key)))
   (assert-false (uim-bool '(custom-key-exist? 'test-baz-key)))
   (uim '(define-key test-baz-key? '("z")))
   (assert-false (uim-bool '(custom-key-exist? 'test-baz-key)))
     (uim '(define-custom 'test-baz-key '("z")
	   '(global)
	   '(key)
	   "test foo key"
	   "long description will be here."))
   (assert-true  (uim-bool '(custom-key-exist? 'test-baz-key))))

  ("test custom-value"
   (assert-equal 'test-style-ddskk
		 (uim '(custom-value 'test-style)))
   (assert-equal '(anthy canna skk)
		 (uim '(custom-value 'test-available-ims)))
   (assert-equal '("<Control>g" "escape")
		 (uim '(custom-value 'test-cancel-key)))
   (assert-true  (uim-bool '(custom-value 'test-use-candidate-window?)))
   (assert-equal 10
		 (uim '(custom-value 'test-nr-candidate-max)))
   (assert-equal "a string"
		 (uim '(custom-value 'test-string)))
   (assert-equal "/usr/share/skk/SKK-JISYO.L"
		 (uim '(custom-value 'test-dic-file-name))))

  ("test custom-set-value!"
   ;;; choice
   ;; default value
   (assert-equal 'test-style-ddskk
		 (uim '(custom-value 'test-style)))
   ;; valid value
   (assert-true  (uim-bool '(custom-set-value! 'test-style 'test-style-uim)))
   (assert-equal 'test-style-uim
		 (uim '(custom-value 'test-style)))
   ;; invalid value is also accepted
   (assert-true  (uim-bool '(custom-set-value! 'test-style 'test-style-invalid)))
   (assert-equal 'test-style-invalid
		 (uim '(custom-value 'test-style)))

   ;;; ordered-list
   ;; default value
   (assert-equal '(anthy canna skk)
		 (uim '(custom-value 'test-available-ims)))
   ;; valid value
   (assert-true  (uim-bool '(custom-set-value! 'test-available-ims
					       ())))
   (assert-equal ()
		 (uim '(custom-value 'test-available-ims)))
   (assert-true  (uim-bool '(custom-set-value! 'test-available-ims
					       '(anthy))))
   (assert-equal '(anthy)
		 (uim '(custom-value 'test-available-ims)))
   (assert-true  (uim-bool '(custom-set-value! 'test-available-ims
					       '(anthy skk))))
   (assert-equal '(anthy skk)
		 (uim '(custom-value 'test-available-ims)))
   (assert-true  (uim-bool '(custom-set-value! 'test-available-ims
					       '(skk anthy))))
   (assert-equal '(skk anthy)
		 (uim '(custom-value 'test-available-ims)))
   (assert-true  (uim-bool '(custom-set-value! 'test-available-ims
					       '(skk anthy canna))))
   (assert-equal '(skk anthy canna)
		 (uim '(custom-value 'test-available-ims)))
   ;; invalid value is also accepted
   (assert-true  (uim-bool '(custom-set-value! 'test-available-ims
					       '(nonexistent))))
   (assert-equal '(nonexistent)
		 (uim '(custom-value 'test-available-ims)))

   ;;; key
   ;; default value
   (assert-equal '("<Control>g" "escape")
		 (uim '(custom-value 'test-cancel-key)))
   ;; valid value
   (assert-true  (uim-bool '(custom-set-value! 'test-cancel-key '("a"))))
   (assert-equal '("a")
		 (uim '(custom-value 'test-cancel-key)))
   (assert-true  (uim-bool '(procedure? test-cancel-key?)))
   ;; invalid value is also accepted
   (assert-true  (uim-bool '(custom-set-value! 'test-cancel-key
					  '(test-nonexistent "a"))))
   (assert-equal '(test-nonexistent "a")
		 (uim '(custom-value 'test-cancel-key)))
   (assert-true  (uim-bool '(procedure? test-cancel-key?)))

   ;;; boolean
   ;; default value
   (assert-true  (uim-bool '(custom-value 'test-use-candidate-window?)))
   ;; valid value
   (assert-true  (uim-bool '(custom-set-value! 'test-use-candidate-window? #f)))
   (assert-false (uim-bool '(custom-value 'test-use-candidate-window?)))
   ;; boolean regards all non-#f value as true
   (assert-true (uim-bool '(custom-set-value! 'test-use-candidate-window? 10)))
   (assert-true (uim-bool '(custom-value 'test-use-candidate-window?)))

   ;;; integer
   ;; default value
   (assert-equal 10
		 (uim '(custom-value 'test-nr-candidate-max)))
   ;; valid value
   (assert-true  (uim-bool '(custom-set-value! 'test-nr-candidate-max 5)))
   (assert-equal 5
		 (uim '(custom-value 'test-nr-candidate-max)))
   ;; invalid value is also accepted
   (assert-true  (uim-bool '(custom-set-value! 'test-nr-candidate-max 25)))
   (assert-equal 25
		 (uim '(custom-value 'test-nr-candidate-max)))

   ;;; string
   ;; default value
   (assert-equal "a string"
		 (uim '(custom-value 'test-string)))
   ;; valid value
   (assert-true  (uim-bool '(custom-set-value! 'test-string "a altered string")))
   (assert-equal "a altered string"
		 (uim '(custom-value 'test-string)))
   ;; invalid value is also accepted
   (assert-true  (uim-bool '(custom-set-value! 'test-string #f)))
   (assert-false (uim-bool '(custom-value 'test-string)))

   ;;; pathname
   ;; default value
   (assert-equal "/usr/share/skk/SKK-JISYO.L"
		 (uim '(custom-value 'test-dic-file-name)))
   ;; valid value
   (assert-true  (uim-bool '(custom-set-value! 'test-dic-file-name
					 "/usr/local/share/skk/SKK-JISYO.ML")))
   (assert-equal "/usr/local/share/skk/SKK-JISYO.ML"
		 (uim '(custom-value 'test-dic-file-name)))
   ;; invalid value is also accepted
   (assert-true  (uim-bool '(custom-set-value! 'test-dic-file-name #f)))
   (assert-false (uim-bool '(custom-value 'test-dic-file-name)))))
