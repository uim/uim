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

;; This file is tested with revision 190 of new repository

;; TODO:
;;
;; custom-broadcast-custom
;; custom-broadcast-customs

(use test.unit)

(require "test/uim-test-utils")

(define sort-symbol
  (lambda (symbols)
    (map string->symbol
	 (sort (map symbol->string
		    symbols)
	       string<?))))

(define-uim-test-case "testcase custom validators"
  (setup
   (lambda ()
     (uim '(require "custom.scm"))))

  ("test anything?"
   (assert-true (uim-bool '(anything? #f)))
   (assert-true (uim-bool '(anything? "foo")))
   (assert-true (uim-bool '(anything? 'foo)))
   (assert-true (uim-bool '(anything? -1)))
   (assert-true (uim-bool '(anything? 0)))
   (assert-true (uim-bool '(anything? 1)))
   (assert-true (uim-bool '(anything? 10)))
   (assert-true (uim-bool '(anything? ())))
   (assert-true (uim-bool '(anything? '(1 "2" 'three)))))
  ("test custom-boolean?"
   (assert-true (uim-bool '(custom-boolean? #f)))
   (assert-true (uim-bool '(custom-boolean? "foo")))
   (assert-true (uim-bool '(custom-boolean? 'foo)))
   (assert-true (uim-bool '(custom-boolean? -1)))
   (assert-true (uim-bool '(custom-boolean? 0)))
   (assert-true (uim-bool '(custom-boolean? 1)))
   (assert-true (uim-bool '(custom-boolean? 10)))
   (assert-true (uim-bool '(custom-boolean? ())))
   (assert-true (uim-bool '(custom-boolean? '(1 "2" 'three)))))
  ("test custom-integer?"
   (assert-false (uim-bool '(custom-integer? #f 2 10)))
   (assert-false (uim-bool '(custom-integer? "foo" 2 10)))
   (assert-false (uim-bool '(custom-integer? 'foo 2 10)))
   (assert-false (uim-bool '(custom-integer? -1 2 10)))
   (assert-false (uim-bool '(custom-integer? 0 2 10)))
   (assert-false (uim-bool '(custom-integer? 1 2 10)))
   (assert-true  (uim-bool '(custom-integer? 2 2 10)))
   (assert-true  (uim-bool '(custom-integer? 3 2 10)))
   (assert-true  (uim-bool '(custom-integer? 9 2 10)))
   (assert-true  (uim-bool '(custom-integer? 10 2 10)))
   (assert-false (uim-bool '(custom-integer? 11 2 10)))
   (assert-false (uim-bool '(custom-integer? () 2 10)))
   (assert-false (uim-bool '(custom-integer? '(1 "2" 'three) 2 10))))
  ("test custom-string?"
   (assert-false (uim-bool '(custom-string? #f ".*")))
   (assert-true  (uim-bool '(custom-string? "" ".*")))
   (assert-true  (uim-bool '(custom-string? "foo" ".*")))
   (assert-false (uim-bool '(custom-string? 'foo ".*")))
   (assert-false (uim-bool '(custom-string? -1 ".*")))
   (assert-false (uim-bool '(custom-string? 0 ".*")))
   (assert-false (uim-bool '(custom-string? 1 ".*")))
   (assert-false (uim-bool '(custom-string? 10 ".*")))
   (assert-false (uim-bool '(custom-string? () ".*")))
   (assert-false (uim-bool '(custom-string? '(1 "2" 'three) ".*"))))
  ("test custom-pathname?"
   (assert-false (uim-bool '(custom-pathname? #f)))
   (assert-false (uim-bool '(custom-pathname? 'foo)))
   (assert-false (uim-bool '(custom-pathname? -1)))
   (assert-false (uim-bool '(custom-pathname? 0)))
   (assert-false (uim-bool '(custom-pathname? 1)))
   (assert-false (uim-bool '(custom-pathname? 10)))
   (assert-false (uim-bool '(custom-pathname? ())))
   (assert-false (uim-bool '(custom-pathname? '(1 "2" 'three))))
   (assert-true  (uim-bool '(custom-pathname? "/usr/share/uim/foo.scm")))
   (assert-true  (uim-bool '(custom-pathname? "~/.uim")))
   (assert-true  (uim-bool '(custom-pathname? "share/uim/bar.scm")))
   (assert-true  (uim-bool '(custom-pathname? "baz.scm"))))
  ("test custom-valid-choice?"
   (assert-false (uim-bool '(custom-valid-choice?
			     #f
			     '(uim-color-uim "uim" "uim native")
			     '(uim-color-atok "ATOK like" "Similar to ATOK"))))
   (assert-false (uim-bool '(custom-valid-choice?
			     "foo"
			     '(uim-color-uim "uim" "uim native")
			     '(uim-color-atok "ATOK like" "Similar to ATOK"))))
   (assert-false (uim-bool '(custom-valid-choice?
			     -1
			     '(uim-color-uim "uim" "uim native")
			     '(uim-color-atok "ATOK like" "Similar to ATOK"))))
   (assert-false (uim-bool '(custom-valid-choice?
			     0
			     '(uim-color-uim "uim" "uim native")
			     '(uim-color-atok "ATOK like" "Similar to ATOK"))))
   (assert-false (uim-bool '(custom-valid-choice?
			     1
			     '(uim-color-uim "uim" "uim native")
			     '(uim-color-atok "ATOK like" "Similar to ATOK"))))
   (assert-false (uim-bool '(custom-valid-choice?
			     10
			     '(uim-color-uim "uim" "uim native")
			     '(uim-color-atok "ATOK like" "Similar to ATOK"))))
   (assert-false (uim-bool '(custom-valid-choice?
			     ()
			     '(uim-color-uim "uim" "uim native")
			     '(uim-color-atok "ATOK like" "Similar to ATOK"))))
   (assert-false (uim-bool '(custom-valid-choice?
			     '(1 "2" 'three)
			     '(uim-color-uim "uim" "uim native")
			     '(uim-color-atok "ATOK like" "Similar to ATOK"))))
   (assert-true  (uim-bool '(custom-valid-choice?
			     'uim-color-uim
			     '(uim-color-uim "uim" "uim native")
			     '(uim-color-atok "ATOK like" "Similar to ATOK"))))
   (assert-false (uim-bool '(custom-valid-choice?
			     'uim-color-nonexistent
			     '(uim-color-uim "uim" "uim native")
			     '(uim-color-atok "ATOK like" "Similar to ATOK")))))
  ("test custom-ordered-list?"
   ;; siod interprets #f as ()
;;   (assert-false (uim-bool '(custom-ordered-list?
;;			     #f
;;			     '(uim-color-uim "uim" "uim native")
;;			     '(uim-color-atok "ATOK like" "Similar to ATOK"))))
   (assert-false (uim-bool '(custom-ordered-list?
			     "foo"
			     '(uim-color-uim "uim" "uim native")
			     '(uim-color-atok "ATOK like" "Similar to ATOK"))))
   (assert-false (uim-bool '(custom-ordered-list?
			     -1
			     '(uim-color-uim "uim" "uim native")
			     '(uim-color-atok "ATOK like" "Similar to ATOK"))))
   (assert-false (uim-bool '(custom-ordered-list?
			     0
			     '(uim-color-uim "uim" "uim native")
			     '(uim-color-atok "ATOK like" "Similar to ATOK"))))
   (assert-false (uim-bool '(custom-ordered-list?
			     1
			     '(uim-color-uim "uim" "uim native")
			     '(uim-color-atok "ATOK like" "Similar to ATOK"))))
   (assert-false (uim-bool '(custom-ordered-list?
			     10
			     '(uim-color-uim "uim" "uim native")
			     '(uim-color-atok "ATOK like" "Similar to ATOK"))))
   (assert-true  (uim-bool '(custom-ordered-list?
			     ()
			     '(uim-color-uim "uim" "uim native")
			     '(uim-color-atok "ATOK like" "Similar to ATOK"))))
   (assert-false (uim-bool '(custom-ordered-list?
			     '(1 "2" 'three)
			     '(uim-color-uim "uim" "uim native")
			     '(uim-color-atok "ATOK like" "Similar to ATOK"))))
   (assert-false (uim-bool '(custom-ordered-list?
			     'uim-color-uim
			     '(uim-color-uim "uim" "uim native")
			     '(uim-color-atok "ATOK like" "Similar to ATOK"))))

   (assert-true  (uim-bool '(custom-ordered-list?
			     '(uim-color-uim)
			     '(uim-color-uim "uim" "uim native")
			     '(uim-color-atok "ATOK like" "Similar to ATOK"))))
   (assert-true  (uim-bool '(custom-ordered-list?
			     '(uim-color-uim uim-color-atok)
			     '(uim-color-uim "uim" "uim native")
			     '(uim-color-atok "ATOK like" "Similar to ATOK"))))
   (assert-true  (uim-bool '(custom-ordered-list?
			     '(uim-color-atok uim-color-uim)
			     '(uim-color-uim "uim" "uim native")
			     '(uim-color-atok "ATOK like" "Similar to ATOK"))))
   (assert-true  (uim-bool '(custom-ordered-list?
			     '(uim-color-atok uim-color-uim uim-color-user)
			     '(uim-color-uim "uim" "uim native")
			     '(uim-color-atok "ATOK like" "Similar to ATOK")
			     '(uim-color-user "user defined" "user defined"))))
   (assert-true  (uim-bool '(custom-ordered-list?
			     '(uim-color-atok uim-color-user)
			     '(uim-color-uim "uim" "uim native")
			     '(uim-color-atok "ATOK like" "Similar to ATOK")
			     '(uim-color-user "user defined" "user defined"))))
   (assert-false (uim-bool '(custom-ordered-list?
			     '(uim-color-nonexistent)
			     '(uim-color-uim "uim" "uim native")
			     '(uim-color-atok "ATOK like" "Similar to ATOK"))))
   (assert-false (uim-bool '(custom-ordered-list?
			     '(uim-color-uim uim-color-nonexistent)
			     '(uim-color-uim "uim" "uim native")
			     '(uim-color-atok "ATOK like" "Similar to ATOK"))))
   (assert-false (uim-bool '(custom-ordered-list?
			     '(uim-color-uim uim-color-atok uim-color-nonexistent)
			     '(uim-color-uim "uim" "uim native")
			     '(uim-color-atok "ATOK like" "Similar to ATOK")))))
  ("test key-definition?"
   ;; more detailed test is done by test valid-strict-key-str?

   ;; null key fails
   (assert-false (uim-bool '(valid-strict-key-str? "")))
   
   ;; invalid key definitions
   (assert-false  (uim-bool '(valid-key-str? "nonexistent")))
   (assert-false  (uim-bool '(valid-key-str? "<Shift>nonexistent")))
   (assert-false  (uim-bool '(valid-key-str? "<Nonexistent>a")))

   ;; single key with single modifier
   (assert-true  (uim-bool '(valid-strict-key-str? "<Shift>a")))

   ;; single key with multiple modifiers
   (assert-true  (uim-bool '(valid-strict-key-str? "<Shift><Control><Meta>A")))

   ;; single key with single translator
   (assert-false (uim-bool '(valid-strict-key-str? "<IgnoreShift>0")))

   ;; single key with multiple translators
   (assert-false (uim-bool '(valid-strict-key-str? "<IgnoreShift><IgnoreCase>return")))))

(define-uim-test-case "testcase custom custom-symbol"
  (setup
   (lambda ()
     (uim '(require "custom.scm"))))

  ("test custom-choice-rec-new"
   (assert-equal (uim '(list #f "" ""))
		 (uim '(custom-choice-rec-new))))

  ("test custom-choice-label"
   (assert-equal "uim"
		 (uim '(custom-choice-label 'uim-color 'uim-color-uim)))
   (assert-equal "ATOK like"
		 (uim '(custom-choice-label 'uim-color 'uim-color-atok)))
   (assert-error (lambda ()
                   (uim '(custom-choice-label 'uim-color
                                              'uim-color-nonexistent))))
   (assert-error (lambda ()
                   (uim '(custom-choice-label 'uim-nonexistent
                                              'uim-nonexistent)))))
  ("test custom-choice-desc"
   (assert-equal "uim native"
		 (uim '(custom-choice-desc 'uim-color 'uim-color-uim)))
   (assert-equal "Similar to ATOK"
		 (uim '(custom-choice-desc 'uim-color 'uim-color-atok)))
   (assert-error (lambda () (uim '(custom-choice-desc 'uim-color
                                                      'uim-color-nonexistent))))
   (assert-error (lambda () (uim '(custom-choice-desc 'uim-nonexistent
                                                      'uim-nonexistent))))))

(define-uim-test-case "testcase custom custom-group"
  (setup
   (lambda ()
     (uim '(begin
	     (load "custom.scm") ;; to reset previously defined groups
	     (define test-group-recs-length 0)
	     (define-custom-group 'global
	       (_ "Global settings")
	       (_ "long description will be here."))

	     (define-custom-group 'advanced
	       (_ "Advanced settings")
	       (_ "long description will be here."))

	     (define-custom 'uim-color 'uim-color-uim
	       '(global)
	       '(choice
		 (uim-color-uim "uim" "uim native")
		 (uim-color-atok "ATOK like" "Similar to ATOK"))
	       (_ "Preedit color")
	       (_ "long description will be here."))
	     (define-custom-group 'im-switching
	       (_ "Input method switching")
	       (_ "long description will be here."))

	     (define-custom 'enable-im-switch #f
	       '(global im-switching advanced)
	       '(boolean)
	       (_ "Enable IM switching by hotkey")
	       (_ "long description will be here."))

	     (define-custom 'candidate-window-position "caret"
	       '(global)
	       '(string "^(caret|left|right)$")
	       (_ "Candidate window position")
	       (_ "long description will be here."))
	     (define-custom-group 'anthy
	       anthy-im-canonical-name
	       anthy-im-desc)

	     (define-custom 'anthy-use-candidate-window? #t
	       '(anthy)
	       '(boolean)
	       (_ "Use candidate window")
	       (_ "long description will be here."))

	     (define-custom 'anthy-candidate-op-count 1
	       '(anthy)
	       '(integer 0 99)
	       (_ "Conversion key press count to show candidate window")
	       (_ "long description will be here."))

	     (define-custom 'anthy-nr-candidate-max 10
	       '(anthy)
	       '(integer 1 20)
	       (_ "Number of candidates in candidate window at a time")
	       (_ "long description will be here."))

	     (define-custom 'anthy-select-candidate-by-numeral-key? #f
	       '(anthy)
	       '(boolean)
	       (_ "Select candidate by numeral keys")
	       (_ "long description will be here."))

	     (define-custom 'anthy-show-segment-separator? #f
	       '(anthy advanced)
	       '(boolean)
	       (_ "Show segment separator")
	       (_ "long description will be here."))

	     (define-custom 'anthy-segment-separator "|"
	       '(anthy advanced)
	       '(string ".*")
	       (_ "Segment separator")
	       (_ "long description will be here."))
	     (define-custom-group 'canna
	       canna-im-canonical-name
	       canna-im-desc)

	     (define-custom 'canna-use-candidate-window? #t
	       '(canna)
	       '(boolean)
	       (_ "Use candidate window")
	       (_ "long description will be here."))

	     (define-custom 'canna-candidate-op-count 1
	       '(canna)
	       '(integer 0 99)
	       (_ "Conversion key press count to show candidate window")
	       (_ "long description will be here."))

	     (define-custom 'canna-nr-candidate-max 10
	       '(canna)
	       '(integer 1 20)
	       (_ "Number of candidates in candidate window at a time")
	       (_ "long description will be here."))

	     (define-custom 'canna-show-segment-separator? #f
	       '(canna advanced)
	       '(boolean)
	       (_ "Show segment separator")
	       (_ "long description will be here."))

	     (define-custom 'canna-segment-separator "|"
	       '(canna advanced)
	       '(string ".*")
	       (_ "Segment separator")
	       (_ "long description will be here."))
	     (define-custom-group 'skk
	       skk-im-canonical-name
	       skk-im-desc)

	     (define-custom 'skk-dic-file-name (string-append (sys-datadir)
							      "/skk/SKK-JISYO.L")
	       '(skk)
	       '(pathname)
	       (_ "Dictionary file")
	       (_ "long description will be here."))

	     (define-custom 'skk-personal-dic-filename
	       (string-append (getenv "HOME") "/.skk-jisyo")
	       '(skk)
	       '(pathname)
	       (_ "Personal dictionary file")
	       (_ "long description will be here."))

	     (define-custom 'skk-uim-personal-dic-filename
	       (string-append (getenv "HOME") "/.skk-uim-jisyo")
	       '(skk)
	       '(pathname)
	       (_ "Personal dictionary file (dedicated to uim)")
	       (_ "long description will be here."))

	     (define-custom 'skk-use-candidate-window? #t
	       '(skk)
	       '(boolean)
	       (_ "Use candidate window")
	       (_ "long description will be here."))

	     (define-custom 'skk-candidate-op-count 0
	       '(skk)
	       '(integer 0 99)
	       (_ "Conversion key press count to show candidate window")
	       (_ "long description will be here."))

	     (define-custom 'skk-nr-candidate-max 10
	       '(skk)
	       '(integer 1 20)
	       (_ "Number of candidates in candidate window at a time")
	       (_ "long description will be here."))

	     (define-custom 'skk-use-recursive-learning? #t
	       '(skk advanced)
	       '(boolean)
	       (_ "Use recursive learning")
	       (_ "long description will be here."))

	     (define-custom 'skk-egg-like-newline? #f
	       '(skk advanced)
	       '(boolean)
	       (_ "Use Enter key as just committing (egg-like operation)")
	       (_ "long description will be here."))

	     (define-custom 'skk-commit-newline-explicitly? #f
	       '(skk advanced)
	       '(boolean)
	       (_ "Commit newline as ASCII string instead of native key-event")
	       (_ "long description will be here."))

	     (define-custom 'skk-style 'skk-style-ddskk-like
	       '(skk advanced)
	       '(choice
		 (skk-style-ddskk-like "ddskk" "Similar to ddskk")
		 (skk-style-uim "uim" "uim native"))
	       (_ "Visual style")
	       (_ "long description will be here."))
	     (define-custom-group 'prime
	       prime-im-canonical-name
	       prime-im-desc)
	     (define-custom 'prime-nr-candidate-max 10
	       '(prime)
	       '(integer 1 20)
	       (_ "Number of candidates in candidate window at a time")
	       (_ "long description will be here."))

	     (define-custom 'prime-always-show-window? #t
	       '(prime)
	       '(boolean)
	       (_ "Always showing candidate window")
	       (_ "long description will be here."))

	     (define-custom 'prime-auto-register-mode? #t
	       '(prime)
	       '(boolean)
	       (_ "Enable auto register mode")
	       (_ "long description will be here."))

	     (define-custom 'prime-pseudo-mode-cursor? #f
	       '(prime)
	       '(boolean)
	       (_ "Enable pseudo mode cursor")
	       (_ "long description will be here."))

	     (define-custom 'prime-char-annotation? #t
	       '(prime)
	       '(boolean)
	       (_ "Show candidate annotations")
	       (_ "long description will be here."))


	     (define-custom 'prime-mask-pending-preedit? #f
	       '(prime)
	       '(boolean)
	       (_ "Mask preedit strings (For tcode users)")
	       (_ "long description will be here."))
	     (define-custom-group 'other-ims
	       (_ "Other input methods")
	       (_ "long description will be here."))

	     (define-custom 'generic-use-candidate-window? #t
	       '(other-ims)
	       '(boolean)
	       (_ "Use candidate window")
	       (_ "long description will be here."))

	     (define-custom 'generic-candidate-op-count 1
	       '(other-ims)
	       '(integer 0 99)
	       (_ "Conversion key press count to show candidate window")
	       (_ "long description will be here."))

	     (define-custom 'generic-nr-candidate-max 10
	       '(other-ims)
	       '(integer 1 20)
	       (_ "Number of candidates in candidate window at a time")
	       (_ "long description will be here."))
	     (define-custom-group 'spellcheck
	       spellcheck-im-canonical-name
	       spellcheck-im-desc)

	     (define-custom 'spellcheck-use-candidate-window? #t
	       '(spellcheck)
	       '(boolean)
	       (_ "Use candidate window")
	       (_ "long description will be here."))

	     (define-custom 'spellcheck-candidate-op-count 1
	       '(spellcheck)
	       '(integer 0 99)
	       (_ "Conversion key press count to show candidate window")
	       (_ "long description will be here."))

	     (define-custom 'spellcheck-preedit-immediate-commit? #f
	       '(spellcheck)
	       '(boolean)
	       (_ "spellcheck-preedit-immediate-commit?")
	       (_ "long description will be here."))

	     (define-custom 'spellcheck-always-show-window? #t
	       '(spellcheck)
	       '(boolean)
	       (_ "Always showing candidate window")
	       (_ "long description will be here."))))))

  ("test custom-group-rec-new"
   (assert-equal (uim '(list #f "" ""))
		 (uim '(custom-group-rec-new))))

  ("test define-custom-group, custom-group-rec"
   (uim '(set! test-group-recs-length
	       (length custom-group-rec-alist)))

   (assert-false (uim-bool '(custom-group-rec 'test-group)))
   (uim '(define-custom-group
	  'test-group
	  "test group"
	  "long description of test group"))
   (assert-true  (uim-bool '(custom-group-rec 'test-group)))
   (assert-equal (uim '(list 'test-group
			     "test group"
			     "long description of test group"))
		 (uim '(custom-group-rec 'test-group)))
   (assert-equal (uim '(+ test-group-recs-length 1))
		 (uim '(length custom-group-rec-alist)))

   (assert-false (uim-bool '(custom-group-rec 'test-group2)))
   (uim '(define-custom-group
	  'test-group2
	  "test group 2"
	  "long description of test group 2"))
   (assert-true  (uim-bool '(custom-group-rec 'test-group2)))
   (assert-equal (uim '(list 'test-group2
			     "test group 2"
			     "long description of test group 2"))
		 (uim '(custom-group-rec 'test-group2)))
   (assert-equal (uim '(+ test-group-recs-length 2))
		 (uim '(length custom-group-rec-alist)))

   (assert-false (uim-bool '(custom-group-rec 'test-group3)))
   (uim '(define-custom-group
	  'test-group3
	  "test group 3"
	  "long description of test group 3"))
   (assert-true  (uim-bool '(custom-group-rec 'test-group3)))
   (assert-equal (uim '(list 'test-group3
			     "test group 3"
			     "long description of test group 3"))
		 (uim '(custom-group-rec 'test-group3)))
   (assert-equal (uim '(+ test-group-recs-length 3))
		 (uim '(length custom-group-rec-alist)))

   ;; verify again after other groups added
   (assert-equal (uim '(list 'test-group
			     "test group"
			     "long description of test group"))
		 (uim '(custom-group-rec 'test-group)))
   (assert-equal (uim '(list 'test-group2
			     "test group 2"
			     "long description of test group 2"))
		 (uim '(custom-group-rec 'test-group2)))
   (assert-equal (uim '(list 'test-group3
			     "test group 3"
			     "long description of test group 3"))
		 (uim '(custom-group-rec 'test-group3))))
  ("test custom-list-groups"
   (assert-equal '(advanced anthy canna global im-switching other-ims prime skk spellcheck)
		 (sort-symbol (uim '(custom-list-groups)))))
  ("test custom-list-primary-groups"
   ;; defined order have to be kept
   (assert-equal '(global anthy canna skk prime other-ims spellcheck)
		 (uim '(custom-list-primary-groups))))
  ("test custom-collect-by-group"
   ;; defined order have to be kept
   (assert-equal '(uim-color enable-im-switch candidate-window-position anthy-use-candidate-window? anthy-candidate-op-count anthy-nr-candidate-max anthy-select-candidate-by-numeral-key? anthy-show-segment-separator? anthy-segment-separator canna-use-candidate-window? canna-candidate-op-count canna-nr-candidate-max canna-show-segment-separator? canna-segment-separator skk-dic-file-name skk-personal-dic-filename skk-uim-personal-dic-filename skk-use-candidate-window? skk-candidate-op-count skk-nr-candidate-max skk-use-recursive-learning? skk-egg-like-newline? skk-commit-newline-explicitly? skk-style prime-nr-candidate-max prime-always-show-window? prime-auto-register-mode? prime-pseudo-mode-cursor? prime-char-annotation? prime-mask-pending-preedit? generic-use-candidate-window? generic-candidate-op-count generic-nr-candidate-max spellcheck-use-candidate-window? spellcheck-candidate-op-count spellcheck-preedit-immediate-commit? spellcheck-always-show-window?)
		 (uim '(custom-collect-by-group #f)))  ;; any group
   (assert-equal '(uim-color enable-im-switch candidate-window-position)
		 (uim '(custom-collect-by-group 'global)))
   (assert-equal '(anthy-use-candidate-window? anthy-candidate-op-count anthy-nr-candidate-max anthy-select-candidate-by-numeral-key? anthy-show-segment-separator? anthy-segment-separator)
		 (uim '(custom-collect-by-group 'anthy)))
   (assert-equal '(canna-use-candidate-window? canna-candidate-op-count canna-nr-candidate-max canna-show-segment-separator? canna-segment-separator)
		 (uim '(custom-collect-by-group 'canna)))
   (assert-equal '(skk-dic-file-name skk-personal-dic-filename skk-uim-personal-dic-filename skk-use-candidate-window? skk-candidate-op-count skk-nr-candidate-max skk-use-recursive-learning? skk-egg-like-newline? skk-commit-newline-explicitly? skk-style)
		 (uim '(custom-collect-by-group 'skk)))
   (assert-equal '(prime-nr-candidate-max prime-always-show-window? prime-auto-register-mode? prime-pseudo-mode-cursor? prime-char-annotation? prime-mask-pending-preedit?)
		 (uim '(custom-collect-by-group 'prime)))
   (assert-equal '(generic-use-candidate-window? generic-candidate-op-count generic-nr-candidate-max)
		 (uim '(custom-collect-by-group 'other-ims)))
   (assert-equal '(spellcheck-use-candidate-window? spellcheck-candidate-op-count spellcheck-preedit-immediate-commit? spellcheck-always-show-window?)
		 (uim '(custom-collect-by-group 'spellcheck)))))

(define-uim-test-case "testcase custom custom-group methods"
  (setup
   (lambda ()
     ;;(uim '(load "custom.scm"))
     (uim '(define-custom-group
	    'test-group
	    "test group"
	    "long description of test group"))
     (uim '(define-custom-group
	    'test-group2
	    "test group 2"
	    "long description of test group 2"))
     (uim '(define-custom-group
	    'test-group3
	    "test group 3"
	    "long description of test group 3"))
     (uim '(define-custom-group
	    'test-group4
	    "test group 4"
	    "long description of test group 4"))

     (uim '(define-custom 'test-custom #t
	     '(test-group)
	     '(boolean)
	     "test custom"
	     "long description will be here."))
     (uim '(define-custom 'test-custom2 #t
	     '(test-group test-group2)
	     '(boolean)
	     "test custom2"
	     "long description will be here."))
     (uim '(define-custom 'test-custom3 #t
	     '(test-group test-group3 test-group2)
	     '(boolean)
	     "test custom3"
	     "long description will be here."))
     (uim '(define-custom 'test-custom4 #t
	     '(test-group4)
	     '(boolean)
	     "test custom4"
	     "long description will be here."))))

  ("test custom-group-label"
   (assert-equal "test group"
		 (uim '(custom-group-label 'test-group)))
   (assert-equal "test group 2"
		 (uim '(custom-group-label 'test-group2)))
   (assert-equal "test group 3"
		 (uim '(custom-group-label 'test-group3))))

  ("test custom-group-desc"
   (assert-equal "long description of test group"
		 (uim '(custom-group-desc 'test-group)))
   (assert-equal "long description of test group 2"
		 (uim '(custom-group-desc 'test-group2)))
   (assert-equal "long description of test group 3"
		 (uim '(custom-group-desc 'test-group3))))

  ("test custom-group-subgroups"
   (assert-equal '(main test-group2 test-group3)
		 (uim '(custom-group-subgroups 'test-group)))
   (assert-equal ()
		 (uim '(custom-group-subgroups 'test-group2)))
   (assert-equal ()
		 (uim '(custom-group-subgroups 'test-group3)))
   (assert-equal '(main)
		 (uim '(custom-group-subgroups 'test-group4)))))

(define-uim-test-case "testcase custom hooks"
  (setup
   (lambda ()
     (uim '(require "custom.scm"))
     (uim '(define test-hook ()))
     (uim '(define test-custom1-trace ()))
     (uim '(define test-custom2-trace ()))
     (uim '(define test-custom3-trace ()))
     (uim '(define-custom 'test-custom1 'test-custom1-ddskk
	     '(global)
	     '(choice
	       (test-custom1-uim "uim" "uim native")
	       (test-custom1-ddskk "ddskk like" "Similar to ddskk")
	       (test-custom1-canna "canna like" "Similar to canna"))
	     "Test custom1"
	     "long description will be here."))
     (uim '(define-custom 'test-custom2 'test-custom2-ddskk
	     '(global)
	     '(choice
	       (test-custom2-uim "uim" "uim native")
	       (test-custom2-ddskk "ddskk like" "Similar to ddskk")
	       (test-custom2-canna "canna like" "Similar to canna"))
	     "Test custom2"
	     "long description will be here."))
     (uim '(define-custom 'test-custom3 'test-custom3-ddskk
	     '(global)
	     '(choice
	       (test-custom3-uim "uim" "uim native")
	       (test-custom3-ddskk "ddskk like" "Similar to ddskk")
	       (test-custom3-canna "canna like" "Similar to canna"))
	     "Test custom3"
	     "long description will be here."))))

  ("test custom-hook-procs (null)"
   (assert-equal ()
		 (uim '(custom-hook-procs 'test-custom1 test-hook)))
   (assert-equal ()
		 (uim '(custom-hook-procs 'test-custom2 test-hook)))
   (assert-equal ()
		 (uim '(custom-hook-procs 'test-custom3 test-hook))))

  ("test custom-add-hook, custom-hook-procs, custom-call-hook-procs"
   (assert-equal ()
		 (uim '(custom-hook-procs 'test-custom1 test-hook)))

   (uim '(custom-add-hook 'test-custom1
			  'test-hook
			  (lambda ()
			    (set! test-custom1-trace
				  (cons 'first
					test-custom1-trace)))))
   (assert-equal 1
		 (uim '(length (custom-hook-procs 'test-custom1 test-hook))))
   (assert-equal 'test-custom1-ddskk
		 (uim '(custom-value 'test-custom1)))
   (uim '(custom-call-hook-procs 'test-custom1 test-hook))
   (assert-equal '(first)
		 (uim 'test-custom1-trace))
   (assert-equal ()
		 (uim '(custom-hook-procs 'test-custom2 test-hook)))
   (assert-equal ()
		 (uim '(custom-hook-procs 'test-custom3 test-hook)))
   
   (uim '(custom-add-hook 'test-custom1
			  'test-hook
			  (lambda ()
			    (set! test-custom1-trace
				  (cons 'second
					test-custom1-trace)))))
   (assert-equal 2
		 (uim '(length (custom-hook-procs 'test-custom1 test-hook))))
   (assert-equal 'test-custom1-ddskk
		 (uim '(custom-value 'test-custom1)))
   (assert-true  (uim-bool '(custom-call-hook-procs 'test-custom1 test-hook)))
   (assert-equal '(first second first)
		 (uim 'test-custom1-trace))
   (assert-equal ()
		 (uim '(custom-hook-procs 'test-custom2 test-hook)))
   (assert-equal ()
		 (uim '(custom-hook-procs 'test-custom3 test-hook)))
  
   (uim '(custom-add-hook 'test-custom1
			  'test-hook
			  (lambda ()
			    (set! test-custom1-trace
				  (cons 'third
					test-custom1-trace)))))
   (assert-equal 3
		 (uim '(length (custom-hook-procs 'test-custom1 test-hook))))
   (assert-equal 'test-custom1-ddskk
		 (uim '(custom-value 'test-custom1)))
   (uim '(custom-call-hook-procs 'test-custom1 test-hook))
   (assert-equal '(first second third first second first)
		 (uim 'test-custom1-trace))
   (assert-equal ()
		 (uim '(custom-hook-procs 'test-custom2 test-hook)))
   (assert-equal ()
		 (uim '(custom-hook-procs 'test-custom3 test-hook)))

   ;; test isolation of hooks
   (assert-equal ()
		 (uim 'test-custom3-trace))
   (uim '(custom-add-hook 'test-custom3
			  'test-hook
			  (lambda ()
			    (set! test-custom3-trace
				  (cons 'fourth
					test-custom3-trace)))))

   (assert-equal 1
		 (uim '(length (custom-hook-procs 'test-custom3 test-hook))))
   (assert-equal 3
		 (uim '(length (custom-hook-procs 'test-custom1 test-hook))))
   (assert-equal 'test-custom1-ddskk
		 (uim '(custom-value 'test-custom1)))
   (assert-equal 'test-custom3-ddskk
		 (uim '(custom-value 'test-custom3)))
   (uim '(custom-call-hook-procs 'test-custom3 test-hook))
   (assert-equal '(fourth)
		 (uim 'test-custom3-trace))
   (assert-equal '(first second third first second first)
		 (uim 'test-custom1-trace))
   (assert-equal ()
		 (uim '(custom-hook-procs 'test-custom2 test-hook))))

  ("test custom-remove-hook"
   ;; null
   (assert-equal ()
		 (uim 'test-hook))
   (assert-equal ()
		 (uim '(custom-hook-procs 'test-custom1 test-hook)))
   (assert-false (uim-bool '(custom-remove-hook 'test-custom1 'test-hook)))
   (assert-equal ()
		 (uim '(custom-hook-procs 'test-custom1 test-hook)))
   ;; null as 'any'
   (assert-equal ()
		 (uim 'test-hook))
   (assert-equal ()
		 (uim '(custom-hook-procs 'test-custom1 test-hook)))
   (assert-false (uim-bool '(custom-remove-hook #f 'test-hook)))
   (assert-equal ()
		 (uim '(custom-hook-procs 'test-custom1 test-hook)))
   ;; 1 proc
   (assert-equal ()
		 (uim 'test-hook))
   (uim '(custom-add-hook 'test-custom1 'test-hook (lambda () 1)))
   (assert-equal '(1)
		 (uim '(map (lambda (f) (f))
			    (custom-hook-procs 'test-custom1 test-hook))))
   (assert-true  (uim-bool '(custom-remove-hook 'test-custom1 'test-hook)))
   (assert-equal ()
		 (uim '(custom-hook-procs 'test-custom1 test-hook)))
   ;; 2 procs
   (assert-equal ()
		 (uim 'test-hook))
   (uim '(custom-add-hook 'test-custom1 'test-hook (lambda () 1)))
   (uim '(custom-add-hook 'test-custom1 'test-hook (lambda () 2)))
   (assert-equal '(2 1)
		 (uim '(map (lambda (f) (f))
			    (custom-hook-procs 'test-custom1 test-hook))))
   (assert-true  (uim-bool '(custom-remove-hook 'test-custom1 'test-hook)))
   (assert-equal ()
		 (uim '(custom-hook-procs 'test-custom1 test-hook)))
   ;; 3 procs
   (assert-equal ()
		 (uim 'test-hook))
   (uim '(custom-add-hook 'test-custom1 'test-hook (lambda () 1)))
   (uim '(custom-add-hook 'test-custom1 'test-hook (lambda () 2)))
   (uim '(custom-add-hook 'test-custom1 'test-hook (lambda () 3)))
   (assert-equal '(3 2 1)
		 (uim '(map (lambda (f) (f))
			    (custom-hook-procs 'test-custom1 test-hook))))
   (assert-true  (uim-bool '(custom-remove-hook 'test-custom1 'test-hook)))
   (assert-equal ()
		 (uim '(custom-hook-procs 'test-custom1 test-hook)))
   ;; 3 procs as 'any'
   (assert-equal ()
		 (uim 'test-hook))
   (uim '(custom-add-hook 'test-custom1 'test-hook (lambda () 1)))
   (uim '(custom-add-hook 'test-custom1 'test-hook (lambda () 2)))
   (uim '(custom-add-hook 'test-custom1 'test-hook (lambda () 3)))
   (assert-equal '(3 2 1)
		 (uim '(map (lambda (f) (f))
			    (custom-hook-procs 'test-custom1 test-hook))))
   (assert-true  (uim-bool '(custom-remove-hook #f 'test-hook)))
   (assert-equal ()
		 (uim '(custom-hook-procs 'test-custom1 test-hook)))
   ;; 3 procs * 3 customs (1)
   (uim '(set! test-hook ()))
   (assert-equal ()
		 (uim 'test-hook))
   (uim '(custom-add-hook 'test-custom1 'test-hook (lambda () 11)))
   (uim '(custom-add-hook 'test-custom1 'test-hook (lambda () 12)))
   (uim '(custom-add-hook 'test-custom1 'test-hook (lambda () 13)))
   (uim '(custom-add-hook 'test-custom2 'test-hook (lambda () 21)))
   (uim '(custom-add-hook 'test-custom2 'test-hook (lambda () 22)))
   (uim '(custom-add-hook 'test-custom2 'test-hook (lambda () 23)))
   (uim '(custom-add-hook 'test-custom3 'test-hook (lambda () 31)))
   (uim '(custom-add-hook 'test-custom3 'test-hook (lambda () 32)))
   (uim '(custom-add-hook 'test-custom3 'test-hook (lambda () 33)))
   (assert-equal '(13 12 11)
		 (uim '(map (lambda (f) (f))
			    (custom-hook-procs 'test-custom1 test-hook))))
   (assert-true  (uim-bool '(custom-remove-hook 'test-custom1 'test-hook)))
   (assert-equal ()
		 (uim '(custom-hook-procs 'test-custom1 test-hook)))
   (assert-equal '(33 32 31 23 22 21)
		 (uim '(map (lambda (pair) ((cdr pair)))
			    test-hook)))
   ;; 3 procs * 3 customs (2)
   (uim '(set! test-hook ()))
   (assert-equal ()
		 (uim 'test-hook))
   (uim '(custom-add-hook 'test-custom1 'test-hook (lambda () 11)))
   (uim '(custom-add-hook 'test-custom1 'test-hook (lambda () 12)))
   (uim '(custom-add-hook 'test-custom1 'test-hook (lambda () 13)))
   (uim '(custom-add-hook 'test-custom2 'test-hook (lambda () 21)))
   (uim '(custom-add-hook 'test-custom2 'test-hook (lambda () 22)))
   (uim '(custom-add-hook 'test-custom2 'test-hook (lambda () 23)))
   (uim '(custom-add-hook 'test-custom3 'test-hook (lambda () 31)))
   (uim '(custom-add-hook 'test-custom3 'test-hook (lambda () 32)))
   (uim '(custom-add-hook 'test-custom3 'test-hook (lambda () 33)))
   (assert-equal '(23 22 21)
		 (uim '(map (lambda (f) (f))
			    (custom-hook-procs 'test-custom2 test-hook))))
   (assert-true  (uim-bool '(custom-remove-hook 'test-custom2 'test-hook)))
   (assert-equal ()
		 (uim '(custom-hook-procs 'test-custom2 test-hook)))
   (assert-equal '(33 32 31 13 12 11)
		 (uim '(map (lambda (pair) ((cdr pair)))
			    test-hook)))
   ;; 3 procs * 3 customs (3)
   (uim '(set! test-hook ()))
   (assert-equal ()
		 (uim 'test-hook))
   (uim '(custom-add-hook 'test-custom1 'test-hook (lambda () 11)))
   (uim '(custom-add-hook 'test-custom1 'test-hook (lambda () 12)))
   (uim '(custom-add-hook 'test-custom1 'test-hook (lambda () 13)))
   (uim '(custom-add-hook 'test-custom2 'test-hook (lambda () 21)))
   (uim '(custom-add-hook 'test-custom2 'test-hook (lambda () 22)))
   (uim '(custom-add-hook 'test-custom2 'test-hook (lambda () 23)))
   (uim '(custom-add-hook 'test-custom3 'test-hook (lambda () 31)))
   (uim '(custom-add-hook 'test-custom3 'test-hook (lambda () 32)))
   (uim '(custom-add-hook 'test-custom3 'test-hook (lambda () 33)))
   (assert-equal '(33 32 31)
		 (uim '(map (lambda (f) (f))
			    (custom-hook-procs 'test-custom3 test-hook))))
   (assert-true  (uim-bool '(custom-remove-hook 'test-custom3 'test-hook)))
   (assert-equal ()
		 (uim '(custom-hook-procs 'test-custom3 test-hook)))
   (assert-equal '(23 22 21 13 12 11)
		 (uim '(map (lambda (pair) ((cdr pair)))
			    test-hook)))
   ;; 3 procs * 3 customs (mixed)
   (uim '(set! test-hook ()))
   (assert-equal ()
		 (uim 'test-hook))
   (uim '(custom-add-hook 'test-custom1 'test-hook (lambda () 11)))
   (uim '(custom-add-hook 'test-custom2 'test-hook (lambda () 21)))
   (uim '(custom-add-hook 'test-custom3 'test-hook (lambda () 31)))
   (uim '(custom-add-hook 'test-custom1 'test-hook (lambda () 12)))
   (uim '(custom-add-hook 'test-custom2 'test-hook (lambda () 22)))
   (uim '(custom-add-hook 'test-custom3 'test-hook (lambda () 32)))
   (uim '(custom-add-hook 'test-custom1 'test-hook (lambda () 13)))
   (uim '(custom-add-hook 'test-custom2 'test-hook (lambda () 23)))
   (uim '(custom-add-hook 'test-custom3 'test-hook (lambda () 33)))
   (assert-equal '(13 12 11)
		 (uim '(map (lambda (f) (f))
			    (custom-hook-procs 'test-custom1 test-hook))))
   (assert-true  (uim-bool '(custom-remove-hook 'test-custom1 'test-hook)))
   (assert-equal ()
		 (uim '(custom-hook-procs 'test-custom1 test-hook)))
   (assert-equal '(33 23 32 22 31 21)
		 (uim '(map (lambda (pair) ((cdr pair)))
			    test-hook)))
   ;; 3 procs * 3 customs (mixed) as 'any'
   (uim '(set! test-hook ()))
   (assert-equal ()
		 (uim 'test-hook))
   (uim '(custom-add-hook 'test-custom1 'test-hook (lambda () 11)))
   (uim '(custom-add-hook 'test-custom2 'test-hook (lambda () 21)))
   (uim '(custom-add-hook 'test-custom3 'test-hook (lambda () 31)))
   (uim '(custom-add-hook 'test-custom1 'test-hook (lambda () 12)))
   (uim '(custom-add-hook 'test-custom2 'test-hook (lambda () 22)))
   (uim '(custom-add-hook 'test-custom3 'test-hook (lambda () 32)))
   (uim '(custom-add-hook 'test-custom1 'test-hook (lambda () 13)))
   (uim '(custom-add-hook 'test-custom2 'test-hook (lambda () 23)))
   (uim '(custom-add-hook 'test-custom3 'test-hook (lambda () 33)))
   (assert-equal '(33 23 13 32 22 12 31 21 11)
		 (uim '(map (lambda (pair) ((cdr pair)))
			    test-hook)))
   (assert-true  (uim-bool '(custom-remove-hook #f 'test-hook)))
   (assert-equal ()
		 (uim 'test-hook))
   ;; 3 procs * 3 customs (mixed) as non-existent custom
   (uim '(set! test-hook ()))
   (assert-equal ()
		 (uim 'test-hook))
   (uim '(custom-add-hook 'test-custom1 'test-hook (lambda () 11)))
   (uim '(custom-add-hook 'test-custom2 'test-hook (lambda () 21)))
   (uim '(custom-add-hook 'test-custom3 'test-hook (lambda () 31)))
   (uim '(custom-add-hook 'test-custom1 'test-hook (lambda () 12)))
   (uim '(custom-add-hook 'test-custom2 'test-hook (lambda () 22)))
   (uim '(custom-add-hook 'test-custom3 'test-hook (lambda () 32)))
   (uim '(custom-add-hook 'test-custom1 'test-hook (lambda () 13)))
   (uim '(custom-add-hook 'test-custom2 'test-hook (lambda () 23)))
   (uim '(custom-add-hook 'test-custom3 'test-hook (lambda () 33)))
   (assert-equal ()
		 (uim '(map (lambda (f) (f))
			    (custom-hook-procs 'test-custom4 test-hook))))
   (assert-false (uim-bool '(custom-remove-hook 'test-custom4 'test-hook)))
   (assert-equal ()
		 (uim '(custom-hook-procs 'test-custom4 test-hook)))
   (assert-equal '(33 23 13 32 22 12 31 21 11)
		 (uim '(map (lambda (pair) ((cdr pair)))
			    test-hook))))

  ("test custom-active?"
   (uim '(custom-add-hook 'test-custom1
			  'custom-activity-hooks
			  (lambda ()
			    (symbol-bound? 'car))))
   (assert-true  (uim-bool '(custom-active? 'test-custom1)))

   (uim '(custom-add-hook 'test-custom1
			  'custom-activity-hooks
			  (lambda ()
			    (symbol-bound? 'cdr))))
   (assert-true  (uim-bool '(custom-active? 'test-custom1)))
  
   (uim '(begin
	   (custom-add-hook 'test-custom1
			    'custom-activity-hooks
			    (lambda ()
			      (symbol-bound? 'test-nonexistent)))))
   (assert-false (uim-bool '(custom-active? 'test-custom1)))

   (uim '(custom-add-hook 'test-custom1
			  'custom-activity-hooks
			  (lambda ()
			    (symbol-bound? 'cons))))
   (assert-false (uim-bool '(custom-active? 'test-custom1))))

  ("test custom-update-hooks"
   (uim '(custom-add-hook 'test-custom1
			  'custom-activity-hooks
			  (lambda ()
			    (eq? test-custom3 'test-custom3-uim))))
   (uim '(custom-add-hook 'test-custom2
			  'custom-activity-hooks
			  (lambda ()
			    (eq? test-custom3 'test-custom3-uim))))
   (uim '(custom-add-hook 'test-custom1
			  'custom-update-hooks
			  (lambda ()
			    (set! test-custom1-trace
				  (cons 'updated test-custom1-trace)))))
   (uim '(custom-add-hook 'test-custom2
			  'custom-update-hooks
			  (lambda ()
			    (set! test-custom2-trace
				  (cons 'updated test-custom2-trace)))))
   (assert-equal ()
		 (uim 'test-custom1-trace))
   (assert-equal ()
		 (uim 'test-custom2-trace))
   (assert-equal 'test-custom3-ddskk
		 (uim 'test-custom3))
   (assert-false (uim-bool '(custom-active? 'test-custom1)))
   (assert-false (uim-bool '(custom-active? 'test-custom2)))
   ;; update hook
   (assert-true  (uim-bool '(custom-set-value! 'test-custom3 'test-custom3-uim)))
   (assert-equal '(updated)
		 (uim 'test-custom1-trace))
   (assert-equal '(updated)
		 (uim 'test-custom2-trace))
   (assert-true  (uim-bool '(custom-active? 'test-custom1)))
   (assert-true  (uim-bool '(custom-active? 'test-custom2))))
  ("test custom-update-hooks (self update)"
   (uim '(custom-add-hook 'test-custom1
			  'custom-activity-hooks
			  (lambda ()
			    (eq? test-custom1 'test-custom1-uim))))
   (uim '(custom-add-hook 'test-custom1
			  'custom-update-hooks
			  (lambda ()
			    (set! test-custom1-trace
				  (cons 'updated test-custom1-trace)))))
   (assert-equal ()
		 (uim 'test-custom1-trace))
   (assert-equal 'test-custom1-ddskk
		 (uim 'test-custom1))
   (assert-false (uim-bool '(custom-active? 'test-custom1)))
   ;; update hook
   (assert-true  (uim-bool '(custom-set-value! 'test-custom1 'test-custom1-uim)))
   (assert-equal '(updated)
		 (uim 'test-custom1-trace))
   (assert-true  (uim-bool '(custom-active? 'test-custom1))))

  ("test custom-register-update-cb"
   (uim '(define test-update-gate (lambda (func ptr custom-sym)
				    (set! test-custom1-trace
					  (list func ptr custom-sym)))))
   (uim '(custom-register-update-cb 'test-custom1
				    'custom1-ptr
				    test-update-gate 'custom1-func))
   (assert-equal ()
		 (uim 'test-custom1-trace))
   (assert-equal 'test-custom1-ddskk
		 (uim 'test-custom1))
   ;; update hook
   (assert-true  (uim-bool '(custom-set-value! 'test-custom1 'test-custom1-uim)))
   (assert-equal '(custom1-func custom1-ptr test-custom1)
		 (uim 'test-custom1-trace)))
("test custom-register-update-cb (2 callbaks)"
   (uim '(define test-update-gate (lambda (func ptr custom-sym)
				    (set! test-custom1-trace
					  (cons (list func ptr custom-sym)
						test-custom1-trace)))))
   (uim '(custom-register-update-cb 'test-custom1
				    'custom1-ptr
				    test-update-gate 'custom1-func))
   (uim '(custom-register-update-cb 'test-custom1
				    'custom1-ptr2
				    test-update-gate 'custom1-func2))
   (assert-equal ()
		 (uim 'test-custom1-trace))
   (assert-equal 'test-custom1-ddskk
		 (uim 'test-custom1))
   ;; update hook
   (assert-true  (uim-bool '(custom-set-value! 'test-custom1 'test-custom1-uim)))
   (assert-equal '((custom1-func custom1-ptr test-custom1)
		   (custom1-func2 custom1-ptr2 test-custom1))
		 (uim 'test-custom1-trace))))

(define-uim-test-case "testcase custom get and set hooks"
  (setup
   (lambda ()
     (uim '(require "custom.scm"))
     (uim '(define test-custom1-trace ()))
     (uim '(define test-custom3-trace ()))
     (uim '(define-custom 'test-custom1 'test-custom1-ddskk
	     '(global)
	     '(choice
	       (test-custom1-uim "uim" "uim native")
	       (test-custom1-ddskk "ddskk like" "Similar to ddskk")
	       (test-custom1-canna "canna like" "Similar to canna"))
	     "Test custom1"
	     "long description will be here."))))

  ("test custom-get-hooks"
   (assert-equal ()
		 (uim 'test-custom1-trace))
   (assert-equal 'test-custom1-ddskk
		 (uim '(custom-value 'test-custom1)))
   (assert-equal ()
		 (uim 'test-custom1-trace))
   (uim '(custom-add-hook 'test-custom1
			  'custom-get-hooks
			  (lambda ()
			    (set! test-custom1-trace
				  (cons 'first
					test-custom1-trace)))))
   (assert-equal ()
		 (uim 'test-custom1-trace))
   (assert-equal 'test-custom1-ddskk
		 (uim '(custom-value 'test-custom1)))
   (assert-equal '(first)
		 (uim 'test-custom1-trace)))
  ("test custom-get-hooks (self update)"
   (assert-equal ()
		 (uim 'test-custom1-trace))
   (assert-equal 'test-custom1-ddskk
		 (uim '(custom-value 'test-custom1)))
   (assert-equal ()
		 (uim 'test-custom1-trace))
   (uim '(custom-add-hook 'test-custom1
			  'custom-get-hooks
			  (lambda ()
			    (set! test-custom1
				  'test-custom1-uim)
			    (set! test-custom1-trace
				  (cons 'first
					test-custom1-trace)))))
   (assert-equal ()
		 (uim 'test-custom1-trace))
   (assert-equal 'test-custom1-uim
		 (uim '(custom-value 'test-custom1)))
   (assert-equal '(first)
		 (uim 'test-custom1-trace)))

  ("test custom-set-hooks"
   (assert-equal 'test-custom1-ddskk
		 (uim '(custom-value 'test-custom1)))
   (assert-equal '()
		 (uim 'test-custom1-trace))
   (assert-true  (uim-bool '(custom-set-value! 'test-custom1 'test-custom1-uim)))
   (assert-equal 'test-custom1-uim
		 (uim '(custom-value 'test-custom1)))
   (assert-equal '()
		 (uim 'test-custom1-trace))
   (uim '(custom-add-hook 'test-custom1
			  'custom-set-hooks
			  (lambda ()
			    (set! test-custom1-trace
				  (cons 'second
					test-custom1-trace)))))
   (assert-equal '()
		 (uim 'test-custom1-trace))
   (assert-true  (uim-bool '(custom-set-value! 'test-custom1 'test-custom1-canna)))
   (assert-equal '(second)
		 (uim 'test-custom1-trace))
   (assert-equal 'test-custom1-canna
		 (uim '(custom-value 'test-custom1)))
   (assert-equal '(second)
		 (uim 'test-custom1-trace)))
  ("test custom-set-hooks (self update)"
   (assert-equal 'test-custom1-ddskk
		 (uim '(custom-value 'test-custom1)))
   (assert-equal '()
		 (uim 'test-custom1-trace))
   (assert-true  (uim-bool '(custom-set-value! 'test-custom1 'test-custom1-uim)))
   (assert-equal 'test-custom1-uim
		 (uim '(custom-value 'test-custom1)))
   (assert-equal '()
		 (uim 'test-custom1-trace))
   (uim '(custom-add-hook 'test-custom1
			  'custom-set-hooks
			  (lambda ()
			    (set! test-custom1
				  'test-custom1-canna)
			    (set! test-custom1-trace
				  (cons 'second
					test-custom1-trace)))))
   (assert-equal '()
		 (uim 'test-custom1-trace))
   (assert-true  (uim-bool '(custom-set-value! 'test-custom1 'test-custom1-ddskk)))
   (assert-equal '(second)
		 (uim 'test-custom1-trace))
   (assert-equal 'test-custom1-canna
		 (uim '(custom-value 'test-custom1)))
   (assert-equal '(second)
		 (uim 'test-custom1-trace))))

(define-uim-test-case "testcase custom define-custom"
  (setup
   (lambda ()
     (uim '(require "custom.scm"))))

  ("test define-custom (symbol)"
   (assert-false (uim-bool '(symbol-bound? 'test-style)))

   (uim '(define-custom 'test-style 'test-style-ddskk
	   '(global)
	   '(choice
	     (test-style-uim "uim" "uim native")
	     (test-style-ddskk "ddskk like" "Similar to ddskk")
	     (test-style-canna "canna like" "Similar to canna"))
	   "Test style"
	   "long description will be here."))

   (assert-true (uim-bool '(symbol-bound? 'test-style)))))

(define-uim-test-case "testcase custom methods"
  (setup
   (lambda ()
     (uim '(require "custom.scm"))
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
	     "long description will be here."))
     (uim '(define-custom 'test-modelist 'hiragana
	     '(test)
	     '(choice
	       (hiragana "hiragana" "hiragana")
	       (katakana "katakana" "katakana")
	       (latin "latin" "latin")
	       (wide-latin "wide-latin" "wide-latin"))
	     "Mode list"
	     "long description will be here."))))

  ("test custom-valid?"
   ;; choice
   (assert-true  (uim-bool '(custom-valid? 'test-style 'test-style-uim)))
   (assert-true  (uim-bool '(custom-valid? 'test-style 'test-style-ddskk)))
   (assert-true  (uim-bool '(custom-valid? 'test-style 'test-style-canna)))
   (assert-false (uim-bool '(custom-valid? 'test-style 'test-style-invalid)))
   ;; ordered-list
   (assert-false (uim-bool '(custom-valid? 'test-available-ims 'anthy)))
   (assert-false (uim-bool '(custom-valid? 'test-available-ims 'canna)))
   (assert-false (uim-bool '(custom-valid? 'test-available-ims 'skk)))
   (assert-false (uim-bool '(custom-valid? 'test-available-ims 'nonexistent)))
   (assert-true  (uim-bool '(custom-valid? 'test-available-ims ())))
   (assert-true  (uim-bool '(custom-valid? 'test-available-ims '(anthy))))
   (assert-true  (uim-bool '(custom-valid? 'test-available-ims '(canna))))
   (assert-true  (uim-bool '(custom-valid? 'test-available-ims '(skk))))
   (assert-true  (uim-bool '(custom-valid? 'test-available-ims '(anthy skk))))
   (assert-true  (uim-bool '(custom-valid? 'test-available-ims '(skk anthy))))
   (assert-true  (uim-bool '(custom-valid? 'test-available-ims
					   '(skk anthy canna))))
   (assert-false (uim-bool '(custom-valid? 'test-available-ims
					   '(nonexistent))))
   (assert-false (uim-bool '(custom-valid? 'test-available-ims
					   '(anthy nonexistent))))
   ;; integer
   (assert-false (uim-bool '(custom-valid? 'test-nr-candidate-max 0)))
   (assert-true  (uim-bool '(custom-valid? 'test-nr-candidate-max 1)))
   (assert-true  (uim-bool '(custom-valid? 'test-nr-candidate-max 10)))
   (assert-true  (uim-bool '(custom-valid? 'test-nr-candidate-max 20)))
   (assert-false (uim-bool '(custom-valid? 'test-nr-candidate-max 21))))

  ("test custom-value"
   (assert-equal 'test-style-ddskk
		 (uim '(custom-value 'test-style)))
   (assert-equal '(anthy canna skk)
		 (uim '(custom-value 'test-available-ims)))
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
   ;; invalid value is ignored
   (assert-false (uim-bool '(custom-set-value! 'test-style 'test-style-invalid)))
   (assert-equal 'test-style-uim
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
   ;; invalid value is ignored
   (assert-false (uim-bool '(custom-set-value! 'test-available-ims
					       '(nonexistent))))
   (assert-equal '(skk anthy canna)
		 (uim '(custom-value 'test-available-ims)))

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
   ;; invalid value is ignored
   (assert-false (uim-bool '(custom-set-value! 'test-nr-candidate-max 25)))
   (assert-equal 5
		 (uim '(custom-value 'test-nr-candidate-max)))

   ;;; string
   ;; default value
   (assert-equal "a string"
		 (uim '(custom-value 'test-string)))
   ;; valid value
   (assert-true  (uim-bool '(custom-set-value! 'test-string "a altered string")))
   (assert-equal "a altered string"
		 (uim '(custom-value 'test-string)))
   ;; invalid value is ignored
   (assert-false (uim-bool '(custom-set-value! 'test-string #f)))
   (assert-equal "a altered string"
		 (uim '(custom-value 'test-string)))

   ;;; pathname
   ;; default value
   (assert-equal "/usr/share/skk/SKK-JISYO.L"
		 (uim '(custom-value 'test-dic-file-name)))
   ;; valid value
   (assert-true  (uim-bool '(custom-set-value! 'test-dic-file-name
					 "/usr/local/share/skk/SKK-JISYO.ML")))
   (assert-equal "/usr/local/share/skk/SKK-JISYO.ML"
		 (uim '(custom-value 'test-dic-file-name)))
   ;; invalid value is ignored
   (assert-false (uim-bool '(custom-set-value! 'test-dic-file-name #f)))
   (assert-equal "/usr/local/share/skk/SKK-JISYO.ML"
		 (uim '(custom-value 'test-dic-file-name)))

   ;;; choice (2)
   ;; default value
   (assert-equal 'hiragana
		 (uim '(custom-value 'test-modelist)))
   ;; valid value
   (assert-true  (uim-bool '(custom-set-value! 'test-modelist 'latin)))
   (assert-equal 'latin
		 (uim '(custom-value 'test-modelist)))
   ;; invalid value is ignored
   (assert-false (uim-bool '(custom-set-value! 'test-modelist 'kanji)))
   (assert-equal 'latin
		 (uim '(custom-value 'test-modelist))))

  ("test custom-default?"
   ;;; choice
   ;; default value
   (assert-equal 'test-style-ddskk
		 (uim '(custom-value 'test-style)))
   (assert-true  (uim-bool '(custom-default? 'test-style)))
   ;; valid, but non-default value
   (assert-true  (uim-bool '(custom-set-value! 'test-style 'test-style-uim)))
   (assert-false (uim-bool '(custom-default? 'test-style)))
   ;; come back to default
   (assert-true  (uim-bool '(custom-set-value! 'test-style 'test-style-ddskk)))
   (assert-true  (uim-bool '(custom-default? 'test-style)))

   ;;; ordered-list
   ;; default value
   (assert-equal '(anthy canna skk)
		 (uim '(custom-value 'test-available-ims)))
   (assert-true  (uim-bool '(custom-default? 'test-available-ims)))
   ;; valid, but non-default value
   (assert-true  (uim-bool '(custom-set-value! 'test-available-ims '(anthy))))
   (assert-false (uim-bool '(custom-default? 'test-available-ims)))
   ;; come back to default
   (assert-true  (uim-bool '(custom-set-value! 'test-available-ims
					       '(anthy canna skk))))
   (assert-true  (uim-bool '(custom-default? 'test-available-ims)))

   ;;; boolean
   ;; default value
   (assert-true  (uim-bool '(custom-value 'test-use-candidate-window?)))
   (assert-true  (uim-bool '(custom-default? 'test-use-candidate-window?)))
   ;; valid, but non-default value
   (assert-true  (uim-bool '(custom-set-value! 'test-use-candidate-window? #f)))
   (assert-false (uim-bool '(custom-default? 'test-use-candidate-window?)))
   ;; come back to default
   (assert-true  (uim-bool '(custom-set-value! 'test-use-candidate-window? #t)))
   (assert-true  (uim-bool '(custom-default? 'test-use-candidate-window?)))

   ;;; integer
   ;; default value
   (assert-equal 10
		 (uim '(custom-value 'test-nr-candidate-max)))
   (assert-true  (uim-bool '(custom-default? 'test-nr-candidate-max)))
   ;; valid, but non-default value
   (assert-true  (uim-bool '(custom-set-value! 'test-nr-candidate-max 5)))
   (assert-false (uim-bool '(custom-default? 'test-nr-candidate-max)))
   ;; come back to default
   (assert-true  (uim-bool '(custom-set-value! 'test-nr-candidate-max 10)))
   (assert-true  (uim-bool '(custom-default? 'test-nr-candidate-max)))

   ;;; string
   ;; default value
   (assert-equal "a string"
		 (uim '(custom-value 'test-string)))
   (assert-true  (uim-bool '(custom-default? 'test-string)))
   ;; valid, but non-default value
   (assert-true  (uim-bool '(custom-set-value! 'test-string "a altered string")))
   (assert-false (uim-bool '(custom-default? 'test-string)))
   ;; come back to default
   (assert-true  (uim-bool '(custom-set-value! 'test-string "a string")))
   (assert-true  (uim-bool '(custom-default? 'test-string)))

   ;;; pathname
   ;; default value
   (assert-equal "/usr/share/skk/SKK-JISYO.L"
		 (uim '(custom-value 'test-dic-file-name)))
   (assert-true  (uim-bool '(custom-default? 'test-dic-file-name)))
   ;; valid, but non-default value
   (assert-true  (uim-bool '(custom-set-value! 'test-dic-file-name
					 "/usr/local/share/skk/SKK-JISYO.ML")))
   (assert-false (uim-bool '(custom-default? 'test-dic-file-name)))
   ;; come back to default
   (assert-true  (uim-bool '(custom-set-value! 'test-dic-file-name
					 "/usr/share/skk/SKK-JISYO.L")))
   (assert-true  (uim-bool '(custom-default? 'test-dic-file-name))))

  ("test custom-default-value"
   ;;; choice
   ;; default value
   (assert-equal 'test-style-ddskk
		 (uim '(custom-value 'test-style)))
   (assert-equal 'test-style-ddskk
		 (uim '(custom-default-value 'test-style)))
   ;; default value is not affected by current value
   (assert-true  (uim-bool '(custom-set-value! 'test-style 'test-style-uim)))
   (assert-equal 'test-style-uim
		 (uim '(custom-value 'test-style)))
   (assert-equal 'test-style-ddskk
		 (uim '(custom-default-value 'test-style)))

   ;;; ordered-list
   ;; default value
   (assert-equal '(anthy canna skk)
		 (uim '(custom-value 'test-available-ims)))
   (assert-equal '(anthy canna skk)
		 (uim '(custom-default-value 'test-available-ims)))
   ;; default value is not affected by current value
   (assert-true  (uim-bool '(custom-set-value! 'test-available-ims '(anthy))))
   (assert-equal '(anthy)
		 (uim '(custom-value 'test-available-ims)))
   (assert-equal '(anthy canna skk)
		 (uim '(custom-default-value 'test-available-ims)))

   ;;; boolean
   ;; default value
   (assert-true  (uim-bool '(custom-value 'test-use-candidate-window?)))
   (assert-true  (uim-bool '(custom-default-value 'test-use-candidate-window?)))
   ;; default value is not affected by current value
   (assert-true  (uim-bool '(custom-set-value! 'test-use-candidate-window? #f)))
   (assert-false (uim-bool '(custom-value 'test-use-candidate-window?)))
   (assert-true  (uim-bool '(custom-default-value 'test-use-candidate-window?)))

   ;;; integer
   ;; default value
   (assert-equal 10
		 (uim '(custom-value 'test-nr-candidate-max)))
   (assert-equal 10
		 (uim '(custom-default-value 'test-nr-candidate-max)))
   ;; default value is not affected by current value
   (assert-true  (uim-bool '(custom-set-value! 'test-nr-candidate-max 5)))
   (assert-equal 5
		 (uim '(custom-value 'test-nr-candidate-max)))
   (assert-equal 10
		 (uim '(custom-default-value 'test-nr-candidate-max)))

   ;;; string
   ;; default value
   (assert-equal "a string"
		 (uim '(custom-value 'test-string)))
   (assert-equal "a string"
		 (uim '(custom-default-value 'test-string)))
   ;; default value is not affected by current value
   (assert-true  (uim-bool '(custom-set-value! 'test-string "a altered string")))
   (assert-equal "a altered string"
		 (uim '(custom-value 'test-string)))
   (assert-equal "a string"
		 (uim '(custom-default-value 'test-string)))

   ;;; pathname
   ;; default value
   (assert-equal "/usr/share/skk/SKK-JISYO.L"
		 (uim '(custom-value 'test-dic-file-name)))
   (assert-equal "/usr/share/skk/SKK-JISYO.L"
		 (uim '(custom-default-value 'test-dic-file-name)))
   ;; default value is not affected by current value
   (assert-true  (uim-bool '(custom-set-value! 'test-dic-file-name
					 "/usr/local/share/skk/SKK-JISYO.ML")))
   (assert-equal "/usr/local/share/skk/SKK-JISYO.ML"
		 (uim '(custom-value 'test-dic-file-name)))
   (assert-equal "/usr/share/skk/SKK-JISYO.L"
		 (uim '(custom-default-value 'test-dic-file-name))))

  ("test custom-groups"
   (assert-equal '(global)
		 (uim '(custom-groups 'test-style)))
   (assert-equal '(global)
		 (uim '(custom-groups 'test-available-ims)))
   (assert-equal '(test ui)
		 (uim '(custom-groups 'test-use-candidate-window?)))
   (assert-equal '(test advanced ui)
		 (uim '(custom-groups 'test-nr-candidate-max)))
   (assert-equal '(test)
		 (uim '(custom-groups 'test-string)))
   (assert-equal '(test)
		 (uim '(custom-groups 'test-dic-file-name))))

  ("test custom-type"
   (assert-equal 'choice
		 (uim '(custom-type 'test-style)))
   (assert-equal 'ordered-list
		 (uim '(custom-type 'test-available-ims)))
   (assert-equal 'boolean
		 (uim '(custom-type 'test-use-candidate-window?)))
   (assert-equal 'integer
		 (uim '(custom-type 'test-nr-candidate-max)))
   (assert-equal 'string
		 (uim '(custom-type 'test-string)))
   (assert-equal 'pathname
		 (uim '(custom-type 'test-dic-file-name))))
   
  ("test custom-type-attrs"
   (assert-equal '((test-style-uim "uim" "uim native")
		   (test-style-ddskk "ddskk like" "Similar to ddskk")
		   (test-style-canna "canna like" "Similar to canna"))
		 (uim '(custom-type-attrs 'test-style)))
   (assert-equal '((anthy "Anthy" "Anthy")
		   (canna "Cannd" "Canna")
		   (skk "SKK" "SKK"))
		 (uim '(custom-type-attrs 'test-available-ims)))
   (assert-equal ()
		 (uim '(custom-type-attrs 'test-use-candidate-window?)))
   (assert-equal '(1 20)
		 (uim '(custom-type-attrs 'test-nr-candidate-max)))
   (assert-equal '(".+")
		 (uim '(custom-type-attrs 'test-string)))
   (assert-equal ()
		 (uim '(custom-type-attrs 'test-dic-file-name))))

  ("test custom-range"
   (assert-equal '(test-style-uim test-style-ddskk test-style-canna)
		 (uim '(custom-range 'test-style)))
   (assert-equal '((anthy "Anthy" "Anthy")
		   (canna "Cannd" "Canna")
		   (skk "SKK" "SKK"))
		 (uim '(custom-range 'test-available-ims)))
   (assert-false (uim-bool '(custom-range 'test-use-candidate-window?)))
   (assert-equal '(1 20)
		 (uim '(custom-range 'test-nr-candidate-max)))
   (assert-equal '(".+")
		 (uim '(custom-range 'test-string)))
   (assert-equal ()
		 (uim '(custom-range 'test-dic-file-name))))

  ("test custom-label"
   (assert-equal "Test style"
		 (uim '(custom-label 'test-style)))
   (assert-equal "Test avalilable IMs"
		 (uim '(custom-label 'test-available-ims)))
   (assert-equal "Use candidate window"
		 (uim '(custom-label 'test-use-candidate-window?)))
   (assert-equal "Number of candidates in candidate window at a time"
		 (uim '(custom-label 'test-nr-candidate-max)))
   (assert-equal "A string for testing purpose"
		 (uim '(custom-label 'test-string)))
   (assert-equal "Dictionary file"
		 (uim '(custom-label 'test-dic-file-name))))
   
  ("test custom-desc"
   (assert-equal "long description will be here."
		 (uim '(custom-desc 'test-style)))
   (assert-equal "long description will be here."
		 (uim '(custom-desc 'test-available-ims)))
   (assert-equal "long description will be here."
		 (uim '(custom-desc 'test-use-candidate-window?)))
   (assert-equal "long description will be here."
		 (uim '(custom-desc 'test-nr-candidate-max)))
   (assert-equal "long description will be here."
		 (uim '(custom-desc 'test-style)))
   (assert-equal "long description will be here."
		 (uim '(custom-desc 'test-dic-file-name))))

  ("test custom-value-as-literal"
   (assert-equal "'test-style-ddskk"
		 (uim '(custom-value-as-literal 'test-style)))
   (assert-equal "'(anthy canna skk)"
		 (uim '(custom-value-as-literal 'test-available-ims)))
   (assert-equal "#t"
		 (uim '(custom-value-as-literal 'test-use-candidate-window?)))
   (assert-equal "10"
		 (uim '(custom-value-as-literal 'test-nr-candidate-max)))
   (assert-equal "\"a string\""
		 (uim '(custom-value-as-literal 'test-string)))
   (assert-equal "\"/usr/share/skk/SKK-JISYO.L\""
		 (uim '(custom-value-as-literal 'test-dic-file-name))))

  ("test custom-definition-as-literal"
   (assert-equal "(define test-style 'test-style-ddskk)"
		 (uim '(custom-definition-as-literal 'test-style)))
   (assert-equal "(define test-available-ims '(anthy canna skk))"
		 (uim '(custom-definition-as-literal 'test-available-ims)))
   (assert-equal "(define test-use-candidate-window? #t)"
		 (uim '(custom-definition-as-literal 'test-use-candidate-window?)))
   (assert-equal "(define test-nr-candidate-max 10)"
		 (uim '(custom-definition-as-literal 'test-nr-candidate-max)))
   (assert-equal "(define test-string \"a string\")"
		 (uim '(custom-definition-as-literal 'test-string)))
   (assert-equal "(define test-dic-file-name \"/usr/share/skk/SKK-JISYO.L\")"
		 (uim '(custom-definition-as-literal 'test-dic-file-name)))
   ;; hooked
   (assert-equal "(define test-style 'test-style-ddskk)"
		 (uim '(custom-definition-as-literal 'test-style)))
   (uim '(custom-add-hook 'test-style 'custom-literalize-hooks
			  (lambda () "(define test-style 'hooked)")))
   (assert-equal "(define test-style 'hooked)"
		 (uim '(custom-definition-as-literal 'test-style)))
   (uim '(custom-add-hook 'test-style 'custom-literalize-hooks
			  (lambda () "(define test-style 'hooked2)")))
   (assert-equal "(define test-style 'hooked2)(define test-style 'hooked)"
		 (uim '(custom-definition-as-literal 'test-style)))))

(define-uim-test-case "testcase custom interfaces"
  (setup
   (lambda ()
     (uim '(require "custom.scm"))
     (uim '(define-custom 'test-nr-candidate-max 10
	     '(test advanced ui)
	     '(integer 1 20)
	     "Number of candidates in candidate window at a time"
	     "long description will be here."))))

  ("test custom-prop-update-custom-handler"
   (uim '(define test-context (context-new 1 (find-im 'direct #f))))
   ;; default value
   (assert-equal 10
		 (uim '(custom-value 'test-nr-candidate-max)))
   ;; valid value
   (assert-true  (uim-bool '(custom-prop-update-custom-handler
			     test-context 'test-nr-candidate-max 5)))
   (assert-equal 5
		 (uim '(custom-value 'test-nr-candidate-max)))
   ;; invalid value is ignored
   (assert-false (uim-bool '(custom-prop-update-custom-handler
			     test-context 'test-nr-candidate-max 25)))
   (assert-equal 5
		 (uim '(custom-value 'test-nr-candidate-max)))))


(define-uim-test-case "testcase custom canna-server-name"
  (setup
   (lambda ()
     (uim '(require "custom.scm"))))

  ("test canna-server-name"
   (uim '(set! custom-preserved-canna-server-name ""))
   (uim '(set! custom-activate-canna-server-name? #f))

   (assert-false (uim-bool 'custom-activate-canna-server-name?))
   (assert-equal ""
		 (uim 'custom-preserved-canna-server-name))
   (assert-false (uim-bool 'canna-server-name))
   (assert-false (uim-bool '(custom-active? 'custom-preserved-canna-server-name)))

   (assert-true  (uim-bool '(custom-set-value! 'custom-preserved-canna-server-name
					 "foo")))
   (assert-false (uim-bool 'custom-activate-canna-server-name?))
   (assert-equal "foo"
		 (uim 'custom-preserved-canna-server-name))
   (assert-false (uim-bool 'canna-server-name))
   (assert-false (uim-bool '(custom-active? 'custom-preserved-canna-server-name)))

   (assert-true  (uim-bool '(custom-set-value! 'custom-activate-canna-server-name? #t)))
   (assert-true  (uim-bool 'custom-activate-canna-server-name?))
   (assert-equal "foo"
		 (uim 'custom-preserved-canna-server-name))
   (assert-equal "foo"
		 (uim 'canna-server-name))
   (assert-true  (uim-bool '(custom-active? 'custom-preserved-canna-server-name)))

   (assert-true  (uim-bool '(custom-set-value! 'custom-activate-canna-server-name? #f)))
   (assert-false (uim-bool 'custom-activate-canna-server-name?))
   (assert-equal "foo"
		 (uim 'custom-preserved-canna-server-name))
   (assert-false (uim-bool 'canna-server-name))
   (assert-false (uim-bool '(custom-active? 'custom-preserved-canna-server-name)))))
