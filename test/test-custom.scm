#!/usr/bin/env gosh

;;; Copyright (c) 2003,2004 uim Project http://uim.freedesktop.org/
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
  ("test pathname?"
   (assert-false (uim-bool '(pathname? #f)))
   (assert-false (uim-bool '(pathname? 'foo)))
   (assert-false (uim-bool '(pathname? -1)))
   (assert-false (uim-bool '(pathname? 0)))
   (assert-false (uim-bool '(pathname? 1)))
   (assert-false (uim-bool '(pathname? 10)))
   (assert-false (uim-bool '(pathname? ())))
   (assert-false (uim-bool '(pathname? '(1 "2" 'three))))
   (assert-true  (uim-bool '(pathname? "/usr/share/uim/foo.scm")))
   (assert-true  (uim-bool '(pathname? "~/.uim")))
   (assert-true  (uim-bool '(pathname? "share/uim/bar.scm")))
   (assert-true  (uim-bool '(pathname? "baz.scm"))))
  ("test custom-valid-symbol?"
   (assert-false (uim-bool '(custom-valid-symbol?
			     #f
			     '(uim-color-uim "uim" "uim native")
			     '(uim-color-atok "ATOK like" "Similar to ATOK"))))
   (assert-false (uim-bool '(custom-valid-symbol?
			     "foo"
			     '(uim-color-uim "uim" "uim native")
			     '(uim-color-atok "ATOK like" "Similar to ATOK"))))
   (assert-false (uim-bool '(custom-valid-symbol?
			     -1
			     '(uim-color-uim "uim" "uim native")
			     '(uim-color-atok "ATOK like" "Similar to ATOK"))))
   (assert-false (uim-bool '(custom-valid-symbol?
			     0
			     '(uim-color-uim "uim" "uim native")
			     '(uim-color-atok "ATOK like" "Similar to ATOK"))))
   (assert-false (uim-bool '(custom-valid-symbol?
			     1
			     '(uim-color-uim "uim" "uim native")
			     '(uim-color-atok "ATOK like" "Similar to ATOK"))))
   (assert-false (uim-bool '(custom-valid-symbol?
			     10
			     '(uim-color-uim "uim" "uim native")
			     '(uim-color-atok "ATOK like" "Similar to ATOK"))))
   (assert-false (uim-bool '(custom-valid-symbol?
			     ()
			     '(uim-color-uim "uim" "uim native")
			     '(uim-color-atok "ATOK like" "Similar to ATOK"))))
   (assert-false (uim-bool '(custom-valid-symbol?
			     '(1 "2" 'three)
			     '(uim-color-uim "uim" "uim native")
			     '(uim-color-atok "ATOK like" "Similar to ATOK"))))
   (assert-true  (uim-bool '(custom-valid-symbol?
			     'uim-color-uim
			     '(uim-color-uim "uim" "uim native")
			     '(uim-color-atok "ATOK like" "Similar to ATOK"))))
   (assert-false (uim-bool '(custom-valid-symbol?
			     'uim-color-nonexistent
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

  ("test custom-symbol-rec-new"
   (assert-equal (uim '(list #f "" ""))
		 (uim '(custom-symbol-rec-new))))

  ("test custom-symbol-label"
   (assert-equal "uim"
		 (uim '(custom-symbol-label 'uim-color 'uim-color-uim)))
   (assert-equal "ATOK like"
		 (uim '(custom-symbol-label 'uim-color 'uim-color-atok)))
   (assert-error (lambda ()
                   (uim '(custom-symbol-label 'uim-color
                                              'uim-color-nonexistent))))
   (assert-error (lambda ()
                   (uim '(custom-symbol-label 'uim-nonexistent
                                              'uim-nonexistent)))))
  ("test custom-symbol-desc"
   (assert-equal "uim native"
		 (uim '(custom-symbol-desc 'uim-color 'uim-color-uim)))
   (assert-equal "Similar to ATOK"
		 (uim '(custom-symbol-desc 'uim-color 'uim-color-atok)))
   (assert-error (lambda () (uim '(custom-symbol-desc 'uim-color
                                                      'uim-color-nonexistent))))
   (assert-error (lambda () (uim '(custom-symbol-desc 'uim-nonexistent
                                                      'uim-nonexistent))))))

(define-uim-test-case "testcase custom custom-group"
  (setup
   (lambda ()
     ;;(uim '(load "custom.scm"))
     (uim '(define test-group-recs-length 0))))

  ("test custom-group-rec-new"
   (assert-equal (uim '(list #f "" ""))
		 (uim '(custom-group-rec-new))))

  ("test custom-define-group, custom-group-rec"
   (uim '(set! test-group-recs-length
	       (length custom-group-rec-alist)))

   (assert-false (uim-bool '(custom-group-rec 'test-group)))
   (uim '(custom-define-group
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
   (uim '(custom-define-group
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
   (uim '(custom-define-group
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
   (assert-equal '(advanced anthy canna cannaserver default-im-name global im-switching other-ims prime skk spellcheck)
		 (sort-symbol (uim '(custom-list-groups)))))
  ("test custom-list-primary-groups"
   ;; defined order have to be kept
   (assert-equal '(global anthy canna skk prime other-ims spellcheck)
		 (uim '(custom-list-primary-groups))))
  ("test custom-collect-by-group"
   ;; defined order have to be kept
   (assert-equal '(spellcheck-always-show-window? spellcheck-preedit-immediate-commit? spellcheck-candidate-op-count spellcheck-use-candidate-window? generic-nr-candidate-max generic-candidate-op-count generic-use-candidate-window? prime-mask-pending-preedit? prime-preedit-immediate-commit? prime-always-show-window? prime-nr-candidate-max skk-style skk-commit-newline-explicitly? skk-egg-like-newline? skk-use-recursive-learning? skk-nr-candidate-max skk-candidate-op-count skk-use-candidate-window? skk-uim-personal-dic-filename skk-personal-dic-filename skk-dic-file-name canna-server-name custom-preserved-canna-server-name custom-activate-canna-server-name? canna-segment-separator canna-show-segment-separator? canna-nr-candidate-max canna-candidate-op-count canna-use-candidate-window? anthy-segment-separator anthy-show-segment-separator? anthy-nr-candidate-max anthy-candidate-op-count anthy-use-candidate-window? candidate-window-position switch-im-key? enable-im-switch custom-activate-default-im-name? custom-preserved-default-im-name uim-color)
		 (uim '(custom-collect-by-group #f)))  ;; any group
   (assert-equal '(candidate-window-position switch-im-key? enable-im-switch custom-activate-default-im-name? custom-preserved-default-im-name uim-color)
		 (uim '(custom-collect-by-group 'global)))
   (assert-equal '(anthy-segment-separator anthy-show-segment-separator? anthy-nr-candidate-max anthy-candidate-op-count anthy-use-candidate-window?)
		 (uim '(custom-collect-by-group 'anthy)))
   (assert-equal '(canna-server-name custom-preserved-canna-server-name custom-activate-canna-server-name? canna-segment-separator canna-show-segment-separator? canna-nr-candidate-max canna-candidate-op-count canna-use-candidate-window?)
		 (uim '(custom-collect-by-group 'canna)))
   (assert-equal '(skk-style skk-commit-newline-explicitly? skk-egg-like-newline? skk-use-recursive-learning? skk-nr-candidate-max skk-candidate-op-count skk-use-candidate-window? skk-uim-personal-dic-filename skk-personal-dic-filename skk-dic-file-name)
		 (uim '(custom-collect-by-group 'skk)))
   (assert-equal '(prime-mask-pending-preedit? prime-preedit-immediate-commit? prime-always-show-window? prime-nr-candidate-max)
		 (uim '(custom-collect-by-group 'prime)))
   (assert-equal '(generic-nr-candidate-max generic-candidate-op-count generic-use-candidate-window?)
		 (uim '(custom-collect-by-group 'other-ims)))
   (assert-equal '(spellcheck-always-show-window? spellcheck-preedit-immediate-commit? spellcheck-candidate-op-count spellcheck-use-candidate-window?)
		 (uim '(custom-collect-by-group 'spellcheck)))))

(define-uim-test-case "testcase custom custom-group methods"
  (setup
   (lambda ()
     ;;(uim '(load "custom.scm"))
     (uim '(custom-define-group
	    'test-group
	    "test group"
	    "long description of test group"))
     (uim '(custom-define-group
	    'test-group2
	    "test group 2"
	    "long description of test group 2"))
     (uim '(custom-define-group
	    'test-group3
	    "test group 3"
	    "long description of test group 3"))
     (uim '(custom-define-group
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
     (uim '(define test-custom3-trace ()))
     (uim '(define-custom 'test-custom1 'test-custom1-ddskk
	     '(global)
	     '(symbol
	       (test-custom1-uim "uim" "uim native")
	       (test-custom1-ddskk "ddskk like" "Similar to ddskk")
	       (test-custom1-canna "canna like" "Similar to canna"))
	     "Test custom1"
	     "long description will be here."))
     (uim '(define-custom 'test-custom2 'test-custom2-ddskk
	     '(global)
	     '(symbol
	       (test-custom2-uim "uim" "uim native")
	       (test-custom2-ddskk "ddskk like" "Similar to ddskk")
	       (test-custom2-canna "canna like" "Similar to canna"))
	     "Test custom2"
	     "long description will be here."))
     (uim '(define-custom 'test-custom3 'test-custom3-ddskk
	     '(global)
	     '(symbol
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

  ("test custom-active?"
   (uim '(custom-add-hook 'test-custom1
			  'custom-activity-hook
			  (lambda ()
			    (symbol-bound? 'car))))
   (assert-true  (uim-bool '(custom-active? 'test-custom1)))

   (uim '(custom-add-hook 'test-custom1
			  'custom-activity-hook
			  (lambda ()
			    (symbol-bound? 'cdr))))
   (assert-true  (uim-bool '(custom-active? 'test-custom1)))
  
   (uim '(begin
	   (custom-add-hook 'test-custom1
			    'custom-activity-hook
			    (lambda ()
			      (symbol-bound? 'test-nonexistent)))))
   (assert-false (uim-bool '(custom-active? 'test-custom1)))

   (uim '(custom-add-hook 'test-custom1
			  'custom-activity-hook
			  (lambda ()
			    (symbol-bound? 'cons))))
   (assert-false (uim-bool '(custom-active? 'test-custom1)))))

(define-uim-test-case "testcase custom get and set hooks"
  (setup
   (lambda ()
     (uim '(require "custom.scm"))
     (uim '(define test-custom1-trace ()))
     (uim '(define test-custom3-trace ()))
     (uim '(define-custom 'test-custom1 'test-custom1-ddskk
	     '(global)
	     '(symbol
	       (test-custom1-uim "uim" "uim native")
	       (test-custom1-ddskk "ddskk like" "Similar to ddskk")
	       (test-custom1-canna "canna like" "Similar to canna"))
	     "Test custom1"
	     "long description will be here."))))

  ("test custom-get-hook"
   (assert-equal ()
		 (uim 'test-custom1-trace))
   (assert-equal 'test-custom1-ddskk
		 (uim '(custom-value 'test-custom1)))
   (assert-equal ()
		 (uim 'test-custom1-trace))
   (uim '(custom-add-hook 'test-custom1
			  'custom-get-hook
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
  ("test custom-get-hook (self update)"
   (assert-equal ()
		 (uim 'test-custom1-trace))
   (assert-equal 'test-custom1-ddskk
		 (uim '(custom-value 'test-custom1)))
   (assert-equal ()
		 (uim 'test-custom1-trace))
   (uim '(custom-add-hook 'test-custom1
			  'custom-get-hook
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

  ("test custom-set-hook"
   (assert-equal 'test-custom1-ddskk
		 (uim '(custom-value 'test-custom1)))
   (assert-equal '()
		 (uim 'test-custom1-trace))
   (uim '(custom-set! 'test-custom1 'test-custom1-uim))
   (assert-equal 'test-custom1-uim
		 (uim '(custom-value 'test-custom1)))
   (assert-equal '()
		 (uim 'test-custom1-trace))
   (uim '(custom-add-hook 'test-custom1
			  'custom-set-hook
			  (lambda ()
			    (set! test-custom1-trace
				  (cons 'second
					test-custom1-trace)))))
   (assert-equal '()
		 (uim 'test-custom1-trace))
   (uim '(custom-set! 'test-custom1 'test-custom1-canna))
   (assert-equal '(second)
		 (uim 'test-custom1-trace))
   (assert-equal 'test-custom1-canna
		 (uim '(custom-value 'test-custom1)))
   (assert-equal '(second)
		 (uim 'test-custom1-trace)))
  ("test custom-set-hook (self update)"
   (assert-equal 'test-custom1-ddskk
		 (uim '(custom-value 'test-custom1)))
   (assert-equal '()
		 (uim 'test-custom1-trace))
   (uim '(custom-set! 'test-custom1 'test-custom1-uim))
   (assert-equal 'test-custom1-uim
		 (uim '(custom-value 'test-custom1)))
   (assert-equal '()
		 (uim 'test-custom1-trace))
   (uim '(custom-add-hook 'test-custom1
			  'custom-set-hook
			  (lambda ()
			    (set! test-custom1
				  'test-custom1-canna)
			    (set! test-custom1-trace
				  (cons 'second
					test-custom1-trace)))))
   (assert-equal '()
		 (uim 'test-custom1-trace))
   (uim '(custom-set! 'test-custom1 'test-custom1-ddskk))
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
	   '(symbol
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
	     '(symbol
	       (test-style-uim "uim" "uim native")
	       (test-style-ddskk "ddskk like" "Similar to ddskk")
	       (test-style-canna "canna like" "Similar to canna"))
	     "Test style"
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

  ("test custom-valid?"
   (assert-true  (uim-bool '(custom-valid? 'test-style 'test-style-uim)))
   (assert-true  (uim-bool '(custom-valid? 'test-style 'test-style-ddskk)))
   (assert-true  (uim-bool '(custom-valid? 'test-style 'test-style-canna)))
   (assert-false (uim-bool '(custom-valid? 'test-style 'test-style-invalid)))
   (assert-false (uim-bool '(custom-valid? 'test-nr-candidate-max 0)))
   (assert-true  (uim-bool '(custom-valid? 'test-nr-candidate-max 1)))
   (assert-true  (uim-bool '(custom-valid? 'test-nr-candidate-max 10)))
   (assert-true  (uim-bool '(custom-valid? 'test-nr-candidate-max 20)))
   (assert-false (uim-bool '(custom-valid? 'test-nr-candidate-max 21))))

  ("test custom-value"
   (assert-equal 'test-style-ddskk
		 (uim '(custom-value 'test-style)))
   (assert-true  (uim-bool '(custom-value 'test-use-candidate-window?)))
   (assert-equal 10
		 (uim '(custom-value 'test-nr-candidate-max)))
   (assert-equal "a string"
		 (uim '(custom-value 'test-string)))
   (assert-equal "/usr/share/skk/SKK-JISYO.L"
		 (uim '(custom-value 'test-dic-file-name))))

  ("test custom-set!"
   ;; default value
   (assert-equal 'test-style-ddskk
		 (uim '(custom-value 'test-style)))
   ;; valid value
   (uim '(custom-set! 'test-style 'test-style-uim))
   (assert-equal 'test-style-uim
		 (uim '(custom-value 'test-style)))
   ;; invalid value is ignored
   (uim '(custom-set! 'test-style 'test-style-invalid))
   (assert-equal 'test-style-uim
		 (uim '(custom-value 'test-style)))

   ;; default value
   (assert-true  (uim-bool '(custom-value 'test-use-candidate-window?)))
   ;; valid value
   (uim '(custom-set! 'test-use-candidate-window? #f))
   (assert-false (uim-bool '(custom-value 'test-use-candidate-window?)))
   ;; boolean regards all non-#f value as true
   (uim '(custom-set! 'test-use-candidate-window? 10))
   (assert-true (uim-bool '(custom-value 'test-use-candidate-window?)))

   ;; default value
   (assert-equal 10
		 (uim '(custom-value 'test-nr-candidate-max)))
   ;; valid value
   (uim '(custom-set! 'test-nr-candidate-max 5))
   (assert-equal 5
		 (uim '(custom-value 'test-nr-candidate-max)))
   ;; invalid value is ignored
   (uim '(custom-set! 'test-nr-candidate-max 25))
   (assert-equal 5
		 (uim '(custom-value 'test-nr-candidate-max)))

   ;; default value
   (assert-equal "a string"
		 (uim '(custom-value 'test-string)))
   ;; valid value
   (uim '(custom-set! 'test-string "a altered string"))
   (assert-equal "a altered string"
		 (uim '(custom-value 'test-string)))
   ;; invalid value is ignored
   (uim '(custom-set! 'test-string #f))
   (assert-equal "a altered string"
		 (uim '(custom-value 'test-string)))

   ;; default value
   (assert-equal "/usr/share/skk/SKK-JISYO.L"
		 (uim '(custom-value 'test-dic-file-name)))
   ;; valid value
   (uim '(custom-set! 'test-dic-file-name "/usr/local/share/skk/SKK-JISYO.ML"))
   (assert-equal "/usr/local/share/skk/SKK-JISYO.ML"
		 (uim '(custom-value 'test-dic-file-name)))
   ;; invalid value is ignored
   (uim '(custom-set! 'test-dic-file-name #f))
   (assert-equal "/usr/local/share/skk/SKK-JISYO.ML"
		 (uim '(custom-value 'test-dic-file-name))))

  ("test custom-default?"
   ;; default value
   (assert-equal 'test-style-ddskk
		 (uim '(custom-value 'test-style)))
   (assert-true  (uim-bool '(custom-default? 'test-style)))
   ;; valid, but non-default value
   (uim '(custom-set! 'test-style 'test-style-uim))
   (assert-false (uim-bool '(custom-default? 'test-style)))
   ;; come back to default
   (uim '(custom-set! 'test-style 'test-style-ddskk))
   (assert-true  (uim-bool '(custom-default? 'test-style)))

   ;; default value
   (assert-true  (uim-bool '(custom-value 'test-use-candidate-window?)))
   (assert-true  (uim-bool '(custom-default? 'test-use-candidate-window?)))
   ;; valid, but non-default value
   (uim '(custom-set! 'test-use-candidate-window? #f))
   (assert-false (uim-bool '(custom-default? 'test-use-candidate-window?)))
   ;; come back to default
   (uim '(custom-set! 'test-use-candidate-window? #t))
   (assert-true  (uim-bool '(custom-default? 'test-use-candidate-window?)))

   ;; default value
   (assert-equal 10
		 (uim '(custom-value 'test-nr-candidate-max)))
   (assert-true  (uim-bool '(custom-default? 'test-nr-candidate-max)))
   ;; valid, but non-default value
   (uim '(custom-set! 'test-nr-candidate-max 5))
   (assert-false (uim-bool '(custom-default? 'test-nr-candidate-max)))
   ;; come back to default
   (uim '(custom-set! 'test-nr-candidate-max 10))
   (assert-true  (uim-bool '(custom-default? 'test-nr-candidate-max)))

   ;; default value
   (assert-equal "a string"
		 (uim '(custom-value 'test-string)))
   (assert-true  (uim-bool '(custom-default? 'test-string)))
   ;; valid, but non-default value
   (uim '(custom-set! 'test-string "a altered string"))
   (assert-false (uim-bool '(custom-default? 'test-string)))
   ;; come back to default
   (uim '(custom-set! 'test-string "a string"))
   (assert-true  (uim-bool '(custom-default? 'test-string)))

   ;; default value
   (assert-equal "/usr/share/skk/SKK-JISYO.L"
		 (uim '(custom-value 'test-dic-file-name)))
   (assert-true  (uim-bool '(custom-default? 'test-dic-file-name)))
   ;; valid, but non-default value
   (uim '(custom-set! 'test-dic-file-name "/usr/local/share/skk/SKK-JISYO.ML"))
   (assert-false (uim-bool '(custom-default? 'test-dic-file-name)))
   ;; come back to default
   (uim '(custom-set! 'test-dic-file-name "/usr/share/skk/SKK-JISYO.L"))
   (assert-true  (uim-bool '(custom-default? 'test-dic-file-name))))

  ("test custom-default-value"
   ;; default value
   (assert-equal 'test-style-ddskk
		 (uim '(custom-value 'test-style)))
   (assert-equal 'test-style-ddskk
		 (uim '(custom-default-value 'test-style)))
   ;; default value is not affected by current value
   (uim '(custom-set! 'test-style 'test-style-uim))
   (assert-equal 'test-style-uim
		 (uim '(custom-value 'test-style)))
   (assert-equal 'test-style-ddskk
		 (uim '(custom-default-value 'test-style)))
   
   ;; default value
   (assert-true  (uim-bool '(custom-value 'test-use-candidate-window?)))
   (assert-true  (uim-bool '(custom-default-value 'test-use-candidate-window?)))
   ;; default value is not affected by current value
   (uim '(custom-set! 'test-use-candidate-window? #f))
   (assert-false (uim-bool '(custom-value 'test-use-candidate-window?)))
   (assert-true  (uim-bool '(custom-default-value 'test-use-candidate-window?)))

   ;; default value
   (assert-equal 10
		 (uim '(custom-value 'test-nr-candidate-max)))
   (assert-equal 10
		 (uim '(custom-default-value 'test-nr-candidate-max)))
   ;; default value is not affected by current value
   (uim '(custom-set! 'test-nr-candidate-max 5))
   (assert-equal 5
		 (uim '(custom-value 'test-nr-candidate-max)))
   (assert-equal 10
		 (uim '(custom-default-value 'test-nr-candidate-max)))

   ;; default value
   (assert-equal "a string"
		 (uim '(custom-value 'test-string)))
   (assert-equal "a string"
		 (uim '(custom-default-value 'test-string)))
   ;; default value is not affected by current value
   (uim '(custom-set! 'test-string "a altered string"))
   (assert-equal "a altered string"
		 (uim '(custom-value 'test-string)))
   (assert-equal "a string"
		 (uim '(custom-default-value 'test-string)))

   ;; default value
   (assert-equal "/usr/share/skk/SKK-JISYO.L"
		 (uim '(custom-value 'test-dic-file-name)))
   (assert-equal "/usr/share/skk/SKK-JISYO.L"
		 (uim '(custom-default-value 'test-dic-file-name)))
   ;; default value is not affected by current value
   (uim '(custom-set! 'test-dic-file-name "/usr/local/share/skk/SKK-JISYO.ML"))
   (assert-equal "/usr/local/share/skk/SKK-JISYO.ML"
		 (uim '(custom-value 'test-dic-file-name)))
   (assert-equal "/usr/share/skk/SKK-JISYO.L"
		 (uim '(custom-default-value 'test-dic-file-name))))

  ("test custom-groups"
   (assert-equal '(global)
		 (uim '(custom-groups 'test-style)))
   (assert-equal '(test ui)
		 (uim '(custom-groups 'test-use-candidate-window?)))
   (assert-equal '(test advanced ui)
		 (uim '(custom-groups 'test-nr-candidate-max)))
   (assert-equal '(test)
		 (uim '(custom-groups 'test-string)))
   (assert-equal '(test)
		 (uim '(custom-groups 'test-dic-file-name))))

  ("test custom-type"
   (assert-equal 'symbol
		 (uim '(custom-type 'test-style)))
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
		 (uim '(custom-desc 'test-use-candidate-window?)))
   (assert-equal "long description will be here."
		 (uim '(custom-desc 'test-nr-candidate-max)))
   (assert-equal "long description will be here."
		 (uim '(custom-desc 'test-style)))
   (assert-equal "long description will be here."
		 (uim '(custom-desc 'test-dic-file-name))))

  ("test custom-canonical-value-as-string"
   (assert-equal "'test-style-ddskk"
		 (uim '(custom-canonical-value-as-string 'test-style)))
   (assert-equal "#t"
		 (uim '(custom-canonical-value-as-string 'test-use-candidate-window?)))
   (assert-equal "10"
		 (uim '(custom-canonical-value-as-string 'test-nr-candidate-max)))
   (assert-equal "\"a string\""
		 (uim '(custom-canonical-value-as-string 'test-string)))
   (assert-equal "\"/usr/share/skk/SKK-JISYO.L\""
		 (uim '(custom-canonical-value-as-string 'test-dic-file-name))))

  ("test custom-canonical-definition-as-string"
   (assert-equal "(define test-style 'test-style-ddskk)"
		 (uim '(custom-canonical-definition-as-string 'test-style)))
   (assert-equal "(define test-use-candidate-window? #t)"
		 (uim '(custom-canonical-definition-as-string 'test-use-candidate-window?)))
   (assert-equal "(define test-nr-candidate-max 10)"
		 (uim '(custom-canonical-definition-as-string 'test-nr-candidate-max)))
   (assert-equal "(define test-string \"a string\")"
		 (uim '(custom-canonical-definition-as-string 'test-string)))
   (assert-equal "(define test-dic-file-name \"/usr/share/skk/SKK-JISYO.L\")"
		 (uim '(custom-canonical-definition-as-string 'test-dic-file-name)))))

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

   (uim '(custom-set! 'custom-preserved-canna-server-name "foo"))
   (assert-false (uim-bool 'custom-activate-canna-server-name?))
   (assert-equal "foo"
		 (uim 'custom-preserved-canna-server-name))
   (assert-false (uim-bool 'canna-server-name))
   (assert-false (uim-bool '(custom-active? 'custom-preserved-canna-server-name)))

   (uim '(custom-set! 'custom-activate-canna-server-name? #t))
   (assert-true  (uim-bool 'custom-activate-canna-server-name?))
   (assert-equal "foo"
		 (uim 'custom-preserved-canna-server-name))
   (assert-equal "foo"
		 (uim 'canna-server-name))
   (assert-true  (uim-bool '(custom-active? 'custom-preserved-canna-server-name)))

   (uim '(custom-set! 'custom-activate-canna-server-name? #f))
   (assert-false (uim-bool 'custom-activate-canna-server-name?))
   (assert-equal "foo"
		 (uim 'custom-preserved-canna-server-name))
   (assert-false (uim-bool 'canna-server-name))
   (assert-false (uim-bool '(custom-active? 'custom-preserved-canna-server-name)))))
