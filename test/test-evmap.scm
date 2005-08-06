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

;; This file is tested with revision 1142 of new repository

(use test.unit)

(require "test/uim-test-utils")

(define-uim-test-case "testcase event-exp-collector"
  (setup
   (lambda ()
     (uim '(require "evmap.scm"))))

  ("test event-exp-collector-exp"
   ;; null expression
   (assert-equal ()
		 (uim '(event-exp-collector-exp
			(event-exp-collector-new))))
   ;; single key expression
   (assert-equal "a"
		 (uim '(event-exp-collector-exp
			(event-exp-collector-new "a"))))
   (assert-equal 'lkey_a
		 (uim '(event-exp-collector-exp
			(event-exp-collector-new #f 'lkey_a))))
   (assert-equal 'pkey_qwerty_a
		 (uim '(event-exp-collector-exp
			(event-exp-collector-new #f #f 'pkey_qwerty_a))))
   ;; single key expression with single modifier
   (assert-equal (uim '(list "a" mod_Shift))
		 (uim '(event-exp-collector-exp
			(event-exp-collector-new "a" #f #f mod_Shift))))
   (assert-equal (uim '(list 'lkey_a mod_Shift))
		 (uim '(event-exp-collector-exp
			(event-exp-collector-new #f 'lkey_a #f mod_Shift))))
   (assert-equal (uim '(list 'pkey_qwerty_a mod_Shift))
		 (uim '(event-exp-collector-exp
			(event-exp-collector-new #f #f 'pkey_qwerty_a mod_Shift))))
   ;; single key expression with multiple modifiers
   (assert-equal (uim '(list "a" (bitwise-or mod_Shift
					     mod_Control_L
					     mod_Alt_R)))
		 (uim '(event-exp-collector-exp
			(event-exp-collector-new "a" #f #f
						 (bitwise-or mod_Control_L
							     mod_Shift
							     mod_Alt_R)))))
   (assert-equal (uim '(list 'lkey_a (bitwise-or mod_Shift
						 mod_Control_L
						 mod_Alt_R)))
		 (uim '(event-exp-collector-exp
			(event-exp-collector-new #f 'lkey_a #f
						 (bitwise-or mod_Control_L
							     mod_Shift
							     mod_Alt_R)))))
   (assert-equal (uim '(list 'pkey_qwerty_a (bitwise-or mod_Shift
					     mod_Control_L
					     mod_Alt_R)))
		 (uim '(event-exp-collector-exp
			(event-exp-collector-new #f #f 'pkey_qwerty_a
						 (bitwise-or mod_Control_L
							     mod_Shift
							     mod_Alt_R)))))
   ;; single key expression with multiple modifiers including ignore_Shift
   (assert-equal (uim '(list "a" (bitwise-or mod_Shift
					     mod_Control_L
					     mod_Alt_R
					     mod_ignore_Shift)))
		 (uim '(event-exp-collector-exp
			(event-exp-collector-new "a" #f #f
						 (bitwise-or mod_Control_L
							     mod_Shift
							     mod_ignore_Shift
							     mod_Alt_R)))))
   (assert-equal (uim '(list 'lkey_a (bitwise-or mod_Shift
						 mod_Control_L
						 mod_Alt_R
						 mod_ignore_Shift)))
		 (uim '(event-exp-collector-exp
			(event-exp-collector-new #f 'lkey_a #f
						 (bitwise-or mod_Control_L
							     mod_Shift
							     mod_ignore_Shift
							     mod_Alt_R)))))
   (assert-equal (uim '(list 'pkey_qwerty_a (bitwise-or mod_Shift
					     mod_Control_L
					     mod_Alt_R
					     mod_ignore_Shift)))
		 (uim '(event-exp-collector-exp
			(event-exp-collector-new #f #f 'pkey_qwerty_a
						 (bitwise-or mod_Control_L
							     mod_Shift
							     mod_ignore_Shift
							     mod_Alt_R)))))
   ;; multiple key expression with multiple modifiers including ignore_Shift
   (assert-equal (uim '(list "A" 'lkey_a 'pkey_qwerty_a
			     (bitwise-or mod_Shift
					 mod_Control_L
					 mod_Alt_R
					 mod_ignore_Shift)))
		 (uim '(event-exp-collector-exp
			(event-exp-collector-new "A" 'lkey_a 'pkey_qwerty_a
						 (bitwise-or mod_Control_L
							     mod_Shift
							     mod_ignore_Shift
							     mod_Alt_R)))))
   ;; predicate
   (assert-true  (uim-bool '(eq? key-event-press
				 (event-exp-collector-exp
				  (event-exp-collector-new
				   #f #f #f mod_None
				   (list key-event-press))))))
   (assert-true  (uim-bool '(equal? (list key-event-press
					  key-event-autorepeat)
				    (event-exp-collector-exp
				     (event-exp-collector-new
				      #f #f #f mod_None
				      (list key-event-autorepeat
					    key-event-press))))))
   (assert-true  (uim-bool '(equal? (list key-event-press
					  key-event-autorepeat
					  (event-exp-predicate 'char-vowel))
				    (event-exp-collector-exp
				     (event-exp-collector-new
				      #f #f #f mod_None
				      (list key-event-autorepeat
					    (event-exp-predicate 'char-vowel)
					    key-event-press))))))
   (assert-true  (uim-bool '(equal? (list "A" 'lkey_a 'pkey_qwerty_a
					  (bitwise-or mod_Shift_L
						      mod_Control
						      mod_Alt_R)
					  key-event-press key-event-autorepeat)
				    (event-exp-collector-exp
				     (event-exp-collector-new
				      "A" 'lkey_a 'pkey_qwerty_a
				      (bitwise-or mod_Shift_L
						  mod_Control
						  mod_Alt_R)
				      (list key-event-autorepeat
					    key-event-press)))))))

  ("test event-exp-collector-fold"
   ;; null expression
   (assert-equal ()
		 (uim '(event-exp-collector-exp
			(event-exp-collector-fold ()))))
   ;; single element
   (assert-equal "a"
		 (uim '(event-exp-collector-exp
			(event-exp-collector-fold "a"))))
   (assert-equal 'lkey_a
		 (uim '(event-exp-collector-exp
			(event-exp-collector-fold 'lkey_a))))
   (assert-error (lambda ()
		   (uim '(event-exp-collector-exp
			  (event-exp-collector-fold 'pkey_qwerty_a)))))
   (uim '(require "physical-key.scm"))
   (assert-equal 'pkey_qwerty_a
		 (uim '(event-exp-collector-exp
			(event-exp-collector-fold 'pkey_qwerty_a))))
   (assert-equal ()
		 (uim '(event-exp-collector-exp
			(event-exp-collector-fold 'mod_None))))
   (assert-equal (uim 'mod_Shift)
		 (uim '(event-exp-collector-exp
			(event-exp-collector-fold 'mod_Shift))))
   (assert-true  (uim-bool '(eq? key-event-press
				 (event-exp-collector-exp
				  (event-exp-collector-fold 'press)))))
   (assert-true  (uim-bool '(eq? key-event-autorepeat
				 (event-exp-collector-exp
				  (event-exp-collector-fold 'autorepeat)))))
   (assert-true  (uim-bool '(eq? (event-exp-predicate 'peek)
				 (event-exp-collector-exp
				  (event-exp-collector-fold 'peek)))))
   ;; single element in list
   (assert-equal "a"
		 (uim '(event-exp-collector-exp
			(event-exp-collector-fold '("a")))))
   (assert-equal 'lkey_a
		 (uim '(event-exp-collector-exp
			(event-exp-collector-fold '(lkey_a)))))
   (assert-equal 'pkey_qwerty_a
		 (uim '(event-exp-collector-exp
			(event-exp-collector-fold '(pkey_qwerty_a)))))
   (assert-equal ()
		 (uim '(event-exp-collector-exp
			(event-exp-collector-fold '(mod_None)))))
   (assert-equal (uim 'mod_Shift)
		 (uim '(event-exp-collector-exp
			(event-exp-collector-fold '(mod_Shift)))))
   (assert-true  (uim-bool '(eq? key-event-press
				 (event-exp-collector-exp
				  (event-exp-collector-fold '(press))))))
   (assert-true  (uim-bool '(eq? key-event-autorepeat
				 (event-exp-collector-exp
				  (event-exp-collector-fold '(autorepeat))))))
   (assert-true  (uim-bool '(eq? (event-exp-predicate 'peek)
				 (event-exp-collector-exp
				  (event-exp-collector-fold '(peek))))))
   ;; (with) modifiers
   (assert-equal (uim '(list "a" (bitwise-or mod_Shift
					     mod_Control_L
					     mod_Alt
					     mod_ignore_Super)))
		 (uim '(event-exp-collector-exp
			(event-exp-collector-fold '("a"
						    mod_Control_L
						    mod_Shift
						    mod_ignore_Super
						    mod_Alt)))))
   (assert-equal (uim '(list 'lkey_a (bitwise-or mod_Shift
						 mod_Control_L
						 mod_Alt
						 mod_ignore_Super)))
		 (uim '(event-exp-collector-exp
			(event-exp-collector-fold '(lkey_a
						    mod_Control_L
						    mod_Shift
						    mod_ignore_Super
						    mod_Alt)))))
   (assert-equal (uim '(list 'pkey_qwerty_a (bitwise-or mod_Shift
							mod_Control_L
							mod_Alt
							mod_ignore_Super)))
		 (uim '(event-exp-collector-exp
			(event-exp-collector-fold '(pkey_qwerty_a
						    mod_Control_L
						    mod_Shift
						    mod_ignore_Super
						    mod_Alt)))))
   (assert-equal (uim '(bitwise-or mod_Control_L
				   mod_Shift
				   mod_ignore_Super
				   mod_Alt))
		 (uim '(event-exp-collector-exp
			(event-exp-collector-fold '(mod_None
						    mod_Control_L
						    mod_Shift
						    mod_ignore_Super
						    mod_Alt)))))
   (assert-equal (uim '(bitwise-or mod_Control_L
				   mod_Shift
				   mod_ignore_Super
				   mod_Alt))
		 (uim '(event-exp-collector-exp
			(event-exp-collector-fold '(mod_Control_L
						    mod_Shift
						    mod_ignore_Super
						    mod_Alt)))))
   (assert-true  (uim-bool '(equal? (list (bitwise-or mod_Control_L
						      mod_Shift
						      mod_ignore_Super
						      mod_Alt)
					  key-event-press)
				    (event-exp-collector-exp
				     (event-exp-collector-fold
				      '(press
					mod_Control_L
					mod_Shift
					mod_ignore_Super
					mod_Alt))))))
   (assert-true  (uim-bool '(equal? (list (bitwise-or mod_Control_L
						      mod_Shift
						      mod_ignore_Super
						      mod_Alt)
					  key-event-autorepeat)
				    (event-exp-collector-exp
				     (event-exp-collector-fold
				      '(autorepeat
					mod_Control_L
					mod_Shift
					mod_ignore_Super
					mod_Alt))))))
   (assert-true  (uim-bool '(equal? (list (bitwise-or mod_Control_L
						      mod_Shift
						      mod_ignore_Super
						      mod_Alt)
					  (event-exp-predicate 'peek))
				    (event-exp-collector-exp
				     (event-exp-collector-fold
				      '(peek
					mod_Control_L
					mod_Shift
					mod_ignore_Super
					mod_Alt))))))
   ;; complex expression
   (assert-true  (uim-bool '(equal? (list "A" 'lkey_a mod_Shift)
				    (event-exp-collector-exp
				     (event-exp-collector-fold
				      '(mod_Shift lkey_a "A"))))))
   (assert-true  (uim-bool '(equal? (list "A"
					  'lkey_a
					  'pkey_qwerty_a
					  (bitwise-or mod_Control_L
						      mod_Shift
						      mod_ignore_Super
						      mod_Alt)
					  key-event-press
					  key-event-autorepeat
					  (event-exp-predicate 'char-upper-case)
					  (event-exp-predicate 'char-vowel)
					  (event-exp-predicate 'peek))
				    (event-exp-collector-exp
				     (event-exp-collector-fold
				      '(char-vowel
					char-upper-case
					mod_Control_L
					peek
					mod_Shift
					autorepeat
					"A"
					press
					pkey_qwerty_a
					mod_ignore_Super
					lkey_a
					mod_Alt))))))))

(define-uim-test-case "testcase event expressions"
  (setup
   (lambda ()
     (uim '(require "evmap.scm"))
     (uim '(require "physical-key.scm"))))

  ("test event-exp-match?"
   (uim '(begin
	   (define test-evexp-normalize
	     (lambda (exp)
	       (event-exp-collector-exp
		(event-exp-collector-fold exp))))
	   #t))
   (assert-equal "a"
		 (uim '(test-evexp-normalize "a")))
   (assert-equal (uim '(list "a" mod_Shift))
		 (uim '(test-evexp-normalize (list "a" 'mod_Shift))))
   ;; null expression fails against any event
   (assert-false (uim-bool '(event-exp-match? (test-evexp-normalize ())
					      (key-event-new))))
   (assert-false (uim-bool '(event-exp-match? (test-evexp-normalize ())
					      (key-event-new "a"))))
   (assert-false (uim-bool '(event-exp-match? (test-evexp-normalize ())
					      (key-event-new #f 'lkey_a))))
   ;; explicit default key-event expression
   (assert-true  (uim-bool '(event-exp-match? (test-evexp-normalize
					       '(mod_None press nonrepeat))
					      (key-event-new))))
   (assert-false (uim-bool '(event-exp-match? (test-evexp-normalize
					       '("" mod_None press nonrepeat))
					      (key-event-new))))
   (assert-false (uim-bool '(event-exp-match? (test-evexp-normalize
					       '(lkey_a mod_None))
					      (key-event-new))))
   (assert-false (uim-bool '(event-exp-match? (test-evexp-normalize
					       '(mod_None release))
					      (key-event-new))))
   (assert-false (uim-bool '(event-exp-match? (test-evexp-normalize
					       '(mod_None autorepeat))
					      (key-event-new))))
   ;; single key expression
   (assert-true  (uim-bool '(event-exp-match? (test-evexp-normalize
					       "a")
					      (key-event-new "a"))))
   (assert-true  (uim-bool '(event-exp-match? (test-evexp-normalize
					       'lkey_a)
					      (key-event-new #f 'lkey_a))))
   (assert-true  (uim-bool '(event-exp-match? (test-evexp-normalize
					       'pkey_qwerty_a)
					      (key-event-new #f
							     #f
							     'pkey_qwerty_a))))
   ;; single key expression: additional element makes matching failed
   (assert-false (uim-bool '(event-exp-match? (test-evexp-normalize
					       "a")
					      (key-event-new "a"
							     #f
							     #f
							     mod_Shift))))
   (assert-false (uim-bool '(event-exp-match? (test-evexp-normalize
					       'lkey_a)
					      (key-event-new #f
							     'lkey_a
							     #f
							     mod_Shift))))
   (assert-false (uim-bool '(event-exp-match? (test-evexp-normalize
					       'pkey_qwerty_a)
					      (key-event-new #f
							     #f
							     'pkey_qwerty_a
							     mod_Shift))))
   ;; single key expression with single modifier
   (assert-true  (uim-bool '(event-exp-match? (test-evexp-normalize
					       '("a" mod_Shift))
					      (key-event-new "a"
							     #f
							     #f
							     mod_Shift))))
   (assert-true  (uim-bool '(event-exp-match? (test-evexp-normalize
					       '("a" mod_Shift))
					      (key-event-new "a"
							     #f
							     #f
							     mod_Shift_L))))
   (assert-true  (uim-bool '(event-exp-match? (test-evexp-normalize
					       '("a" mod_Shift))
					      (key-event-new "a"
							     #f
							     #f
							     mod_Shift_R))))
   (assert-true  (uim-bool '(event-exp-match? (test-evexp-normalize
					       '("a" mod_Shift))
					      (key-event-new "a"
							     #f
							     #f
							     (bitwise-or
							      mod_Shift_L
							      mod_Shift_R)))))
   (assert-false (uim-bool '(event-exp-match? (test-evexp-normalize
					       '("a" mod_Shift))
					      (key-event-new "a"
							     #f
							     #f
							     mod_None))))
   (assert-true  (uim-bool '(event-exp-match? (test-evexp-normalize
					       '(lkey_a mod_Shift))
					      (key-event-new #f
							     'lkey_a
							     #f
							     mod_Shift))))
   (assert-true  (uim-bool '(event-exp-match? (test-evexp-normalize
					       '(pkey_qwerty_a mod_Shift))
					      (key-event-new #f
							     #f
							     'pkey_qwerty_a
							     mod_Shift))))
   ;; single key expression with multiple modifiers
   (assert-true  (uim-bool '(event-exp-match? (test-evexp-normalize
					       '("a"
						 mod_Shift
						 mod_Control_L
						 mod_Alt_R))
					      (key-event-new "a"
							     #f
							     #f
							     (bitwise-or
							      mod_Shift
							      mod_Control_L
							      mod_Alt_R)))))
   (assert-true  (uim-bool '(event-exp-match? (test-evexp-normalize
					       '("a"
						 mod_Shift
						 mod_Control_L
						 mod_Alt_R))
					      (key-event-new "a"
							     #f
							     #f
							     (bitwise-or
							      mod_Shift_L
							      mod_Control_L
							      mod_Alt_R)))))
   (assert-true  (uim-bool '(event-exp-match? (test-evexp-normalize
					       '("a"
						 mod_Shift
						 mod_Control_L
						 mod_Alt_R))
					      (key-event-new "a"
							     #f
							     #f
							     (bitwise-or
							      mod_Shift_R
							      mod_Control_L
							      mod_Alt_R)))))
   (assert-false (uim-bool '(event-exp-match? (test-evexp-normalize
					       '("a"
						 mod_Shift
						 mod_Control_L
						 mod_Alt_R))
					      (key-event-new "A"
							     #f
							     #f
							     (bitwise-or
							      mod_Shift
							      mod_Control_L
							      mod_Alt_R)))))
   (assert-false (uim-bool '(event-exp-match? (test-evexp-normalize
					       '("a"
						 mod_Shift
						 mod_Control_L
						 mod_Alt_R))
					      (key-event-new "a"
							     #f
							     #f
							     (bitwise-or
							      mod_Shift
							      mod_Alt_R)))))
   (assert-true  (uim-bool '(event-exp-match? (test-evexp-normalize
					       '(lkey_a
						 mod_Shift
						 mod_Control_L
						 mod_Alt_R))
					      (key-event-new #f
							     'lkey_a
							     #f
							     (bitwise-or
							      mod_Shift
							      mod_Control_L
							      mod_Alt_R)))))
   (assert-true  (uim-bool '(event-exp-match? (test-evexp-normalize
					       '(pkey_qwerty_a
						 mod_Shift
						 mod_Control_L
						 mod_Alt_R))
					      (key-event-new #f
							     #f
							     'pkey_qwerty_a
							     (bitwise-or
							      mod_Shift
							      mod_Control_L
							      mod_Alt_R)))))
   ;; single key expression with multiple modifiers including ignore_Shift
   (assert-true  (uim-bool '(event-exp-match? (test-evexp-normalize
					       '("a"
						 mod_Control_L
						 mod_Alt_R
						 mod_ignore_Shift))
					      (key-event-new "a"
							     #f
							     #f
							     (bitwise-or
							      mod_Shift
							      mod_Control_L
							      mod_Alt_R)))))
   (assert-true  (uim-bool '(event-exp-match? (test-evexp-normalize
					       '("a"
						 mod_Control_L
						 mod_Alt_R
						 mod_ignore_Shift))
					      (key-event-new "a"
							     #f
							     #f
							     (bitwise-or
							      mod_Control_L
							      mod_Alt_R)))))
   (assert-true  (uim-bool '(event-exp-match? (test-evexp-normalize
					       '("a"
						 mod_Control_L
						 mod_Alt_R
						 mod_ignore_Shift))
					      (key-event-new "a"
							     #f
							     #f
							     (bitwise-or
							      mod_Shift_L
							      mod_Shift_R
							      mod_Control_L
							      mod_Alt_R)))))
   (assert-true  (uim-bool '(event-exp-match? (test-evexp-normalize
					       '(lkey_a
						 mod_Control_L
						 mod_Alt_R
						 mod_ignore_Shift))
					      (key-event-new #f
							     'lkey_a
							     #f
							     (bitwise-or
							      mod_Shift
							      mod_Control_L
							      mod_Alt_R)))))
   (assert-true  (uim-bool '(event-exp-match? (test-evexp-normalize
					       '(pkey_qwerty_a
						 mod_Control_L
						 mod_Alt_R
						 mod_ignore_Shift))
					      (key-event-new #f
							     #f
							     'pkey_qwerty_a
							     (bitwise-or
							      mod_Shift
							      mod_Control_L
							      mod_Alt_R)))))
   ;; multiple key expression with multiple modifiers including ignore_Shift
   (assert-true  (uim-bool '(event-exp-match? (test-evexp-normalize
					       '("A"
						 lkey_a
						 pkey_qwerty_a
						 mod_Control_L
						 mod_Alt_R
						 mod_ignore_Shift))
					      (key-event-new "A"
							     'lkey_a
							     'pkey_qwerty_a
							     (bitwise-or
							      mod_Shift
							      mod_Control_L
							      mod_Alt_R)))))
   (assert-true  (uim-bool '(event-exp-match? (test-evexp-normalize
					       '("A"
						 lkey_a
						 pkey_qwerty_a
						 mod_Control_L
						 mod_Alt_R
						 mod_ignore_Shift))
					      (key-event-new "A"
							     'lkey_a
							     'pkey_qwerty_a
							     (bitwise-or
							      mod_Control_L
							      mod_Alt_R)))))
   (assert-true  (uim-bool '(event-exp-match? (test-evexp-normalize
					       '("A"
						 lkey_a
						 pkey_qwerty_a
						 mod_Control_L
						 mod_Alt_R
						 mod_ignore_Shift))
					      (key-event-new "A"
							     'lkey_a
							     'pkey_qwerty_a
							     (bitwise-or
							      mod_Shift_L
							      mod_Shift_R
							      mod_Control_L
							      mod_Alt_R)))))
   (assert-false (uim-bool '(event-exp-match? (test-evexp-normalize
					       '("A"
						 lkey_a
						 pkey_qwerty_a
						 mod_Control_L
						 mod_Alt_R
						 mod_ignore_Shift))
					      (key-event-new "A"
							     #f
							     'pkey_qwerty_a
							     (bitwise-or
							      mod_Shift
							      mod_Control_L
							      mod_Alt_R)))))
   ;; predicate
   (assert-true  (uim-bool '(event-exp-match? (test-evexp-normalize
					       'press)
					      (key-event-new #f
							     #f
							     #f
							     mod_None
							     #t
							     #f))))
   (assert-false (uim-bool '(event-exp-match? (test-evexp-normalize
					       'press)
					      (key-event-new #f
							     #f
							     #f
							     mod_None
							     #f
							     #f))))
   (assert-true  (uim-bool '(event-exp-match? (test-evexp-normalize
					       '(autorepeat press))
					      (key-event-new #f
							     #f
							     #f
							     mod_None
							     #t
							     #t))))
   (assert-false (uim-bool '(event-exp-match? (test-evexp-normalize
					       '(autorepeat press))
					      (key-event-new #f
							     #f
							     #f
							     mod_None
							     #t
							     #f))))
   (assert-true  (uim-bool '(event-exp-match? (test-evexp-normalize
					       '(autorepeat press char-vowel))
					      (key-event-new "a"
							     #f
							     #f
							     mod_None
							     #t
							     #t))))
   (assert-true  (uim-bool '(event-exp-match? (test-evexp-normalize
					       '(autorepeat press char-vowel))
					      (key-event-new "A"
							     #f
							     #f
							     mod_None
							     #t
							     #t))))
   (assert-false (uim-bool '(event-exp-match? (test-evexp-normalize
					       '(autorepeat press char-vowel))
					      (key-event-new "b"
							     #f
							     #f
							     mod_None
							     #t
							     #t))))
   (assert-false (uim-bool '(event-exp-match? (test-evexp-normalize
					       '(autorepeat press char-vowel))
					      (key-event-new "a"
							     #f
							     #f
							     mod_None
							     #f
							     #t))))
   ;; complex expression
   (assert-true  (uim-bool '(event-exp-match? (test-evexp-normalize
					       '("A" lkey_a mod_Shift))
					      (key-event-new "A"
							     'lkey_a
							     #f
							     mod_Shift))))
   (assert-false (uim-bool '(event-exp-match? (test-evexp-normalize
					       '("A" lkey_a mod_Shift))
					      (key-event-new "A"
							     #f
							     #f
							     mod_Shift))))
   (assert-false (uim-bool '(event-exp-match? (test-evexp-normalize
					       '("A" lkey_a mod_Shift))
					      (key-event-new #f
							     'lkey_a
							     #f
							     mod_Shift))))
   (assert-false (uim-bool '(event-exp-match? (test-evexp-normalize
					       '("A" lkey_a mod_Shift))
					      (key-event-new "A"
							     'lkey_a
							     #f
							     mod_None))))
   (assert-false (uim-bool '(event-exp-match? (test-evexp-normalize
					       '("A" lkey_a mod_Shift))
					      (key-event-new "A"
							     'lkey_a
							     #f
							     (bitwise-or
							      mod_Shift
							      mod_Control)))))
   (assert-true  (uim-bool '(event-exp-match? (test-evexp-normalize
					       '("A" lkey_a mod_Shift))
					      (key-event-new "A"
							     'lkey_a
							     'pkey_qwerty_a
							     mod_Shift))))
   (assert-true  (uim-bool '(event-exp-match? (test-evexp-normalize
					       '(char-vowel
						 char-upper-case
						 mod_Control_L
						 peek
						 mod_Shift
						 autorepeat
						 "A"
						 press
						 pkey_qwerty_a
						 mod_ignore_Super
						 lkey_a
						 mod_Alt))
					      (key-event-new "A"
							     'lkey_a
							     'pkey_qwerty_a
							     (bitwise-or
							      mod_Control_L
							      mod_Shift
							      mod_Alt)
							     #t
							     #t))))
   (assert-true  (uim-bool '(event-exp-match? (test-evexp-normalize
					       '(char-vowel
						 char-upper-case
						 mod_Control_L
						 peek
						 mod_Shift
						 autorepeat
						 "A"
						 press
						 pkey_qwerty_a
						 mod_ignore_Super
						 lkey_a
						 mod_Alt))
					      (key-event-new "A"
							     'lkey_a
							     'pkey_qwerty_a
							     (bitwise-or
							      mod_Control_L
							      mod_Super_L
							      mod_Shift
							      mod_Alt)
							     #t
							     #t))))
   (assert-false (uim-bool '(event-exp-match? (test-evexp-normalize
					       '(char-vowel
						 char-upper-case
						 mod_Control_L
						 peek
						 mod_Shift
						 autorepeat
						 "A"
						 press
						 pkey_qwerty_a
						 mod_ignore_Super
						 lkey_a
						 mod_Alt))
					      (key-event-new "a"
							     'lkey_a
							     'pkey_qwerty_a
							     (bitwise-or
							      mod_Control_L
							      mod_Super_L
							      mod_Shift
							      mod_Alt)
							     #t
							     #t))))
   ;; event consumption control by explicit directive 'consume' and 'peek'
   (uim '(define test-ev (key-event-new "a")))
   (assert-true  (uim-bool '(event-exp-match? (test-evexp-normalize
					       "a")
					      test-ev)))
   (assert-false (uim-bool '(event-consumed test-ev)))
   (uim '(define test-ev (key-event-new "a")))
   (assert-true  (uim-bool '(event-exp-match? (test-evexp-normalize
					       '("a" consume))
					      test-ev)))
   (assert-true  (uim-bool '(event-consumed test-ev)))
   (uim '(define test-ev (key-event-new "a")))
   (assert-true  (uim-bool '(event-exp-match? (test-evexp-normalize
					       '("a" peek))
					      test-ev)))
   (assert-equal 'peek
		 (uim '(event-consumed test-ev))))

  ("test event-exp-macro?"
   ;; non-macro element
   (assert-false (uim-bool '(event-exp-macro? ())))
   (assert-true  (uim-bool '(event-exp-macro? 'lkey_a)))
   (assert-true  (uim-bool '(event-exp-macro? 'pkey_qwerty_a)))
   (assert-false (uim-bool '(event-exp-macro? 'mod_None)))
   (assert-false (uim-bool '(event-exp-macro? 'mod_Shift)))
   (assert-false (uim-bool '(event-exp-macro? mod_None)))
   (assert-false (uim-bool '(event-exp-macro? mod_Shift)))
   (assert-false (uim-bool '(event-exp-macro? 'press)))
   (assert-false (uim-bool '(event-exp-macro? 'release)))
   (assert-false (uim-bool '(event-exp-macro? 'peek)))
   (assert-false (uim-bool '(event-exp-macro? key-event-press)))
   (assert-false (uim-bool '(event-exp-macro? key-event-autorepeat)))
   (assert-false (uim-bool '(event-exp-macro? (event-exp-predicate 'peek))))
   (assert-false (uim-bool '(event-exp-macro? 'press-release)))
   (assert-false (uim-bool '(event-exp-macro? 'set)))
   (assert-false (uim-bool '(event-exp-macro? 'ordered-chord)))
   (assert-false (uim-bool '(event-exp-macro? 'chord)))
   (assert-false (uim-bool '(event-exp-macro?
			     (assq-cdr 'press-release
				       event-exp-macro-alist))))
   (assert-false (uim-bool '(event-exp-macro?
			     (assq-cdr 'set
				       event-exp-macro-alist))))
   (assert-false (uim-bool '(event-exp-macro?
			     (assq-cdr 'ordered-chord
				       event-exp-macro-alist))))
   (assert-false (uim-bool '(event-exp-macro?
			     (assq-cdr 'chord
				       event-exp-macro-alist))))
   ;; non-macro element in list
   (assert-false (uim-bool '(event-exp-macro? '(()))))
   (assert-true  (uim-bool '(event-exp-macro? '("a"))))
   (assert-true  (uim-bool '(event-exp-macro? '(lkey_a))))
   (assert-true  (uim-bool '(event-exp-macro? '(pkey_qwerty_a))))
   (assert-false (uim-bool '(event-exp-macro? '(mod_None))))
   (assert-false (uim-bool '(event-exp-macro? '(mod_Shift))))
   (assert-false (uim-bool '(event-exp-macro? (list mod_None))))
   (assert-false (uim-bool '(event-exp-macro? (list mod_Shift))))
   (assert-false (uim-bool '(event-exp-macro? '(press))))
   (assert-false (uim-bool '(event-exp-macro? '(release))))
   (assert-false (uim-bool '(event-exp-macro? '(peek))))
   (assert-false (uim-bool '(event-exp-macro? (list key-event-press))))
   (assert-false (uim-bool '(event-exp-macro? (list key-event-autorepeat))))
   (assert-false (uim-bool '(event-exp-macro? (list
					       (event-exp-predicate 'peek)))))
   ;; TODO: reject these arg-less macros
   ;;(assert-false (uim-bool '(event-exp-macro? '(press-release))))
   ;;(assert-false (uim-bool '(event-exp-macro? '(set))))
   ;;(assert-false (uim-bool '(event-exp-macro? '(ordered-chord))))
   ;;(assert-false (uim-bool '(event-exp-macro? '(chord))))
   (assert-false (uim-bool '(event-exp-macro?
			     (list (assq-cdr 'press-release
					     event-exp-macro-alist)))))
   (assert-false (uim-bool '(event-exp-macro?
			     (list (assq-cdr 'set
					     event-exp-macro-alist)))))
   (assert-false (uim-bool '(event-exp-macro?
			     (list (assq-cdr 'ordered-chord
					     event-exp-macro-alist)))))
   (assert-false (uim-bool '(event-exp-macro?
			     (list (assq-cdr 'chord
					     event-exp-macro-alist)))))
   ;; abbreviation of press-release macro
   (assert-true  (uim-bool '(event-exp-macro? "a")))
   ;; ordinary macros
   (assert-true  (uim-bool '(event-exp-macro? '(press-release "a"))))
   (assert-true  (uim-bool '(event-exp-macro? '(press-release lkey_a))))
   (assert-true  (uim-bool '(event-exp-macro? '(press-release ("a"
							       mod_Shift)))))
   (assert-true  (uim-bool '(event-exp-macro? '(press-release ("a"
							       mod_Shift)))))
   (assert-true  (uim-bool '(event-exp-macro? '(set "a" "b" "c"))))
   (assert-true  (uim-bool '(event-exp-macro? '(set ("a" press)
						    ("b" press)
						    ("c" press)))))
   (assert-true  (uim-bool '(event-exp-macro? '(ordered-chord "a" "b" "c"))))
   (assert-true  (uim-bool '(event-exp-macro? '(ordered-chord ("a" press)
							      ("b" press)
							      ("c" press)))))
   (assert-true  (uim-bool '(event-exp-macro? '(chord "a" "b" "c"))))
   (assert-true  (uim-bool '(event-exp-macro? '(chord ("a" press)
						      ("b" press)
						      ("c" press)))))
   ;;(assert-true  (uim-bool '(event-exp-macro? '(interval $1 200))))
   ;;(assert-true  (uim-bool '(event-exp-macro? '(interval $1 0 100))))
   ;;(assert-true  (uim-bool '(event-exp-macro? '(interval $1 200 500))))

   ;; invalid macros
   ;; This expression is detected as implicit-macro. To avoid
   ;; performance influence, strict validation is not performed.
   ;;(assert-false (uim-bool '(event-exp-macro? '(non-existent "a" "b" "c"))))
   (assert-false (uim-bool '(event-exp-macro? '(non-existent ("a" press)
							     ("b" press)
							     ("c" press))))))

  ("test event-exp-expand-macro-press-release"
   ;; single key specifier
   (assert-equal '(((press "a") (release "a")))
		 (uim '(event-exp-expand-macro-press-release '("a"))))
   (assert-equal '(((press lkey_a) (release lkey_a)))
		 (uim '(event-exp-expand-macro-press-release '(lkey_a))))
   (assert-equal '(((press pkey_qwerty_a) (release pkey_qwerty_a)))
		 (uim '(event-exp-expand-macro-press-release
			'(pkey_qwerty_a))))
   ;; single key specifier with modifiers
   (assert-equal '(((press   "a" mod_Shift mod_Control_L)
		    (release "a" mod_Shift mod_Control_L)))
		 (uim '(event-exp-expand-macro-press-release
			'("a" mod_Shift mod_Control_L))))
   (assert-equal '(((press   lkey_a mod_Shift mod_Control_L)
		    (release lkey_a mod_Shift mod_Control_L)))
		 (uim '(event-exp-expand-macro-press-release
			'(lkey_a mod_Shift mod_Control_L))))
   (assert-equal '(((press   pkey_qwerty_a mod_Shift mod_Control_L)
		    (release pkey_qwerty_a mod_Shift mod_Control_L)))
		 (uim '(event-exp-expand-macro-press-release
			'(pkey_qwerty_a mod_Shift mod_Control_L))))
      ;; single key specifier with directives
   (assert-equal '(((press   "a" peek)
		    (release "a" peek)))
		 (uim '(event-exp-expand-macro-press-release
			'("a" peek))))
   (assert-equal '(((press   "a" peek char-lower-case)
		    (release "a" peek char-lower-case)))
		 (uim '(event-exp-expand-macro-press-release
			'("a" peek char-lower-case)))))

  ("test event-exp-expand-macro-set"
   ;; null
   (assert-equal ()
		 (uim '(event-exp-expand-macro-set ())))
   ;; 1 key
   (assert-equal '(("a"))
		 (uim '(event-exp-expand-macro-set '("a"))))
   (assert-equal '((lkey_a))
		 (uim '(event-exp-expand-macro-set '(lkey_a))))
   (assert-equal '((pkey_qwerty_a))
		 (uim '(event-exp-expand-macro-set '(pkey_qwerty_a))))
   ;; 2 keys
   (assert-equal '(("a" "b") ("b" "a"))
		 (uim '(event-exp-expand-macro-set '("a" "b"))))
   ;; 3 keys
   (assert-equal '(("a" "b" "c") ("a" "c" "b")
		   ("b" "a" "c") ("b" "c" "a")
		   ("c" "a" "b") ("c" "b" "a"))
		 (uim '(event-exp-expand-macro-set '("a" "b" "c"))))

   ;; 1 key with modifiers
   (assert-equal '((("a" mod_Shift mod_Control_L)))
		 (uim '(event-exp-expand-macro-set
			'(("a" mod_Shift mod_Control_L)))))
   (assert-equal '(((lkey_a mod_Shift mod_Control_L)))
		 (uim '(event-exp-expand-macro-set
			'((lkey_a mod_Shift mod_Control_L)))))
   (assert-equal '(((pkey_qwerty_a mod_Shift mod_Control_L)))
		 (uim '(event-exp-expand-macro-set
			'((pkey_qwerty_a mod_Shift mod_Control_L)))))
   ;; 2 keys with modifiers
   (assert-equal '((("a" mod_Shift mod_Control_L)
		    ("b" mod_Shift mod_Control_L))
		   (("b" mod_Shift mod_Control_L)
		    ("a" mod_Shift mod_Control_L)))
		 (uim '(event-exp-expand-macro-set
			'(("a" mod_Shift mod_Control_L)
			  ("b" mod_Shift mod_Control_L))))))

  ("test event-exp-expand-macro-ordered-chord"
   ;; null
   (assert-equal ()
		 (uim '(event-exp-expand-macro-ordered-chord ())))
   ;; 1 key
   (assert-equal '(((press "a")
		    (release "a")))
		 (uim '(event-exp-expand-macro-ordered-chord
			'("a"))))
   (assert-equal '(((press "a" mod_Shift)
		    (release "a" mod_Shift)))
		 (uim '(event-exp-expand-macro-ordered-chord
			'(("a" mod_Shift)))))
   (assert-equal '(((press lkey_a)
		    (release lkey_a)))
		 (uim '(event-exp-expand-macro-ordered-chord
			'(lkey_a))))
   (assert-equal '(((press lkey_a mod_Shift)
		    (release lkey_a mod_Shift)))
		 (uim '(event-exp-expand-macro-ordered-chord
			'((lkey_a mod_Shift)))))
   (assert-equal '(((press pkey_qwerty_a) (release pkey_qwerty_a)))
		 (uim '(event-exp-expand-macro-ordered-chord
			'(pkey_qwerty_a))))
   ;; 2 keys
   (assert-equal '(((press "a") (press "b") (release "a") (release "b"))
		   ((press "a") (press "b") (release "b") (release "a")))
		 (uim '(event-exp-expand-macro-ordered-chord '("a" "b"))))
   (assert-equal '(((press "a" mod_Shift) (press "b" mod_Shift)
		    (release "a" mod_Shift) (release "b" mod_Shift))
		   ((press "a" mod_Shift) (press "b" mod_Shift)
		    (release "b" mod_Shift) (release "a" mod_Shift)))
		 (uim '(event-exp-expand-macro-ordered-chord
			'(("a" mod_Shift) ("b" mod_Shift)))))
   ;; args for ordered-chord cannot contain 'press' or 'release' predicate
   (assert-equal ()
		 (uim '(event-exp-expand-macro-ordered-chord
			'(("a" press)))))
   (assert-equal ()
		 (uim '(event-exp-expand-macro-ordered-chord
			'(("a" release)))))
   (assert-equal ()
		 (uim '(event-exp-expand-macro-ordered-chord
			'((lkey_a press)))))
   (assert-equal ()
		 (uim '(event-exp-expand-macro-ordered-chord
			'((lkey_a mod_Shift press)))))
   (assert-equal ()
		 (uim '(event-exp-expand-macro-ordered-chord
			'(("a" press) ("a" release))))))

  ("test event-exp-expand-macro-chord"
   ;; null
   (assert-equal ()
		 (uim '(event-exp-expand-macro-chord ())))
   ;; 1 key
   (assert-equal '(((press "a")
		    (release "a")))
		 (uim '(event-exp-expand-macro-chord
			'("a"))))
   (assert-equal '(((press "a" mod_Shift)
		    (release "a" mod_Shift)))
		 (uim '(event-exp-expand-macro-chord
			'(("a" mod_Shift)))))
   (assert-equal '(((press lkey_a)
		    (release lkey_a)))
		 (uim '(event-exp-expand-macro-chord
			'(lkey_a))))
   (assert-equal '(((press lkey_a mod_Shift)
		    (release lkey_a mod_Shift)))
		 (uim '(event-exp-expand-macro-chord
			'((lkey_a mod_Shift)))))
   (assert-equal '(((press pkey_qwerty_a) (release pkey_qwerty_a)))
		 (uim '(event-exp-expand-macro-chord
			'(pkey_qwerty_a))))
   ;; 2 keys
   (assert-equal '(((press "a") (press "b") (release "a") (release "b"))
		   ((press "a") (press "b") (release "b") (release "a"))
		   ((press "b") (press "a") (release "a") (release "b"))
		   ((press "b") (press "a") (release "b") (release "a")))
		 (uim '(event-exp-expand-macro-chord '("a" "b"))))
   (assert-equal '(((press "a" mod_Shift) (press "b" mod_Shift)
		    (release "a" mod_Shift) (release "b" mod_Shift))
		   ((press "a" mod_Shift) (press "b" mod_Shift)
		    (release "b" mod_Shift) (release "a" mod_Shift))
		   ((press "b" mod_Shift) (press "a" mod_Shift)
		    (release "a" mod_Shift) (release "b" mod_Shift))
		   ((press "b" mod_Shift) (press "a" mod_Shift)
		    (release "b" mod_Shift) (release "a" mod_Shift)))
		 (uim '(event-exp-expand-macro-chord
			'(("a" mod_Shift) ("b" mod_Shift)))))
   ;; args for ordered-chord cannot contain 'press' or 'release' predicate
   (assert-equal ()
		 (uim '(event-exp-expand-macro-chord
			'(("a" press)))))
   (assert-equal ()
		 (uim '(event-exp-expand-macro-chord
			'(("a" release)))))
   (assert-equal ()
		 (uim '(event-exp-expand-macro-chord
			'((lkey_a press)))))
   (assert-equal ()
		 (uim '(event-exp-expand-macro-chord
			'((lkey_a mod_Shift press)))))
   (assert-equal ()
		 (uim '(event-exp-expand-macro-chord
			'(("a" press) ("a" release))))))

  ("test event-exp-list-expand-macro"
   ;; 'press-release' macro
   (assert-equal '(((press "a") (release "a")))
		 (uim '(event-exp-list-expand-macro
			'((press-release "a"))
			())))
   (assert-equal '(((press "a") (release "a")
		    (press "b") (release "b")))
		 (uim '(event-exp-list-expand-macro
			'((press-release "a") (press-release "b"))
			())))
   (assert-equal '(((press "a") (release "a")
		    (press "b") (release "b")
		    (press "c") (release "c")))
		 (uim '(event-exp-list-expand-macro
			'((press-release "a")
			  (press-release "b")
			  (press-release "c"))
			())))
   (assert-equal '(((press "a") (release "a")
		    ("b" press)))
		 (uim '(event-exp-list-expand-macro
			'((press-release "a") ("b" press))
			())))
   (assert-equal '(((press "a") (release "a")
		    ("b" press)
		    (press "c") (release "c")))
		 (uim '(event-exp-list-expand-macro
			'((press-release "a") ("b" press) (press-release "c"))
			())))
   (assert-equal '(((press "a") (release "a")))
		 (uim '(event-exp-list-expand-macro
			'((press-release "a"))
			())))
   (assert-equal '(((press "a") (release "a")
		    ("b" press)))
		 (uim '(event-exp-list-expand-macro
			'((press-release "a") ("b" press))
			())))
   ;; 'set' macro
   (assert-equal '(("a"))
		 (uim '(event-exp-list-expand-macro
			'((set "a"))
			())))
   (assert-equal '(("a" "b")
		   ("b" "a"))
		 (uim '(event-exp-list-expand-macro
			'((set "a" "b"))
			())))
   (assert-equal '(("a" "b" ("c" press))
		   ("b" "a" ("c" press)))
		 (uim '(event-exp-list-expand-macro
			'((set "a" "b") ("c" press))
			())))
   (assert-equal '(("a" "b" ("c" press) "d")
		   ("b" "a" ("c" press) "d"))
		 (uim '(event-exp-list-expand-macro
			'((set "a" "b") ("c" press) (set "d"))
			())))
   (assert-equal '(("a" "b" ("c" press) "d" "e")
		   ("a" "b" ("c" press) "e" "d")
		   ("b" "a" ("c" press) "d" "e")
		   ("b" "a" ("c" press) "e" "d"))
		 (uim '(event-exp-list-expand-macro
			'((set "a" "b") ("c" press) (set "d" "e"))
			())))
   ;; 'ordered-chord' macro
   (assert-equal '(((press "a") (press "b") (release "a") (release "b"))
		   ((press "a") (press "b") (release "b") (release "a")))
		 (uim '(event-exp-list-expand-macro
			'((ordered-chord "a" "b"))
			())))
   (assert-equal '(((press "a") (press "b" mod_Shift)
		    (release "a") (release "b" mod_Shift))
		   ((press "a") (press "b" mod_Shift)
		    (release "b" mod_Shift) (release "a")))
		 (uim '(event-exp-list-expand-macro
			'((ordered-chord "a" ("b" mod_Shift)))
			())))
   ;; 'chord' macro
   (assert-equal '(((press "a") (press "b") (release "a") (release "b"))
		   ((press "a") (press "b") (release "b") (release "a"))
		   ((press "b") (press "a") (release "a") (release "b"))
		   ((press "b") (press "a") (release "b") (release "a")))
		 (uim '(event-exp-list-expand-macro
			'((chord "a" "b"))
			())))
   (assert-equal '(((press "a" mod_Shift) (press "b" mod_Shift)
		    (release "a" mod_Shift) (release "b" mod_Shift))
		   ((press "a" mod_Shift) (press "b" mod_Shift)
		    (release "b" mod_Shift) (release "a" mod_Shift))
		   ((press "b" mod_Shift) (press "a" mod_Shift)
		    (release "a" mod_Shift) (release "b" mod_Shift))
		   ((press "b" mod_Shift) (press "a" mod_Shift)
		    (release "b" mod_Shift) (release "a" mod_Shift)))
		 (uim '(event-exp-list-expand-macro
			'((chord ("a" mod_Shift) ("b" mod_Shift)))
			())))
   ;; mixed
   (assert-equal '(("a" "b" ("c" press)
		    (press "d") (press "e") (release "d") (release "e"))
		   ("a" "b" ("c" press)
		    (press "d") (press "e") (release "e") (release "d"))
		   ("b" "a" ("c" press)
		    (press "d") (press "e") (release "d") (release "e"))
		   ("b" "a" ("c" press)
		    (press "d") (press "e") (release "e") (release "d")))
		 (uim '(event-exp-list-expand-macro
			'((set "a" "b") ("c" press) (ordered-chord "d" "e"))
			())))
   )

  ("test event-exp-seq-parse"
   ;; 'press-release' macro with peek directive
   (assert-true  (uim-bool
		  '(equal? (list
			    (list
			     (list "a"
				   (event-exp-predicate 'press)
				   (event-exp-predicate 'peek))
			     (list "a"
				   (event-exp-predicate 'release)
				   (event-exp-predicate 'peek))))
			   (event-exp-seq-parse
			    '((press-release "a" peek))))))
   ;; 'press-release' macro
   (assert-true  (uim-bool
		  '(equal? (list
			    (list
			     (list "a" (event-exp-predicate 'press))
			     (list "a" (event-exp-predicate 'release))))
			   (event-exp-seq-parse
			    '((press-release "a"))))))
   (assert-true  (uim-bool
		  '(equal? (list
			    (list
			     (list "a" (event-exp-predicate 'press))
			     (list "a" (event-exp-predicate 'release))
			     (list "b" (event-exp-predicate 'press))
			     (list "b" (event-exp-predicate 'release))))
			   (event-exp-seq-parse
			    '((press-release "a") (press-release "b"))))))
   (assert-true  (uim-bool
		  '(equal? (list
			    (list
			     (list "a" (event-exp-predicate 'press))
			     (list "a" (event-exp-predicate 'release))
			     (list "b" (event-exp-predicate 'press))
			     (list "b" (event-exp-predicate 'release))
			     (list "c" (event-exp-predicate 'press))
			     (list "c" (event-exp-predicate 'release))))
			   (event-exp-seq-parse
			    '((press-release "a")
			      (press-release "b")
			      (press-release "c"))))))
   (assert-true  (uim-bool
		  '(equal? (list
			    (list
			     (list "a" (event-exp-predicate 'press))
			     (list "a" (event-exp-predicate 'release))
			     (list "b" (event-exp-predicate 'press))))
			   (event-exp-seq-parse
			    '((press-release "a") ("b" press))))))
   (assert-true  (uim-bool
		  '(equal? (list
			    (list
			     (list "a" (event-exp-predicate 'press))
			     (list "a" (event-exp-predicate 'release))
			     (list "b" (event-exp-predicate 'press))
			     (list "c" (event-exp-predicate 'press))
			     (list "c" (event-exp-predicate 'release))))
			   (event-exp-seq-parse
			    '((press-release "a")
			      ("b" press)
			      (press-release "c"))))))
   (assert-true  (uim-bool
		  '(equal? (list
			    (list
			     (list "a" (event-exp-predicate 'press))
			     (list "a" (event-exp-predicate 'release))
			     (list "b" (event-exp-predicate 'press))))
			   (event-exp-seq-parse
			    '((press-release "a") ("b" press))))))
   ;; 'set' macro
   (assert-true  (uim-bool
		  '(equal? '(("a"))
			   (event-exp-seq-parse
			    '((set "a"))))))
   (assert-true  (uim-bool
		  '(equal? '(("a" "b")
			     ("b" "a"))
			   (event-exp-seq-parse
			    '((set "a" "b"))))))
   (assert-true  (uim-bool
		  '(equal? (list
			    (list "a"
				  "b"
				  (list "c" (event-exp-predicate 'press)))
			    (list "b"
				  "a"
				  (list "c" (event-exp-predicate 'press))))
			   (event-exp-seq-parse
			    '((set "a" "b") ("c" press))))))
   (assert-true  (uim-bool
		  '(equal? (list
			    (list "a"
				  "b"
				  (list "c" (event-exp-predicate 'press))
				  "d"
				  "e")
			    (list "a"
				  "b"
				  (list "c" (event-exp-predicate 'press))
				  "e"
				  "d")
			    (list "b"
				  "a"
				  (list "c" (event-exp-predicate 'press))
				  "d"
				  "e")
			    (list "b"
				  "a"
				  (list "c" (event-exp-predicate 'press))
				  "e"
				  "d"))
			   (event-exp-seq-parse
			    '((set "a" "b") ("c" press) (set "d" "e"))))))
   ;; 'ordered-chord' macro
   (assert-true  (uim-bool
		  '(equal? (list
			    (list
			     (list "a" (event-exp-predicate 'press))
			     (list "b" (event-exp-predicate 'press))
			     (list "a" (event-exp-predicate 'release))
			     (list "b" (event-exp-predicate 'release)))
			    (list
			     (list "a" (event-exp-predicate 'press))
			     (list "b" (event-exp-predicate 'press))
			     (list "b" (event-exp-predicate 'release))
			     (list "a" (event-exp-predicate 'release))))
			   (event-exp-seq-parse
			    '((ordered-chord "a" "b"))))))
   (assert-true  (uim-bool
		  '(equal? (list
			    (list
			     (list "a" (event-exp-predicate 'press))
			     (list "b" mod_Shift (event-exp-predicate 'press))
			     (list "a" (event-exp-predicate 'release))
			     (list "b" mod_Shift (event-exp-predicate 'release)))
			    (list
			     (list "a" (event-exp-predicate 'press))
			     (list "b" mod_Shift (event-exp-predicate 'press))
			     (list "b" mod_Shift (event-exp-predicate 'release))
			     (list "a" (event-exp-predicate 'release))))
			   (event-exp-seq-parse
			    '((ordered-chord "a" ("b" mod_Shift)))))))
   ;; 'chord' macro
   (assert-true  (uim-bool
		  '(equal? (list
			    (list
			     (list "a" (event-exp-predicate 'press))
			     (list "b" (event-exp-predicate 'press))
			     (list "a" (event-exp-predicate 'release))
			     (list "b" (event-exp-predicate 'release)))
			    (list
			     (list "a" (event-exp-predicate 'press))
			     (list "b" (event-exp-predicate 'press))
			     (list "b" (event-exp-predicate 'release))
			     (list "a" (event-exp-predicate 'release)))
			    (list
			     (list "b" (event-exp-predicate 'press))
			     (list "a" (event-exp-predicate 'press))
			     (list "a" (event-exp-predicate 'release))
			     (list "b" (event-exp-predicate 'release)))
			    (list
			     (list "b" (event-exp-predicate 'press))
			     (list "a" (event-exp-predicate 'press))
			     (list "b" (event-exp-predicate 'release))
			     (list "a" (event-exp-predicate 'release))))
			   (event-exp-seq-parse
			    '((chord "a" "b"))))))
   (assert-true  (uim-bool
		  '(equal? (list
			    (list
			     (list "a" mod_Shift (event-exp-predicate 'press))
			     (list "b" mod_Shift (event-exp-predicate 'press))
			     (list "a" mod_Shift (event-exp-predicate 'release))
			     (list "b" mod_Shift (event-exp-predicate 'release)))
			    (list
			     (list "a" mod_Shift (event-exp-predicate 'press))
			     (list "b" mod_Shift (event-exp-predicate 'press))
			     (list "b" mod_Shift (event-exp-predicate 'release))
			     (list "a" mod_Shift (event-exp-predicate 'release)))
			    (list
			     (list "b" mod_Shift (event-exp-predicate 'press))
			     (list "a" mod_Shift (event-exp-predicate 'press))
			     (list "a" mod_Shift (event-exp-predicate 'release))
			     (list "b" mod_Shift (event-exp-predicate 'release)))
			    (list
			     (list "b" mod_Shift (event-exp-predicate 'press))
			     (list "a" mod_Shift (event-exp-predicate 'press))
			     (list "b" mod_Shift (event-exp-predicate 'release))
			     (list "a" mod_Shift (event-exp-predicate 'release))))
			   (event-exp-seq-parse
			    '((chord ("a" mod_Shift) ("b" mod_Shift)))))))
   ;; mixed
   (assert-true  (uim-bool
		  '(equal? (list
			    (list "a"
				  "b"
				  (list "c" (event-exp-predicate 'press))
				  (list "d" (event-exp-predicate 'press))
				  (list "e" (event-exp-predicate 'press))
				  (list "d" (event-exp-predicate 'release))
				  (list "e" (event-exp-predicate 'release)))
			    (list "a"
				  "b"
				  (list "c" (event-exp-predicate 'press))
				  (list "d" (event-exp-predicate 'press))
				  (list "e" (event-exp-predicate 'press))
				  (list "e" (event-exp-predicate 'release))
				  (list "d" (event-exp-predicate 'release)))
			    (list "b"
				  "a"
				  (list "c" (event-exp-predicate 'press))
				  (list "d" (event-exp-predicate 'press))
				  (list "e" (event-exp-predicate 'press))
				  (list "d" (event-exp-predicate 'release))
				  (list "e" (event-exp-predicate 'release)))
			    (list "b"
				  "a"
				  (list "c" (event-exp-predicate 'press))
				  (list "d" (event-exp-predicate 'press))
				  (list "e" (event-exp-predicate 'press))
				  (list "e" (event-exp-predicate 'release))
				  (list "d" (event-exp-predicate 'release))))
			   (event-exp-seq-parse
			    '((set "a" "b") ("c" press) (ordered-chord "d" "e"))))))))

(define-uim-test-case "testcase action expressions"
  (setup
   (lambda ()
     (uim '(begin
	     (require "evmap.scm")
	     (require "physical-key.scm")
	     (define test-ruletree (evmap-parse-ruleset
				    '(				      
				      ((("n" peek))      ("N"))
				      ((("n" peek))      (("N" loopback)))
				      (("k" "a")         ("KA"))
				      (("k" "i")         ("KI"))
				      (("k" "u")         ("KU"))
				      (("k" "k" "a")     ("KKA"))
				      (("k" "k" "u")     ("KKU"))
				      (("k" "k" "i")     ("KKI"))
				      (("k" "k")         ("KK"))
				      (("k" "k" "a" "e") ("KKAE"))
				      ;;((("n" char-upper-case))      ("N"))
				      )))))))

  ("test list-copy!"
   (uim '(define test-list-dst '(1 2 3 4 5)))
   (uim '(define test-list-src '((1) (2) (3) (4) (5))))
   (assert-false (uim-bool '(every eq? test-list-dst test-list-src)))
   (assert-false (uim-bool '(eq? test-list-dst test-list-src)))
   (uim '(list-copy! test-list-dst test-list-src))
   (assert-true  (uim-bool '(every eq? test-list-dst test-list-src)))
   (assert-false (uim-bool '(eq? test-list-dst test-list-src))))

  ("test action-exp-directive-positional-var"
   (uim '(define test-emc (evmap-context-new test-ruletree)))
   (uim '(define test-ev (key-event-new "j" 'lkey_j)))
   (uim '(define test-pos1 (action-exp-directive-positional-var 1)))
   (uim '(define test-pos2 (action-exp-directive-positional-var 2)))
   (uim '(define test-pos3 (action-exp-directive-positional-var 3)))
   (assert-false (uim-bool '(evmap-context-preedit-string test-emc)))

   ;;; first type
   (assert-true  (uim-bool '(evmap-context-input!
			     test-emc
			     (key-event-new "k" 'lkey_k 'pkey_qwerty_k))))
   (assert-equal '("k")
		 (uim '(evmap-context-preedit-string test-emc)))
   ;; positional var keeps all matched event information
   (assert-equal (uim '(list 'key 'drop-release #f -1 #f
			     "k" 'lkey_k 'pkey_qwerty_k mod_None #t #f))
		 (uim '(evmap-context-positional-var test-emc 1)))
   (assert-error (lambda ()
		   (uim '(evmap-context-positional-var test-emc 2))))
   (assert-true  (uim-bool '(test-pos1 test-emc test-ev)))
   (assert-equal (uim '(list 'key 'drop-release #f -1 #f
			     "k" 'lkey_k 'pkey_qwerty_k mod_None #t #f))
		 (uim 'test-ev))
   ;; action-exp-directive-positional-var returns equivalent to
   ;; evmap-context-positional-var
   (assert-true  (uim-bool '(equal? (evmap-context-positional-var test-emc 1)
				    test-ev)))
   ;; but the two objects are different and can be modified without
   ;; altering evmap-context
   (assert-false (uim-bool '(eq? (evmap-context-positional-var test-emc 1)
				 test-ev)))
   (assert-error (lambda ()
		   (uim '(test-pos2 test-emc test-ev))))

   ;;; second type
;;   (assert-true  (uim-bool '(evmap-context-input!
;;			     test-emc
;;			     (key-release-event-new "k"))))
;;   (assert-equal '("k")
;;		 (uim '(evmap-context-preedit-string test-emc)))

   (assert-true  (uim-bool '(evmap-context-input!
			     test-emc
			     (key-event-new "k"))))
   (assert-equal '("KK")
		 (uim '(evmap-context-preedit-string test-emc)))
   (assert-equal (uim '(list 'key 'drop-release #f -1 #f
			     "k" 'lkey_k 'pkey_qwerty_k mod_None #t #f))
		 (uim '(evmap-context-positional-var test-emc 1)))
   (assert-equal (uim '(list 'key 'drop-release #f -1 #f
			     "k" #f #f mod_None #t #f))
		 (uim '(evmap-context-positional-var test-emc 2)))
   (assert-error (lambda ()
		   (uim '(evmap-context-positional-var test-emc 3))))
   (assert-true  (uim-bool '(test-pos1 test-emc test-ev)))
   (assert-equal (uim '(list 'key 'drop-release #f -1 #f
			     "k" 'lkey_k 'pkey_qwerty_k mod_None #t #f))
		 (uim 'test-ev))
   (assert-true  (uim-bool '(equal? (evmap-context-positional-var test-emc 1)
				    test-ev)))
   (assert-false (uim-bool '(eq? (evmap-context-positional-var test-emc 1)
				 test-ev)))
   (assert-true  (uim-bool '(test-pos2 test-emc test-ev)))
   (assert-equal (uim '(list 'key 'drop-release #f -1 #f
			     "k" #f #f mod_None #t #f))
		 (uim 'test-ev))
   (assert-true  (uim-bool '(equal? (evmap-context-positional-var test-emc 2)
				    test-ev)))
   (assert-false (uim-bool '(eq? (evmap-context-positional-var test-emc 2)
				 test-ev)))
   (assert-error (lambda ()
		   (uim '(test-pos3 test-emc test-ev)))))

  ("test action-exp-collector-fold"
   (uim '(define test-alist (event-exp-collector-pred-alist
			     (action-exp-collector-new))))
   ;; null expression
   (assert-equal (uim '(list #f #f #f mod_None () test-alist))
		 (uim '(action-exp-collector-fold ())))
   ;; single element
   (assert-equal (uim '(list "a" #f #f mod_None () test-alist))
		 (uim '(action-exp-collector-fold "a")))
   (assert-equal (uim '(list #f 'lkey_a #f mod_None () test-alist))
		 (uim '(action-exp-collector-fold 'lkey_a)))
   (assert-equal (uim '(list #f #f 'pkey_qwerty_a mod_None () test-alist))
		 (uim '(action-exp-collector-fold 'pkey_qwerty_a)))
   (assert-equal (uim '(list #f #f #f mod_None () test-alist))
		 (uim '(action-exp-collector-fold 'mod_None)))
   (assert-equal (uim '(list #f #f #f mod_Shift () test-alist))
		 (uim '(action-exp-collector-fold 'mod_Shift)))
   (assert-true  (uim-bool '(equal? (list #f #f #f mod_None
					  (list
					   (action-exp-directive 'press))
					  test-alist)
				    (action-exp-collector-fold 'press))))
   (assert-true  (uim-bool '(equal? (list #f #f #f mod_None
					  (list
					   (action-exp-directive '$1))
					  test-alist)
				    (action-exp-collector-fold '$1))))
   ;; single element in list
   (assert-equal (uim '(list "a" #f #f mod_None () test-alist))
		 (uim '(action-exp-collector-fold '("a"))))
   (assert-equal (uim '(list #f 'lkey_a #f mod_None () test-alist))
		 (uim '(action-exp-collector-fold '(lkey_a))))
   (assert-equal (uim '(list #f #f 'pkey_qwerty_a mod_None () test-alist))
		 (uim '(action-exp-collector-fold '(pkey_qwerty_a))))
   (assert-equal (uim '(list #f #f #f mod_None () test-alist))
		 (uim '(action-exp-collector-fold '(mod_None))))
   (assert-equal (uim '(list #f #f #f mod_Shift () test-alist))
		 (uim '(action-exp-collector-fold '(mod_Shift))))
   (assert-true  (uim-bool '(equal? (list #f #f #f mod_None
					  (list
					   (action-exp-directive 'press))
					  test-alist)
				    (action-exp-collector-fold
				     '(press)))))
   (assert-true  (uim-bool '(equal? (list #f #f #f mod_None
					  (list
					   (action-exp-directive '$1))
					  test-alist)
				    (action-exp-collector-fold
				     '($1)))))
   ;; (with) modifiers
   (assert-equal (uim '(list "a" #f #f (bitwise-or mod_Shift
						   mod_Control_L
						   mod_Alt
						   mod_ignore_Super)
			     () test-alist))
		 (uim '(action-exp-collector-fold
			'("a"
			  mod_Control_L
			  mod_Shift
			  mod_ignore_Super
			  mod_Alt))))
   (assert-equal (uim '(list #f 'lkey_a #f (bitwise-or mod_Shift
						       mod_Control_L
						       mod_Alt
						       mod_ignore_Super)
			     () test-alist))
		 (uim '(action-exp-collector-fold '(lkey_a
						    mod_Control_L
						    mod_Shift
						    mod_ignore_Super
						    mod_Alt))))
   (assert-equal (uim '(list #f #f 'pkey_qwerty_a (bitwise-or mod_Shift
							      mod_Control_L
							      mod_Alt
							      mod_ignore_Super)
			     () test-alist))
		 (uim '(action-exp-collector-fold '(pkey_qwerty_a
						    mod_Control_L
						    mod_Shift
						    mod_ignore_Super
						    mod_Alt))))
   (assert-equal (uim '(list #f #f #f (bitwise-or mod_Control_L
						  mod_Shift
						  mod_ignore_Super
						  mod_Alt)
			     () test-alist))
		 (uim '(action-exp-collector-fold '(mod_None
						    mod_Control_L
						    mod_Shift
						    mod_ignore_Super
						    mod_Alt))))
   (assert-equal (uim '(list #f #f #f (bitwise-or mod_Control_L
						  mod_Shift
						  mod_ignore_Super
						  mod_Alt)
			     () test-alist))
		 (uim '(action-exp-collector-fold
			'(mod_Control_L
			  mod_Shift
			  mod_ignore_Super
			  mod_Alt))))
   (assert-true  (uim-bool '(equal? (list #f #f #f (bitwise-or mod_Control_L
							       mod_Shift
							       mod_ignore_Super
							       mod_Alt)
					  (list
					   (action-exp-directive 'press))
					  test-alist)
				    (action-exp-collector-fold
				      '(press
					mod_Control_L
					mod_Shift
					mod_ignore_Super
					mod_Alt)))))
   (assert-true  (uim-bool '(equal? (list #f #f #f (bitwise-or mod_Control_L
							       mod_Shift
							       mod_ignore_Super
							       mod_Alt)
					  (list
					   (action-exp-directive 'autorepeat))
					  test-alist)
				    (action-exp-collector-fold
				      '(autorepeat
					mod_Control_L
					mod_Shift
					mod_ignore_Super
					mod_Alt)))))
   (assert-true  (uim-bool '(equal? (list #f #f #f (bitwise-or mod_Control_L
							       mod_Shift
							       mod_ignore_Super
							       mod_Alt)
					  (list
					   (action-exp-directive 'loopback))
					  test-alist)
				    (action-exp-collector-fold
				     '(loopback
				       mod_Control_L
				       mod_Shift
				       mod_ignore_Super
				       mod_Alt)))))
   ;; complex expression
   (assert-equal (uim '(list "A" 'lkey_a #f mod_Shift () test-alist))
		 (uim '(action-exp-collector-fold
			'(mod_Shift lkey_a "A"))))
   (assert-true  (uim-bool '(equal? (list "A"
					  'lkey_a
					  'pkey_qwerty_a
					  (bitwise-or mod_Control_L
						      mod_Shift
						      mod_ignore_Super
						      mod_Alt)
					  (list
					   (action-exp-directive '$2)
					   (action-exp-directive 'nonrepeat)
					   (action-exp-directive 'press)
					   (action-exp-directive 'loopback)
					   (action-exp-directive 'char-downcase))
					  test-alist)
				    (action-exp-collector-fold
				     '(char-downcase
				       mod_Control_L
				       loopback
				       mod_Shift
				       press
				       "A"
				       nonrepeat
				       pkey_qwerty_a
				       mod_ignore_Super
				       lkey_a
				       $2
				       mod_Alt))))))

  ("test action-exp-seq-parse"
   ;; null expression
   (assert-equal ()
		 (uim '(action-exp-seq-parse ())))
   ;; single element
   (assert-equal '("a")
		 (uim '(action-exp-seq-parse '("a"))))
   (assert-equal '(lkey_a)
		 (uim '(action-exp-seq-parse '(lkey_a))))
   (assert-equal '(pkey_qwerty_a)
		 (uim '(action-exp-seq-parse '(pkey_qwerty_a))))
   (assert-equal (uim '(list #f))
		 (uim '(action-exp-seq-parse '(mod_None))))
   (assert-equal (uim '(list mod_Shift))
		 (uim '(action-exp-seq-parse '(mod_Shift))))
   (assert-true  (uim-bool '(equal? (list
				     (action-exp-directive 'press))
				    (action-exp-seq-parse '(press)))))
   (assert-true  (uim-bool '(equal? (list
				     (action-exp-directive '$1))
				    (action-exp-seq-parse '($1)))))
   ;; multiple elements
   (assert-equal '("a" "b")
		 (uim '(action-exp-seq-parse '("a" "b"))))
   (assert-equal '(lkey_a lkey_b)
		 (uim '(action-exp-seq-parse '(lkey_a lkey_b))))
   (assert-equal '(pkey_qwerty_a pkey_qwerty_b)
		 (uim '(action-exp-seq-parse '(pkey_qwerty_a pkey_qwerty_b))))
   (assert-equal (uim '(list mod_Shift mod_Control))
		 (uim '(action-exp-seq-parse '(mod_Shift mod_Control))))
   (assert-true  (uim-bool '(equal? (list
				     (action-exp-directive 'press)
				     (action-exp-directive 'release))
				    (action-exp-seq-parse '(press release)))))
   (assert-true  (uim-bool '(equal? (list
				     (action-exp-directive '$1)
				     (action-exp-directive '$2))
				    (action-exp-seq-parse '($1 $2)))))
   ;; (with) modifiers
   (assert-equal (uim '(list
			(list "a"(bitwise-or mod_Shift
					     mod_Control_L
					     mod_Alt
					     mod_ignore_Super))))
		 (uim '(action-exp-seq-parse
			'(("a"
			   mod_Control_L
			   mod_Shift
			   mod_ignore_Super
			   mod_Alt)))))
   (assert-equal (uim '(list
			(list 'lkey_a (bitwise-or mod_Shift
						  mod_Control_L
						  mod_Alt
						  mod_ignore_Super))))
		 (uim '(action-exp-seq-parse '((lkey_a
						mod_Control_L
						mod_Shift
						mod_ignore_Super
						mod_Alt)))))
   (assert-equal (uim '(list
			(list 'pkey_qwerty_a (bitwise-or mod_Shift
							 mod_Control_L
							 mod_Alt
							 mod_ignore_Super))))
		 (uim '(action-exp-seq-parse '((pkey_qwerty_a
						mod_Control_L
						mod_Shift
						mod_ignore_Super
						mod_Alt)))))
   (assert-equal (uim '(list
			(bitwise-or mod_Control_L
				    mod_Shift
				    mod_ignore_Super
				    mod_Alt)))
		 (uim '(action-exp-seq-parse '((mod_None
						mod_Control_L
						mod_Shift
						mod_ignore_Super
						mod_Alt)))))
   (assert-equal (uim '(list
			(bitwise-or mod_Control_L
				    mod_Shift
				    mod_ignore_Super
				    mod_Alt)))
		 (uim '(action-exp-seq-parse '((mod_Control_L
						mod_Shift
						mod_ignore_Super
						mod_Alt)))))
   (assert-true  (uim-bool '(equal? (list
				     (list (bitwise-or mod_Control_L
						       mod_Shift
						       mod_ignore_Super
						       mod_Alt)
					   (action-exp-directive 'press)))
				    (action-exp-seq-parse
				     '((press
					mod_Control_L
					mod_Shift
					mod_ignore_Super
					mod_Alt))))))
   (assert-true  (uim-bool '(equal? (list
				     (list (bitwise-or mod_Control_L
						       mod_Shift
						       mod_ignore_Super
						       mod_Alt)
					   (action-exp-directive 'autorepeat)))
				    (action-exp-seq-parse
				     '((autorepeat
					mod_Control_L
					mod_Shift
					mod_ignore_Super
					mod_Alt))))))
   (assert-true  (uim-bool '(equal? (list
				     (list (bitwise-or mod_Control_L
						       mod_Shift
						       mod_ignore_Super
						       mod_Alt)
					   (action-exp-directive 'loopback)))
				    (action-exp-seq-parse
				     '((loopback
					mod_Control_L
					mod_Shift
					mod_ignore_Super
					mod_Alt))))))
   ;; complex expression
   (assert-equal (uim '(list
			(list "A" 'lkey_a mod_Shift)))
		 (uim '(action-exp-seq-parse
			'((mod_Shift lkey_a "A")))))
   (assert-true  (uim-bool '(equal? (list
				     (list "A"
					   'lkey_a
					   'pkey_qwerty_a
					   (bitwise-or mod_Control_L
						       mod_Shift
						       mod_ignore_Super
						       mod_Alt)
					   (action-exp-directive '$2)
					   (action-exp-directive 'press)
					   (action-exp-directive 'nonrepeat)
					   (action-exp-directive 'char-downcase)
					   (action-exp-directive 'loopback)))
				    (action-exp-seq-parse
				     '((char-downcase
					mod_Control_L
					loopback
					mod_Shift
					press
					"A"
					nonrepeat
					pkey_qwerty_a
					mod_ignore_Super
					lkey_a
					$2
					mod_Alt)))))))

  ("test action-exp-seq-extract"
   (uim '(define test-emc (evmap-context-new test-ruletree)))
   (assert-false (uim-bool '(evmap-context-preedit-string test-emc)))

   (assert-true  (uim-bool '(evmap-context-input!
			     test-emc
			     (key-event-new "k" 'lkey_k 'pkey_qwerty_k))))
;;   (assert-true  (uim-bool '(evmap-context-input!
;;			     test-emc
;;			     (key-release-event-new "k"))))
   (assert-true  (uim-bool '(evmap-context-input!
			     test-emc
			     (key-event-new "k"))))
;;   (assert-true  (uim-bool '(evmap-context-input!
;;			     test-emc
;;			     (key-release-event-new "k"))))
   (assert-true  (uim-bool '(evmap-context-input!
			     test-emc
			     (key-event-new "u"))))
;;   (assert-true  (uim-bool '(evmap-context-input!
;;			     test-emc
;;			     (key-release-event-new "u" 'lkey_u 'pkey_qwerty_u))))
   ;; null expression
   (assert-false (uim-bool '(action-exp-seq-extract ()
						    test-emc)))
   ;; simple expression
   (assert-equal "kkU"
		 (uim '(action-exp-seq-extract "kkU"
					       test-emc)))
   (assert-equal 'lkey_a
		 (uim '(action-exp-seq-extract 'lkey_a
					       test-emc)))
   (assert-equal 'pkey_qwerty_a
		 (uim '(action-exp-seq-extract 'pkey_qwerty_a
					       test-emc)))
   (assert-equal (uim 'mod_Shift)
		 (uim '(action-exp-seq-extract mod_Shift
					       test-emc)))
   ;; simple expression in list
   (assert-equal '("kkU")
		 (uim '(action-exp-seq-extract '("kkU")
					       test-emc)))
   (assert-equal '(lkey_a)
		 (uim '(action-exp-seq-extract '(lkey_a)
					       test-emc)))
   (assert-equal '(pkey_qwerty_a)
		 (uim '(action-exp-seq-extract '(pkey_qwerty_a)
					       test-emc)))
   (assert-equal (uim '(list mod_Shift))
		 (uim '(action-exp-seq-extract (list mod_Shift)
					       test-emc)))
   ;; simple expressions in list
   (assert-equal '("kkU" "a" "s")
		 (uim '(action-exp-seq-extract '("kkU" "a" "s")
					       test-emc)))
   (assert-equal '(lkey_a lkey_b lkey_c)
		 (uim '(action-exp-seq-extract '(lkey_a lkey_b lkey_c)
					       test-emc)))
   (assert-equal '(pkey_qwerty_a pkey_qwerty_c)
		 (uim '(action-exp-seq-extract '(pkey_qwerty_a pkey_qwerty_c)
					       test-emc)))
   (assert-equal (uim '(list mod_Shift mod_Control))
		 (uim '(action-exp-seq-extract (list mod_Shift mod_Control)
					       test-emc)))
   (assert-equal (uim '(list "kkU" 'lkey_s mod_Shift "s"))
		 (uim '(action-exp-seq-extract (list "kkU" 'lkey_s mod_Shift "s")
					       test-emc)))
   ;; complex expressions
   (assert-equal (uim '(list (list 'key #f #f -1 #f
				   "kkU" 'lkey_a #f mod_None #t #f)))
		 (uim '(action-exp-seq-extract '(("kkU" lkey_a))
					       test-emc)))
   (assert-equal (uim '(list (list 'key #f #f -1 #f
				   "kkU" 'lkey_a #f mod_Shift #t #f)))
		 (uim '(action-exp-seq-extract (list
						(list "kkU" 'lkey_a mod_Shift))
					       test-emc)))
   (assert-equal (uim '(list (list 'key #f #f -1 #f
				   "kkU" 'lkey_a #f mod_None #t #f)))
		 (uim '(action-exp-seq-extract (list
						(list "kkU"
						      'lkey_a
						      (action-exp-directive
						       'press)))
					       test-emc)))
   (assert-equal (uim '(list (list 'key #f #f -1 #f
				   "kkU" 'lkey_a #f mod_None #f #f)))
		 (uim '(action-exp-seq-extract (list
						(list "kkU"
						      'lkey_a
						      (action-exp-directive
						       'release)))
					       test-emc)))
   (assert-equal (uim '(list (list 'key #f #t -1 #f
				   "kkU" 'lkey_a #f mod_None #t #f)))
		 (uim '(action-exp-seq-extract (list
						(list "kkU"
						      'lkey_a
						      (action-exp-directive
						       'loopback)))
					       test-emc)))
   (assert-equal (uim '(list (list 'key #f #f -1 #f
				   "kkU" 'lkey_a #f mod_None #t #f)))
		 (uim '(action-exp-seq-extract (list
						(list "kkU"
						      'lkey_a
						      (action-exp-directive
						       'return)))
					       test-emc)))
   (assert-equal (uim '(list (list 'key #f #f -1 #f
				   "S" 'lkey_s #f mod_None #t #f)))
		 (uim '(action-exp-seq-extract (list
						(list "s"
						      'lkey_s
						      (action-exp-directive
						       'char-upcase)))
					       test-emc)))
   (assert-equal (uim '(list (list 'key #f #f -1 #f
				   "s" 'lkey_s #f mod_None #t #f)))
		 (uim '(action-exp-seq-extract (list
						(list "S"
						      'lkey_s
						      (action-exp-directive
						       'char-downcase)))
					       test-emc)))
   ;; sequence of actions
   (assert-equal (uim '(list "a"
			     (list 'key #f #f -1 #f
				   "kkU" 'lkey_a #f mod_None #f #f)
			     'lkey_s
			     (list 'key #f #f -1 #f
				   "A" #f 'pkey_qwerty_a
				   (bitwise-or mod_Shift
					       mod_Control_L)
				   #t #f)))
		 (uim '(action-exp-seq-extract (list
						"a"
						(list "kkU"
						      'lkey_a
						      (action-exp-directive
						       'release))
						'lkey_s
						(list "A"
						      'pkey_qwerty_a
						      (bitwise-or
						       mod_Shift
						       mod_Control_L)))
					       test-emc)))
   ;; positional variables
   (assert-equal (uim '(list (list 'key 'drop-release #f -1 #f
				   "k" 'lkey_k 'pkey_qwerty_k mod_None #t #f)))
		 (uim '(action-exp-seq-extract (list
						(action-exp-directive '$1))
					       test-emc)))
;;   (assert-equal (uim '(list (list 'key 'drop-release #f -1 #f
;;				   "k" #f #f mod_None #f #f)))
;;		 (uim '(action-exp-seq-extract (list
;;						(action-exp-directive '$2))
;;					       test-emc)))
   (assert-equal (uim '(list (list 'key 'drop-release #f -1 #f
				   "k" #f #f mod_None #t #f)))
		 (uim '(action-exp-seq-extract (list
						(action-exp-directive '$2))
					       test-emc)))
;;   (assert-equal (uim '(list (list 'key 'drop-release #f -1 #f
;;				   "k" #f #f mod_None #f #f)))
;;		 (uim '(action-exp-seq-extract (list
;;						(action-exp-directive '$4))
;;					       test-emc)))
   (assert-equal (uim '(list (list 'key 'drop-release #f -1 #f
				   "u" #f #f mod_None #t #f)))
		 (uim '(action-exp-seq-extract (list
						(action-exp-directive '$3))
					       test-emc)))
;;   (assert-equal (uim '(list (list 'key 'drop-release #f -1 #f
;;				   "u" 'lkey_u 'pkey_qwerty_u mod_None #f #f)))
;;		 (uim '(action-exp-seq-extract (list
;;						(action-exp-directive '$6))
;;					       test-emc)))
   (assert-error (lambda ()
		   (uim '(action-exp-seq-extract (list
						  (action-exp-directive '$4))
						 test-emc))))

   ;; multiple positional variables
   (assert-equal (uim '(list (list 'key 'drop-release #f -1 #f
				   "k" 'lkey_k 'pkey_qwerty_k mod_None #t #f)
			     (list 'key 'drop-release #f -1 #f
				   "k" #f #f mod_None #t #f)))
		 (uim '(action-exp-seq-extract (list
						(action-exp-directive '$1)
						(action-exp-directive '$2))
					       test-emc)))
   (assert-equal (uim '(list (list 'key 'drop-release #f -1 #f
				   "k" 'lkey_k 'pkey_qwerty_k mod_None #t #f)
			     "a"
			     'lkey_d
			     (list 'key 'drop-release #f -1 #f
				   "k" #f #f mod_None #t #f)))
		 (uim '(action-exp-seq-extract (list
						(action-exp-directive '$1)
						"a"
						'lkey_d
						(action-exp-directive '$2))
					       test-emc)))
   ;; transposed
   (assert-equal (uim '(list (list 'key 'drop-release #f -1 #f
				   "k" #f #f mod_None #t #f)
			     (list 'key 'drop-release #f -1 #f
				   "k" 'lkey_k 'pkey_qwerty_k mod_None #t #f)))
		 (uim '(action-exp-seq-extract (list
						(action-exp-directive '$2)
						(action-exp-directive '$1))
					       test-emc)))
   ;; duplicated
   (assert-equal (uim '(list (list 'key 'drop-release #f -1 #f
				   "k" #f #f mod_None #t #f)
			     (list 'key 'drop-release #f -1 #f
				   "k" 'lkey_k 'pkey_qwerty_k mod_None #t #f)
			     (list 'key 'drop-release #f -1 #f
				   "k" 'lkey_k 'pkey_qwerty_k mod_None #t #f)))
		 (uim '(action-exp-seq-extract (list
						(action-exp-directive '$2)
						(action-exp-directive '$1)
						(action-exp-directive '$1))
					       test-emc)))
   ;; overwrite some elements
   (assert-equal (uim '(list (list 'key 'drop-release #t -1 #f
				   "k" 'lkey_a 'pkey_qwerty_k mod_Shift #t #f)
			     (list 'key 'drop-release #f -1 #f
				   "k" 'lkey_k 'pkey_qwerty_k mod_None #t #f)))
		 (uim '(action-exp-seq-extract (list
						(list
						 'lkey_a
						 mod_Shift
						 (action-exp-directive '$1)
						 (action-exp-directive
						  'loopback))
						(action-exp-directive '$1))
					       test-emc)))
   (assert-equal (uim '(list (list 'key 'drop-release #f -1 #f
				   "K" 'lkey_k 'pkey_qwerty_k mod_None #t #f)))
		 (uim '(action-exp-seq-extract (list
						(list
						 (action-exp-directive '$1)
						 (action-exp-directive
						  'char-upcase)))
					       test-emc)))
   ;; peek
   (uim '(define test-emc (evmap-context-new test-ruletree)))
   (assert-true  (uim-bool '(evmap-context-input!
			     test-emc
			     (key-event-new "n" 'lkey_n))))
;;   (assert-true  (uim-bool '(evmap-context-input!
;;			     test-emc
;;			     (key-release-event-new "n" 'lkey_n))))
   ;; peek match stores a dummy event to the result. so the positional
   ;; var don't contain "n" and 'lkey_n
   (assert-equal (uim '(list (list 'key #f #f -1 #f
				   #f #f #f mod_None #t #f)))
		 (uim '(action-exp-seq-extract (list
						(action-exp-directive '$1))
					       test-emc)))
   (assert-equal (uim '(list (list 'key #f #t -1 #f
				   "N" #f #f mod_None #t #f)))
		 (uim '(action-exp-seq-extract (list
						(list
						 "N"
						 (action-exp-directive '$1)
						 (action-exp-directive
						  'loopback)))
					       test-emc)))
   )

)

(define-uim-test-case "testcase evmap-tree"
  (setup
   (lambda ()
     (uim '(require "evmap.scm"))
     (uim '(require "physical-key.scm"))))

  ("test evmap-tree-leaf?"
   (assert-true  (uim-bool '(evmap-tree-leaf? (evmap-tree-new))))
   (assert-true  (uim-bool '(evmap-tree-leaf? (evmap-tree-new "a" '("ab")))))
   (uim '(define test-branch (evmap-tree-new "c" '("cd"))))
   (assert-false (uim-bool '(evmap-tree-leaf? (evmap-tree-new "a"
							      '("ab")
							      (list
							       test-branch)))))
   
   (uim '(define test-root (evmap-tree-new "a" '("ab"))))
   (assert-true  (uim-bool '(evmap-tree-leaf? test-root)))
   (uim '(evmap-tree-insert-node! test-root test-branch))
   (assert-false (uim-bool '(evmap-tree-leaf? test-root))))

  ("test evmap-tree-node?"
   (assert-false (uim-bool '(evmap-tree-node? (evmap-tree-new))))
   (assert-false (uim-bool '(evmap-tree-node? (evmap-tree-new "a"
							      '("ab")))))
   (uim '(define test-branch (evmap-tree-new "c" '("cd"))))
   (assert-true  (uim-bool '(evmap-tree-node? (evmap-tree-new "a"
							      '("ab")
							      (list
							       test-branch)))))
   
   (uim '(define test-root (evmap-tree-new "a" '("ab"))))
   (assert-false (uim-bool '(evmap-tree-node? test-root)))
   (uim '(evmap-tree-insert-node! test-root test-branch))
   (assert-true  (uim-bool '(evmap-tree-node? test-root))))

  ("test evmap-tree-find-branches"
   (uim '(define test-root (evmap-tree-new "k" '("k"))))
   (uim '(define test-kk-node (evmap-tree-new "k" '("kk"))))
   (uim '(evmap-tree-insert-node! test-root
				  (evmap-tree-new "a" '("ka"))))
   (uim '(evmap-tree-insert-node! test-root
				  (evmap-tree-new "i" '("ki"))))
   (uim '(evmap-tree-insert-node! test-root
				  (evmap-tree-new "u" '("ku"))))
   (uim '(evmap-tree-insert-node! test-root test-kk-node))
   (uim '(evmap-tree-insert-node! test-kk-node
				  (evmap-tree-new "a" '("kka"))))
   (uim '(evmap-tree-insert-node! test-kk-node
				  (evmap-tree-new "i" '("kki"))))
   (uim '(evmap-tree-insert-node! test-kk-node
				  (evmap-tree-new "u" '("kku"))))
   (assert-equal 2
		 (uim '(length (evmap-tree-find-branches
				test-root
				(key-event-new "i")))))
   (assert-equal 2
		 (uim '(length (evmap-tree-find-branches
				test-root
				"i"
				equal?))))
   (assert-equal '("i" "a")
		 (uim '(map evmap-tree-event
			    (evmap-tree-find-branches
			     test-root
			     (key-event-new "i")))))
   (assert-equal 4
		 (uim '(length (evmap-tree-find-branches
				test-root
				(key-event-new "k")))))
   (assert-equal 4
		 (uim '(length (evmap-tree-find-branches
				test-root
				"k"
				equal?))))
   (assert-equal '("k" "u" "i" "a")
		 (uim '(map evmap-tree-event
			    (evmap-tree-find-branches
			     test-root
			     (key-event-new "k")))))
   (assert-equal '("k" "u" "i" "a")
		 (uim '(map evmap-tree-event
			    (evmap-tree-find-branches
			     test-root
			     "k"
			     equal?))))
   (assert-true  (uim-bool '(eq? (car (evmap-tree-find-branches
				       test-root
				       (key-event-new "k")))
				 test-kk-node)))
   (assert-true  (uim-bool '(eq? (car (evmap-tree-find-branches
				       test-root
				       "k"
				       equal?))
				 test-kk-node))))

  ("test evmap-tree-insert-node!"
   (uim '(define test-root (evmap-tree-new "k" '("k"))))
   (uim '(define test-kk-node (evmap-tree-new "k" '("kk"))))
   (uim '(define test-a-leaf (evmap-tree-new "a" '("ka"))))
   (uim '(define test-i-leaf (evmap-tree-new "i" '("ki"))))
   (uim '(define test-u-leaf (evmap-tree-new "u" '("ku"))))
   (assert-equal 0
		 (uim '(length (evmap-tree-branches test-root))))

   (uim '(evmap-tree-insert-node! test-root test-a-leaf))
   (assert-equal 1
		 (uim '(length (evmap-tree-branches test-root))))
   (assert-true  (uim-bool '(equal? (list test-a-leaf)
				    (evmap-tree-branches test-root))))

   (uim '(evmap-tree-insert-node! test-root test-i-leaf))
   (assert-equal 2
		 (uim '(length (evmap-tree-branches test-root))))
   (assert-true  (uim-bool '(equal? (list test-i-leaf
					  test-a-leaf)
				    (evmap-tree-branches test-root))))

   (uim '(evmap-tree-insert-node! test-root test-u-leaf))
   (assert-equal 3
		 (uim '(length (evmap-tree-branches test-root))))
   (assert-true  (uim-bool '(equal? (list test-u-leaf
					  test-i-leaf
					  test-a-leaf)
				    (evmap-tree-branches test-root))))

   (uim '(evmap-tree-insert-node! test-kk-node
				  (evmap-tree-new "a" '("kka"))))
   (uim '(evmap-tree-insert-node! test-kk-node
				  (evmap-tree-new "i" '("kki"))))
   (uim '(evmap-tree-insert-node! test-kk-node
				  (evmap-tree-new "u" '("kku"))))
   (uim '(evmap-tree-insert-node! test-root test-kk-node))
   (assert-equal 4
		 (uim '(length (evmap-tree-branches test-root))))
   (assert-true  (uim-bool '(equal? (list test-kk-node
					  test-u-leaf
					  test-i-leaf
					  test-a-leaf)
				    (evmap-tree-branches test-root)))))

  ("test evmap-tree-insert-rule!"
   (uim '(define test-root (evmap-tree-new)))
   (assert-equal (uim ''(#f #f
			    ()))
		 (uim 'test-root))

   (uim '(evmap-tree-insert-rule! test-root '("k" "a") '("ka")))
   (assert-equal (uim ''(#f #f
			    (("k" #f
			          (("a" ("ka") ()))))))
		 (uim 'test-root))

   (uim '(evmap-tree-insert-rule! test-root '("k" "i") '("ki")))
   (assert-equal (uim ''(#f #f
			    (("k" #f
			          (("i" ("ki") ())
				   ("a" ("ka") ()))))))
		 (uim 'test-root))

   (uim '(evmap-tree-insert-rule! test-root '("k" "u") '("ku")))
   (assert-equal (uim ''(#f #f
			    (("k" #f
			          (("u" ("ku") ())
				   ("i" ("ki") ())
				   ("a" ("ka") ()))))))
		 (uim 'test-root))

   (uim '(evmap-tree-insert-rule! test-root '("k" "k" "a") '("kka")))
   (assert-equal (uim ''(#f #f
			    (("k" #f
			          (("k" #f
				        (("a" ("kka") ())))
				   ("u" ("ku") ())
				   ("i" ("ki") ())
				   ("a" ("ka") ()))))))
		 (uim 'test-root))

   (uim '(evmap-tree-insert-rule! test-root '("k" "k" "u") '("kku")))
   (assert-equal (uim ''(#f #f
			    (("k" #f
			          (("k" #f
				        (("u" ("kku") ())
					 ("a" ("kka") ())))
				   ("u" ("ku") ())
				   ("i" ("ki") ())
				   ("a" ("ka") ()))))))
		 (uim 'test-root))

   (uim '(evmap-tree-insert-rule! test-root '("k" "k" "i") '("kki")))
   (assert-equal (uim ''(#f #f
			    (("k" #f
			          (("k" #f
				        (("i" ("kki") ())
					 ("u" ("kku") ())
					 ("a" ("kka") ())))
				   ("u" ("ku") ())
				   ("i" ("ki") ())
				   ("a" ("ka") ()))))))
		 (uim 'test-root))

   ;; node actions can be filled after branched
   (uim '(evmap-tree-insert-rule! test-root '("k" "k")     '("kk")))
   (assert-equal (uim ''(#f #f
			    (("k" #f
			          (("k" ("kk")
				        (("i" ("kki") ())
					 ("u" ("kku") ())
					 ("a" ("kka") ())))
				   ("u" ("ku") ())
				   ("i" ("ki") ())
				   ("a" ("ka") ()))))))
		 (uim 'test-root))

   ;; node actions can be filled after branched
   (uim '(evmap-tree-insert-rule! test-root '("k")         '("k")))
   (assert-equal (uim ''(#f #f
			    (("k" ("k")
			          (("k" ("kk")
				        (("i" ("kki") ())
					 ("u" ("kku") ())
					 ("a" ("kka") ())))
				   ("u" ("ku") ())
				   ("i" ("ki") ())
				   ("a" ("ka") ()))))))
		 (uim 'test-root))

   ;; actions can be overwritten
   (uim '(evmap-tree-insert-rule! test-root '("k")         '("K")))
   (assert-equal (uim ''(#f #f
			    (("k" ("K")
			          (("k" ("kk")
				        (("i" ("kki") ())
					 ("u" ("kku") ())
					 ("a" ("kka") ())))
				   ("u" ("ku") ())
				   ("i" ("ki") ())
				   ("a" ("ka") ()))))))
		 (uim 'test-root))

   ;; depth 4
   (uim '(evmap-tree-insert-rule! test-root '("k" "k" "a" "e") '("kkae")))
   (assert-equal (uim ''(#f #f
			    (("k" ("K")
			          (("k" ("kk")
				        (("i" ("kki") ())
					 ("u" ("kku") ())
					 ("a" ("kka")
					      (("e" ("kkae") ())))))
				   ("u" ("ku") ())
				   ("i" ("ki") ())
				   ("a" ("ka") ()))))))
		 (uim 'test-root)))

  ("test evmap-parse-ruleset"
   (uim '(define test-ruletree1 (evmap-parse-ruleset
				 '((("k")             ("K"))))))
   (uim '(define test-ruletree2 (evmap-parse-ruleset
				 '((("k" "a")         ("ka"))
				   (("k" "i")         ("ki"))
				   (("k" "u")         ("ku"))
				   (("k" "k" "a")     ("kka"))
				   (("k" "k" "u")     ("kku"))
				   (("k" "k" "i")     ("kki"))
				   (("k" "k")         ("kk"))
				   (("k")             ("k"))
				   (("k")             ("K"))
				   (("k" "k" "a" "e") ("kkae"))))))
   (uim '(define test-ruletree3 (evmap-parse-ruleset
				 '((("n" "n")                  ("ん"))
				   (("n" (char-nonvowel peek))
				    ("ん" ($2 loopback)))))))
   (uim '(define test-ruletree4 (evmap-parse-ruleset
				 '(((pkey_jp106_x (chord lkey_Thumb_Shift_R pkey_jp106_bracketleft))
				    ("ぴ"))
				   (((chord lkey_Thumb_Shift_L pkey_jp106_s)) ("あ"))))))
   (uim '(define test-press (event-exp-predicate 'press)))
   (uim '(define test-release (event-exp-predicate 'release)))

;;   (assert-true  (uim-bool '(equal?
;;			     (list #f #f
;;				      (list
;;				       (list
;;					(list "k" test-press) #f
;;				                              (list
;;							       (list
;;								(list "k" test-release) '("K") ())))))
;;			     test-ruletree1)))
   (assert-true  (uim-bool '(equal?
			     (list #f #f
				      (list
				       (list "k" '("K") ())))
			     test-ruletree1)))

;;   (assert-true  (uim-bool (equal? (#f #f
;;			    (("k" ("K")
;;			          (("k" ("kk")
;;				        (("i" ("kki") ())
;;					 ("u" ("kku") ())
;;					 ("a" ("kka")
;;					      (("e" ("kkae") ())))))
;;				   ("u" ("ku") ())
;;				   ("i" ("ki") ())
;;				   ("a" ("ka") ()))))))
;;		 (uim 'test-ruletree2)))
   (assert-true  (uim-bool '(evmap-parse-ruleset
			     '((("k" "k" "a") ("っ" "か"))))))
   ))

(define-uim-test-case "testcase evmap-context"
  (setup
   (lambda ()
     (uim '(begin
	     (require "evmap.scm")
	     (define test-ruletree (evmap-parse-ruleset
				    '((("k" "a")         ("KA"))
				      (("k" "i")         ("KI"))
				      (("k" "u")         ("KU"))
				      (("k" "k" "a")     ("KKA"))
				      (("k" "k" "u")     ("KKU"))
				      (("k" "k" "i")     ("KKI"))
				      (("k" "k")         ("KK"))
				      (("k" "k" "a" "e") ("KKAE")))))))))

  ("test evmap-context-complete?"
   (uim '(define test-emc (evmap-context-new test-ruletree)))
   (assert-false (uim-bool '(evmap-context-complete? test-emc)))

   (assert-true  (uim-bool '(evmap-context-input! test-emc
						  (key-event-new "k"))))
   (assert-false (uim-bool '(evmap-context-complete? test-emc)))

;;   (assert-true  (uim-bool '(evmap-context-input! test-emc
;;						  (key-release-event-new "k"))))
;;   (assert-false (uim-bool '(evmap-context-complete? test-emc)))

   (assert-true  (uim-bool '(evmap-context-input! test-emc
						  (key-event-new "k"))))
   (assert-false (uim-bool '(evmap-context-complete? test-emc)))

;;   (assert-true  (uim-bool '(evmap-context-input! test-emc
;;						  (key-release-event-new "k"))))
;;   (assert-false (uim-bool '(evmap-context-complete? test-emc)))

   (assert-true  (uim-bool '(evmap-context-input! test-emc
						  (key-event-new "i"))))
;;   (assert-false (uim-bool '(evmap-context-complete? test-emc)))

;;   (assert-true  (uim-bool '(evmap-context-input! test-emc
;;						  (key-release-event-new "i"))))
   (assert-true  (uim-bool '(evmap-context-complete? test-emc)))

   (assert-false (uim-bool '(evmap-context-input! test-emc
						  (key-event-new "k"))))
   (assert-true  (uim-bool '(evmap-context-complete? test-emc))))

  ("test evmap-context-partial?"
   (uim '(define test-emc (evmap-context-new test-ruletree)))
   (assert-false (uim-bool '(evmap-context-partial? test-emc)))

   (assert-true  (uim-bool '(evmap-context-input! test-emc
						  (key-event-new "k"))))
   (assert-false (uim-bool '(evmap-context-partial? test-emc)))

;;   (assert-true  (uim-bool '(evmap-context-input! test-emc
;;						  (key-release-event-new "k"))))
;;   (assert-false (uim-bool '(evmap-context-partial? test-emc)))

   (assert-true  (uim-bool '(evmap-context-input! test-emc
						  (key-event-new "k"))))
;;   (assert-false (uim-bool '(evmap-context-partial? test-emc)))

;;   (assert-true  (uim-bool '(evmap-context-input! test-emc
;;						  (key-release-event-new "k"))))
   (assert-true  (uim-bool '(evmap-context-partial? test-emc)))

   (assert-true  (uim-bool '(evmap-context-input! test-emc
						  (key-event-new "i"))))
   (assert-false (uim-bool '(evmap-context-partial? test-emc)))

;;   (assert-true  (uim-bool '(evmap-context-input! test-emc
;;						  (key-release-event-new "i"))))
;;   (assert-false (uim-bool '(evmap-context-partial? test-emc)))

   (assert-false (uim-bool '(evmap-context-input! test-emc
						  (key-event-new "k"))))
   (assert-false (uim-bool '(evmap-context-partial? test-emc))))

  ("test evmap-context-event-seq-string"
   (uim '(define test-emc (evmap-context-new test-ruletree)))
   (assert-equal ()
		 (uim '(evmap-context-event-seq-string test-emc)))

   (assert-true  (uim-bool '(evmap-context-input! test-emc
						  (key-event-new "k"))))
   (assert-equal '("k")
		 (uim '(evmap-context-event-seq-string test-emc)))

;;   (assert-true  (uim-bool '(evmap-context-input! test-emc
;;						  (key-release-event-new "k"))))
;;   (assert-equal '("k")
;;		 (uim '(evmap-context-event-seq-string test-emc)))

   (assert-true  (uim-bool '(evmap-context-input! test-emc
						  (key-event-new "k"))))
   (assert-equal '("k" "k")
		 (uim '(evmap-context-event-seq-string test-emc)))

;;   (assert-true  (uim-bool '(evmap-context-input! test-emc
;;						  (key-release-event-new "k"))))
;;   (assert-equal '("k" "k")
;;		 (uim '(evmap-context-event-seq-string test-emc)))

   (assert-true  (uim-bool '(evmap-context-input! test-emc
						  (key-event-new "i"))))
   (assert-equal '("k" "k" "i")
		 (uim '(evmap-context-event-seq-string test-emc)))

;;   (assert-true  (uim-bool '(evmap-context-input! test-emc
;;						  (key-release-event-new "i"))))
;;   (assert-equal '("k" "k" "i")
;;		 (uim '(evmap-context-event-seq-string test-emc)))

   (assert-false (uim-bool '(evmap-context-input! test-emc
						  (key-event-new "k"))))
   (assert-equal '("k" "k" "i")
		 (uim '(evmap-context-event-seq-string test-emc))))

  ("test evmap-context-composed-string"
   (uim '(define test-emc (evmap-context-new test-ruletree)))
   (assert-false (uim-bool '(evmap-context-composed-string test-emc)))

   (assert-true  (uim-bool '(evmap-context-input! test-emc
						  (key-event-new "k"))))
   (assert-false (uim-bool '(evmap-context-composed-string test-emc)))

;;   (assert-true  (uim-bool '(evmap-context-input! test-emc
;;						  (key-release-event-new "k"))))
;;   (assert-false (uim-bool '(evmap-context-composed-string test-emc)))

   (assert-true  (uim-bool '(evmap-context-input! test-emc
						  (key-event-new "k"))))
   (assert-equal '("KK")
		 (uim '(evmap-context-composed-string test-emc)))
   ;; previous implementation cannot compose on pressing key
;;   (assert-false (uim-bool '(evmap-context-composed-string test-emc)))

;;   (assert-true  (uim-bool '(evmap-context-input! test-emc
;;						  (key-release-event-new "k"))))
;;   (assert-equal '("KK")
;;		 (uim '(evmap-context-composed-string test-emc)))

   (assert-true  (uim-bool '(evmap-context-input! test-emc
						  (key-event-new "i"))))
   (assert-equal '("KKI")
		 (uim '(evmap-context-composed-string test-emc)))
;;   (assert-false (uim-bool '(evmap-context-composed-string test-emc)))

;;   (assert-true  (uim-bool '(evmap-context-input! test-emc
;;						  (key-release-event-new "i"))))
;;   (assert-equal '("KKI")
;;		 (uim '(evmap-context-composed-string test-emc)))

   (assert-false (uim-bool '(evmap-context-input! test-emc
						  (key-event-new "k"))))
   (assert-equal '("KKI")
		 (uim '(evmap-context-composed-string test-emc))))

  ("test evmap-context-preedit-string"
   (uim '(define test-emc (evmap-context-new test-ruletree)))
   (assert-false (uim-bool '(evmap-context-preedit-string test-emc)))

   (assert-true  (uim-bool '(evmap-context-input! test-emc
						  (key-event-new "k"))))
   (assert-equal '("k")
		 (uim '(evmap-context-preedit-string test-emc)))

;;   (assert-true  (uim-bool '(evmap-context-input! test-emc
;;						  (key-release-event-new "k"))))
;;   (assert-equal '("k")
;;		 (uim '(evmap-context-preedit-string test-emc)))

   (assert-true  (uim-bool '(evmap-context-input! test-emc
						  (key-event-new "k"))))
   (assert-equal '("KK")
		 (uim '(evmap-context-preedit-string test-emc)))
   ;; previous implementation cannot compose on pressing key
;;   (assert-equal '("k" "k")
;;		 (uim '(evmap-context-preedit-string test-emc)))

;;   (assert-true  (uim-bool '(evmap-context-input! test-emc
;;						  (key-release-event-new "k"))))
;;   (assert-equal '("KK")
;;		 (uim '(evmap-context-preedit-string test-emc)))

   (assert-true  (uim-bool '(evmap-context-input! test-emc
						  (key-event-new "i"))))
   (assert-equal '("KKI")
		 (uim '(evmap-context-preedit-string test-emc)))
   ;; current implementation cannot compose on pressing key
;;   (assert-equal '("k" "k" "i")
;;		 (uim '(evmap-context-preedit-string test-emc)))

;;   (assert-true  (uim-bool '(evmap-context-input! test-emc
;;						  (key-release-event-new "i"))))
;;   (assert-equal '("KKI")
;;		 (uim '(evmap-context-preedit-string test-emc)))

   (assert-false (uim-bool '(evmap-context-input! test-emc
						  (key-event-new "k"))))
   (assert-equal '("KKI")
		 (uim '(evmap-context-preedit-string test-emc))))

  ("test evmap-context-positional-var"
   ;; does not exist
   (uim '(define test-emc (evmap-context-new test-ruletree)))
   (assert-error (lambda ()
		   (uim-bool '(evmap-context-positional-var test-emc 0))))
   (assert-error (lambda ()
		   (uim-bool '(evmap-context-positional-var test-emc 1))))

   ;; contains one character
   (uim '(define test-emc (evmap-context-new test-ruletree)))
   (assert-false (uim-bool '(evmap-context-preedit-string test-emc)))

   (assert-true  (uim-bool '(evmap-context-input!
			     test-emc
			     (key-event-new "k" 'lkey_k 'pkey_qwerty_k))))
   (assert-equal '("k")
		 (uim '(evmap-context-preedit-string test-emc)))
   (assert-error (lambda ()
		   (uim-bool '(evmap-context-positional-var test-emc 0))))
   ;; positional var keeps all matched event information
   (assert-equal (uim '(list 'key 'drop-release #f -1 #f
			     "k" 'lkey_k 'pkey_qwerty_k mod_None #t #f))
		 (uim '(evmap-context-positional-var test-emc 1)))
   (assert-error (lambda ()
		   (uim-bool '(evmap-context-positional-var test-emc 2))))

;;   (assert-true  (uim-bool '(evmap-context-input! test-emc
;;						  (key-release-event-new "k"))))
;;   (assert-equal '("k")
;;		 (uim '(evmap-context-preedit-string test-emc)))
;;   (assert-equal (uim '(list 'key #t #f -1 #f
;;			     "k" 'lkey_k 'pkey_qwerty_k mod_None #t #f))
;;		 (uim '(evmap-context-positional-var test-emc 1)))
;;   (assert-equal (uim '(list 'key #t #f -1 #f "k" #f #f mod_None #f #f))
;;		 (uim '(evmap-context-positional-var test-emc 2)))
;;   (assert-error (lambda ()
;;		   (uim-bool '(evmap-context-positional-var test-emc 3))))

   (assert-true  (uim-bool '(evmap-context-input! test-emc
						  (key-event-new "u"))))
   (assert-equal '("KU")
		 (uim '(evmap-context-preedit-string test-emc)))
   ;; matched events and composed string are independent
   (assert-equal (uim '(list 'key 'drop-release #f -1 #f
			     "k" 'lkey_k 'pkey_qwerty_k mod_None #t #f))
		 (uim '(evmap-context-positional-var test-emc 1)))
   (assert-equal (uim '(list 'key 'drop-release #f -1 #f "u" #f #f mod_None #t #f))
		 (uim '(evmap-context-positional-var test-emc 2)))
   (assert-error (lambda ()
		   (uim-bool '(evmap-context-positional-var test-emc 3))))
   ;; previous implementation cannot compose on pressing key
;;   (assert-equal '("k" "u")
;;		 (uim '(evmap-context-preedit-string test-emc)))
;;   (assert-equal (uim '(list 'key #t #f -1 #f
;;			     "k" 'lkey_k 'pkey_qwerty_k mod_None #t #f))
;;		 (uim '(evmap-context-positional-var test-emc 1)))
;;   (assert-equal (uim '(list 'key #t #f -1 #f "k" #f #f mod_None #f #f))
;;		 (uim '(evmap-context-positional-var test-emc 2)))
;;   (assert-equal (uim '(list 'key #t #f -1 #f "u" #f #f mod_None #t #f))
;;		 (uim '(evmap-context-positional-var test-emc 3)))
;;   (assert-error (lambda ()
;;		   (uim-bool '(evmap-context-positional-var test-emc 4))))

;;   (assert-true  (uim-bool '(evmap-context-input! test-emc
;;						  (key-release-event-new "u"))))
;;   (assert-equal '("KU")
;;		 (uim '(evmap-context-preedit-string test-emc)))
;;   ;; matched events and composed string are independent
;;   (assert-equal (uim '(list 'key #t #f -1 #f
;;			     "k" 'lkey_k 'pkey_qwerty_k mod_None #t #f))
;;		 (uim '(evmap-context-positional-var test-emc 1)))
;;   (assert-equal (uim '(list 'key #t #f -1 #f "k" #f #f mod_None #f #f))
;;		 (uim '(evmap-context-positional-var test-emc 2)))
;;   (assert-equal (uim '(list 'key #t #f -1 #f "u" #f #f mod_None #t #f))
;;		 (uim '(evmap-context-positional-var test-emc 3)))
;;   (assert-equal (uim '(list 'key #t #f -1 #f "u" #f #f mod_None #f #f))
;;		 (uim '(evmap-context-positional-var test-emc 4)))
;;   (assert-error (lambda ()
;;		   (uim-bool '(evmap-context-positional-var test-emc 5))))

   ;; undo to remove "u"
   (assert-true (uim-bool '(evmap-context-undo! test-emc)))
   (assert-equal (uim '(list 'key 'drop-release #f -1 #f
			     "k" 'lkey_k 'pkey_qwerty_k mod_None #t #f))
		 (uim '(evmap-context-positional-var test-emc 1)))
;;   (assert-equal (uim '(list 'key 'drop-release #f -1 #f
;;			     "k" #f #f mod_None #f #f))
;;		 (uim '(evmap-context-positional-var test-emc 2)))
;;   (assert-error (lambda ()
;;		   (uim-bool '(evmap-context-positional-var test-emc 3))))
   (assert-error (lambda ()
		   (uim-bool '(evmap-context-positional-var test-emc 2))))
   )

  ("test evmap-context-input!"
   ;;; inputting non-existent mapping at first
   (uim '(define test-emc (evmap-context-new test-ruletree)))
   (assert-false (uim-bool '(evmap-context-composed-string test-emc)))

   (assert-false (uim-bool '(evmap-context-input! test-emc
						  (key-event-new "s"))))
   ;;; inputting non-existent mapping at second
   (uim '(define test-emc (evmap-context-new test-ruletree)))
   (assert-false (uim-bool '(evmap-context-composed-string test-emc)))

   (assert-true  (uim-bool '(evmap-context-input! test-emc
						  (key-event-new "k"))))
   (assert-false (uim-bool '(evmap-context-composed-string test-emc)))
   (assert-false (uim-bool '(evmap-context-input! test-emc
						  (key-event-new "z"))))
   (assert-false (uim-bool '(evmap-context-composed-string test-emc)))

   ;;; composing "KU"
   (uim '(define test-emc (evmap-context-new test-ruletree)))
   (assert-false (uim-bool '(evmap-context-composed-string test-emc)))

   (assert-true  (uim-bool '(evmap-context-input! test-emc
						  (key-event-new "k"))))
   (assert-false (uim-bool '(evmap-context-composed-string test-emc)))

;;   (assert-true  (uim-bool '(evmap-context-input! test-emc
;;						  (key-release-event-new "k"))))
;;   (assert-false (uim-bool '(evmap-context-composed-string test-emc)))

   (assert-true  (uim-bool '(evmap-context-input! test-emc
						  (key-event-new "u"))))
   (assert-equal '("KU")
		 (uim '(evmap-context-composed-string test-emc)))
   ;; previous implementation cannot compose on pressing key
;;   (assert-false (uim-bool '(evmap-context-composed-string test-emc)))

;;   (assert-true  (uim-bool '(evmap-context-input! test-emc
;;						  (key-release-event-new "u"))))
;;   (assert-equal '("KU")
;;		 (uim '(evmap-context-composed-string test-emc)))

   ;;; exceeded input
   (assert-false (uim-bool '(evmap-context-input! test-emc
						  (key-event-new "u"))))
   (assert-equal '("KU")
		 (uim '(evmap-context-composed-string test-emc)))

   ;;; composing "KKI"
   (uim '(define test-emc (evmap-context-new test-ruletree)))
   (assert-false (uim-bool '(evmap-context-composed-string test-emc)))

   (assert-true  (uim-bool '(evmap-context-input! test-emc
						  (key-event-new "k"))))
   (assert-false (uim-bool '(evmap-context-composed-string test-emc)))

;;   (assert-true  (uim-bool '(evmap-context-input! test-emc
;;						  (key-release-event-new "k"))))
;;   (assert-false (uim-bool '(evmap-context-composed-string test-emc)))

   (assert-true  (uim-bool '(evmap-context-input! test-emc
						  (key-event-new "k"))))
   (assert-equal '("KK")
		 (uim '(evmap-context-composed-string test-emc)))
   ;; previous implementation cannot compose on pressing key
;;   (assert-false (uim-bool '(evmap-context-composed-string test-emc)))

;;   (assert-true  (uim-bool '(evmap-context-input! test-emc
;;						  (key-release-event-new "k"))))
   (assert-equal '("KK")
		 (uim '(evmap-context-composed-string test-emc)))

   (assert-true  (uim-bool '(evmap-context-input! test-emc
						  (key-event-new "i"))))
   (assert-equal '("KKI")
		 (uim '(evmap-context-composed-string test-emc)))
;;   (assert-false (uim-bool '(evmap-context-composed-string test-emc)))

;;   (assert-true  (uim-bool '(evmap-context-input! test-emc
;;						  (key-release-event-new "i"))))
;;   (assert-equal '("KKI")
;;		 (uim '(evmap-context-composed-string test-emc)))

   ;;; exceeded input
   (assert-false (uim-bool '(evmap-context-input! test-emc
						  (key-event-new "k"))))
   (assert-equal '("KKI")
		 (uim '(evmap-context-composed-string test-emc))))

  ("test evmap-context-undo!"
   ;;; inputting non-existent mapping at first
   (uim '(define test-emc (evmap-context-new test-ruletree)))
   (assert-false (uim-bool '(evmap-context-composed-string test-emc)))

   (assert-false (uim-bool '(evmap-context-input! test-emc
						  (key-event-new "s"))))
   (assert-false (uim-bool '(evmap-context-undo! test-emc)))

   ;;; undoing one character
   (uim '(define test-emc (evmap-context-new test-ruletree)))
   (assert-false (uim-bool '(evmap-context-composed-string test-emc)))

   (assert-true  (uim-bool '(evmap-context-input! test-emc
						  (key-event-new "k"))))
   (assert-equal '("k")
		 (uim '(evmap-context-preedit-string test-emc)))

   ;; undo removes press half of "k"
   (assert-true  (uim-bool '(evmap-context-undo! test-emc)))
   (assert-false (uim-bool '(evmap-context-composed-string test-emc)))

   (uim '(define test-emc (evmap-context-new test-ruletree)))
   (assert-false (uim-bool '(evmap-context-composed-string test-emc)))

   (assert-true  (uim-bool '(evmap-context-input! test-emc
						  (key-event-new "k"))))
   (assert-equal '("k")
		 (uim '(evmap-context-preedit-string test-emc)))

;;   (assert-true  (uim-bool '(evmap-context-input! test-emc
;;						  (key-release-event-new "k"))))
;;   (assert-equal '("k")
;;		 (uim '(evmap-context-preedit-string test-emc)))

   ;; undo removes both press and release of "k"
   (assert-true  (uim-bool '(evmap-context-undo! test-emc)))
   (assert-false (uim-bool '(evmap-context-composed-string test-emc)))

   ;;; inputting non-existent mapping as second character
   (uim '(define test-emc (evmap-context-new test-ruletree)))
   (assert-false (uim-bool '(evmap-context-composed-string test-emc)))

   (assert-true  (uim-bool '(evmap-context-input! test-emc
						  (key-event-new "k"))))
   (assert-equal '("k")
		 (uim '(evmap-context-preedit-string test-emc)))

;;   (assert-true  (uim-bool '(evmap-context-input! test-emc
;;						  (key-release-event-new "k"))))
;;   (assert-equal '("k")
;;		 (uim '(evmap-context-preedit-string test-emc)))

   (assert-false (uim-bool '(evmap-context-input! test-emc
						  (key-event-new "z"))))
   (assert-equal '("k")
		 (uim '(evmap-context-preedit-string test-emc)))

   ;; undo removes both press and release of "k"
   (assert-true  (uim-bool '(evmap-context-undo! test-emc)))
   (assert-false (uim-bool '(evmap-context-composed-string test-emc)))

   ;;; composing "KU"
   (uim '(define test-emc (evmap-context-new test-ruletree)))
   (assert-false (uim-bool '(evmap-context-preedit-string test-emc)))

   (assert-true  (uim-bool '(evmap-context-input! test-emc
						  (key-event-new "k"))))
   (assert-equal '("k")
		 (uim '(evmap-context-preedit-string test-emc)))

;;   (assert-true  (uim-bool '(evmap-context-input! test-emc
;;						  (key-release-event-new "k"))))
;;   (assert-equal '("k")
;;		 (uim '(evmap-context-preedit-string test-emc)))

   ;; undo to remove "k"
   (assert-true (uim-bool '(evmap-context-undo! test-emc)))
   (assert-false (uim-bool '(evmap-context-preedit-string test-emc)))

   ;; input again
   (assert-true  (uim-bool '(evmap-context-input! test-emc
						  (key-event-new "k"))))
   (assert-equal '("k")
		 (uim '(evmap-context-preedit-string test-emc)))

;;   (assert-true  (uim-bool '(evmap-context-input! test-emc
;;						  (key-release-event-new "k"))))
   (assert-equal '("k")
		 (uim '(evmap-context-preedit-string test-emc)))

   (assert-true  (uim-bool '(evmap-context-input! test-emc
						  (key-event-new "u"))))
   ;; previous implementation cannot compose on pressing key
;;   (assert-equal '("k" "u")
;;		 (uim '(evmap-context-preedit-string test-emc)))

;;   (assert-true  (uim-bool '(evmap-context-input! test-emc
;;						  (key-release-event-new "u"))))
   (assert-equal '("KU")
		 (uim '(evmap-context-preedit-string test-emc)))

   ;; undo to remove "u"
   (assert-true (uim-bool '(evmap-context-undo! test-emc)))
   (assert-equal '("k")
		 (uim '(evmap-context-preedit-string test-emc)))

   ;; input again
   (assert-true  (uim-bool '(evmap-context-input! test-emc
						  (key-event-new "u"))))
   ;; previous implementation cannot compose on pressing key
;;   (assert-equal '("k" "u")
;;		 (uim '(evmap-context-preedit-string test-emc)))

;;   (assert-true  (uim-bool '(evmap-context-input! test-emc
;;						  (key-release-event-new "u"))))
   (assert-equal '("KU")
		 (uim '(evmap-context-preedit-string test-emc)))

   ;;; exceeded input
   (assert-false (uim-bool '(evmap-context-input! test-emc
						  (key-event-new "u"))))
   (assert-equal '("KU")
		 (uim '(evmap-context-preedit-string test-emc)))
   ;; undo removes "u"
   (assert-true (uim-bool '(evmap-context-undo! test-emc)))
   (assert-equal '("k")
		 (uim '(evmap-context-preedit-string test-emc)))

   ;;; composing "KKI" and modify as "KA"
   (uim '(define test-emc (evmap-context-new test-ruletree)))
   (assert-false (uim-bool '(evmap-context-preedit-string test-emc)))

   (assert-true  (uim-bool '(evmap-context-input! test-emc
						  (key-event-new "k"))))
   (assert-equal '("k")
		 (uim '(evmap-context-preedit-string test-emc)))

;;   (assert-true  (uim-bool '(evmap-context-input! test-emc
;;						  (key-release-event-new "k"))))
;;   (assert-equal '("k")
;;		 (uim '(evmap-context-preedit-string test-emc)))

   (assert-true  (uim-bool '(evmap-context-input! test-emc
						  (key-event-new "k"))))
   (assert-equal '("KK")
		 (uim '(evmap-context-preedit-string test-emc)))
   ;; previous implementation cannot compose on pressing key
;;   (assert-equal '("k" "k")
;;		 (uim '(evmap-context-preedit-string test-emc)))

;;   (assert-true  (uim-bool '(evmap-context-input! test-emc
;;						  (key-release-event-new "k"))))
;;   (assert-equal '("KK")
;;		 (uim '(evmap-context-preedit-string test-emc)))

   (assert-true  (uim-bool '(evmap-context-input! test-emc
						  (key-event-new "i"))))
   (assert-equal '("KKI")
		 (uim '(evmap-context-preedit-string test-emc)))
   ;; previous implementation cannot compose on pressing key
;;   (assert-equal '("k" "k" "i")
;;		 (uim '(evmap-context-preedit-string test-emc)))

;;   (assert-true  (uim-bool '(evmap-context-input! test-emc
;;						  (key-release-event-new "i"))))
;;   (assert-equal '("KKI")
;;		 (uim '(evmap-context-preedit-string test-emc)))

   ;; undo removes "i"
   (assert-true (uim-bool '(evmap-context-undo! test-emc)))
   (assert-equal '("KK")
		 (uim '(evmap-context-preedit-string test-emc)))

   ;; undo removes second "k"
   (assert-true (uim-bool '(evmap-context-undo! test-emc)))
   (assert-equal '("k")
		 (uim '(evmap-context-preedit-string test-emc)))

   ;; input "a" to compose "KA"
   (assert-true  (uim-bool '(evmap-context-input! test-emc
						  (key-event-new "a"))))
   (assert-equal '("KA")
		 (uim '(evmap-context-preedit-string test-emc)))
   ;; previous implementation cannot compose on pressing key
;;   (assert-equal '("k" "a")
;;		 (uim '(evmap-context-preedit-string test-emc)))

;;   (assert-true  (uim-bool '(evmap-context-input! test-emc
;;						  (key-release-event-new "a"))))
;;   (assert-equal '("KA")
;;		 (uim '(evmap-context-preedit-string test-emc)))
   ))

(define-uim-test-case "testcase key-event translator"
  (setup
   (lambda ()
     (uim '(begin
	     (define enable-ja-nicola-jp106-pseudo-thumb-shift? #t)
	     (require "evmap.scm")
	     (require "event-translator.scm")
	     (define key-event-translator-ruletree
	       (evmap-parse-ruleset key-event-translator-ruleset))
	     (define test-ruletree (evmap-parse-ruleset
				    ja-nicola-jp106-pseudo-thumb-shift-ruleset))))))

  ("test key-event-translator-translate!"
   (uim '(define test-emc (key-event-translator-new)))
   (uim '(define test-ev (key-event-new #f 'lkey_Henkan)))
   (assert-equal ()
		 (uim '(key-event-translator-translate! test-emc test-ev)))
   (assert-equal (uim '(list 'key #f #f -1 #f
			     #f 'lkey_Thumb_Shift_R #f mod_None #t #f))
		 (uim 'test-ev))
   )
)

