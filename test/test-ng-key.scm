#!/usr/bin/env gosh

;;; Copyright (c) 2005-2009 uim Project http://code.google.com/p/uim/
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

;; This file is tested with revision 707 of new repository

(use test.unit)

(require "test/uim-test-utils")

(define-uim-test-case "testcase key"
  (setup
   (lambda ()
     (uim '(require "ng-key.scm"))))

  ("test modifier-symbol?"
   (assert-true  (uim-bool '(modifier-symbol? 'mod_None)))
   (assert-true  (uim-bool '(modifier-symbol? 'mod_Shift)))
   (assert-true  (uim-bool '(modifier-symbol? 'mod_Shift_R)))
   (assert-true  (uim-bool '(modifier-symbol? 'mod_Shift_L)))
   (assert-true  (uim-bool '(modifier-symbol? 'mod_Control)))
   (assert-true  (uim-bool '(modifier-symbol? 'mod_Control_R)))
   (assert-true  (uim-bool '(modifier-symbol? 'mod_Control_L)))
   (assert-true  (uim-bool '(modifier-symbol? 'mod_Alt)))
   (assert-true  (uim-bool '(modifier-symbol? 'mod_Alt_R)))
   (assert-true  (uim-bool '(modifier-symbol? 'mod_Alt_L)))
   (assert-true  (uim-bool '(modifier-symbol? 'mod_Meta)))
   (assert-true  (uim-bool '(modifier-symbol? 'mod_Meta_R)))
   (assert-true  (uim-bool '(modifier-symbol? 'mod_Meta_L)))
   (assert-true  (uim-bool '(modifier-symbol? 'mod_Super)))
   (assert-true  (uim-bool '(modifier-symbol? 'mod_Super_R)))
   (assert-true  (uim-bool '(modifier-symbol? 'mod_Super_L)))
   (assert-true  (uim-bool '(modifier-symbol? 'mod_Hyper)))
   (assert-true  (uim-bool '(modifier-symbol? 'mod_Hyper_R)))
   (assert-true  (uim-bool '(modifier-symbol? 'mod_Hyper_L)))
   (assert-true  (uim-bool '(modifier-symbol? 'mod_Caps_Lock)))
   (assert-true  (uim-bool '(modifier-symbol? 'mod_ignore_Shift)))
   (assert-true  (uim-bool '(modifier-symbol? 'mod_ignore_Control)))
   (assert-true  (uim-bool '(modifier-symbol? 'mod_ignore_Alt)))
   (assert-true  (uim-bool '(modifier-symbol? 'mod_ignore_Meta)))
   (assert-true  (uim-bool '(modifier-symbol? 'mod_ignore_Super)))
   (assert-true  (uim-bool '(modifier-symbol? 'mod_ignore_Hyper))))

  ("test modifier-has?"
   (assert-true  (uim-bool '(modifier-has? mod_None mod_None)))
   (assert-false (uim-bool '(modifier-has? mod_None mod_Shift)))
   (assert-false (uim-bool '(modifier-has? mod_None mod_Shift_L)))
   (assert-false (uim-bool '(modifier-has? mod_None mod_Shift_R)))
   (assert-false (uim-bool '(modifier-has? mod_None mod_ignore_Shift)))
   (assert-false (uim-bool '(modifier-has? mod_None mod_Caps_Lock)))

   (assert-true  (uim-bool '(modifier-has? mod_Shift mod_None)))
   (assert-true  (uim-bool '(modifier-has? mod_Shift mod_Shift)))
   (assert-false (uim-bool '(modifier-has? mod_Shift mod_Shift_L)))
   (assert-false (uim-bool '(modifier-has? mod_Shift mod_Shift_R)))
   (assert-false (uim-bool '(modifier-has? mod_Shift mod_ignore_Shift)))
   (assert-false (uim-bool '(modifier-has? mod_Shift mod_Caps_Lock)))

   (assert-true  (uim-bool '(modifier-has? mod_Shift_L mod_None)))
   (assert-false (uim-bool '(modifier-has? mod_Shift_L mod_Shift)))
   (assert-true  (uim-bool '(modifier-has? mod_Shift_L mod_Shift_L)))
   (assert-false (uim-bool '(modifier-has? mod_Shift_L mod_Shift_R)))
   (assert-false (uim-bool '(modifier-has? mod_Shift_L mod_ignore_Shift)))
   (assert-false (uim-bool '(modifier-has? mod_Shift_L mod_Caps_Lock)))

   (assert-true  (uim-bool '(modifier-has? mod_Shift_R mod_None)))
   (assert-false (uim-bool '(modifier-has? mod_Shift_R mod_Shift)))
   (assert-false (uim-bool '(modifier-has? mod_Shift_R mod_Shift_L)))
   (assert-true  (uim-bool '(modifier-has? mod_Shift_R mod_Shift_R)))
   (assert-false (uim-bool '(modifier-has? mod_Shift_R mod_ignore_Shift)))
   (assert-false (uim-bool '(modifier-has? mod_Shift_R mod_Caps_Lock)))

   (assert-true  (uim-bool '(modifier-has? mod_ignore_Shift mod_None)))
   (assert-false (uim-bool '(modifier-has? mod_ignore_Shift mod_Shift)))
   (assert-false (uim-bool '(modifier-has? mod_ignore_Shift mod_Shift_L)))
   (assert-false (uim-bool '(modifier-has? mod_ignore_Shift mod_Shift_R)))
   (assert-true  (uim-bool '(modifier-has? mod_ignore_Shift mod_ignore_Shift)))
   (assert-false (uim-bool '(modifier-has? mod_ignore_Shift mod_Caps_Lock)))

   (assert-true  (uim-bool '(modifier-has? (bitwise-ior mod_Shift
						       mod_Shift_L
						       mod_Shift_R)
					   mod_None)))
   (assert-true  (uim-bool '(modifier-has? (bitwise-ior mod_Shift
						       mod_Shift_L
						       mod_Shift_R)
					   mod_Shift)))
   (assert-true  (uim-bool '(modifier-has? (bitwise-ior mod_Shift
						       mod_Shift_L
						       mod_Shift_R)
					   mod_Shift_L)))
   (assert-true  (uim-bool '(modifier-has? (bitwise-ior mod_Shift
						       mod_Shift_L
						       mod_Shift_R)
					   mod_Shift_R)))
   (assert-false (uim-bool '(modifier-has? (bitwise-ior mod_Shift
						       mod_Shift_L
						       mod_Shift_R)
					   mod_ignore_Shift)))
   (assert-false (uim-bool '(modifier-has? (bitwise-ior mod_Shift
						       mod_Shift_L
						       mod_Shift_R)
					   mod_Caps_Lock)))

   (assert-true  (uim-bool '(modifier-has? mod_Caps_Lock mod_None)))
   (assert-false (uim-bool '(modifier-has? mod_Caps_Lock mod_Shift)))
   (assert-false (uim-bool '(modifier-has? mod_Caps_Lock mod_Shift_L)))
   (assert-false (uim-bool '(modifier-has? mod_Caps_Lock mod_Shift_R)))
   (assert-true  (uim-bool '(modifier-has? mod_Caps_Lock mod_Caps_Lock))))

  ("test modifier-aggregate"
   (assert-equal (uim 'mod_None)
		 (uim '(modifier-aggregate mod_None mod_None)))
   (assert-equal (uim 'mod_None)
		 (uim '(modifier-aggregate mod_None mod_Shift_L)))
   (assert-equal (uim 'mod_None)
		 (uim '(modifier-aggregate mod_None mod_Shift_R)))
   (assert-equal (uim 'mod_None)
		 (uim '(modifier-aggregate mod_None mod_Shift)))
   (assert-equal (uim 'mod_ignore_Shift)
		 (uim '(modifier-aggregate mod_None mod_ignore_Shift)))

   (assert-equal (uim 'mod_Shift_L)
		 (uim '(modifier-aggregate mod_Shift_L mod_None)))
   (assert-equal (uim 'mod_Shift_L)
		 (uim '(modifier-aggregate mod_Shift_L mod_Shift_L)))
   (assert-equal (uim 'mod_Shift_L)
		 (uim '(modifier-aggregate mod_Shift_L mod_Shift_R)))
   (assert-equal (uim 'mod_Shift)
		 (uim '(modifier-aggregate mod_Shift_L mod_Shift)))
   (assert-equal (uim 'mod_ignore_Shift)
		 (uim '(modifier-aggregate mod_Shift_L mod_ignore_Shift)))

   (assert-equal (uim 'mod_Shift_R)
		 (uim '(modifier-aggregate mod_Shift_R mod_None)))
   (assert-equal (uim 'mod_Shift_R)
		 (uim '(modifier-aggregate mod_Shift_R mod_Shift_L)))
   (assert-equal (uim 'mod_Shift_R)
		 (uim '(modifier-aggregate mod_Shift_R mod_Shift_R)))
   (assert-equal (uim 'mod_Shift)
		 (uim '(modifier-aggregate mod_Shift_R mod_Shift)))
   (assert-equal (uim 'mod_ignore_Shift)
		 (uim '(modifier-aggregate mod_Shift_R mod_ignore_Shift)))

   (assert-equal (uim 'mod_Shift)
		 (uim '(modifier-aggregate mod_Shift mod_None)))
   (assert-equal (uim 'mod_Shift)
		 (uim '(modifier-aggregate mod_Shift mod_Shift_L)))
   (assert-equal (uim 'mod_Shift)
		 (uim '(modifier-aggregate mod_Shift mod_Shift_R)))
   (assert-equal (uim 'mod_Shift)
		 (uim '(modifier-aggregate mod_Shift mod_Shift)))
   (assert-equal (uim 'mod_ignore_Shift)
		 (uim '(modifier-aggregate mod_Shift mod_ignore_Shift)))

   (assert-equal (uim '(bitwise-ior mod_Shift_L mod_Shift_R))
		 (uim '(modifier-aggregate (bitwise-ior mod_Shift_L
						       mod_Shift_R)
					   mod_None)))
   (assert-equal (uim '(bitwise-ior mod_Shift_L mod_Shift_R))
		 (uim '(modifier-aggregate (bitwise-ior mod_Shift_L
						       mod_Shift_R)
					   mod_Shift_L)))
   (assert-equal (uim '(bitwise-ior mod_Shift_L mod_Shift_R))
		 (uim '(modifier-aggregate (bitwise-ior mod_Shift_L
						       mod_Shift_R)
					   mod_Shift_R)))
   (assert-equal (uim 'mod_Shift)
		 (uim '(modifier-aggregate (bitwise-ior mod_Shift_L
						       mod_Shift_R)
					   mod_Shift)))
   (assert-equal (uim 'mod_ignore_Shift)
		 (uim '(modifier-aggregate (bitwise-ior mod_Shift_L
						       mod_Shift_R)
					   mod_ignore_Shift)))

   (assert-equal (uim '(bitwise-ior mod_Shift mod_Shift_L mod_Shift_R))
		 (uim '(modifier-aggregate (bitwise-ior mod_Shift
						       mod_Shift_L
						       mod_Shift_R)
					   mod_None)))
   (assert-equal (uim '(bitwise-ior mod_Shift mod_Shift_L mod_Shift_R))
		 (uim '(modifier-aggregate (bitwise-ior mod_Shift
						       mod_Shift_L
						       mod_Shift_R)
					   mod_Shift_L)))
   (assert-equal (uim '(bitwise-ior mod_Shift mod_Shift_L mod_Shift_R))
		 (uim '(modifier-aggregate (bitwise-ior mod_Shift
						       mod_Shift_L
						       mod_Shift_R)
					   mod_Shift_R)))
   (assert-equal (uim 'mod_Shift)
		 (uim '(modifier-aggregate (bitwise-ior mod_Shift
						       mod_Shift_L
						       mod_Shift_R)
					   mod_Shift)))
   (assert-equal (uim 'mod_ignore_Shift)
		 (uim '(modifier-aggregate (bitwise-ior mod_Shift
						       mod_Shift_L
						       mod_Shift_R)
					   mod_ignore_Shift)))
   (assert-equal (uim 'mod_ignore_Shift)
		 (uim '(modifier-aggregate (bitwise-ior mod_Shift
						       mod_Shift_L
						       mod_Shift_R
						       mod_ignore_Shift)
					   mod_ignore_Shift))))

  ("test modifier-match?"
   (assert-true  (uim-bool '(modifier-match? mod_None mod_None)))
   (assert-false (uim-bool '(modifier-match? mod_None mod_Shift)))
   (assert-false (uim-bool '(modifier-match? mod_None mod_Shift_L)))
   (assert-false (uim-bool '(modifier-match? mod_None mod_Shift_R)))
   (assert-true  (uim-bool '(modifier-match? mod_None mod_ignore_Shift)))
   (assert-false (uim-bool '(modifier-match? mod_None mod_Caps_Lock)))
   (assert-false (uim-bool '(modifier-match? mod_None
					     (bitwise-ior mod_Shift
							 mod_ignore_Shift))))
   (assert-false (uim-bool '(modifier-match? mod_None
					     (bitwise-ior mod_Shift_L
							 mod_ignore_Shift))))
   (assert-false (uim-bool '(modifier-match? mod_None
					     (bitwise-ior mod_Shift
							 mod_Shift_R
							 mod_Shift_L
							 mod_ignore_Shift))))

   (assert-false (uim-bool '(modifier-match? mod_Shift mod_None)))
   (assert-true  (uim-bool '(modifier-match? mod_Shift mod_Shift)))
   (assert-true  (uim-bool '(modifier-match? mod_Shift mod_Shift_L)))
   (assert-true  (uim-bool '(modifier-match? mod_Shift mod_Shift_R)))
   (assert-false (uim-bool '(modifier-match? mod_Shift mod_ignore_Shift)))
   (assert-false (uim-bool '(modifier-match? mod_Shift mod_Caps_Lock)))
   (assert-true  (uim-bool '(modifier-match? mod_Shift
					     (bitwise-ior mod_Shift_L
							 mod_Shift_R))))
   (assert-true  (uim-bool '(modifier-match? mod_Shift
					     (bitwise-ior mod_Shift
							 mod_Shift_L
							 mod_Shift_R))))

   (assert-false (uim-bool '(modifier-match? mod_Shift_L mod_None)))
   (assert-false (uim-bool '(modifier-match? mod_Shift_L mod_Shift)))
   (assert-true  (uim-bool '(modifier-match? mod_Shift_L mod_Shift_L)))
   (assert-false (uim-bool '(modifier-match? mod_Shift_L mod_Shift_R)))
   (assert-false (uim-bool '(modifier-match? mod_Shift_L mod_ignore_Shift)))
   (assert-false (uim-bool '(modifier-match? mod_Shift_L mod_Caps_Lock)))
   (assert-false (uim-bool '(modifier-match? mod_Shift_L
					     (bitwise-ior mod_Shift_L
							 mod_Shift_R))))

   (assert-false (uim-bool '(modifier-match? mod_Shift_R mod_None)))
   (assert-false (uim-bool '(modifier-match? mod_Shift_R mod_Shift)))
   (assert-false (uim-bool '(modifier-match? mod_Shift_R mod_Shift_L)))
   (assert-true  (uim-bool '(modifier-match? mod_Shift_R mod_Shift_R)))
   (assert-false (uim-bool '(modifier-match? mod_Shift_R mod_ignore_Shift)))
   (assert-false (uim-bool '(modifier-match? mod_Shift_R mod_Caps_Lock)))

   (assert-true  (uim-bool '(modifier-match? mod_ignore_Shift mod_None)))
   (assert-true  (uim-bool '(modifier-match? mod_ignore_Shift mod_Shift)))
   (assert-true  (uim-bool '(modifier-match? mod_ignore_Shift mod_Shift_L)))
   (assert-true  (uim-bool '(modifier-match? mod_ignore_Shift mod_Shift_R)))
   (assert-true  (uim-bool '(modifier-match? mod_ignore_Shift mod_ignore_Shift)))
   (assert-false (uim-bool '(modifier-match? mod_ignore_Shift mod_Caps_Lock)))
   (assert-true  (uim-bool '(modifier-match? mod_ignore_Shift
					     (bitwise-ior mod_Shift
							 mod_Shift_L
							 mod_Shift_R))))
   (assert-false (uim-bool '(modifier-match? mod_ignore_Shift
					     (bitwise-ior mod_Shift
							 mod_Shift_L
							 mod_Shift_R
							 mod_Control_L))))

   ;; multiple modifiers
   (assert-false (uim-bool '(modifier-match? (bitwise-ior mod_Shift_L
							 mod_Control_L)
					     mod_None)))
   (assert-false (uim-bool '(modifier-match? (bitwise-ior mod_Shift_L
							 mod_Control_L)
					     mod_Shift)))
   (assert-false (uim-bool '(modifier-match? (bitwise-ior mod_Shift_L
							 mod_Control_L)
					     mod_Shift_L)))
   (assert-false (uim-bool '(modifier-match? (bitwise-ior mod_Shift_L
							 mod_Control_L)
					     mod_Control_L)))
   (assert-true  (uim-bool '(modifier-match? (bitwise-ior mod_Shift_L
							 mod_Control_L)
					     (bitwise-ior mod_Shift_L
							 mod_Control_L))))
   (assert-false (uim-bool '(modifier-match? (bitwise-ior mod_Shift_L
							 mod_Control_L)
					     (bitwise-ior mod_Shift
							 mod_Shift_L
							 mod_Control_L))))
   (assert-false (uim-bool '(modifier-match? (bitwise-ior mod_Shift_L
							 mod_Control_L)
					     (bitwise-ior mod_Shift_L
							 mod_Control
							 mod_Control_L))))
   (assert-false (uim-bool '(modifier-match? (bitwise-ior mod_Shift_L
							 mod_Control_L)
					     (bitwise-ior mod_Shift_L
							 mod_Control))))
   (assert-false (uim-bool '(modifier-match? (bitwise-ior mod_Shift_L
							 mod_Control_L)
					     mod_Shift_R)))
   (assert-false (uim-bool '(modifier-match? (bitwise-ior mod_Shift_L
							 mod_Control_L)
					     mod_ignore_Shift)))
   (assert-false (uim-bool '(modifier-match? (bitwise-ior mod_Shift_L
							 mod_Control_L)
					     mod_Caps_Lock)))

   ;; multiple modifiers with ignore_Shift
   (assert-false (uim-bool '(modifier-match? (bitwise-ior mod_ignore_Shift
							 mod_Shift_L
							 mod_Control_L)
					     mod_None)))
   (assert-false (uim-bool '(modifier-match? (bitwise-ior mod_ignore_Shift
							 mod_Shift_L
							 mod_Control_L)
					     mod_Shift)))
   (assert-false (uim-bool '(modifier-match? (bitwise-ior mod_ignore_Shift
							 mod_Shift_L
							 mod_Control_L)
					     mod_Shift_L)))
   (assert-true  (uim-bool '(modifier-match? (bitwise-ior mod_ignore_Shift
							 mod_Shift_L
							 mod_Control_L)
					     mod_Control_L)))
   (assert-true  (uim-bool '(modifier-match? (bitwise-ior mod_ignore_Shift
							 mod_Shift_L
							 mod_Control_L)
					     (bitwise-ior mod_Shift_L
							 mod_Control_L))))
   (assert-true  (uim-bool '(modifier-match? (bitwise-ior mod_ignore_Shift
							 mod_Shift_L
							 mod_Control_L)
					     (bitwise-ior mod_Shift
							 mod_Shift_L
							 mod_Control_L))))
   (assert-false (uim-bool '(modifier-match? (bitwise-ior mod_ignore_Shift
							 mod_Shift_L
							 mod_Control_L)
					     (bitwise-ior mod_Shift_L
							 mod_Control
							 mod_Control_L))))
   (assert-false (uim-bool '(modifier-match? (bitwise-ior mod_ignore_Shift
							 mod_Shift_L
							 mod_Control_L)
					     (bitwise-ior mod_Shift_L
							 mod_Control))))
   (assert-false (uim-bool '(modifier-match? (bitwise-ior mod_ignore_Shift
							 mod_Shift_L
							 mod_Control_L)
					     mod_Shift_R)))
   (assert-false (uim-bool '(modifier-match? (bitwise-ior mod_ignore_Shift
							 mod_Shift_L
							 mod_Control_L)
					     mod_ignore_Shift)))
   (assert-false (uim-bool '(modifier-match? (bitwise-ior mod_ignore_Shift
							 mod_Shift_L
							 mod_Control_L)
					     mod_Caps_Lock)))

   ;; ignoring Shift
   (assert-false (uim-bool '(modifier-match? (bitwise-ior mod_ignore_Shift
							 mod_Control)
					     mod_None)))
   (assert-true  (uim-bool '(modifier-match? (bitwise-ior mod_ignore_Shift
							 mod_Control)
					     mod_Control_L)))
   (assert-true  (uim-bool '(modifier-match? (bitwise-ior mod_ignore_Shift
							 mod_Control)
					     mod_Control_R)))
   (assert-true  (uim-bool '(modifier-match? (bitwise-ior mod_ignore_Shift
							 mod_Control)
					     mod_Control)))
   (assert-true  (uim-bool '(modifier-match? (bitwise-ior mod_ignore_Shift
							 mod_Control)
					     (bitwise-ior mod_Control_L
							 mod_Control_R))))
   (assert-true  (uim-bool '(modifier-match? (bitwise-ior mod_ignore_Shift
							 mod_Control)
					     (bitwise-ior mod_Control
							 mod_Control_L
							 mod_Control_R))))

   (assert-false (uim-bool '(modifier-match? (bitwise-ior mod_ignore_Shift
							 mod_Control)
					     mod_Shift)))
   (assert-true  (uim-bool '(modifier-match? (bitwise-ior mod_ignore_Shift
							 mod_Control)
					     (bitwise-ior mod_Shift
							 mod_Control_L))))
   (assert-true  (uim-bool '(modifier-match? (bitwise-ior mod_ignore_Shift
							 mod_Control)
					     (bitwise-ior mod_Shift
							 mod_Control_R))))
   (assert-true  (uim-bool '(modifier-match? (bitwise-ior mod_ignore_Shift
							 mod_Control)
					     (bitwise-ior mod_Shift
							 mod_Control))))
   (assert-true  (uim-bool '(modifier-match? (bitwise-ior mod_ignore_Shift
							 mod_Control)
					     (bitwise-ior mod_Shift
							 mod_Control_L
							 mod_Control_R))))
   (assert-true  (uim-bool '(modifier-match? (bitwise-ior mod_ignore_Shift
							 mod_Control)
					     (bitwise-ior mod_Shift
							 mod_Control
							 mod_Control_L
							 mod_Control_R))))

   (assert-false (uim-bool '(modifier-match? (bitwise-ior mod_ignore_Shift
							 mod_Control)
					     mod_Shift_L)))
   (assert-true  (uim-bool '(modifier-match? (bitwise-ior mod_ignore_Shift
							 mod_Control)
					     (bitwise-ior mod_Shift_L
							 mod_Control_L))))
   (assert-true  (uim-bool '(modifier-match? (bitwise-ior mod_ignore_Shift
							 mod_Control)
					     (bitwise-ior mod_Shift_L
							 mod_Control_R))))
   (assert-true  (uim-bool '(modifier-match? (bitwise-ior mod_ignore_Shift
							 mod_Control)
					     (bitwise-ior mod_Shift_L
							 mod_Control))))
   (assert-true  (uim-bool '(modifier-match? (bitwise-ior mod_ignore_Shift
							 mod_Control)
					     (bitwise-ior mod_Shift_L
							 mod_Control_L
							 mod_Control_R))))
   (assert-true  (uim-bool '(modifier-match? (bitwise-ior mod_ignore_Shift
							 mod_Control)
					     (bitwise-ior mod_Shift_L
							 mod_Control
							 mod_Control_L
							 mod_Control_R))))

   (assert-false (uim-bool '(modifier-match? (bitwise-ior mod_ignore_Shift
							 mod_Control)
					     (bitwise-ior mod_Shift_L
							 mod_Shift_R))))
   (assert-true  (uim-bool '(modifier-match? (bitwise-ior mod_ignore_Shift
							 mod_Control)
					     (bitwise-ior mod_Shift_L
							 mod_Shift_R
							 mod_Control_L))))
   (assert-true  (uim-bool '(modifier-match? (bitwise-ior mod_ignore_Shift
							 mod_Control)
					     (bitwise-ior mod_Shift_L
							 mod_Shift_R
							 mod_Control_R))))
   (assert-true  (uim-bool '(modifier-match? (bitwise-ior mod_ignore_Shift
							 mod_Control)
					     (bitwise-ior mod_Shift_L
							 mod_Shift_R
							 mod_Control))))
   (assert-true  (uim-bool '(modifier-match? (bitwise-ior mod_ignore_Shift
							 mod_Control)
					     (bitwise-ior mod_Shift_L
							 mod_Shift_R
							 mod_Control_L
							 mod_Control_R))))
   (assert-true  (uim-bool '(modifier-match? (bitwise-ior mod_ignore_Shift
							 mod_Control)
					     (bitwise-ior mod_Shift_L
							 mod_Shift_R
							 mod_Control
							 mod_Control_L
							 mod_Control_R))))

   (assert-false (uim-bool '(modifier-match? (bitwise-ior mod_ignore_Shift
							 mod_Control)
					     (bitwise-ior mod_Shift
							 mod_Shift_L
							 mod_Shift_R))))
   (assert-true  (uim-bool '(modifier-match? (bitwise-ior mod_ignore_Shift
							 mod_Control)
					     (bitwise-ior mod_Shift
							 mod_Shift_L
							 mod_Shift_R
							 mod_Control_L))))
   (assert-true  (uim-bool '(modifier-match? (bitwise-ior mod_ignore_Shift
							 mod_Control)
					     (bitwise-ior mod_Shift
							 mod_Shift_L
							 mod_Shift_R
							 mod_Control_R))))
   (assert-true  (uim-bool '(modifier-match? (bitwise-ior mod_ignore_Shift
							 mod_Control)
					     (bitwise-ior mod_Shift
							 mod_Shift_L
							 mod_Shift_R
							 mod_Control))))
   (assert-true  (uim-bool '(modifier-match? (bitwise-ior mod_ignore_Shift
							 mod_Control)
					     (bitwise-ior mod_Shift
							 mod_Shift_L
							 mod_Shift_R
							 mod_Control_L
							 mod_Control_R))))
   (assert-true  (uim-bool '(modifier-match? (bitwise-ior mod_ignore_Shift
							 mod_Control)
					     (bitwise-ior mod_Shift
							 mod_Shift_L
							 mod_Shift_R
							 mod_Control
							 mod_Control_L
							 mod_Control_R))))

   ;; ignoring multiple modifiers
   (assert-true  (uim-bool '(modifier-match? (bitwise-ior mod_ignore_Shift
							 mod_ignore_Control
							 mod_Alt)
					     mod_Alt)))
   (assert-true  (uim-bool '(modifier-match? (bitwise-ior mod_ignore_Shift
							 mod_ignore_Control
							 mod_Alt)
					     (bitwise-ior mod_Shift
							 mod_Shift_L
							 mod_Shift_R
							 mod_Control
							 mod_Control_L
							 mod_Control_R
							 mod_Alt))))
   (assert-true  (uim-bool '(modifier-match? (bitwise-ior mod_ignore_Shift
							 mod_ignore_Control
							 mod_ignore_Alt)
					     mod_None)))
   (assert-true  (uim-bool '(modifier-match? (bitwise-ior mod_ignore_Shift
							 mod_ignore_Control
							 mod_ignore_Alt)
					     (bitwise-ior mod_Shift
							 mod_Shift_L
							 mod_Shift_R
							 mod_Control
							 mod_Control_L
							 mod_Control_R
							 mod_Alt
							 mod_Alt_L
							 mod_Alt_R)))))

  ("test logical-key?"
   (assert-false (uim-bool '(logical-key? 'lkey_Nonexistent)))
   (assert-false (uim-bool '(logical-key? 'pkey_qwerty_a)))
   (assert-true  (uim-bool '(logical-key? 'lkey_VoidSymbol)))
   (assert-true  (uim-bool '(logical-key? 'lkey_BackSpace)))
   (assert-true  (uim-bool '(logical-key? 'lkey_Shift_L)))
   (assert-true  (uim-bool '(logical-key? 'lkey_Thumb_Shift_L)))
   (assert-true  (uim-bool '(logical-key? 'lkey_F1)))
   (assert-true  (uim-bool '(logical-key? 'lkey_space)))
   (assert-true  (uim-bool '(logical-key? 'lkey_0)))
   (assert-true  (uim-bool '(logical-key? 'lkey_a)))
   (assert-true  (uim-bool '(logical-key? 'lkey_A)))
   (assert-true  (uim-bool '(logical-key? 'lkey_yen)))
   (assert-true  (uim-bool '(logical-key? 'lkey_dead_grave))))

  ("test physical-key?"
   (assert-false (uim-bool '(physical-key? 'pkey_Nonexistent)))
   (assert-false (uim-bool '(physical-key? 'lkey_a)))
   (assert-true  (uim-bool '(physical-key? 'pkey_VoidSymbol)))
   (assert-false (uim-bool '(physical-key? 'pkey_qwerty_BackSpace)))
   (assert-false (uim-bool '(physical-key? 'pkey_qwerty_Shift_L)))
   (assert-false (uim-bool '(physical-key? 'pkey_qwerty_F1)))
   (assert-false (uim-bool '(physical-key? 'pkey_qwerty_space)))
   (assert-false (uim-bool '(physical-key? 'pkey_qwerty_0)))
   (assert-false (uim-bool '(physical-key? 'pkey_qwerty_a)))
   (assert-false (uim-bool '(physical-key? 'pkey_jp106_yen)))
   ;; Temporarily disabled  -- YamaKen 2008-05-10
   ;;(uim '(require "physical-key.scm"))
   ;;(assert-false (uim-bool '(physical-key? 'pkey_Nonexistent)))
   ;;(assert-false (uim-bool '(physical-key? 'lkey_a)))
   ;;(assert-true  (uim-bool '(physical-key? 'pkey_VoidSymbol)))
   ;;(assert-true  (uim-bool '(physical-key? 'pkey_qwerty_BackSpace)))
   ;;(assert-true  (uim-bool '(physical-key? 'pkey_qwerty_Shift_L)))
   ;;(assert-true  (uim-bool '(physical-key? 'pkey_qwerty_F1)))
   ;;(assert-true  (uim-bool '(physical-key? 'pkey_qwerty_space)))
   ;;(assert-true  (uim-bool '(physical-key? 'pkey_qwerty_0)))
   ;;(assert-true  (uim-bool '(physical-key? 'pkey_qwerty_a)))
   ;;(assert-true  (uim-bool '(physical-key? 'pkey_jp106_yen)))
   ))
