;;; Copyright (c) 2005-2013 uim Project https://github.com/uim/uim
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

;; These tests are passed at revision 6605 (new repository)

(define-module test.test-ng-key
  (use test.unit.test-case)
  (use test.uim-test))
(select-module test.test-ng-key)

(define (setup)
  (uim-test-setup)
  (uim-eval '(require "ng-key.scm")))

(define (teardown)
  (uim-test-teardown))

(define (test-modifier-symbol?)
  (assert-uim-true-value '(modifier-symbol? 'mod_None))
  (assert-uim-true-value '(modifier-symbol? 'mod_Shift))
  (assert-uim-true-value '(modifier-symbol? 'mod_Shift_R))
  (assert-uim-true-value '(modifier-symbol? 'mod_Shift_L))
  (assert-uim-true-value '(modifier-symbol? 'mod_Control))
  (assert-uim-true-value '(modifier-symbol? 'mod_Control_R))
  (assert-uim-true-value '(modifier-symbol? 'mod_Control_L))
  (assert-uim-true-value '(modifier-symbol? 'mod_Alt))
  (assert-uim-true-value '(modifier-symbol? 'mod_Alt_R))
  (assert-uim-true-value '(modifier-symbol? 'mod_Alt_L))
  (assert-uim-true-value '(modifier-symbol? 'mod_Meta))
  (assert-uim-true-value '(modifier-symbol? 'mod_Meta_R))
  (assert-uim-true-value '(modifier-symbol? 'mod_Meta_L))
  (assert-uim-true-value '(modifier-symbol? 'mod_Super))
  (assert-uim-true-value '(modifier-symbol? 'mod_Super_R))
  (assert-uim-true-value '(modifier-symbol? 'mod_Super_L))
  (assert-uim-true-value '(modifier-symbol? 'mod_Hyper))
  (assert-uim-true-value '(modifier-symbol? 'mod_Hyper_R))
  (assert-uim-true-value '(modifier-symbol? 'mod_Hyper_L))
  (assert-uim-true-value '(modifier-symbol? 'mod_Caps_Lock))
  (assert-uim-true-value '(modifier-symbol? 'mod_ignore_Shift))
  (assert-uim-true-value '(modifier-symbol? 'mod_ignore_Control))
  (assert-uim-true-value '(modifier-symbol? 'mod_ignore_Alt))
  (assert-uim-true-value '(modifier-symbol? 'mod_ignore_Meta))
  (assert-uim-true-value '(modifier-symbol? 'mod_ignore_Super))
  (assert-uim-true-value '(modifier-symbol? 'mod_ignore_Hyper))
  #f)

(define (test-modifier-has?)
  (assert-uim-true  '(modifier-has? mod_None mod_None))
  (assert-uim-false '(modifier-has? mod_None mod_Shift))
  (assert-uim-false '(modifier-has? mod_None mod_Shift_L))
  (assert-uim-false '(modifier-has? mod_None mod_Shift_R))
  (assert-uim-false '(modifier-has? mod_None mod_ignore_Shift))
  (assert-uim-false '(modifier-has? mod_None mod_Caps_Lock))

  (assert-uim-true  '(modifier-has? mod_Shift mod_None))
  (assert-uim-true  '(modifier-has? mod_Shift mod_Shift))
  (assert-uim-false '(modifier-has? mod_Shift mod_Shift_L))
  (assert-uim-false '(modifier-has? mod_Shift mod_Shift_R))
  (assert-uim-false '(modifier-has? mod_Shift mod_ignore_Shift))
  (assert-uim-false '(modifier-has? mod_Shift mod_Caps_Lock))

  (assert-uim-true  '(modifier-has? mod_Shift_L mod_None))
  (assert-uim-false '(modifier-has? mod_Shift_L mod_Shift))
  (assert-uim-true  '(modifier-has? mod_Shift_L mod_Shift_L))
  (assert-uim-false '(modifier-has? mod_Shift_L mod_Shift_R))
  (assert-uim-false '(modifier-has? mod_Shift_L mod_ignore_Shift))
  (assert-uim-false '(modifier-has? mod_Shift_L mod_Caps_Lock))

  (assert-uim-true  '(modifier-has? mod_Shift_R mod_None))
  (assert-uim-false '(modifier-has? mod_Shift_R mod_Shift))
  (assert-uim-false '(modifier-has? mod_Shift_R mod_Shift_L))
  (assert-uim-true  '(modifier-has? mod_Shift_R mod_Shift_R))
  (assert-uim-false '(modifier-has? mod_Shift_R mod_ignore_Shift))
  (assert-uim-false '(modifier-has? mod_Shift_R mod_Caps_Lock))

  (assert-uim-true  '(modifier-has? mod_ignore_Shift mod_None))
  (assert-uim-false '(modifier-has? mod_ignore_Shift mod_Shift))
  (assert-uim-false '(modifier-has? mod_ignore_Shift mod_Shift_L))
  (assert-uim-false '(modifier-has? mod_ignore_Shift mod_Shift_R))
  (assert-uim-true  '(modifier-has? mod_ignore_Shift mod_ignore_Shift))
  (assert-uim-false '(modifier-has? mod_ignore_Shift mod_Caps_Lock))

  (assert-uim-true  '(modifier-has? (bitwise-ior mod_Shift
                                                 mod_Shift_L
                                                 mod_Shift_R)
                                    mod_None))
  (assert-uim-true  '(modifier-has? (bitwise-ior mod_Shift
                                                 mod_Shift_L
                                                 mod_Shift_R)
                                    mod_Shift))
  (assert-uim-true  '(modifier-has? (bitwise-ior mod_Shift
                                                 mod_Shift_L
                                                 mod_Shift_R)
                                    mod_Shift_L))
  (assert-uim-true  '(modifier-has? (bitwise-ior mod_Shift
                                                 mod_Shift_L
                                                 mod_Shift_R)
                                    mod_Shift_R))
  (assert-uim-false '(modifier-has? (bitwise-ior mod_Shift
                                                 mod_Shift_L
                                                 mod_Shift_R)
                                    mod_ignore_Shift))
  (assert-uim-false '(modifier-has? (bitwise-ior mod_Shift
                                                 mod_Shift_L
                                                 mod_Shift_R)
                                    mod_Caps_Lock))

  (assert-uim-true  '(modifier-has? mod_Caps_Lock mod_None))
  (assert-uim-false '(modifier-has? mod_Caps_Lock mod_Shift))
  (assert-uim-false '(modifier-has? mod_Caps_Lock mod_Shift_L))
  (assert-uim-false '(modifier-has? mod_Caps_Lock mod_Shift_R))
  (assert-uim-true  '(modifier-has? mod_Caps_Lock mod_Caps_Lock))
  #f)

(define (test-modifier-aggregate)
  (assert-uim-equal (uim 'mod_None)
                    '(modifier-aggregate mod_None mod_None))
  (assert-uim-equal (uim 'mod_None)
                    '(modifier-aggregate mod_None mod_Shift_L))
  (assert-uim-equal (uim 'mod_None)
                    '(modifier-aggregate mod_None mod_Shift_R))
  (assert-uim-equal (uim 'mod_None)
                    '(modifier-aggregate mod_None mod_Shift))
  (assert-uim-equal (uim 'mod_ignore_Shift)
                    '(modifier-aggregate mod_None mod_ignore_Shift))

  (assert-uim-equal (uim 'mod_Shift_L)
                    '(modifier-aggregate mod_Shift_L mod_None))
  (assert-uim-equal (uim 'mod_Shift_L)
                    '(modifier-aggregate mod_Shift_L mod_Shift_L))
  (assert-uim-equal (uim 'mod_Shift_L)
                    '(modifier-aggregate mod_Shift_L mod_Shift_R))
  (assert-uim-equal (uim 'mod_Shift)
                    '(modifier-aggregate mod_Shift_L mod_Shift))
  (assert-uim-equal (uim 'mod_ignore_Shift)
                    '(modifier-aggregate mod_Shift_L mod_ignore_Shift))

  (assert-uim-equal (uim 'mod_Shift_R)
                    '(modifier-aggregate mod_Shift_R mod_None))
  (assert-uim-equal (uim 'mod_Shift_R)
                    '(modifier-aggregate mod_Shift_R mod_Shift_L))
  (assert-uim-equal (uim 'mod_Shift_R)
                    '(modifier-aggregate mod_Shift_R mod_Shift_R))
  (assert-uim-equal (uim 'mod_Shift)
                    '(modifier-aggregate mod_Shift_R mod_Shift))
  (assert-uim-equal (uim 'mod_ignore_Shift)
                    '(modifier-aggregate mod_Shift_R mod_ignore_Shift))

  (assert-uim-equal (uim 'mod_Shift)
                    '(modifier-aggregate mod_Shift mod_None))
  (assert-uim-equal (uim 'mod_Shift)
                    '(modifier-aggregate mod_Shift mod_Shift_L))
  (assert-uim-equal (uim 'mod_Shift)
                    '(modifier-aggregate mod_Shift mod_Shift_R))
  (assert-uim-equal (uim 'mod_Shift)
                    '(modifier-aggregate mod_Shift mod_Shift))
  (assert-uim-equal (uim 'mod_ignore_Shift)
                    '(modifier-aggregate mod_Shift mod_ignore_Shift))

  (assert-uim-equal (uim '(bitwise-ior mod_Shift_L mod_Shift_R))
                    '(modifier-aggregate (bitwise-ior mod_Shift_L
                                                      mod_Shift_R)
                                         mod_None))
  (assert-uim-equal (uim '(bitwise-ior mod_Shift_L mod_Shift_R))
                    '(modifier-aggregate (bitwise-ior mod_Shift_L
                                                      mod_Shift_R)
                                         mod_Shift_L))
  (assert-uim-equal (uim '(bitwise-ior mod_Shift_L mod_Shift_R))
                    '(modifier-aggregate (bitwise-ior mod_Shift_L
                                                      mod_Shift_R)
                                         mod_Shift_R))
  (assert-uim-equal (uim 'mod_Shift)
                    '(modifier-aggregate (bitwise-ior mod_Shift_L
                                                      mod_Shift_R)
                                         mod_Shift))
  (assert-uim-equal (uim 'mod_ignore_Shift)
                    '(modifier-aggregate (bitwise-ior mod_Shift_L
                                                      mod_Shift_R)
                                         mod_ignore_Shift))

  (assert-uim-equal (uim '(bitwise-ior mod_Shift mod_Shift_L mod_Shift_R))
                    '(modifier-aggregate (bitwise-ior mod_Shift
                                                      mod_Shift_L
                                                      mod_Shift_R)
                                         mod_None))
  (assert-uim-equal (uim '(bitwise-ior mod_Shift mod_Shift_L mod_Shift_R))
                    '(modifier-aggregate (bitwise-ior mod_Shift
                                                      mod_Shift_L
                                                      mod_Shift_R)
                                         mod_Shift_L))
  (assert-uim-equal (uim '(bitwise-ior mod_Shift mod_Shift_L mod_Shift_R))
                    '(modifier-aggregate (bitwise-ior mod_Shift
                                                      mod_Shift_L
                                                      mod_Shift_R)
                                         mod_Shift_R))
  (assert-uim-equal (uim 'mod_Shift)
                    '(modifier-aggregate (bitwise-ior mod_Shift
                                                      mod_Shift_L
                                                      mod_Shift_R)
                                         mod_Shift))
  (assert-uim-equal (uim 'mod_ignore_Shift)
                    '(modifier-aggregate (bitwise-ior mod_Shift
                                                      mod_Shift_L
                                                      mod_Shift_R)
                                         mod_ignore_Shift))
  (assert-uim-equal (uim 'mod_ignore_Shift)
                    '(modifier-aggregate (bitwise-ior mod_Shift
                                                      mod_Shift_L
                                                      mod_Shift_R
                                                      mod_ignore_Shift)
                                         mod_ignore_Shift))
  #f)

(define (test-modifier-match?)
  (assert-uim-true  '(modifier-match? mod_None mod_None))
  (assert-uim-false '(modifier-match? mod_None mod_Shift))
  (assert-uim-false '(modifier-match? mod_None mod_Shift_L))
  (assert-uim-false '(modifier-match? mod_None mod_Shift_R))
  (assert-uim-true  '(modifier-match? mod_None mod_ignore_Shift))
  (assert-uim-false '(modifier-match? mod_None mod_Caps_Lock))
  (assert-uim-false '(modifier-match? mod_None
                                      (bitwise-ior mod_Shift
                                                   mod_ignore_Shift)))
  (assert-uim-false '(modifier-match? mod_None
                                      (bitwise-ior mod_Shift_L
                                                   mod_ignore_Shift)))
  (assert-uim-false '(modifier-match? mod_None
                                      (bitwise-ior mod_Shift
                                                   mod_Shift_R
                                                   mod_Shift_L
                                                   mod_ignore_Shift)))

  (assert-uim-false '(modifier-match? mod_Shift mod_None))
  (assert-uim-true  '(modifier-match? mod_Shift mod_Shift))
  (assert-uim-true  '(modifier-match? mod_Shift mod_Shift_L))
  (assert-uim-true  '(modifier-match? mod_Shift mod_Shift_R))
  (assert-uim-false '(modifier-match? mod_Shift mod_ignore_Shift))
  (assert-uim-false '(modifier-match? mod_Shift mod_Caps_Lock))
  (assert-uim-true  '(modifier-match? mod_Shift
                                      (bitwise-ior mod_Shift_L
                                                   mod_Shift_R)))
  (assert-uim-true  '(modifier-match? mod_Shift
                                      (bitwise-ior mod_Shift
                                                   mod_Shift_L
                                                   mod_Shift_R)))

  (assert-uim-false '(modifier-match? mod_Shift_L mod_None))
  (assert-uim-false '(modifier-match? mod_Shift_L mod_Shift))
  (assert-uim-true  '(modifier-match? mod_Shift_L mod_Shift_L))
  (assert-uim-false '(modifier-match? mod_Shift_L mod_Shift_R))
  (assert-uim-false '(modifier-match? mod_Shift_L mod_ignore_Shift))
  (assert-uim-false '(modifier-match? mod_Shift_L mod_Caps_Lock))
  (assert-uim-false '(modifier-match? mod_Shift_L
                                      (bitwise-ior mod_Shift_L
                                                   mod_Shift_R)))

  (assert-uim-false '(modifier-match? mod_Shift_R mod_None))
  (assert-uim-false '(modifier-match? mod_Shift_R mod_Shift))
  (assert-uim-false '(modifier-match? mod_Shift_R mod_Shift_L))
  (assert-uim-true  '(modifier-match? mod_Shift_R mod_Shift_R))
  (assert-uim-false '(modifier-match? mod_Shift_R mod_ignore_Shift))
  (assert-uim-false '(modifier-match? mod_Shift_R mod_Caps_Lock))

  (assert-uim-true  '(modifier-match? mod_ignore_Shift mod_None))
  (assert-uim-true  '(modifier-match? mod_ignore_Shift mod_Shift))
  (assert-uim-true  '(modifier-match? mod_ignore_Shift mod_Shift_L))
  (assert-uim-true  '(modifier-match? mod_ignore_Shift mod_Shift_R))
  (assert-uim-true  '(modifier-match? mod_ignore_Shift mod_ignore_Shift))
  (assert-uim-false '(modifier-match? mod_ignore_Shift mod_Caps_Lock))
  (assert-uim-true  '(modifier-match? mod_ignore_Shift
                                      (bitwise-ior mod_Shift
                                                   mod_Shift_L
                                                   mod_Shift_R)))
  (assert-uim-false '(modifier-match? mod_ignore_Shift
                                      (bitwise-ior mod_Shift
                                                   mod_Shift_L
                                                   mod_Shift_R
                                                   mod_Control_L)))

  ;; multiple modifiers
  (assert-uim-false '(modifier-match? (bitwise-ior mod_Shift_L
                                                   mod_Control_L)
                                      mod_None))
  (assert-uim-false '(modifier-match? (bitwise-ior mod_Shift_L
                                                   mod_Control_L)
                                      mod_Shift))
  (assert-uim-false '(modifier-match? (bitwise-ior mod_Shift_L
                                                   mod_Control_L)
                                      mod_Shift_L))
  (assert-uim-false '(modifier-match? (bitwise-ior mod_Shift_L
                                                   mod_Control_L)
                                      mod_Control_L))
  (assert-uim-true  '(modifier-match? (bitwise-ior mod_Shift_L
                                                   mod_Control_L)
                                      (bitwise-ior mod_Shift_L
                                                   mod_Control_L)))
  (assert-uim-false '(modifier-match? (bitwise-ior mod_Shift_L
                                                   mod_Control_L)
                                      (bitwise-ior mod_Shift
                                                   mod_Shift_L
                                                   mod_Control_L)))
  (assert-uim-false '(modifier-match? (bitwise-ior mod_Shift_L
                                                   mod_Control_L)
                                      (bitwise-ior mod_Shift_L
                                                   mod_Control
                                                   mod_Control_L)))
  (assert-uim-false '(modifier-match? (bitwise-ior mod_Shift_L
                                                   mod_Control_L)
                                      (bitwise-ior mod_Shift_L
                                                   mod_Control)))
  (assert-uim-false '(modifier-match? (bitwise-ior mod_Shift_L
                                                   mod_Control_L)
                                      mod_Shift_R))
  (assert-uim-false '(modifier-match? (bitwise-ior mod_Shift_L
                                                   mod_Control_L)
                                      mod_ignore_Shift))
  (assert-uim-false '(modifier-match? (bitwise-ior mod_Shift_L
                                                   mod_Control_L)
                                      mod_Caps_Lock))

  ;; multiple modifiers with ignore_Shift
  (assert-uim-false '(modifier-match? (bitwise-ior mod_ignore_Shift
                                                   mod_Shift_L
                                                   mod_Control_L)
                                      mod_None))
  (assert-uim-false '(modifier-match? (bitwise-ior mod_ignore_Shift
                                                   mod_Shift_L
                                                   mod_Control_L)
                                      mod_Shift))
  (assert-uim-false '(modifier-match? (bitwise-ior mod_ignore_Shift
                                                   mod_Shift_L
                                                   mod_Control_L)
                                      mod_Shift_L))
  (assert-uim-true  '(modifier-match? (bitwise-ior mod_ignore_Shift
                                                   mod_Shift_L
                                                   mod_Control_L)
                                      mod_Control_L))
  (assert-uim-true  '(modifier-match? (bitwise-ior mod_ignore_Shift
                                                   mod_Shift_L
                                                   mod_Control_L)
                                      (bitwise-ior mod_Shift_L
                                                   mod_Control_L)))
  (assert-uim-true  '(modifier-match? (bitwise-ior mod_ignore_Shift
                                                   mod_Shift_L
                                                   mod_Control_L)
                                      (bitwise-ior mod_Shift
                                                   mod_Shift_L
                                                   mod_Control_L)))
  (assert-uim-false '(modifier-match? (bitwise-ior mod_ignore_Shift
                                                   mod_Shift_L
                                                   mod_Control_L)
                                      (bitwise-ior mod_Shift_L
                                                   mod_Control
                                                   mod_Control_L)))
  (assert-uim-false '(modifier-match? (bitwise-ior mod_ignore_Shift
                                                   mod_Shift_L
                                                   mod_Control_L)
                                      (bitwise-ior mod_Shift_L
                                                   mod_Control)))
  (assert-uim-false '(modifier-match? (bitwise-ior mod_ignore_Shift
                                                   mod_Shift_L
                                                   mod_Control_L)
                                      mod_Shift_R))
  (assert-uim-false '(modifier-match? (bitwise-ior mod_ignore_Shift
                                                   mod_Shift_L
                                                   mod_Control_L)
                                      mod_ignore_Shift))
  (assert-uim-false '(modifier-match? (bitwise-ior mod_ignore_Shift
                                                   mod_Shift_L
                                                   mod_Control_L)
                                      mod_Caps_Lock))

  ;; ignoring Shift
  (assert-uim-false '(modifier-match? (bitwise-ior mod_ignore_Shift
                                                   mod_Control)
                                      mod_None))
  (assert-uim-true  '(modifier-match? (bitwise-ior mod_ignore_Shift
                                                   mod_Control)
                                      mod_Control_L))
  (assert-uim-true  '(modifier-match? (bitwise-ior mod_ignore_Shift
                                                   mod_Control)
                                      mod_Control_R))
  (assert-uim-true  '(modifier-match? (bitwise-ior mod_ignore_Shift
                                                   mod_Control)
                                      mod_Control))
  (assert-uim-true  '(modifier-match? (bitwise-ior mod_ignore_Shift
                                                   mod_Control)
                                      (bitwise-ior mod_Control_L
                                                   mod_Control_R)))
  (assert-uim-true  '(modifier-match? (bitwise-ior mod_ignore_Shift
                                                   mod_Control)
                                      (bitwise-ior mod_Control
                                                   mod_Control_L
                                                   mod_Control_R)))

  (assert-uim-false '(modifier-match? (bitwise-ior mod_ignore_Shift
                                                   mod_Control)
                                      mod_Shift))
  (assert-uim-true  '(modifier-match? (bitwise-ior mod_ignore_Shift
                                                   mod_Control)
                                      (bitwise-ior mod_Shift
                                                   mod_Control_L)))
  (assert-uim-true  '(modifier-match? (bitwise-ior mod_ignore_Shift
                                                   mod_Control)
                                      (bitwise-ior mod_Shift
                                                   mod_Control_R)))
  (assert-uim-true  '(modifier-match? (bitwise-ior mod_ignore_Shift
                                                   mod_Control)
                                      (bitwise-ior mod_Shift
                                                   mod_Control)))
  (assert-uim-true  '(modifier-match? (bitwise-ior mod_ignore_Shift
                                                   mod_Control)
                                      (bitwise-ior mod_Shift
                                                   mod_Control_L
                                                   mod_Control_R)))
  (assert-uim-true  '(modifier-match? (bitwise-ior mod_ignore_Shift
                                                   mod_Control)
                                      (bitwise-ior mod_Shift
                                                   mod_Control
                                                   mod_Control_L
                                                   mod_Control_R)))

  (assert-uim-false '(modifier-match? (bitwise-ior mod_ignore_Shift
                                                   mod_Control)
                                      mod_Shift_L))
  (assert-uim-true  '(modifier-match? (bitwise-ior mod_ignore_Shift
                                                   mod_Control)
                                      (bitwise-ior mod_Shift_L
                                                   mod_Control_L)))
  (assert-uim-true  '(modifier-match? (bitwise-ior mod_ignore_Shift
                                                   mod_Control)
                                      (bitwise-ior mod_Shift_L
                                                   mod_Control_R)))
  (assert-uim-true  '(modifier-match? (bitwise-ior mod_ignore_Shift
                                                   mod_Control)
                                      (bitwise-ior mod_Shift_L
                                                   mod_Control)))
  (assert-uim-true  '(modifier-match? (bitwise-ior mod_ignore_Shift
                                                   mod_Control)
                                      (bitwise-ior mod_Shift_L
                                                   mod_Control_L
                                                   mod_Control_R)))
  (assert-uim-true  '(modifier-match? (bitwise-ior mod_ignore_Shift
                                                   mod_Control)
                                      (bitwise-ior mod_Shift_L
                                                   mod_Control
                                                   mod_Control_L
                                                   mod_Control_R)))

  (assert-uim-false '(modifier-match? (bitwise-ior mod_ignore_Shift
                                                   mod_Control)
                                      (bitwise-ior mod_Shift_L
                                                   mod_Shift_R)))
  (assert-uim-true  '(modifier-match? (bitwise-ior mod_ignore_Shift
                                                   mod_Control)
                                      (bitwise-ior mod_Shift_L
                                                   mod_Shift_R
                                                   mod_Control_L)))
  (assert-uim-true  '(modifier-match? (bitwise-ior mod_ignore_Shift
                                                   mod_Control)
                                      (bitwise-ior mod_Shift_L
                                                   mod_Shift_R
                                                   mod_Control_R)))
  (assert-uim-true  '(modifier-match? (bitwise-ior mod_ignore_Shift
                                                   mod_Control)
                                      (bitwise-ior mod_Shift_L
                                                   mod_Shift_R
                                                   mod_Control)))
  (assert-uim-true  '(modifier-match? (bitwise-ior mod_ignore_Shift
                                                   mod_Control)
                                      (bitwise-ior mod_Shift_L
                                                   mod_Shift_R
                                                   mod_Control_L
                                                   mod_Control_R)))
  (assert-uim-true  '(modifier-match? (bitwise-ior mod_ignore_Shift
                                                   mod_Control)
                                      (bitwise-ior mod_Shift_L
                                                   mod_Shift_R
                                                   mod_Control
                                                   mod_Control_L
                                                   mod_Control_R)))

  (assert-uim-false '(modifier-match? (bitwise-ior mod_ignore_Shift
                                                   mod_Control)
                                      (bitwise-ior mod_Shift
                                                   mod_Shift_L
                                                   mod_Shift_R)))
  (assert-uim-true  '(modifier-match? (bitwise-ior mod_ignore_Shift
                                                   mod_Control)
                                      (bitwise-ior mod_Shift
                                                   mod_Shift_L
                                                   mod_Shift_R
                                                   mod_Control_L)))
  (assert-uim-true  '(modifier-match? (bitwise-ior mod_ignore_Shift
                                                   mod_Control)
                                      (bitwise-ior mod_Shift
                                                   mod_Shift_L
                                                   mod_Shift_R
                                                   mod_Control_R)))
  (assert-uim-true  '(modifier-match? (bitwise-ior mod_ignore_Shift
                                                   mod_Control)
                                      (bitwise-ior mod_Shift
                                                   mod_Shift_L
                                                   mod_Shift_R
                                                   mod_Control)))
  (assert-uim-true  '(modifier-match? (bitwise-ior mod_ignore_Shift
                                                   mod_Control)
                                      (bitwise-ior mod_Shift
                                                   mod_Shift_L
                                                   mod_Shift_R
                                                   mod_Control_L
                                                   mod_Control_R)))
  (assert-uim-true  '(modifier-match? (bitwise-ior mod_ignore_Shift
                                                   mod_Control)
                                      (bitwise-ior mod_Shift
                                                   mod_Shift_L
                                                   mod_Shift_R
                                                   mod_Control
                                                   mod_Control_L
                                                   mod_Control_R)))

  ;; ignoring multiple modifiers
  (assert-uim-true  '(modifier-match? (bitwise-ior mod_ignore_Shift
                                                   mod_ignore_Control
                                                   mod_Alt)
                                      mod_Alt))
  (assert-uim-true  '(modifier-match? (bitwise-ior mod_ignore_Shift
                                                   mod_ignore_Control
                                                   mod_Alt)
                                      (bitwise-ior mod_Shift
                                                   mod_Shift_L
                                                   mod_Shift_R
                                                   mod_Control
                                                   mod_Control_L
                                                   mod_Control_R
                                                   mod_Alt)))
  (assert-uim-true  '(modifier-match? (bitwise-ior mod_ignore_Shift
                                                   mod_ignore_Control
                                                   mod_ignore_Alt)
                                      mod_None))
  (assert-uim-true  '(modifier-match? (bitwise-ior mod_ignore_Shift
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
                                                   mod_Alt_R)))
  #f)

(define (test-logical-key?)
  (assert-uim-false      '(logical-key? 'lkey_Nonexistent))
  (assert-uim-false      '(logical-key? 'pkey_qwerty_a))
  (assert-uim-true-value '(logical-key? 'lkey_VoidSymbol))
  (assert-uim-true-value '(logical-key? 'lkey_BackSpace))
  (assert-uim-true-value '(logical-key? 'lkey_Shift_L))
  (assert-uim-true-value '(logical-key? 'lkey_Thumb_Shift_L))
  (assert-uim-true-value '(logical-key? 'lkey_F1))
  (assert-uim-true-value '(logical-key? 'lkey_space))
  (assert-uim-true-value '(logical-key? 'lkey_0))
  (assert-uim-true-value '(logical-key? 'lkey_a))
  (assert-uim-true-value '(logical-key? 'lkey_A))
  (assert-uim-true-value '(logical-key? 'lkey_yen))
  (assert-uim-true-value '(logical-key? 'lkey_dead_grave))
  #f)

(define (test-physical-key?)
  (assert-uim-false      '(physical-key? 'pkey_Nonexistent))
  (assert-uim-false      '(physical-key? 'lkey_a))
  (assert-uim-true-value '(physical-key? 'pkey_VoidSymbol))
  (assert-uim-false      '(physical-key? 'pkey_qwerty_BackSpace))
  (assert-uim-false      '(physical-key? 'pkey_qwerty_Shift_L))
  (assert-uim-false      '(physical-key? 'pkey_qwerty_F1))
  (assert-uim-false      '(physical-key? 'pkey_qwerty_space))
  (assert-uim-false      '(physical-key? 'pkey_qwerty_0))
  (assert-uim-false      '(physical-key? 'pkey_qwerty_a))
  (assert-uim-false      '(physical-key? 'pkey_jp106_yen))
  ;; Temporarily disabled  -- YamaKen 2008-05-10
  ;;(uim-eval '(require "physical-key.scm"))
  ;;(assert-uim-false      '(physical-key? 'pkey_Nonexistent))
  ;;(assert-uim-false      '(physical-key? 'lkey_a))
  ;;(assert-uim-true-value '(physical-key? 'pkey_VoidSymbol))
  ;;(assert-uim-true-value '(physical-key? 'pkey_qwerty_BackSpace))
  ;;(assert-uim-true-value '(physical-key? 'pkey_qwerty_Shift_L))
  ;;(assert-uim-true-value '(physical-key? 'pkey_qwerty_F1))
  ;;(assert-uim-true-value '(physical-key? 'pkey_qwerty_space))
  ;;(assert-uim-true-value '(physical-key? 'pkey_qwerty_0))
  ;;(assert-uim-true-value '(physical-key? 'pkey_qwerty_a))
  ;;(assert-uim-true-value '(physical-key? 'pkey_jp106_yen))
  #f)


(provide "test/test-ng-key")
