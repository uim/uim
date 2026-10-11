;;  test-wlos.scm: Unit tests for wlos.scm
;;
;;; Copyright (c) 2008-2013 uim Project https://github.com/uim/uim
;;
;;  All rights reserved.
;;
;;  Redistribution and use in source and binary forms, with or without
;;  modification, are permitted provided that the following conditions
;;  are met:
;;
;;  1. Redistributions of source code must retain the above copyright
;;     notice, this list of conditions and the following disclaimer.
;;  2. Redistributions in binary form must reproduce the above copyright
;;     notice, this list of conditions and the following disclaimer in the
;;     documentation and/or other materials provided with the distribution.
;;  3. Neither the name of authors nor the names of its contributors
;;     may be used to endorse or promote products derived from this software
;;     without specific prior written permission.
;;
;;  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS
;;  IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
;;  THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;;  PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR
;;  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;;  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;;  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(require-extension (unittest))

(require "wlos.scm")

(set! *test-track-progress* #f)


(test-begin "object")
;; class relationships
(test-false (class-is-a? object class))
(test-true  (class-is-a? object object))
(test-false (object-is-a? (make-object) class))
(test-true  (object-is-a? (make-object) object))
(test-error (class-superclass object))
;; object identity
(test-true  (object-equal? (make-object) (make-object)))
(test-true  (let ((o (make-object)))
	      (object-equal? o o)))
(test-false (eq? (make-object) (make-object)))
;; object-equal? is not a identity comparison method
(test-true  (object-equal? (make-object)
			   (object-copy (make-object))))
(test-true  (object-equal? (object-copy (make-object))
			   (object-copy (make-object))))
(test-end)

(test-begin "define-class comparable")
(test-false (symbol-bound? 'comparable))
(test-false (symbol-bound? 'record-spec-comparable))
(test-false (symbol-bound? 'make-comparable))
(test-false (symbol-bound? 'comparable-class))
(test-false (symbol-bound? 'comparable-equal?))
(test-false (symbol-bound? 'comparable-copy))
(test-false (symbol-bound? 'comparable-<))
(test-false (symbol-bound? 'comparable-<=))
(test-false (symbol-bound? 'comparable->))
(test-false (symbol-bound? 'comparable->=))
(define-class comparable object
  '()
  '(<
    <=
    >
    >=))
;; specs
(test-true  (vector? comparable))
(test-equal `((class ,comparable))
	    record-spec-comparable)
;; class relationships
(test-eq    object
	    (class-superclass comparable))
(test-true  (class-is-a? comparable comparable))
(test-true  (class-is-a? comparable object))
(test-false (class-is-a? comparable class))
;; instantiation
(test-true  (procedure? make-comparable))
(test-equal (vector comparable)
	    (make-comparable))
(test-error (make-comparable #f))
(test-error (make-comparable comparable))
(test-error (make-comparable #f #f))
;; class
(test-true  (procedure? comparable-class))
(test-eq    comparable
	    (comparable-class (make-comparable)))
;; equal?
(test-true  (procedure? comparable-equal?))
;; copy
(test-true  (procedure? comparable-copy))
;; <
(test-true  (procedure? comparable-<))
(test-eq    %undefined-method
	    (class-find-method comparable '<))
;; <=
(test-true  (procedure? comparable-<=))
(test-eq    %undefined-method
	    (class-find-method comparable '<=))
;; >
(test-true  (procedure? comparable->))
(test-eq    %undefined-method
	    (class-find-method comparable '>))
; >=
(test-true  (procedure? comparable->=))
(test-eq    %undefined-method
	    (class-find-method comparable '>=))
(test-end)

(test-begin "define-class comparable-number-str")
(test-false (symbol-bound? 'comparable-number-str))
(test-false (symbol-bound? 'record-spec-comparable-number-str))
(test-false (symbol-bound? 'make-comparable-number-str))
(test-false (symbol-bound? 'comparable-number-str-class))
(test-false (symbol-bound? 'comparable-number-str-equal?))
(test-false (symbol-bound? 'comparable-number-str-copy))
(test-false (symbol-bound? 'comparable-number-str-<))
(test-false (symbol-bound? 'comparable-number-str-<=))
(test-false (symbol-bound? 'comparable-number-str->))
(test-false (symbol-bound? 'comparable-number-str->=))
(define-class comparable-number-str comparable
  '(value)
  '())
;; specs
(test-true  (vector? comparable-number-str))
(test-equal `((class ,comparable-number-str) value)
	    record-spec-comparable-number-str)
;; class relationships
(test-eq    comparable
	    (class-superclass comparable-number-str))
(test-true  (class-is-a? comparable-number-str comparable-number-str))
(test-true  (class-is-a? comparable-number-str comparable))
(test-true  (class-is-a? comparable-number-str object))
(test-false (class-is-a? comparable-number-str class))
;;;; instantiation
(test-true  (procedure? make-comparable-number-str))
(test-equal (vector comparable-number-str #f)
	    (make-comparable-number-str))
(test-equal (vector comparable-number-str "3")
	    (make-comparable-number-str "3"))
(test-error (make-comparable-number-str #f #f))
(test-error (make-comparable-number-str comparable-number-str #f))
(test-error (make-comparable-number-str #f #f #f))
;; value
(test-eq    #f
	    (comparable-number-str-value (make-comparable-number-str)))
(test-equal "3"
	    (comparable-number-str-value (make-comparable-number-str "3")))
(test-equal "3"
	    (let ((obj (make-comparable-number-str)))
	      (comparable-number-str-set-value! obj "3")
	      (comparable-number-str-value obj)))
(test-equal "4"
	    (let ((obj (make-comparable-number-str "3")))
	      (comparable-number-str-set-value! obj "4")
	      (comparable-number-str-value obj)))
;; class
(test-true  (procedure? comparable-number-str-class))
(test-eq    comparable-number-str
	    (comparable-number-str-class (make-comparable-number-str)))
;; equal?
(test-true  (procedure? comparable-number-str-equal?))
;; copy
(test-true  (procedure? comparable-number-str-copy))
;; <
(test-true  (procedure? comparable-number-str-<))
(test-eq    %undefined-method
	    (class-find-method comparable-number-str '<))
;; <=
(test-true  (procedure? comparable-number-str-<=))
(test-eq    %undefined-method
	    (class-find-method comparable-number-str '<=))
;; >
(test-true  (procedure? comparable-number-str->))
(test-eq    %undefined-method
	    (class-find-method comparable-number-str '>))
; >=
(test-true  (procedure? comparable-number-str->=))
(test-eq    %undefined-method
	    (class-find-method comparable-number-str '>=))
(test-end)

(test-begin "class-set-method! comparable-number-str")
(define make-comparable-number-str-compare
  (lambda (compare)
    (lambda (self other)
      (compare (string->number (comparable-number-str-value self))
	       (string->number (comparable-number-str-value other))))))
;; equal?
(class-set-method! comparable-number-str equal?
		   (make-comparable-number-str-compare =))
(test-true  (procedure? comparable-number-str-equal?))
(test-false (eq? equal?
		 (class-find-method comparable-number-str 'equal?)))
;; <
(class-set-method! comparable-number-str <
		   (make-comparable-number-str-compare <))
(test-true  (procedure? comparable-number-str-<))
(test-false (eq? %undefined-method
		 (class-find-method comparable-number-str '<)))
;; <=
(class-set-method! comparable-number-str <=
		   (make-comparable-number-str-compare <=))
(test-true  (procedure? comparable-number-str-<=))
(test-false (eq? %undefined-method
		 (class-find-method comparable-number-str '<=)))
;; >
(class-set-method! comparable-number-str >
		   (make-comparable-number-str-compare >))
(test-true  (procedure? comparable-number-str->))
(test-false (eq? %undefined-method
		 (class-find-method comparable-number-str '>)))
; >=
(class-set-method! comparable-number-str >=
		   (make-comparable-number-str-compare >=))
(test-true  (procedure? comparable-number-str->=))
(test-false (eq? %undefined-method
		 (class-find-method comparable-number-str '>=)))
(test-end)

(test-begin "comparable-number-str method call by index")
(define foo (make-comparable-number-str "31"))
(define bar (make-comparable-number-str "153"))
(test-true  (comparable-<  foo bar))
(test-false (comparable-<  foo foo))
(test-true  (comparable-<= foo bar))
(test-true  (comparable-<= foo foo))
(test-true  (comparable->  bar foo))
(test-false (comparable->  foo foo))
(test-true  (comparable->= bar foo))
(test-true  (comparable->= foo foo))
(test-true  (comparable-equal? foo foo))
(test-false (comparable-equal? foo bar))
(test-true  (comparable-equal? foo (object-copy foo)))
(test-end)

(test-begin "comparable-number-str method call by name")
(test-true  (call-method '<  foo bar))
(test-false (call-method '<  foo foo))
(test-true  (call-method '<= foo bar))
(test-true  (call-method '<= foo foo))
(test-true  (call-method '>  bar foo))
(test-false (call-method '>  foo foo))
(test-true  (call-method '>= bar foo))
(test-true  (call-method '>= foo foo))
(test-true  (call-method 'equal? foo foo))
(test-false (call-method 'equal? foo bar))
(test-true  (call-method 'equal? foo (object-copy foo)))
(test-end)

(test-begin "comparable-number-str with SRFI-95 sort")
(require-extension (srfi 95))
(define comparables
  (map make-comparable-number-str
       '("3" "-5" "13" "-1" "0" "43")))
(define sorted (sort comparables comparable-<))
(test-equal '("-5" "-1" "0" "3" "13" "43")
	    (map comparable-number-str-value sorted))
(test-end)

(test-report-result)
