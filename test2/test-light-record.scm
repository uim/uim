;;  test-light-record.scm: Unit tests for light-record.scm
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

(require "light-record.scm")

(set! *test-track-progress* #f)

(define dummy (lambda () (+ 1 2) #f))

(test-begin "record-field-spec-name")
(test-eq    'fld0
	    (record-field-spec-name 'fld0))
(test-eq    'fld0
	    (record-field-spec-name '(fld0)))
(test-eq    'fld0
	    (record-field-spec-name '(fld0 "val")))
(test-end)

(test-begin "record-field-spec-default-value")
(test-eq    #f
	    (record-field-spec-default-value 'fld0))
(test-eq    #f
	    (record-field-spec-default-value '(fld0)))
(test-equal "val"
	    (record-field-spec-default-value '(fld0 "val")))
(test-end)

;; These tests must be processed before consequent
;; %retrieve-record-accessor tests to pool the accessor procedures
;; correctly.
(test-begin "%make-record-getter list")
(test-eq    car
	    (%make-record-getter 0 list-ref))
(test-eq    cadr
	    (%make-record-getter 1 list-ref))
(test-eq    caddr
	    (%make-record-getter 2 list-ref))
(test-eq    (%make-record-getter 3 list-ref)
	    (%make-record-getter 3 list-ref))
(test-eqv   3
	    ((%make-record-getter 3 list-ref) '(0 1 2 3 4 5)))
(test-eq    (%make-record-getter 4 list-ref)
	    (%make-record-getter 4 list-ref))
(test-eqv   4
	    ((%make-record-getter 4 list-ref) '(0 1 2 3 4 5)))
(test-eq    (%make-record-getter 5 list-ref)
	    (%make-record-getter 5 list-ref))
(test-eqv   5
	    ((%make-record-getter 5 list-ref) '(0 1 2 3 4 5)))
(test-end)

;; These tests must be processed before consequent
;; %retrieve-record-accessor tests to pool the accessor procedures
;; correctly.
(test-begin "%make-record-getter vector")
(test-eq    (%make-record-getter 0 vector-ref)
	    (%make-record-getter 0 vector-ref))
(test-eqv   0
	    ((%make-record-getter 0 vector-ref) '#(0 1 2 3 4 5)))
(test-eq    (%make-record-getter 1 vector-ref)
	    (%make-record-getter 1 vector-ref))
(test-eqv   1
	    ((%make-record-getter 1 vector-ref) '#(0 1 2 3 4 5)))
(test-eq    (%make-record-getter 2 vector-ref)
	    (%make-record-getter 2 vector-ref))
(test-eqv   2
	    ((%make-record-getter 2 vector-ref) '#(0 1 2 3 4 5)))
(test-eq    (%make-record-getter 3 vector-ref)
	    (%make-record-getter 3 vector-ref))
(test-eqv   3
	    ((%make-record-getter 3 vector-ref) '#(0 1 2 3 4 5)))
(test-eq    (%make-record-getter 4 vector-ref)
	    (%make-record-getter 4 vector-ref))
(test-eqv   4
	    ((%make-record-getter 4 vector-ref) '#(0 1 2 3 4 5)))
(test-eq    (%make-record-getter 5 vector-ref)
	    (%make-record-getter 5 vector-ref))
(test-eqv   5
	    ((%make-record-getter 5 vector-ref) '#(0 1 2 3 4 5)))
(test-end)

;; These tests must be processed before consequent
;; %retrieve-record-accessor tests to pool the accessor procedures
;; correctly.
(test-begin "%make-record-setter list")
(test-eq    set-car!
	    (%make-record-setter 0 %list-set!))
(test-equal '(zero 1 2 3 4 5)
	    (let ((lst (list 0 1 2 3 4 5)))
	      ((%make-record-setter 0 %list-set!) lst 'zero)
	      lst))
(test-equal '(0 one 2 3 4 5)
	    (let ((lst (list 0 1 2 3 4 5)))
	      ((%make-record-setter 1 %list-set!) lst 'one)
	      lst))
(test-equal '(0 1 two 3 4 5)
	    (let ((lst (list 0 1 2 3 4 5)))
	      ((%make-record-setter 2 %list-set!) lst 'two)
	      lst))
(test-equal '(0 1 2 three 4 5)
	    (let ((lst (list 0 1 2 3 4 5)))
	      ((%make-record-setter 3 %list-set!) lst 'three)
	      lst))
(test-equal '(0 1 2 3 four 5)
	    (let ((lst (list 0 1 2 3 4 5)))
	      ((%make-record-setter 4 %list-set!) lst 'four)
	      lst))
(test-equal '(0 1 2 3 4 five)
	    (let ((lst (list 0 1 2 3 4 5)))
	      ((%make-record-setter 5 %list-set!) lst 'five)
	      lst))
(test-end)

;; These tests must be processed before consequent
;; %retrieve-record-accessor tests to pool the accessor procedures
;; correctly.
(test-begin "%make-record-setter vector")
(test-equal '#(zero 1 2 3 4 5)
	    (let ((vec (vector 0 1 2 3 4 5)))
	      ((%make-record-setter 0 vector-set!) vec 'zero)
	      vec))
(test-equal '#(0 one 2 3 4 5)
	    (let ((vec (vector 0 1 2 3 4 5)))
	      ((%make-record-setter 1 vector-set!) vec 'one)
	      vec))
(test-equal '#(0 1 two 3 4 5)
	    (let ((vec (vector 0 1 2 3 4 5)))
	      ((%make-record-setter 2 vector-set!) vec 'two)
	      vec))
(test-equal '#(0 1 2 three 4 5)
	    (let ((vec (vector 0 1 2 3 4 5)))
	      ((%make-record-setter 3 vector-set!) vec 'three)
	      vec))
(test-equal '#(0 1 2 3 four 5)
	    (let ((vec (vector 0 1 2 3 4 5)))
	      ((%make-record-setter 4 vector-set!) vec 'four)
	      vec))
(test-equal '#(0 1 2 3 4 five)
	    (let ((vec (vector 0 1 2 3 4 5)))
	      ((%make-record-setter 5 vector-set!) vec 'five)
	      vec))
(test-end)

(test-begin "%retrieve-record-accessor")
(test-eq    car
	    (%retrieve-record-accessor 0 list-ref dummy))
(test-eq    cadr
	    (%retrieve-record-accessor 1 list-ref dummy))
(test-eq    caddr
	    (%retrieve-record-accessor 2 list-ref dummy))
(test-true  (procedure?
	     (%retrieve-record-accessor 3 list-ref dummy)))
(test-true  (procedure?
	     (%retrieve-record-accessor 4 list-ref dummy)))
(test-true  (procedure?
	     (%retrieve-record-accessor 5 list-ref dummy)))
(test-eq    set-car!
	    (%retrieve-record-accessor 0 %list-set! dummy))
(test-true  (procedure?
	     (%retrieve-record-accessor 1 %list-set! dummy)))
(test-true  (procedure?
	     (%retrieve-record-accessor 2 %list-set! dummy)))
(test-true  (procedure?
	     (%retrieve-record-accessor 3 %list-set! dummy)))
(test-true  (procedure?
	     (%retrieve-record-accessor 4 %list-set! dummy)))
(test-true  (procedure?
	     (%retrieve-record-accessor 5 %list-set! dummy)))
(test-eq    'zero
	    (let ((lst (list 0 1 2 3 4 5)))
	      ((%retrieve-record-accessor 0 %list-set! dummy) lst 'zero)
	      ((%retrieve-record-accessor 0 list-ref dummy) lst)))
(test-eq    'one
	    (let ((lst (list 0 1 2 3 4 5)))
	      ((%retrieve-record-accessor 1 %list-set! dummy) lst 'one)
	      ((%retrieve-record-accessor 1 list-ref dummy) lst)))
(test-eq    'two
	    (let ((lst (list 0 1 2 3 4 5)))
	      ((%retrieve-record-accessor 2 %list-set! dummy) lst 'two)
	      ((%retrieve-record-accessor 2 list-ref dummy) lst)))
(test-eq    'three
	    (let ((lst (list 0 1 2 3 4 5)))
	      ((%retrieve-record-accessor 3 %list-set! dummy) lst 'three)
	      ((%retrieve-record-accessor 3 list-ref dummy) lst)))
(test-eq    'four
	    (let ((lst (list 0 1 2 3 4 5)))
	      ((%retrieve-record-accessor 4 %list-set! dummy) lst 'four)
	      ((%retrieve-record-accessor 4 list-ref dummy) lst)))
(test-eq    'five
	    (let ((lst (list 0 1 2 3 4 5)))
	      ((%retrieve-record-accessor 5 %list-set! dummy) lst 'five)
	      ((%retrieve-record-accessor 5 list-ref dummy) lst)))
(test-end)

(test-begin "%retrieve-record-accessor accessor identity")
(for-each (lambda (i)
	    (test-eq (%retrieve-record-accessor i list-ref dummy)
		     (%retrieve-record-accessor i list-ref dummy)))
	  (iota 6))
(for-each (lambda (i)
	    (test-eq (%retrieve-record-accessor i %list-set! dummy)
		     (%retrieve-record-accessor i %list-set! dummy)))
	  (iota 6))
(for-each (lambda (i)
	    (test-eq (%retrieve-record-accessor i vector-ref dummy)
		     (%retrieve-record-accessor i vector-ref dummy)))
	  (iota 6))
(for-each (lambda (i)
	    (test-eq (%retrieve-record-accessor i vector-set! dummy)
		     (%retrieve-record-accessor i vector-set! dummy)))
	  (iota 6))
(test-end)

(test-begin "%define-record-getter")
;; index 0
(test-eq    car
	    (%make-record-getter 0 list-ref))
(test-false (symbol-bound? 'recgt-fld0))
(%define-record-getter recgt fld0 0 list-ref)
(test-true  (procedure? recgt-fld0))
(test-eq    car recgt-fld0)
(test-eqv   0
	    (recgt-fld0 '(0 1 2 3 4 5)))
;; index 1
(test-eq    cadr
	    (%make-record-getter 1 list-ref))
(test-false (symbol-bound? 'recgt-fld1))
(%define-record-getter recgt fld1 1 list-ref)
(test-true  (procedure? recgt-fld1))
(test-eq    cadr recgt-fld1)
(test-eqv   1
	    (recgt-fld1 '(0 1 2 3 4 5)))
;; index 2
(test-eq    caddr
	    (%make-record-getter 2 list-ref))
(test-false (symbol-bound? 'recgt-fld2))
(%define-record-getter recgt fld2 2 list-ref)
(test-true  (procedure? recgt-fld2))
(test-eq    caddr recgt-fld2)
(test-eqv   2
	    (recgt-fld2 '(0 1 2 3 4 5)))
;; index 3
(test-false (symbol-bound? 'recgt-fld3))
(%define-record-getter recgt fld3 3 list-ref)
(test-true  (procedure? recgt-fld3))
(test-eqv   3
	    (recgt-fld3 '(0 1 2 3 4 5)))
;; index 4
(test-false (symbol-bound? 'recgt-fld4))
(%define-record-getter recgt fld4 4 list-ref)
(test-true  (procedure? recgt-fld4))
(test-eqv   4
	    (recgt-fld4 '(0 1 2 3 4 5)))
;; index 5
(test-false (symbol-bound? 'recgt-fld5))
(%define-record-getter recgt fld5 5 list-ref)
(test-true  (procedure? recgt-fld5))
(test-eqv   5
	    (recgt-fld5 '(0 1 2 3 4 5)))
(test-end)

(test-begin "%define-record-setter")
;; index 0
(test-eq    set-car!
	    (%make-record-setter 0 %list-set!))
(test-false (symbol-bound? 'recst-set-fld0!))
(%define-record-setter recst fld0 0 %list-set!)
(test-true  (procedure? recst-set-fld0!))
(test-eq    set-car! recst-set-fld0!)
(test-equal '(zero 1 2 3 4 5)
	    (let ((lst (list 0 1 2 3 4 5)))
	      (recst-set-fld0! lst 'zero)
	      lst))
;; index 1
(test-false (symbol-bound? 'recst-set-fld1!))
(%define-record-setter recst fld1 1 %list-set!)
(test-true  (procedure? recst-set-fld1!))
(test-equal '(0 one 2 3 4 5)
	    (let ((lst (list 0 1 2 3 4 5)))
	      (recst-set-fld1! lst 'one)
	      lst))
;; index 2
(test-false (symbol-bound? 'recst-set-fld2!))
(%define-record-setter recst fld2 2 %list-set!)
(test-true  (procedure? recst-set-fld2!))
(test-equal '(0 1 two 3 4 5)
	    (let ((lst (list 0 1 2 3 4 5)))
	      (recst-set-fld2! lst 'two)
	      lst))
;; index 3
(test-false (symbol-bound? 'recst-set-fld3!))
(%define-record-setter recst fld3 3 %list-set!)
(test-true  (procedure? recst-set-fld3!))
(test-equal '(0 1 2 three 4 5)
	    (let ((lst (list 0 1 2 3 4 5)))
	      (recst-set-fld3! lst 'three)
	      lst))
;; index 4
(test-false (symbol-bound? 'recst-set-fld4!))
(%define-record-setter recst fld4 4 %list-set!)
(test-true  (procedure? recst-set-fld4!))
(test-equal '(0 1 2 3 four 5)
	    (let ((lst (list 0 1 2 3 4 5)))
	      (recst-set-fld4! lst 'four)
	      lst))
;; index 5
(test-false (symbol-bound? 'recst-set-fld5!))
(%define-record-setter recst fld5 5 %list-set!)
(test-true  (procedure? recst-set-fld5!))
(test-equal '(0 1 2 3 4 five)
	    (let ((lst (list 0 1 2 3 4 5)))
	      (recst-set-fld5! lst 'five)
	      lst))
(test-end)

(test-begin "define-record-generic")
(test-false (symbol-bound? 'record-spec-recgn))
(test-false (symbol-bound? 'make-recgn))
(test-false (symbol-bound? 'recgn-copy))
(test-false (symbol-bound? 'recgn-fld0))
(test-false (symbol-bound? 'recgn-fld1))
(test-false (symbol-bound? 'recgn-fld2))
(test-false (symbol-bound? 'recgn-set-fld0!))
(test-false (symbol-bound? 'recgn-set-fld1!))
(test-false (symbol-bound? 'recgn-set-fld2!))
(define-record-generic recgn
  '(fld0 fld1)
  list-copy list-copy list-ref %list-set!)
;; record-spec
(test-equal '(fld0 fld1)
	    record-spec-recgn)
;; make-record
(procedure? make-recgn)
(test-equal '(#f #f)
	    (make-recgn))
(test-equal '(0 #f)
	    (make-recgn 0))
(test-equal '(0 1)
	    (make-recgn 0 1))
(test-error (make-recgn 0 1 2))
(test-equal '((+ 1 2) 3)
	    (make-recgn '(+ 1 2) (+ 1 2)))
;; record-copy
(let ((rec (make-recgn 'first 'second)))
  (test-false (eq? rec
		   (recgn-copy rec)))
  (test-false (eq? (cdr rec)
		   (cdr (recgn-copy rec))))
  (test-true  (equal? rec
		      (recgn-copy rec)))
  (test-true  (equal? (cdr rec)
		      (cdr (recgn-copy rec)))))
;; record-get index 0
(test-eq    #f
	    (recgn-fld0 (make-recgn)))
(test-eqv   0
	    (recgn-fld0 (make-recgn 0 1)))
;; record-get index 1
(test-eq    #f
	    (recgn-fld1 (make-recgn)))
(test-eqv   1
	    (recgn-fld1 (make-recgn 0 1)))
;; record-get index 2
(test-false (symbol-bound? 'recgn-fld2))
;; record-set! index 0
(test-equal '(zero #f)
	    (let ((rec (make-recgn)))
	      (recgn-set-fld0! rec 'zero)
	      rec))
(test-equal '(zero 1)
	    (let ((rec (make-recgn 0 1)))
	      (recgn-set-fld0! rec 'zero)
	      rec))
;; record-set! index 1
(test-equal '(#f one)
	    (let ((rec (make-recgn)))
	      (recgn-set-fld1! rec 'one)
	      rec))
(test-equal '(0 one)
	    (let ((rec (make-recgn 0 1)))
	      (recgn-set-fld1! rec 'one)
	      rec))
;; record-set! index 2
(test-false (symbol-bound? 'recgn-set-fld2!))
(test-end)

(test-begin "define-record-generic with some default values")
(define-record-generic recgna
  (append record-spec-recgn `((fld2 two) fld3 (fld4 ,(string-append "fo" "ur"))))
  list-copy list-copy list-ref %list-set!)
(test-equal '(fld0 fld1 (fld2 two) fld3 (fld4 "four"))
	    record-spec-recgna)
(test-equal '(#f #f two #f "four")
	    (make-recgna))
(test-equal '(0 1 2 #f "four")
	    (make-recgna 0 1 2))
(test-end)

(test-begin "null list-record")
(test-false (symbol-bound? 'record-spec-rec0))
(test-false (symbol-bound? 'make-rec0))
(test-false (symbol-bound? 'rec0-copy))
(define-list-record rec0
  '())
;; record-spec
(test-equal '()
	    record-spec-rec0)
;; make-record
(test-true  (procedure? make-rec0))
(test-equal '()
	    (make-rec0))
(test-error (make-rec0 0))
;; record-copy
(test-true  (procedure? rec0-copy))
(test-equal '()
	    (rec0-copy (make-rec0)))
(test-end)

(test-begin "1-member list-record")
(test-false (symbol-bound? 'record-spec-rec1))
(test-false (symbol-bound? 'make-rec1))
(test-false (symbol-bound? 'rec1-copy))
(define-list-record rec1
  '(fld0))
;; record-spec
(test-equal '(fld0)
	    record-spec-rec1)
;; make-record
(test-true  (procedure? make-rec1))
(test-equal '(#f)
	    (make-rec1))
(test-equal '(0)
	    (make-rec1 0))
(test-error (make-rec1 0 1))
;; record-copy
(test-true  (procedure? rec1-copy))
(test-equal '(#f)
	    (rec1-copy (make-rec1)))
(test-equal '(0)
	    (rec1-copy (make-rec1 0)))
;; record-get index 0
(test-eq    #f
	    (rec1-fld0 (make-rec1)))
(test-eqv   0
	    (rec1-fld0 (make-rec1 0)))
;; record-set! index 0
(test-equal '(zero)
	    (let ((rec (make-rec1)))
	      (rec1-set-fld0! rec 'zero)
	      rec))
(test-equal '(zero)
	    (let ((rec (make-rec1 0)))
	      (rec1-set-fld0! rec 'zero)
	      rec))
(test-end)

(test-begin "1-member list-record with default value")
(define-list-record rec1a
  `((fld0 ,(string-append "fir" "st"))))
(test-equal '((fld0 "first"))
	    record-spec-rec1a)
;; make-record
(test-true  (procedure? make-rec1a))
(test-equal '("first")
	    (make-rec1a))
(test-equal '(0)
	    (make-rec1a 0))
(test-error (make-rec1a 0 1))
;; record-copy
(test-true  (procedure? rec1a-copy))
(test-equal '("first")
	    (rec1a-copy (make-rec1a)))
(test-equal '(0)
	    (rec1a-copy (make-rec1a 0)))
;; record-get index 0
(test-equal "first"
	    (rec1a-fld0 (make-rec1a)))
(test-eqv   0
	    (rec1a-fld0 (make-rec1a 0)))
(test-end)

(test-begin "null vector-record")
(test-false (symbol-bound? 'record-spec-vrec0))
(test-false (symbol-bound? 'make-vrec0))
(test-false (symbol-bound? 'vrec0-copy))
(define-vector-record vrec0
  '())
;; record-spec
(test-equal '()
	    record-spec-vrec0)
;; make-record
(test-true  (procedure? make-vrec0))
(test-equal '#()
	    (make-vrec0))
(test-error (make-vrec0 0))
;; record-copy
(test-true  (procedure? vrec0-copy))
(test-equal '#()
	    (vrec0-copy (make-vrec0)))
(test-end)

(test-begin "1-member vector-record")
(test-false (symbol-bound? 'record-spec-vrec1))
(test-false (symbol-bound? 'make-vrec1))
(test-false (symbol-bound? 'vrec1-copy))
(define-vector-record vrec1
  '(fld0))
;; record-spec
(test-equal '(fld0)
	    record-spec-vrec1)
;; make-record
(test-true  (procedure? make-vrec1))
(test-equal '#(#f)
	    (make-vrec1))
(test-equal '#(0)
	    (make-vrec1 0))
(test-error (make-vrec1 0 1))
;; record-copy
(test-true  (procedure? vrec1-copy))
(test-equal '#(#f)
	    (vrec1-copy (make-vrec1)))
(test-equal '#(0)
	    (vrec1-copy (make-vrec1 0)))
;; record-get index 0
(%define-record-getter vrec1 fld0 0 vector-ref)
(test-eq    #f
	    (vrec1-fld0 (make-vrec1)))
(test-eqv   0
	    (vrec1-fld0 (make-vrec1 0)))
;; record-set! index 0
(test-equal '#(zero)
	    (let ((rec (make-vrec1)))
	      (vrec1-set-fld0! rec 'zero)
	      rec))
(test-equal '#(zero)
	    (let ((rec (make-vrec1 0)))
	      (vrec1-set-fld0! rec 'zero)
	      rec))
(test-end)

(test-begin "1-member vector-record with default value")
(define-vector-record vrec1a
  `((fld0 ,(string-append "fir" "st"))))
(test-equal '((fld0 "first"))
	    record-spec-vrec1a)
;; make-record
(test-true  (procedure? make-vrec1a))
(test-equal '#("first")
	    (make-vrec1a))
(test-equal '#(0)
	    (make-vrec1a 0))
(test-error (make-vrec1a 0 1))
;; record-copy
(test-true  (procedure? vrec1a-copy))
(test-equal '#("first")
	    (vrec1a-copy (make-vrec1a)))
(test-equal '#(0)
	    (vrec1a-copy (make-vrec1a 0)))
;; record-get index 0
(test-equal "first"
	    (vrec1a-fld0 (make-vrec1a)))
(test-eqv   0
	    (vrec1a-fld0 (make-vrec1a 0)))
(test-end)

(test-report-result)
