;;  FileName : test-vector.scm
;;  About    : unit test for R5RS vector
;;
;;  Copyright (C) 2005-2006 Kazuki Ohta <mover AT hct.zaq.ne.jp>
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

(load "./test/unittest.scm")

(define tn test-name)

;; vector
(assert-equal? "vector test" '#()        (vector))
(assert-equal? "vector test" '#(a)       (vector 'a))
(assert-equal? "vector test" '#(a b c d) (vector 'a 'b 'c 'd))

;; vector?
(assert-true "vector? test" (vector? '#()))
(assert-true "vector? test" (vector? '#(a)))
(assert-true "vector? test" (vector? '#(a b c d)))

;; make-vector
(assert-equal? "make-vector test" '#() (make-vector 0 #f))
(assert-equal? "make-vector test" '#() (make-vector 0 '()))
(assert-equal? "make-vector test" '#(#f)    (make-vector 1 #f))
(assert-equal? "make-vector test" '#(#f #f) (make-vector 2 #f))
(assert-equal? "make-vector test" '#(#(a b) #(a b)) (make-vector 2 '#(a b)))
(assert-error  "make-vector test" (lambda ()
				   (make-vector -1 #f)))

;; vector-length
(assert-equal? "vector-length test" 0 (vector-length '#()))
(assert-equal? "vector-length test" 1 (vector-length '#(a)))
(assert-equal? "vector-length test" 2 (vector-length '#(a b)))

;; vector-ref
(assert-equal? "vector-ref test" 'a (vector-ref '#(a b c d e) 0))
(assert-equal? "vector-ref test" 'c (vector-ref '#(a b c d e) 2))
(assert-equal? "vector-ref test" 'e (vector-ref '#(a b c d e) 4))
(assert-error  "vector-ref test" (lambda ()
				   (vector-ref '#() -1)))
(assert-error  "vector-ref test" (lambda ()
				   (vector-ref '#() 1)))

;; vector-set!
(tn "vector-set!")
(assert-equal? (tn)
	       '#(#t a "abc" #f ())
	       (begin
		 (define tmpvec (vector 1 'a "abc" #f '()))
		 (vector-set! tmpvec 0 #t)
		 tmpvec))
(assert-equal? (tn)
	       '#(1 a #t #f ())
	       (begin
		 (define tmpvec (vector 1 'a "abc" #f '()))
		 (vector-set! tmpvec 2 #t)
		 tmpvec))
(assert-equal? (tn)
	       '#(1 a "abc" #f #t)
	       (begin
		 (define tmpvec (vector 1 'a "abc" #f '()))
		 (vector-set! tmpvec 4 #t)
		 tmpvec))
(assert-error  (tn)
	       (lambda ()
		 (vector-set! '#() -1 #t)))
(assert-error  (tn)
	       (lambda ()
		 (vector-set! '#() 1 #t)))
(tn "vector-set! const vector")
(if (and (provided? "sigscheme")
         (provided? "const-vector-literal"))
    (begin
      (assert-error  (tn)
                     (lambda ()
                       (define tmpvec '#(1 'a "abc" #f '()))
                       (vector-set! tmpvec 0 #t)))
      (assert-error  (tn)
                     (lambda ()
                       (define tmpvec '#(1 'a "abc" #f '()))
                       (vector-set! tmpvec 2 #t)))
      (assert-error  (tn)
                     (lambda ()
                       (define tmpvec '#(1 'a "abc" #f '()))
                       (vector-set! tmpvec 4 #t))))
    (begin
      (assert-equal? (tn)
                     '#(#t a "abc" #f ())
                     (begin
                       (define tmpvec '#(1 'a "abc" #f '()))
                       (vector-set! tmpvec 0 #t)
                       tmpvec))
      (assert-equal? (tn)
                     '#(1 a #t #f ())
                     (begin
                       (define tmpvec '#(1 'a "abc" #f '()))
                       (vector-set! tmpvec 2 #t)
                       tmpvec))
      (assert-equal? (tn)
                     '#(1 a "abc" #f #t)
                     (begin
                       (define tmpvec '#(1 'a "abc" #f '()))
                       (vector-set! tmpvec 4 #t)
                       tmpvec))))

;; vector->list
(assert-equal? "vector->list test" '()    (vector->list '#()))
(assert-equal? "vector->list test" '(a)   (vector->list '#(a)))
(assert-equal? "vector->list test" '(a b) (vector->list '#(a b)))

;; list->vector
(assert-equal? "list->vector test" '#()    (list->vector '()))
(assert-equal? "list->vector test" '#(a)   (list->vector '(a)))
(assert-equal? "list->vector test" '#(a b) (list->vector '(a b)))

;; vector-fill!
(tn "vector-fill!")
(assert-equal? (tn)
	       '#()
	       (begin
		 (define tmpvec (vector))
		 (vector-fill! tmpvec #f)
		 tmpvec))
(assert-equal? (tn)
	       '#(#f #f #f #f)
	       (begin
		 (define tmpvec (vector #t #t #t #t))
		 (vector-fill! tmpvec #f)
		 tmpvec))
(tn "vector-fill! const vector")
(if (and (provided? "sigscheme")
         (provided? "const-vector-literal"))
    (begin
      (assert-error  (tn)
                     (lambda ()
                       (define tmpvec '#())
                       (vector-fill! tmpvec #f)))
      (assert-error  (tn)
                     (lambda ()
                       (define tmpvec '#(#t #t #t #t))
                       (vector-fill! tmpvec #f))))
    (begin
      (assert-equal? (tn)
                     '#()
                     (begin
                       (define tmpvec '#())
                       (vector-fill! tmpvec #f)
                       tmpvec))
      (assert-equal? (tn)
                     '#(#f #f #f #f)
                     (begin
                       (define tmpvec '#(#t #t #t #t))
                       (vector-fill! tmpvec #f)
                       tmpvec))))

(total-report)
