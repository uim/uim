;;; Copyright (c) 2003-2008 uim Project http://code.google.com/p/uim/
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
;;;

(define-module test.util.test-r5rs
  (use test.unit.test-case)
  (use test.uim-test-utils-new))
(select-module test.util.test-r5rs)

(define (setup)
  (uim-test-setup)
  (uim '(define lst '(1 "2" three (4) 5 six "7" (8 8) -9))))

(define (teardown)
  (uim-test-teardown))

(define (test-else)
  (assert-equal "else"
                (uim '(cond
                       ((equal? 1 11)
                        1)
                       ((eq? 'second 'twelve)
                        2)
                       ((string=? "third" "thirty")
                        3)
                       (else
                        "else"))))
  (assert-equal 3
                (uim '(cond
                       ((equal? 1 11)
                        1)
                       ((eq? 'second 'twelve)
                        2)
                       ((string=? "third" "third")
                        3)
                       (else
                        "else"))))
  (assert-false (uim-bool '(cond
                            ((equal? 1 11)
                             1)
                            ((eq? 'second 'twelve)
                             2)
                            ((string=? "third" "thirty")
                             3)
                            (else
                             #f))))
  #f)

(define (test-boolean?)
  (assert-true  (uim-bool '(boolean? #f)))
  (assert-true  (uim-bool '(boolean? #t)))
  (assert-false (uim-bool '(boolean? "foo")))
  (assert-false (uim-bool '(boolean? 'foo)))
  (assert-false (uim-bool '(boolean? -1)))
  (assert-false (uim-bool '(boolean? 0)))

  ;; SIOD
  ;;(assert-true  (uim-bool '(boolean? 1)))  ; Siod specific
  ;; SigScheme
  (assert-false (uim-bool '(boolean? 1)))

  (assert-false (uim-bool '(boolean? 10)))
  ;;(assert-true  (uim-bool '(boolean? ()))) ; SIOD specific
  (assert-false (uim-bool '(boolean? ()))) ; SigScheme
  (assert-false (uim-bool '(boolean? '(1 "2" 'three))))
  (assert-false (uim-bool '(boolean? 'nil)))
  (assert-false (uim-bool '(symbol-bound? 'nil)))
  #f)

(define (test-integer?)
  (assert-false (uim-bool '(integer? #f)))
  (assert-false (uim-bool '(integer? "foo")))
  (assert-false (uim-bool '(integer? 'foo)))
  (assert-true  (uim-bool '(integer? -1)))
  (assert-true  (uim-bool '(integer? 0)))
  (assert-true  (uim-bool '(integer? 1)))
  (assert-true  (uim-bool '(integer? 2)))
  (assert-true  (uim-bool '(integer? 10)))
  (assert-false (uim-bool '(integer? ())))
  (assert-false (uim-bool '(integer? '(1 "2" 'three))))
  #f)

(define (test-list?)
  ;;(assert-true  (uim-bool '(list? #f))) ; SIOD specific
  (assert-false (uim-bool '(list? #f))) ; SigScheme
  (assert-false (uim-bool '(list? "foo")))
  (assert-false (uim-bool '(list? 'foo)))
  (assert-false (uim-bool '(list? -1)))
  (assert-false (uim-bool '(list? 0)))
  (assert-false (uim-bool '(list? 1)))
  (assert-false (uim-bool '(list? 2)))
  (assert-false (uim-bool '(list? 10)))
  (assert-true  (uim-bool '(list? ())))
  (assert-true  (uim-bool '(list? '(1))))
  (assert-true  (uim-bool '(list? '(1 "2"))))
  (assert-true  (uim-bool '(list? '(1 "2" 'three))))
  #f)

(define (test-zero?)
  (assert-error (lambda () (uim-bool '(zero? #f))))
  (assert-error (lambda () (uim-bool '(zero? "foo"))))
  (assert-error (lambda () (uim-bool '(zero? 'foo))))
  (assert-false (uim-bool '(zero? -2)))
  (assert-false (uim-bool '(zero? -1)))
  (assert-true  (uim-bool '(zero? 0)))
  (assert-false (uim-bool '(zero? 1)))
  (assert-false (uim-bool '(zero? 2)))
  (assert-false (uim-bool '(zero? 10)))
  (assert-error (lambda () (uim-bool '(zero? ()))))
  (assert-error (lambda () (uim-bool '(zero? '(1)))))
  (assert-error (lambda () (uim-bool '(zero? '(1 "2")))))
  (assert-error (lambda () (uim-bool '(zero? '(1 "2" 'three)))))
  #f)

(define (test-positive?)
  (assert-error (lambda () (uim-bool '(positive? #f))))
  (assert-error (lambda () (uim-bool '(positive? "foo"))))
  (assert-error (lambda () (uim-bool '(positive? 'foo))))
  (assert-false (uim-bool '(positive? -2)))
  (assert-false (uim-bool '(positive? -1)))
  (assert-false (uim-bool '(positive? 0)))
  (assert-true  (uim-bool '(positive? 1)))
  (assert-true  (uim-bool '(positive? 2)))
  (assert-true  (uim-bool '(positive? 10)))
  (assert-error (lambda () (uim-bool '(positive? ()))))
  (assert-error (lambda () (uim-bool '(positive? '(1)))))
  (assert-error (lambda () (uim-bool '(positive? '(1 "2")))))
  (assert-error (lambda () (uim-bool '(positive? '(1 "2" 'three)))))
  #f)

(define (test-negative?)
  (assert-error (lambda () (uim-bool '(negative? #f))))
  (assert-error (lambda () (uim-bool '(negative? "foo"))))
  (assert-error (lambda () (uim-bool '(negative? 'foo))))
  (assert-true  (uim-bool '(negative? -2)))
  (assert-true  (uim-bool '(negative? -1)))
  (assert-false (uim-bool '(negative? 0)))
  (assert-false (uim-bool '(negative? 1)))
  (assert-false (uim-bool '(negative? 2)))
  (assert-false (uim-bool '(negative? 10)))
  (assert-error (lambda () (uim-bool '(negative? ()))))
  (assert-error (lambda () (uim-bool '(negative? '(1)))))
  (assert-error (lambda () (uim-bool '(negative? '(1 "2")))))
  (assert-error (lambda () (uim-bool '(negative? '(1 "2" 'three)))))
  #f)

(define (test-string->symbol)
  (assert-equal 'foo1
                (uim '(string->symbol "foo1")))
  (assert-equal 'Foo1
                (uim '(string->symbol "Foo1")))
  (assert-equal 'FOO1
                (uim '(string->symbol "FOO1")))
  (assert-equal '1foo
                (uim '(string->symbol "1foo")))
  (assert-equal '1Foo
                (uim '(string->symbol "1Foo")))
  (assert-equal '1FOO
                (uim '(string->symbol "1FOO")))
  #f)

(define (test-map)
  (assert-equal ()
                (uim '(map not ())))
  (assert-equal (uim '(list #f))
                (uim '(map not '(#t))))

  ;; these two tests fail due to bug #617 'boolean value
  ;; representation is inconsistent'
  (assert-equal (uim '(list #f #t))
                (uim '(map not '(#t #f))))
  (assert-equal (uim '(list #f #t #f))
                (uim '(map not '(#t #f #t))))

  (assert-equal '()
                (uim '(map +
                           ()
                           ())))
  (assert-equal '(5)
                (uim '(map +
                           '(1)
                           '(4))))
  (assert-equal '(5 7)
                (uim '(map +
                           '(1 2)
                           '(4 5))))
  (assert-equal '(5 7 9)
                (uim '(map +
                           '(1 2 3)
                           '(4 5 6))))
  (assert-equal '()
                (uim '(map +
                           '()
                           '()
                           '())))
  (assert-equal '(12)
                (uim '(map +
                           '(1)
                           '(4)
                           '(7))))
  (assert-equal '(12 15)
                (uim '(map +
                           '(1 2)
                           '(4 5)
                           '(7 8))))
  (assert-equal '(12 15 18)
                (uim '(map +
                           '(1 2 3)
                           '(4 5 6)
                           '(7 8 9))))
  (assert-equal '()
                (uim '(map +
                           '()
                           '()
                           '()
                           '())))
  (assert-equal '(22)
                (uim '(map +
                           '(1)
                           '(4)
                           '(7)
                           '(10))))
  (assert-equal '(22 26)
                (uim '(map +
                           '(1 2)
                           '(4 5)
                           '(7 8)
                           '(10 11))))
  (assert-equal '(22 26 30)
                (uim '(map +
                           '(1 2 3)
                           '(4 5 6)
                           '(7 8 9)
                           '(10 11 12))))
  #f)

(define (test-for-each)
  (assert-equal 3
                (uim '(let ((i 0))
                        (for-each (lambda (x)
                                    (set! i (+ i 1)))
                                  '(1 2 3))
                        i)))
  (assert-equal 6
                (uim '(let ((i 0)
                            (sum 0))
                        (for-each (lambda (x)
                                    (set! i (+ i 1))
                                    (set! sum (+ sum x)))
                                  '(1 2 3))
                        sum)))
  (assert-equal 3
                (uim '(let ((i 0))
                        (for-each (lambda (x y)
                                    (set! i (+ i 1)))
                                  '(1 2 3)
                                  '(4 5 6))
                        i)))
  (assert-equal 21
                (uim '(let ((i 0)
                            (sum 0))
                        (for-each (lambda (x y)
                                    (set! i (+ i 1))
                                    (set! sum (+ sum x y)))
                                  '(1 2 3)
                                  '(4 5 6))
                        sum)))
  #f)

(define (test-list-tail)
  (assert-equal '(1 "2" three (4) 5 six "7" (8 8) -9)
                (uim '(list-tail lst 0)))
  (assert-equal '("2" three (4) 5 six "7" (8 8) -9)
                (uim '(list-tail lst 1)))
  (assert-equal '(three (4) 5 six "7" (8 8) -9)
                (uim '(list-tail lst 2)))
  (assert-equal '((4) 5 six "7" (8 8) -9)
                (uim '(list-tail lst 3)))
  (assert-equal '(-9)
                (uim '(list-tail lst 8)))
  (assert-equal '()
                (uim '(list-tail lst 9)))
  (assert-error (lambda ()
                  (uim '(list-tail lst 10))))
  (assert-error (lambda ()
                  (uim '(list-tail lst -1))))
  #f)

(provide "test/util/test-r5rs")
