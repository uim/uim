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
;;;

;; These tests are passed at revision 6605 (new repository)

(define-module test.util.test-r5rs
  (use test.unit.test-case)
  (use test.uim-test))
(select-module test.util.test-r5rs)

(define (setup)
  (uim-test-setup)
  (uim '(define lst '(1 "2" three (4) 5 six "7" (8 8) -9))))

(define (teardown)
  (uim-test-teardown))

(define (test-else)
  (assert-uim-equal "else"
                    '(cond
                       ((equal? 1 11)
                        1)
                       ((eq? 'second 'twelve)
                        2)
                       ((string=? "third" "thirty")
                        3)
                       (else
                        "else")))
  (assert-uim-equal 3
                    '(cond
                       ((equal? 1 11)
                        1)
                       ((eq? 'second 'twelve)
                        2)
                       ((string=? "third" "third")
                        3)
                       (else
                        "else")))
  (assert-uim-false '(cond
                      ((equal? 1 11)
                       1)
                      ((eq? 'second 'twelve)
                       2)
                      ((string=? "third" "thirty")
                       3)
                      (else
                       #f)))
  #f)

(define (test-boolean?)
  (assert-uim-true  '(boolean? #f))
  (assert-uim-true  '(boolean? #t))
  (assert-uim-false '(boolean? "foo"))
  (assert-uim-false '(boolean? 'foo))
  (assert-uim-false '(boolean? -1))
  (assert-uim-false '(boolean? 0))

  ;; SIOD
  ;;(assert-uim-true  '(boolean? 1))  ; Siod specific
  ;; SigScheme
  (assert-uim-false '(boolean? 1))

  (assert-uim-false '(boolean? 10))
  ;;(assert-uim-true  '(boolean? ())) ; SIOD specific
  (assert-uim-false '(boolean? ())) ; SigScheme
  (assert-uim-false '(boolean? '(1 "2" 'three)))
  (assert-uim-false '(boolean? 'nil))
  (assert-uim-false '(symbol-bound? 'nil))
  #f)

(define (test-integer?)
  (assert-uim-false '(integer? #f))
  (assert-uim-false '(integer? "foo"))
  (assert-uim-false '(integer? 'foo))
  (assert-uim-true  '(integer? -1))
  (assert-uim-true  '(integer? 0))
  (assert-uim-true  '(integer? 1))
  (assert-uim-true  '(integer? 2))
  (assert-uim-true  '(integer? 10))
  (assert-uim-false '(integer? ()))
  (assert-uim-false '(integer? '(1 "2" 'three)))
  #f)

(define (test-list?)
  ;;(assert-uim-true  '(list? #f)) ; SIOD specific
  (assert-uim-false '(list? #f)) ; SigScheme
  (assert-uim-false '(list? "foo"))
  (assert-uim-false '(list? 'foo))
  (assert-uim-false '(list? -1))
  (assert-uim-false '(list? 0))
  (assert-uim-false '(list? 1))
  (assert-uim-false '(list? 2))
  (assert-uim-false '(list? 10))
  (assert-uim-true  '(list? ()))
  (assert-uim-true  '(list? '(1)))
  (assert-uim-true  '(list? '(1 "2")))
  (assert-uim-true  '(list? '(1 "2" 'three)))
  #f)

(define (test-zero?)
  (assert-uim-error '(zero? #f))
  (assert-uim-error '(zero? "foo"))
  (assert-uim-error '(zero? 'foo))
  (assert-uim-false '(zero? -2))
  (assert-uim-false '(zero? -1))
  (assert-uim-true  '(zero? 0))
  (assert-uim-false '(zero? 1))
  (assert-uim-false '(zero? 2))
  (assert-uim-false '(zero? 10))
  (assert-uim-error '(zero? ()))
  (assert-uim-error '(zero? '(1)))
  (assert-uim-error '(zero? '(1 "2")))
  (assert-uim-error '(zero? '(1 "2" 'three)))
  #f)

(define (test-positive?)
  (assert-uim-error '(positive? #f))
  (assert-uim-error '(positive? "foo"))
  (assert-uim-error '(positive? 'foo))
  (assert-uim-false '(positive? -2))
  (assert-uim-false '(positive? -1))
  (assert-uim-false '(positive? 0))
  (assert-uim-true  '(positive? 1))
  (assert-uim-true  '(positive? 2))
  (assert-uim-true  '(positive? 10))
  (assert-uim-error '(positive? ()))
  (assert-uim-error '(positive? '(1)))
  (assert-uim-error '(positive? '(1 "2")))
  (assert-uim-error '(positive? '(1 "2" 'three)))
  #f)

(define (test-negative?)
  (assert-uim-error '(negative? #f))
  (assert-uim-error '(negative? "foo"))
  (assert-uim-error '(negative? 'foo))
  (assert-uim-true  '(negative? -2))
  (assert-uim-true  '(negative? -1))
  (assert-uim-false '(negative? 0))
  (assert-uim-false '(negative? 1))
  (assert-uim-false '(negative? 2))
  (assert-uim-false '(negative? 10))
  (assert-uim-error '(negative? ()))
  (assert-uim-error '(negative? '(1)))
  (assert-uim-error '(negative? '(1 "2")))
  (assert-uim-error '(negative? '(1 "2" 'three)))
  #f)

(define (test-string->symbol)
  (assert-uim-equal 'foo1
                    '(string->symbol "foo1"))
  (assert-uim-equal 'Foo1
                    '(string->symbol "Foo1"))
  (assert-uim-equal 'FOO1
                    '(string->symbol "FOO1"))
  (assert-uim-equal '1foo
                    '(string->symbol "1foo"))
  (assert-uim-equal '1Foo
                    '(string->symbol "1Foo"))
  (assert-uim-equal '1FOO
                    '(string->symbol "1FOO"))
  #f)

(define (test-map)
  (assert-uim-equal '()
                    '(map not ()))
  (assert-uim-equal (uim '(list #f))
                    '(map not '(#t)))

  ;; these two tests fail due to bug #617 'boolean value
  ;; representation is inconsistent'
  (assert-uim-equal (uim '(list #f #t))
                    '(map not '(#t #f)))
  (assert-uim-equal (uim '(list #f #t #f))
                    '(map not '(#t #f #t)))

  (assert-uim-equal '()
                    '(map +
                          '()
                          '()))
  (assert-uim-equal '(5)
                    '(map +
                          '(1)
                          '(4)))
  (assert-uim-equal '(5 7)
                    '(map +
                          '(1 2)
                          '(4 5)))
  (assert-uim-equal '(5 7 9)
                    '(map +
                          '(1 2 3)
                          '(4 5 6)))
  (assert-uim-equal '()
                    '(map +
                          '()
                          '()
                          '()))
  (assert-uim-equal '(12)
                    '(map +
                          '(1)
                          '(4)
                          '(7)))
  (assert-uim-equal '(12 15)
                    '(map +
                          '(1 2)
                          '(4 5)
                          '(7 8)))
  (assert-uim-equal '(12 15 18)
                    '(map +
                          '(1 2 3)
                          '(4 5 6)
                          '(7 8 9)))
  (assert-uim-equal '()
                    '(map +
                          '()
                          '()
                          '()
                          '()))
  (assert-uim-equal '(22)
                    '(map +
                          '(1)
                          '(4)
                          '(7)
                          '(10)))
  (assert-uim-equal '(22 26)
                    '(map +
                          '(1 2)
                          '(4 5)
                          '(7 8)
                          '(10 11)))
  (assert-uim-equal '(22 26 30)
                    '(map +
                          '(1 2 3)
                          '(4 5 6)
                          '(7 8 9)
                          '(10 11 12)))
  #f)

(define (test-for-each)
  (assert-uim-equal 3
                    '(let ((i 0))
                       (for-each (lambda (x)
                                   (set! i (+ i 1)))
                                 '(1 2 3))
                       i))
  (assert-uim-equal 6
                    '(let ((i 0)
                           (sum 0))
                       (for-each (lambda (x)
                                   (set! i (+ i 1))
                                   (set! sum (+ sum x)))
                                 '(1 2 3))
                       sum))
  (assert-uim-equal 3
                    '(let ((i 0))
                       (for-each (lambda (x y)
                                   (set! i (+ i 1)))
                                 '(1 2 3)
                                 '(4 5 6))
                       i))
  (assert-uim-equal 21
                    '(let ((i 0)
                           (sum 0))
                       (for-each (lambda (x y)
                                   (set! i (+ i 1))
                                   (set! sum (+ sum x y)))
                                 '(1 2 3)
                                 '(4 5 6))
                        sum))
  #f)

(define (test-list-tail)
  (assert-uim-equal '(1 "2" three (4) 5 six "7" (8 8) -9)
                    '(list-tail lst 0))
  (assert-uim-equal '("2" three (4) 5 six "7" (8 8) -9)
                    '(list-tail lst 1))
  (assert-uim-equal '(three (4) 5 six "7" (8 8) -9)
                    '(list-tail lst 2))
  (assert-uim-equal '((4) 5 six "7" (8 8) -9)
                    '(list-tail lst 3))
  (assert-uim-equal '(-9)
                    '(list-tail lst 8))
  (assert-uim-equal '()
                    '(list-tail lst 9))
  (assert-uim-error '(list-tail lst 10))
  (assert-uim-error '(list-tail lst -1))
  #f)

(provide "test/util/test-r5rs")
