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

(require-extension (unittest))

(define lst #f)

(define (setup)
  (set! lst '(1 "2" three (4) 5 six "7" (8 8) -9)))

(test-begin "else")
(setup)
(test-equal "else"
            (cond
              ((equal? 1 11)
               1)
              ((eq? 'second 'twelve)
               2)
              ((string=? "third" "thirty")
               3)
              (else
               "else")))
(test-equal 3
            (cond
              ((equal? 1 11)
               1)
              ((eq? 'second 'twelve)
               2)
              ((string=? "third" "third")
               3)
              (else
               "else")))
(test-false (cond
             ((equal? 1 11)
              1)
             ((eq? 'second 'twelve)
              2)
             ((string=? "third" "thirty")
              3)
             (else
              #f)))
(test-end)

(test-begin "boolean?")
(setup)
(test-eq #t (boolean? #f))
(test-eq #t (boolean? #t))
(test-false (boolean? "foo"))
(test-false (boolean? 'foo))
(test-false (boolean? -1))
(test-false (boolean? 0))
(test-false (boolean? 1))
(test-false (boolean? 10))
(test-false (boolean? ()))
(test-false (boolean? '(1 "2" 'three)))
(test-false (boolean? 'nil))
(test-false (symbol-bound? 'nil))
(test-end)

(test-begin "integer?")
(setup)
(test-false (integer? #f))
(test-false (integer? "foo"))
(test-false (integer? 'foo))
(test-eq #t (integer? -1))
(test-eq #t (integer? 0))
(test-eq #t (integer? 1))
(test-eq #t (integer? 2))
(test-eq #t (integer? 10))
(test-false (integer? ()))
(test-false (integer? '(1 "2" 'three)))
(test-end)

(test-begin "list?")
(setup)
(test-false (list? #f))
(test-false (list? "foo"))
(test-false (list? 'foo))
(test-false (list? -1))
(test-false (list? 0))
(test-false (list? 1))
(test-false (list? 2))
(test-false (list? 10))
(test-eq #t (list? ()))
(test-eq #t (list? '(1)))
(test-eq #t (list? '(1 "2")))
(test-eq #t (list? '(1 "2" 'three)))
(test-end)

(test-begin "zero?")
(setup)
(test-error (zero? #f))
(test-error (zero? "foo"))
(test-error (zero? 'foo))
(test-false (zero? -2))
(test-false (zero? -1))
(test-eq #t (zero? 0))
(test-false (zero? 1))
(test-false (zero? 2))
(test-false (zero? 10))
(test-error (zero? ()))
(test-error (zero? '(1)))
(test-error (zero? '(1 "2")))
(test-error (zero? '(1 "2" 'three)))
(test-end)

(test-begin "positive?")
(setup)
(test-error (positive? #f))
(test-error (positive? "foo"))
(test-error (positive? 'foo))
(test-false (positive? -2))
(test-false (positive? -1))
(test-false (positive? 0))
(test-eq #t (positive? 1))
(test-eq #t (positive? 2))
(test-eq #t (positive? 10))
(test-error (positive? ()))
(test-error (positive? '(1)))
(test-error (positive? '(1 "2")))
(test-error (positive? '(1 "2" 'three)))
(test-end)

(test-begin "negative?")
(setup)
(test-error (negative? #f))
(test-error (negative? "foo"))
(test-error (negative? 'foo))
(test-eq #t (negative? -2))
(test-eq #t (negative? -1))
(test-false (negative? 0))
(test-false (negative? 1))
(test-false (negative? 2))
(test-false (negative? 10))
(test-error (negative? ()))
(test-error (negative? '(1)))
(test-error (negative? '(1 "2")))
(test-error (negative? '(1 "2" 'three)))
(test-end)

(test-begin "string->symbol")
(setup)
(test-equal 'foo1
            (string->symbol "foo1"))
(test-equal 'Foo1
            (string->symbol "Foo1"))
(test-equal 'FOO1
            (string->symbol "FOO1"))
;; SigScheme cannot read '1foo as a symbol literal
(test-equal "1foo"
            (symbol->string (string->symbol "1foo")))
(test-equal "1Foo"
            (symbol->string (string->symbol "1Foo")))
(test-equal "1FOO"
            (symbol->string (string->symbol "1FOO")))
(test-end)

(test-begin "map")
(setup)
(test-equal '()
            (map not ()))
(test-equal '(#f)
            (map not '(#t)))
(test-equal '(#f #t)
            (map not '(#t #f)))
(test-equal '(#f #t #f)
            (map not '(#t #f #t)))
(test-equal '()
            (map +
                 '()
                 '()))
(test-equal '(5)
            (map +
                 '(1)
                 '(4)))
(test-equal '(5 7)
            (map +
                 '(1 2)
                 '(4 5)))
(test-equal '(5 7 9)
            (map +
                 '(1 2 3)
                 '(4 5 6)))
(test-equal '()
            (map +
                 '()
                 '()
                 '()))
(test-equal '(12)
            (map +
                 '(1)
                 '(4)
                 '(7)))
(test-equal '(12 15)
            (map +
                 '(1 2)
                 '(4 5)
                 '(7 8)))
(test-equal '(12 15 18)
            (map +
                 '(1 2 3)
                 '(4 5 6)
                 '(7 8 9)))
(test-equal '()
            (map +
                 '()
                 '()
                 '()
                 '()))
(test-equal '(22)
            (map +
                 '(1)
                 '(4)
                 '(7)
                 '(10)))
(test-equal '(22 26)
            (map +
                 '(1 2)
                 '(4 5)
                 '(7 8)
                 '(10 11)))
(test-equal '(22 26 30)
            (map +
                 '(1 2 3)
                 '(4 5 6)
                 '(7 8 9)
                 '(10 11 12)))
(test-end)

(test-begin "for-each")
(setup)
(test-equal 3
            (let ((i 0))
              (for-each (lambda (x)
                          (set! i (+ i 1)))
                        '(1 2 3))
              i))
(test-equal 6
            (let ((i 0)
                  (sum 0))
              (for-each (lambda (x)
                          (set! i (+ i 1))
                          (set! sum (+ sum x)))
                        '(1 2 3))
              sum))
(test-equal 3
            (let ((i 0))
              (for-each (lambda (x y)
                          (set! i (+ i 1)))
                        '(1 2 3)
                        '(4 5 6))
              i))
(test-equal 21
            (let ((i 0)
                  (sum 0))
              (for-each (lambda (x y)
                          (set! i (+ i 1))
                          (set! sum (+ sum x y)))
                        '(1 2 3)
                        '(4 5 6))
               sum))
(test-end)

(test-begin "list-tail")
(setup)
(test-equal '(1 "2" three (4) 5 six "7" (8 8) -9)
            (list-tail lst 0))
(test-equal '("2" three (4) 5 six "7" (8 8) -9)
            (list-tail lst 1))
(test-equal '(three (4) 5 six "7" (8 8) -9)
            (list-tail lst 2))
(test-equal '((4) 5 six "7" (8 8) -9)
            (list-tail lst 3))
(test-equal '(-9)
            (list-tail lst 8))
(test-equal '()
            (list-tail lst 9))
(test-error (list-tail lst 10))
(test-error (list-tail lst -1))
(test-end)

(test-report-result)
