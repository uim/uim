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

;; These tests are passed at revision 5461 (new repository)

(define-module test.util.test-record
  (use test.unit.test-case)
  (use test.uim-test-utils-new))
(select-module test.util.test-record)

(define (setup)
  (uim-test-setup))

(define (teardown)
  (uim-test-teardown))

(define (test-define-record)
  (assert-false (uim-bool '(symbol-bound? 'test-rec-new)))
  (assert-false (uim-bool '(symbol-bound? 'test-rec-first)))
  (assert-false (uim-bool '(symbol-bound? 'test-rec-second)))
  (assert-false (uim-bool '(symbol-bound? 'test-rec-third)))
  (assert-false (uim-bool '(symbol-bound? 'test-rec-fourth)))
  (assert-false (uim-bool '(symbol-bound? 'test-rec-fifth)))
  (assert-false (uim-bool '(symbol-bound? 'test-rec-set-first!)))
  (assert-false (uim-bool '(symbol-bound? 'test-rec-set-second!)))
  (assert-false (uim-bool '(symbol-bound? 'test-rec-set-third!)))
  (assert-false (uim-bool '(symbol-bound? 'test-rec-set-fourth!)))
  (assert-false (uim-bool '(symbol-bound? 'test-rec-set-fifth!)))

  (assert-true  (uim-bool '(begin
                             (define-record 'test-rec
                               '((first #f)
                                 (second foo)
                                 (third "bar")
                                 (fourth 4)))
                             #t))) ;; suppress closure result
   
  (assert-true  (uim-bool '(symbol-bound? 'test-rec-new)))
  (assert-true  (uim-bool '(symbol-bound? 'test-rec-first)))
  (assert-true  (uim-bool '(symbol-bound? 'test-rec-second)))
  (assert-true  (uim-bool '(symbol-bound? 'test-rec-third)))
  (assert-true  (uim-bool '(symbol-bound? 'test-rec-fourth)))
  (assert-false (uim-bool '(symbol-bound? 'test-rec-fifth)))
  (assert-true  (uim-bool '(symbol-bound? 'test-rec-set-first!)))
  (assert-true  (uim-bool '(symbol-bound? 'test-rec-set-second!)))
  (assert-true  (uim-bool '(symbol-bound? 'test-rec-set-third!)))
  (assert-true  (uim-bool '(symbol-bound? 'test-rec-set-fourth!)))
  (assert-false (uim-bool '(symbol-bound? 'test-rec-set-fifth!)))

  ;; create with default values
  (assert-equal (uim ''(#f foo "bar" 4))
                (uim '(test-rec-new)))
  ;; create with initializer values
  (assert-equal '(one two three four)
                (uim '(test-rec-new 'one 'two 'three 'four)))
  ;; create with partial initialization
  (assert-equal '(one foo "bar" 4)
                (uim '(test-rec-new 'one)))
  (assert-equal '(one two "bar" 4)
                (uim '(test-rec-new 'one 'two)))
  (assert-equal '(one two three 4)
                (uim '(test-rec-new 'one 'two 'three)))
  #f)

(define (define-record)
  (uim '(begin
          (define-record 'test-rec
            '((first #f)
              (second foo)
              (third "bar")
              (fourth 4)))
          #t)))  ;; suppress closure result

(define (test-getters)
  (define-record)
  (assert-true (uim-bool '(let ((trec (test-rec-new)))
                            (equal? (test-rec-first trec)
                                    #f))))
  (assert-true (uim-bool '(let ((trec (test-rec-new)))
                            (equal? (test-rec-second trec)
                                    'foo))))
  (assert-true (uim-bool '(let ((trec (test-rec-new)))
                            (equal? (test-rec-third trec)
                                    "bar"))))
  (assert-true (uim-bool '(let ((trec (test-rec-new)))
                            (equal? (test-rec-fourth trec)
                                    4))))
  #f)

(define (test-setters)
  (define-record)
  (assert-true (uim-bool '(let ((trec (test-rec-new)))
                            (test-rec-set-first! trec #t)
                            (equal? (test-rec-first trec)
                                    #t))))
  (assert-true (uim-bool '(let ((trec (test-rec-new)))
                            (test-rec-set-second! trec 'fooFoo)
                            (equal? (test-rec-second trec)
                                    'fooFoo))))
  (assert-true (uim-bool '(let ((trec (test-rec-new)))
                            (test-rec-set-third! trec "barBar")
                            (equal? (test-rec-third trec)
                                    "barBar"))))
  (assert-true (uim-bool '(let ((trec (test-rec-new)))
                            (test-rec-set-fourth! trec 44)
                            (equal? (test-rec-fourth trec)
                                    44))))
  #f)

(provide "test/util/test-record")
