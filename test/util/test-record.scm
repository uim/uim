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

(define-module test.util.test-record
  (use test.unit.test-case)
  (use test.uim-test))
(select-module test.util.test-record)

(define (setup)
  (uim-test-setup))

(define (teardown)
  (uim-test-teardown))

(define (test-define-record)
  (assert-uim-false '(symbol-bound? 'test-rec-new))
  (assert-uim-false '(symbol-bound? 'test-rec-first))
  (assert-uim-false '(symbol-bound? 'test-rec-second))
  (assert-uim-false '(symbol-bound? 'test-rec-third))
  (assert-uim-false '(symbol-bound? 'test-rec-fourth))
  (assert-uim-false '(symbol-bound? 'test-rec-fifth))
  (assert-uim-false '(symbol-bound? 'test-rec-set-first!))
  (assert-uim-false '(symbol-bound? 'test-rec-set-second!))
  (assert-uim-false '(symbol-bound? 'test-rec-set-third!))
  (assert-uim-false '(symbol-bound? 'test-rec-set-fourth!))
  (assert-uim-false '(symbol-bound? 'test-rec-set-fifth!))

  (uim-eval '(begin
               (define-record 'test-rec
                 '((first #f)
                   (second foo)
                   (third "bar")
                   (fourth 4)))))

  (assert-uim-true  '(symbol-bound? 'test-rec-new))
  (assert-uim-true  '(symbol-bound? 'test-rec-first))
  (assert-uim-true  '(symbol-bound? 'test-rec-second))
  (assert-uim-true  '(symbol-bound? 'test-rec-third))
  (assert-uim-true  '(symbol-bound? 'test-rec-fourth))
  (assert-uim-false '(symbol-bound? 'test-rec-fifth))
  (assert-uim-true  '(symbol-bound? 'test-rec-set-first!))
  (assert-uim-true  '(symbol-bound? 'test-rec-set-second!))
  (assert-uim-true  '(symbol-bound? 'test-rec-set-third!))
  (assert-uim-true  '(symbol-bound? 'test-rec-set-fourth!))
  (assert-uim-false '(symbol-bound? 'test-rec-set-fifth!))

  ;; create with default values
  (assert-uim-equal (uim ''(#f foo "bar" 4))
                    '(test-rec-new))
  ;; create with initializer values
  (assert-uim-equal '(one two three four)
                    '(test-rec-new 'one 'two 'three 'four))
  ;; create with partial initialization
  (assert-uim-equal '(one foo "bar" 4)
                    '(test-rec-new 'one))
  (assert-uim-equal '(one two "bar" 4)
                    '(test-rec-new 'one 'two))
  (assert-uim-equal '(one two three 4)
                    '(test-rec-new 'one 'two 'three))
  #f)

(define (define-record)
  (uim-eval '(begin
               (define-record 'test-rec
                 '((first #f)
                   (second foo)
                   (third "bar")
                   (fourth 4))))))

(define (test-getters)
  (define-record)
  (assert-uim-true '(let ((trec (test-rec-new)))
                      (equal? (test-rec-first trec)
                              #f)))
  (assert-uim-true '(let ((trec (test-rec-new)))
                      (equal? (test-rec-second trec)
                              'foo)))
  (assert-uim-true '(let ((trec (test-rec-new)))
                      (equal? (test-rec-third trec)
                              "bar")))
  (assert-uim-true '(let ((trec (test-rec-new)))
                      (equal? (test-rec-fourth trec)
                              4)))
  #f)

(define (test-setters)
  (define-record)
  (assert-uim-true '(let ((trec (test-rec-new)))
                      (test-rec-set-first! trec #t)
                      (equal? (test-rec-first trec)
                              #t)))
  (assert-uim-true '(let ((trec (test-rec-new)))
                      (test-rec-set-second! trec 'fooFoo)
                      (equal? (test-rec-second trec)
                              'fooFoo)))
  (assert-uim-true '(let ((trec (test-rec-new)))
                      (test-rec-set-third! trec "barBar")
                      (equal? (test-rec-third trec)
                              "barBar")))
  (assert-uim-true '(let ((trec (test-rec-new)))
                      (test-rec-set-fourth! trec 44)
                      (equal? (test-rec-fourth trec)
                              44)))
  #f)

(provide "test/util/test-record")
