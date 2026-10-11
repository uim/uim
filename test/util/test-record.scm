;;; Copyright (c) 2003-2026 uim Project https://github.com/uim/uim
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

(test-begin "define-record")
(test-false (symbol-bound? 'test-rec-new))
(test-false (symbol-bound? 'test-rec-first))
(test-false (symbol-bound? 'test-rec-second))
(test-false (symbol-bound? 'test-rec-third))
(test-false (symbol-bound? 'test-rec-fourth))
(test-false (symbol-bound? 'test-rec-fifth))
(test-false (symbol-bound? 'test-rec-set-first!))
(test-false (symbol-bound? 'test-rec-set-second!))
(test-false (symbol-bound? 'test-rec-set-third!))
(test-false (symbol-bound? 'test-rec-set-fourth!))
(test-false (symbol-bound? 'test-rec-set-fifth!))

(define-record 'test-rec
  '((first #f)
    (second foo)
    (third "bar")
    (fourth 4)))

(test-eq #t (symbol-bound? 'test-rec-new))
(test-eq #t (symbol-bound? 'test-rec-first))
(test-eq #t (symbol-bound? 'test-rec-second))
(test-eq #t (symbol-bound? 'test-rec-third))
(test-eq #t (symbol-bound? 'test-rec-fourth))
(test-false (symbol-bound? 'test-rec-fifth))
(test-eq #t (symbol-bound? 'test-rec-set-first!))
(test-eq #t (symbol-bound? 'test-rec-set-second!))
(test-eq #t (symbol-bound? 'test-rec-set-third!))
(test-eq #t (symbol-bound? 'test-rec-set-fourth!))
(test-false (symbol-bound? 'test-rec-set-fifth!))

;; create with default values
(test-equal '(#f foo "bar" 4)
            (test-rec-new))
;; create with initializer values
(test-equal '(one two three four)
            (test-rec-new 'one 'two 'three 'four))
;; create with partial initialization
(test-equal '(one foo "bar" 4)
            (test-rec-new 'one))
(test-equal '(one two "bar" 4)
            (test-rec-new 'one 'two))
(test-equal '(one two three 4)
            (test-rec-new 'one 'two 'three))
(test-end)

(define (define-test-rec)
  (define-record 'test-rec
    '((first #f)
      (second foo)
      (third "bar")
      (fourth 4))))

(test-begin "getters")
(define-test-rec)
(test-eq #t (let ((trec (test-rec-new)))
              (equal? (test-rec-first trec)
                      #f)))
(test-eq #t (let ((trec (test-rec-new)))
              (equal? (test-rec-second trec)
                      'foo)))
(test-eq #t (let ((trec (test-rec-new)))
              (equal? (test-rec-third trec)
                      "bar")))
(test-eq #t (let ((trec (test-rec-new)))
              (equal? (test-rec-fourth trec)
                      4)))
(test-end)

(test-begin "setters")
(define-test-rec)
(test-eq #t (let ((trec (test-rec-new)))
              (test-rec-set-first! trec #t)
              (equal? (test-rec-first trec)
                      #t)))
(test-eq #t (let ((trec (test-rec-new)))
              (test-rec-set-second! trec 'fooFoo)
              (equal? (test-rec-second trec)
                      'fooFoo)))
(test-eq #t (let ((trec (test-rec-new)))
              (test-rec-set-third! trec "barBar")
              (equal? (test-rec-third trec)
                      "barBar")))
(test-eq #t (let ((trec (test-rec-new)))
              (test-rec-set-fourth! trec 44)
              (equal? (test-rec-fourth trec)
                      44)))
(test-end)

(test-report-result)
