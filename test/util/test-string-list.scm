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

(define-module test.util.test-string-list
  (use test.unit.test-case)
  (use test.uim-test-utils-new))
(select-module test.util.test-string-list)

(define (setup)
  (uim-test-setup))

(define (teardown)
  (uim-test-teardown))

(define (test-string-list-concat)
  (assert-equal ""
                (uim '(string-list-concat ())))
  (assert-equal ""
                (uim '(string-list-concat '(""))))
  (assert-equal "foo"
                (uim '(string-list-concat '("foo"))))
  (assert-equal "barfoo"
                (uim '(string-list-concat '("foo" "bar"))))
  (assert-equal "bazbarfoo"
                (uim '(string-list-concat '("foo" "bar" "baz"))))
  #f)

(define (test-string-find)
  (assert-false (uim-bool '(string-find () "")))
  (assert-false (uim-bool '(string-find () "quux")))
  (assert-false (uim-bool '(string-find '("foo") "")))
  (assert-true  (uim-bool '(string-find '("foo") "foo")))
  (assert-false (uim-bool '(string-find '("foo") "quux")))
  (assert-false (uim-bool '(string-find '("foo" "bar") "")))
  (assert-true  (uim-bool '(string-find '("foo" "bar") "foo")))
  (assert-true  (uim-bool '(string-find '("foo" "bar") "bar")))
  (assert-false (uim-bool '(string-find '("foo" "bar") "quux")))
  (assert-false (uim-bool '(string-find '("foo" "bar" "baz") "")))
  (assert-true  (uim-bool '(string-find '("foo" "bar" "baz") "foo")))
  (assert-true  (uim-bool '(string-find '("foo" "bar" "baz") "bar")))
  (assert-true  (uim-bool '(string-find '("foo" "bar" "baz") "baz")))
  (assert-false (uim-bool '(string-find '("foo" "bar" "baz") "quux")))
  #f)

(provide "test/util/test-string-list")
