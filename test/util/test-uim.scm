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

(define-module test.util.test-uim
  (use test.unit.test-case)
  (use test.uim-test))
(select-module test.util.test-uim)

(define (setup)
  (uim-test-setup))

(define (teardown)
  (uim-test-teardown))

(define (test-make-scm-pathname)
  (assert-uim-equal (uim '(map (lambda (x) (string-append x "/"))
                               (string-split (load-path) ":")))
                    '(make-scm-pathname ""))
  (assert-uim-equal (uim '(map (lambda (x) (string-append x "/file"))
                               (string-split (load-path) ":")))
                    '(make-scm-pathname "file"))
  (assert-uim-equal '("/absolute/path/file")
                    '(make-scm-pathname "/absolute/path/file"))
  (assert-uim-equal '("/")
                    '(make-scm-pathname "/"))
  #f)

(define (test-interaction-environment)
  (assert-uim-true  '(eval '(symbol-bound? 'filter-map)
                           (interaction-environment)))
  (assert-uim-false '(eval '(symbol-bound? 'filter-baz)
                           (interaction-environment)))
  ;; SigScheme: syntactic keyword 'define' cannot be evaluated as value
  (uim '(eval (list 'define 'filter-baz filter-map)
              (interaction-environment)))
  (assert-uim-true  '(eval '(symbol-bound? 'filter-baz)
                           (interaction-environment)))
  (assert-uim-true  '(eq? filter-baz filter-map))
  #f)

(define (test-%%enclose-another-env)
  (assert-uim-equal 3
                    '(let* ((x 1)
                            (y 2)
                            (closure (lambda ()
                                       (+ x y))))
                       (closure)))
  (assert-uim-equal 10
                    '(let* ((x 1)
                            (y 2)
                            (closure (lambda ()
                                       (+ x y)))
                            ;; SIOD: broken frame for SigScheme
                            ;;  (another-env '((x . 4)
                            ;;                 (y . 6)))
                            ;; SigScheme: valid 2-frame env
                            (another-env '(((x) . (4))
                                           ((y) . (6)))))
                       (set! closure
                             (%%enclose-another-env closure another-env))
                       (closure)))
  ;; causes error since z is not exist in the another-env
  (assert-uim-error '(let* ((x 1)
                            (y 2)
                            (z 3)
                            (closure (lambda ()
                                       (+ x y z)))
                            ;; SIOD: broken frame for SigScheme
                            ;;  (another-env '((x . 4)
                            ;;                 (y . 6)))
                            ;; SigScheme: valid 2-frame env
                            (another-env '(((x) . (4))
                                           ((y) . (6)))))
                       (set! closure (%%enclose-another-env closure another-env))
                       (closure)))
  #f)

(define (test-getenv)
  (assert-uim-equal (sys-getenv "PWD")
                    '(getenv "PWD"))
  (assert-uim-false '(getenv "UIM_NONEXISTING_ENV"))
  #f)

(define (test-setenv)
  (assert-uim-false '(getenv "UIM_NONEXISTING_ENV"))
  (assert-uim-true  '(setenv "UIM_NONEXISTING_ENV" "FOO" #f))
  (assert-uim-equal "FOO"
                    '(getenv "UIM_NONEXISTING_ENV"))
  (assert-uim-true  '(setenv "UIM_NONEXISTING_ENV" "BAR" #f))
  (assert-uim-equal "FOO"
                    '(getenv "UIM_NONEXISTING_ENV"))
  (assert-uim-true  '(setenv "UIM_NONEXISTING_ENV" "BAR" #t))
  (assert-uim-equal "BAR"
                    '(getenv "UIM_NONEXISTING_ENV"))
  #f)

(define (test-unsetenv)
  (assert-uim-true  '(setenv "UIM_NONEXISTING_ENV" "BAR" #t))
  (assert-uim-equal "BAR"
                    '(getenv "UIM_NONEXISTING_ENV"))
  (assert-uim-true  '(unsetenv "UIM_NONEXISTING_ENV"))
  (assert-uim-false '(getenv "UIM_NONEXISTING_ENV"))
  (assert-uim-true  '(unsetenv "UIM_NONEXISTING_ENV"))
  (assert-uim-false '(getenv "UIM_NONEXISTING_ENV"))
  #f)

(provide "test/util/test-uim")
