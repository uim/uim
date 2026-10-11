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

(test-begin "make-scm-pathname")
(test-equal (map (lambda (x) (string-append x "/"))
                 (string-split (load-path) ":"))
            (make-scm-pathname ""))
(test-equal (map (lambda (x) (string-append x "/file"))
                 (string-split (load-path) ":"))
            (make-scm-pathname "file"))
(test-equal '("/absolute/path/file")
            (make-scm-pathname "/absolute/path/file"))
(test-equal '("/")
            (make-scm-pathname "/"))
(test-end)

(test-begin "interaction-environment")
(test-eq #t (eval '(symbol-bound? 'filter-map)
                  (interaction-environment)))
(test-false (eval '(symbol-bound? 'filter-baz)
                  (interaction-environment)))
;; SigScheme: syntactic keyword 'define' cannot be evaluated as value
(eval (list 'define 'filter-baz filter-map)
      (interaction-environment))
(test-eq #t (eval '(symbol-bound? 'filter-baz)
                  (interaction-environment)))
(test-eq #t (eq? filter-baz filter-map))
(test-end)

(test-begin "%%enclose-another-env")
(test-equal 3
            (let* ((x 1)
                   (y 2)
                   (closure (lambda ()
                              (+ x y))))
              (closure)))
(test-equal 10
            (let* ((x 1)
                   (y 2)
                   (closure (lambda ()
                              (+ x y)))
                   ;; SigScheme: valid 2-frame env
                   (another-env '(((x) . (4))
                                  ((y) . (6)))))
              (set! closure
                    (%%enclose-another-env closure another-env))
              (closure)))
;; causes error since z is not exist in the another-env
(test-error (let* ((x 1)
                   (y 2)
                   (z 3)
                   (closure (lambda ()
                              (+ x y z)))
                   ;; SigScheme: valid 2-frame env
                   (another-env '(((x) . (4))
                                  ((y) . (6)))))
              (set! closure (%%enclose-another-env closure another-env))
              (closure)))
(test-end)

(test-begin "getenv")
(test-false (getenv "UIM_NONEXISTING_ENV"))
(test-end)

(test-begin "setenv")
(unsetenv "UIM_NONEXISTING_ENV")
(test-false (getenv "UIM_NONEXISTING_ENV"))
(test-eq #t (setenv "UIM_NONEXISTING_ENV" "FOO" #f))
(test-equal "FOO"
            (getenv "UIM_NONEXISTING_ENV"))
(test-eq #t (setenv "UIM_NONEXISTING_ENV" "BAR" #f))
(test-equal "FOO"
            (getenv "UIM_NONEXISTING_ENV"))
(test-eq #t (setenv "UIM_NONEXISTING_ENV" "BAR" #t))
(test-equal "BAR"
            (getenv "UIM_NONEXISTING_ENV"))
(test-end)

(test-begin "unsetenv")
(test-eq #t (setenv "UIM_NONEXISTING_ENV" "BAR" #t))
(test-equal "BAR"
            (getenv "UIM_NONEXISTING_ENV"))
(test-eq #t (unsetenv "UIM_NONEXISTING_ENV"))
(test-false (getenv "UIM_NONEXISTING_ENV"))
(test-eq #t (unsetenv "UIM_NONEXISTING_ENV"))
(test-false (getenv "UIM_NONEXISTING_ENV"))
(test-end)

(test-report-result)
