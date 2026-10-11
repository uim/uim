;;  test-template.scm: Template file for unit tests
;;
;;; Copyright (c) 2008-2013 uim Project https://github.com/uim/uim
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

;; SigScheme-specific extension name
(require-extension (unittest))

;; Use these assertions.
;;
;; SRFI-64 compatible assertions:
;;   (test-error expr)
;;   (test-equal expected expr)
;;   (test-eqv   expected expr)
;;   (test-eq    expected expr)
;;
;; SigScheme-specific assertions:
;;   (test-true  expr)
;;   (test-false expr)
;;
;; The SRFI-64 assertion 'test-assert' is not recommended since
;; writing (test-assert <exp>) and (test-assert (not <exp>)) is
;; cumbersome. Use 'test-true' and 'test-false' instead.
;;
;; And enclose the assertions with SRFI-64 compatible 'test-begin' and
;; 'test-end' to form a test suite, and call SigScheme-specific
;; 'test-report-result' at last.

(test-begin "Test1")
(test-error (car 1))
(test-equal "string" (string-append "str" "ing"))
(test-eqv   2 (+ 1 1))
(test-eq    'symbol (symbol-append 'sym 'bol))
(test-assert (eqv? 2 (+ 1 1)))
(test-end)

(test-begin "Test2")
(test-true  (eqv? 2 (+ 1 1)))
(test-false (eqv? 2 (+ 1 2)))
(test-end)

(test-report-result)
