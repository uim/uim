;;; Copyright (c) 2005-2013 uim Project https://github.com/uim/uim
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
;;;;

;; These tests are passed at revision 6605 (new repository)

(define-module test.test-plugin
  (use test.unit.test-case)
  (use test.uim-test))
(select-module test.test-plugin)

(define (setup)
  (uim-test-setup)
  (uim-define-siod-compatible-require))

(define (teardown)
  (uim-test-teardown))

(define (test-require-module)
   (uim-eval
    '(begin
       (set! im-list ())
       (undefine *latin.scm-loaded*)
       (undefine *pyload.scm-loaded*)))
   (assert-uim-false '(symbol-bound? '*latin.scm-loaded*))
   (assert-uim-false '(symbol-bound? '*pyload.scm-loaded*))
   (assert-uim-false '(retrieve-im 'latin))
   (assert-uim-false '(retrieve-im 'py))
   ;; im-module-name == im-name
   (assert-uim-true-value  '(require-module "latin"))
   (assert-uim-equal 'latin
		     '(im-name (retrieve-im 'latin)))
   (assert-uim-equal "latin"
		     '(im-module-name (retrieve-im 'latin)))
   ;; im-module-name != im-name
   (assert-uim-true-value  '(require-module "pyload"))
   (assert-uim-equal 'py
		     '(im-name (retrieve-im 'py)))
   (assert-uim-equal "pyload"
		     '(im-module-name (retrieve-im 'py)))
   ;; raw require does not set im-module-name
   (uim-eval '(set! im-list ()))

   (uim-eval '(undefine *latin.scm-loaded*))
   (assert-uim-false '(symbol-bound? '*latin.scm-loaded*))
   (assert-uim-false '(retrieve-im 'latin))
   (assert-uim-true-value  '(require "latin.scm"))

   (assert-uim-equal 'latin
		     '(im-name (retrieve-im 'latin)))
   (assert-uim-false '(im-module-name (retrieve-im 'latin)))

   ;; nonexistent module
   (assert-uim-false '(require-module "nonexistent"))

   ;; TODO: test load-plugin (requires complete unload-plugin
   ;; implementation)
   )

(provide "test/test-plugin")
