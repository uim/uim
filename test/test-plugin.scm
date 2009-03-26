;;; Copyright (c) 2005-2009 uim Project http://code.google.com/p/uim/
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

(define-module test.test-plugin
  (use test.unit.test-case)
  (use test.uim-test))
(select-module test.test-plugin)

(define (setup)
  (uim-test-setup)
  ;; Cancels LIBUIM_VANILLA=1. See init.scm for further details.
  (uim-eval '(load-enabled-modules))

  (uim-define-siod-compatible-require))

(define (teardown)
  (uim-test-teardown))

(define (test-require-module)
   (uim-eval
    '(begin
       (set! im-list ())
       (undefine *tcode.scm-loaded*)
       (undefine *hangul.scm-loaded*)))
   (assert-false (uim-bool '(symbol-bound? '*tcode.scm-loaded*)))
   (assert-false (uim-bool '(symbol-bound? '*hangul.scm-loaded*)))
   (assert-false (uim-bool '(retrieve-im 'tcode)))
   (assert-false (uim-bool '(retrieve-im 'hangul2)))
   ;; im-module-name == im-name
   (assert-true  (uim-bool '(require-module "tcode")))
   (assert-equal 'tcode
		 (uim '(im-name (retrieve-im 'tcode))))
   (assert-equal "tcode"
		 (uim '(im-module-name (retrieve-im 'tcode))))
   ;; im-module-name != im-name
   (assert-true  (uim-bool '(require-module "hangul")))
   (assert-equal 'hangul2
		 (uim '(im-name (retrieve-im 'hangul2))))
   (assert-equal "hangul"
		 (uim '(im-module-name (retrieve-im 'hangul2))))
   ;; raw require does not set im-module-name
   (uim-eval '(set! im-list ()))

   (uim-eval '(undefine *tcode.scm-loaded*))
   (assert-false (uim-bool '(symbol-bound? '*tcode.scm-loaded*)))
   (assert-false (uim-bool '(retrieve-im 'tcode)))
   (assert-true  (uim-bool '(require "tcode.scm")))

   (assert-equal 'tcode
		 (uim '(im-name (retrieve-im 'tcode))))
   (assert-false (uim-bool '(im-module-name (retrieve-im 'tcode))))
   ;; nonexistent module
   ;; TODO: suppress "ERROR:" message in try-require
   ;;(assert-false (uim-bool '(require-module "nonexistent")))

   ;; TODO: test load-plugin (requires complete unload-plugin
   ;; implementation)
   )

(provide "test/test-plugin")
