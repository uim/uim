;;; Copyright (c) 2005-2026 uim Project https://github.com/uim/uim
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

;; TODO: test load-plugin (requires complete unload-plugin
;; implementation)

(require-extension (unittest))

;; Only the IMs in enabled-im-list are registered. It's defined in
;; loader.scm but loader.scm isn't loaded with LIBUIM_VANILLA=1.
(set! enabled-im-list '(direct latin py))

(test-begin "require-module")
(test-false (retrieve-im 'latin))
(test-false (retrieve-im 'py))
;; im-module-name == im-name
(test-true  (require-module "latin"))
(test-equal 'latin
	    (im-name (retrieve-im 'latin)))
(test-equal "latin"
	    (im-module-name (retrieve-im 'latin)))
;; im-module-name != im-name
(test-true  (require-module "pyload"))
(test-equal 'py
	    (im-name (retrieve-im 'py)))
(test-equal "pyload"
	    (im-module-name (retrieve-im 'py)))
;; raw require does not set im-module-name
(set! im-list ())
(test-false (retrieve-im 'latin))
(test-true  (require "latin.scm"))
(test-equal 'latin
	    (im-name (retrieve-im 'latin)))
(test-false (im-module-name (retrieve-im 'latin)))
;; nonexistent module
(test-false (require-module "nonexistent"))
(test-end)

(test-begin "require-module: broken Scheme file")
;; tutcode has both of a dynlib and a Scheme file.
(test-true (require-dynlib "tutcode"))
(let ((original-try-require try-require)
      (n-calls 0))
  ;; Simulate an error in the Scheme file.
  (set! try-require (lambda (file)
                      (set! n-calls (+ n-calls 1))
                      #f))
  (test-false (require-module "tutcode"))
  (set! try-require original-try-require)
  (test-equal 1 n-calls))
(test-end)

(test-report-result)
