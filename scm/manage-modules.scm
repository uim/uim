;;; manage-modules.scm: Generates input method module configurations
;;;
;;; Copyright (c) 2005 uim Project http://uim.freedesktop.org/
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

(require "lazy-load.scm")
(require "custom.scm")
(require-custom "im-custom.scm")

;; TODO: write test
;; define installed-im-module-list before invocation
(define generate-installed-modules-scm
  (lambda ()
    (set! enabled-im-list
	  (map custom-choice-rec-sym (custom-installed-im-list)))
    (print
     (string-append
      ";; The described order of input methods affects which IM is preferred\n"
      ";; at the default IM selection process for each locale. i.e. list\n"
      ";; preferable IM first for each language\n"
      "(define installed-im-module-list "
      (custom-list-as-literal installed-im-module-list)
      ")\n"
      (custom-definition-as-literal 'enabled-im-list)
      "\n"))))

;; TODO: write test
(define generate-loader-scm
  (lambda ()
    (print
     (string-append
      ";; Don't edit this file manually\n"
      (string-join "\n" (stub-im-generate-all-stub-im-list))))))
