;;; generic-custom.scm: Customization variables for generic.scm
;;;
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
;;;;

(require "i18n.scm")


(define-custom-group 'other-ims
		     (N_ "Other input methods")
		     (N_ "long description will be here."))

(define-custom 'generic-use-candidate-window? #t
  '(other-ims candwin)
  '(boolean)
  (N_ "Use candidate window")
  (N_ "long description will be here."))

(define-custom 'generic-candidate-op-count 1
  '(other-ims candwin)
  '(integer 0 99)
  (N_ "Conversion key press count to show candidate window")
  (N_ "long description will be here."))

(define-custom 'generic-nr-candidate-max 10
  '(other-ims candwin)
  '(integer 1 20)
  (N_ "Number of candidates in candidate window at a time")
  (N_ "long description will be here."))

(define-custom 'generic-commit-candidate-by-numeral-key? #t
  '(other-ims candwin)
  '(boolean)
  (N_ "Select candidate by numeral keys")
  (N_ "long description will be here."))

(define-custom 'generic-show-candidate-implicitly? #t
  '(other-ims candwin)
  '(boolean)
  (N_ "Show candidate window without explicit conversion action")
  (N_ "long description will be here."))

(define-custom 'generic-show-prediction-candidates? #t
  '(other-ims candwin)
  '(boolean)
  (N_ "Show candidates matching with expected keys")
  (N_ "long description will be here."))

;; activity dependency
(custom-add-hook 'generic-candidate-op-count
		 'custom-activity-hooks
		 (lambda ()
		   generic-use-candidate-window?))

(custom-add-hook 'generic-nr-candidate-max
		 'custom-activity-hooks
		 (lambda ()
		   generic-use-candidate-window?))

(custom-add-hook 'generic-commit-candidate-by-numeral-key?
		 'custom-activity-hooks
		 (lambda ()
		   generic-use-candidate-window?))

(custom-add-hook 'generic-show-candidate-implicitly?
		 'custom-activity-hooks
		 (lambda ()
		   generic-use-candidate-window?))
