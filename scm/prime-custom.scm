;;; prime-custom.scm: Customization variables for prime.scm
;;;
;;; Copyright (c) 2003-2005 uim Project http://uim.freedesktop.org/
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
;;; THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
;;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
;;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
;;; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;;; SUCH DAMAGE.
;;;;

(require "i18n.scm")


(define prime-im-label-name (N_ "PRIME"))
(define prime-im-short-desc (N_ "Japanese predictable input method"))

(define-custom-group 'prime
                     (ugettext prime-im-label-name)
                     (ugettext prime-im-short-desc))

;(define-custom 'prime-use-candidate-window? #t
;  '(prime)
;  '(boolean)
;  "Use candidate window"
;  "long description will be here.")

;(define-custom 'prime-candidate-op-count 1
;  '(prime)
;  '(integer 0 99)
;  "Conversion key press count to show candidate window"
;  "long description will be here.")

(define-custom 'prime-nr-candidate-max 10
  '(prime)
  '(integer 1 20)
  (_ "Number of candidates in candidate window at a time")
  (_ "long description will be here."))

(define-custom 'prime-always-show-window? #t
  '(prime)
  '(boolean)
  (_ "Always showing candidate window")
  (_ "long description will be here."))

(define-custom 'prime-auto-register-mode? #t
  '(prime)
  '(boolean)
  (_ "Enable auto register mode")
  (_ "long description will be here."))

(define-custom 'prime-pseudo-mode-cursor? #f
  '(prime)
  '(boolean)
  (_ "Enable pseudo mode cursor")
  (_ "long description will be here."))

(define-custom 'prime-char-annotation? #t
  '(prime)
  '(boolean)
  (_ "Show candidate annotations")
  (_ "long description will be here."))


;; If #t a candidate window displays usage examples of candidate words.
(define-custom 'prime-custom-display-usage? #t
  '(prime)
  '(boolean)
  (_ "Show usage examples of candidate words")
  (_ "long description will be here."))

(define-custom 'prime-mask-pending-preedit? #f
  '(prime)
  '(boolean)
  (_ "Mask preedit strings (For T-Code users)")
  (_ "long description will be here."))

;(define-custom 'prime-use-numeral-key-to-select-cand? #t
;  '(prime)
;  '(boolean)
;  "Use numeral key to select candidate directly"
;  "long description will be here.")
