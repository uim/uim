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

(require "generic.scm")

;; Hangul IMs requires some generic-keys disabled. See the post "A
;; question about the space bar" in uim@pdx.freedesktop.org
;; mailinglist from David Oftedal at Fri, 02 Apr 2004 22:55:25 +0200
(define hangul-proc-on-mode-with-preedit
  (let* ((non-existent-key? (make-single-key-predicate ""))
	 (generic-next-candidate-key? non-existent-key?)
	 (generic-prev-candidate-key? non-existent-key?)
	 (generic-commit-key? non-existent-key?)
	 (generic-proc-input-state-with-preedit-with-this-env
	  (%%enclose-another-env generic-proc-input-state-with-preedit
				 (%%current-environment))))
    (lambda (gc key state rkc)  ;; "gc" stands for "generic-context"
      (generic-proc-input-state-with-preedit-with-this-env gc key state rkc))))

(define hangul-proc-on-mode
  (let* ((non-existent-key? (make-single-key-predicate ""))
	 (generic-next-candidate-key? non-existent-key?)
	 (generic-prev-candidate-key? non-existent-key?)
	 (generic-commit-key? non-existent-key?)
	 (generic-proc-input-state-with-preedit
	  hangul-proc-on-mode-with-preedit)
	 (generic-proc-input-state-with-this-env
	  (%%enclose-another-env generic-proc-input-state (%%current-environment))))
    (lambda (gc key state)  ;; "gc" stands for "generic-context"
      (generic-proc-input-state-with-this-env gc key state))))

;; 'let*' is required rather than 'let'
(define hangul-key-press-handler
  (let* ((generic-proc-input-state hangul-proc-on-mode)
	 (generic-key-press-handler-with-this-env
	  (%%enclose-another-env generic-key-press-handler (%%current-environment))))
    (lambda (gc key state)
      (generic-key-press-handler-with-this-env gc key state))))

(define hangul-register-im
  (lambda (name lang code name-label short-desc init-arg)
    (register-im
     name
     lang
     code
     name-label
     short-desc
     init-arg
     generic-init-handler
     #f ;; release-handler
     context-mode-handler
     hangul-key-press-handler
     generic-key-release-handler
     generic-reset-handler
     generic-get-candidate-handler
     generic-set-candidate-index-handler
     context-prop-activate-handler
     #f
     generic-focus-in-handler
     generic-focus-out-handler
     generic-place-handler
     generic-displace-handler
     )))
    
(define hangul2-init-handler
  (lambda (id im arg)
    (require "hangul2.scm")
    (generic-context-new id im hangul2-rule #t)))

(define hangul3-init-handler
  (lambda (id im arg)
    (require "hangul3.scm")
    (generic-context-new id im hangul3-rule #t)))

(define romaja-init-handler
  (lambda (id im arg)
    (require "romaja.scm")
    (generic-context-new id im romaja-rule #t)))

(hangul-register-im
 'hangul2
 "ko"
 "UTF-8"
 (N_ "Hangul (2-beol)")
 (N_ "2-beol style hangul input method")
 hangul2-init-handler)

;; hangul3 IM does not require generic-keys disabled
(generic-register-im
 'hangul3
 "ko"
 "UTF-8"
 (N_ "Hangul (3-beol)")
 (N_ "3-beol style hangul input method")
 hangul3-init-handler)

(hangul-register-im
 'romaja
 "ko"
 "UTF-8"
 (N_ "Hangul (Romaja)")
 (N_ "Romaja input style hangul input method")
 romaja-init-handler)
