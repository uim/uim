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

(define prime-im-name-label (N_ "PRIME"))
(define prime-im-short-desc (N_ "Japanese predictive input method"))

(define-custom-group 'prime
  (ugettext prime-im-name-label)
  (ugettext prime-im-short-desc))

(define-custom-group 'prime-advanced
  (_ "PRIME (advanced)")
  (_ "Advanced settings for PRIME"))

(define-custom-group 'japanese
  (_ "Japanese")
  (_ "Japanese specific settings"))

(define-custom 'prime-custom-default-language 'Japanese
  '(prime)
  (list 'choice
	(list 'Japanese (_ "Japanese") (_ "Japanese"))
	(list 'English  (_ "English")  (_ "English")))
  (_ "Default language")
  (_ "long description will be here."))

(define-custom 'prime-auto-register-mode? #t
  '(prime)
  '(boolean)
  (_ "Enable auto register mode")
  (_ "long description will be here."))

(define-custom 'prime-custom-japanese-space 'wide
  '(prime japanese)
  (list 'choice
	(list 'wide (_ "Wide width (Zenkaku)") (_ "Wide width (Zenkaku)"))
	(list 'half (_ "Half width (Hankaku)") (_ "Half width (Hankaku)")))
  (_ "Space character")
  (_ "long description will be here."))

(define-custom 'prime-use-unixdomain? #f
  '(prime advanced)
  '(boolean)
  (_ "Use UNIX domain socket to communicate with PRIME")
  (_ "long description will be here."))

;(define-custom 'prime-use-candidate-window? #t
;  '(prime candwin)
;  '(boolean)
;  "Use candidate window"
;  "long description will be here.")

;(define-custom 'prime-candidate-op-count 1
;  '(prime candwin)
;  '(integer 0 99)
;  "Conversion key press count to show candidate window"
;  "long description will be here.")

(define-custom 'prime-nr-candidate-max 10
  '(prime-advanced candwin)
  '(integer 1 20)
  (_ "Number of candidates in candidate window at a time")
  (_ "long description will be here."))

(define-custom 'prime-always-show-window? #t
  '(prime-advanced candwin)
  '(boolean)
  (_ "Always showing candidate window")
  (_ "long description will be here."))

;; If #t a candidate window displays usage examples of candidate words.
(define-custom 'prime-custom-display-usage? #t
  '(prime-advanced annotation)
  '(boolean)
  (_ "Show usage examples of candidate words")
  (_ "long description will be here."))

;; If #t a candidate window displays comments of candidate words.
(define-custom 'prime-custom-display-comment? #t
  '(prime-advanced annotation)
  '(boolean)
  (_ "Show candidate annotations")
  (_ "long description will be here."))

;; If #t a candidate window displays forms of candidate words such as
;; 'l (small L)', 'I (large i)'.
(define-custom 'prime-custom-display-form? #t
  '(prime-advanced annotation)
  '(boolean)
  (_ "Show candidate forms")
  (_ "long description will be here."))

;; ------------------------------------------------------------

(define-custom 'prime-custom-number-selection? #f
  '(prime-advanced special-op)
  '(boolean)
  (_ "Select candidate by numeral keys")
  (_ "long description will be here."))

(define-custom 'prime-custom-app-mode-vi? #f
  '(prime-advanced special-op)
  '(boolean)
  (_ "Enable vi-cooperative mode")
  (_ "long description will be here."))

(define-custom 'prime-pseudo-mode-cursor? #f
  '(prime-advanced special-op)
  '(boolean)
  (_ "Enable pseudo mode cursor")
  (_ "long description will be here."))

;(define-custom 'prime-mask-pending-preedit? #f
;  '(prime)
;  '(boolean)
;  (_ "Mask preedit strings (For T-Code users)")
;  (_ "long description will be here."))

;;
;; toolbar
;;

;; Can't be unified with action definitions in prime.scm until uim
;; 0.4.6.
(define prime-input-mode-indication-alist
  (list
   (list 'action_prime_mode_latin
	 'figure_prime_mode_latin
	 "P"
	 (N_ "Direct input")
	 (N_ "PRIME off"))
   (list 'action_prime_mode_hiragana
	 'figure_prime_mode_hiragana
	 "คื"
	 (N_ "Japanese")
	 (N_ "PRIME on"))
   (list 'action_prime_mode_wide_latin
	 'figure_prime_mode_wide_latin
	 "ฃะ"
	 (N_ "Fullwidth Alphanumeric")
	 (N_ "Fullwidth Alphanumeric input mode"))))

(define prime-widgets '(widget_prime_input_mode))

;;; Input mode

(define default-widget_prime_input_mode 'action_prime_mode_latin)
;; Users don't care this option.
;; (2005-03-03) <Hiroyuki Komatsu>
;(define-custom 'default-widget_prime_input_mode 'action_prime_mode_latin
;  '(prime-advanced toolbar)
;  (cons 'choice
;	(map indication-alist-entry-extract-choice
;	     prime-input-mode-indication-alist))
;  (_ "Default input mode")
;  (_ "long description will be here."))

(define prime-input-mode-actions (map car prime-input-mode-indication-alist))
;; Users don't care this option.
;; (2005-03-03) <Hiroyuki Komatsu>
;(define-custom 'prime-input-mode-actions
;               (map car prime-input-mode-indication-alist)
;   '(prime-advanced toolbar)
;  (cons 'ordered-list
;	(map indication-alist-entry-extract-choice
;	     prime-input-mode-indication-alist))
;  (_ "Input mode menu items")
;  (_ "long description will be here."))

;; value dependency
;(if custom-full-featured?
;    (custom-add-hook 'prime-input-mode-actions
;		     'custom-set-hooks
;		     (lambda ()
;		       (custom-choice-range-reflect-olist-val
;			'default-widget_prime_input_mode
;			'prime-input-mode-actions
;			prime-input-mode-indication-alist))))

;; dynamic reconfiguration
;(custom-add-hook 'default-widget_prime_input_mode
;		 'custom-set-hooks
;		 (lambda ()
;		   (prime-configure-widgets)))

;(custom-add-hook 'prime-input-mode-actions
;		 'custom-set-hooks
;		 (lambda ()
;		   (prime-configure-widgets)))
