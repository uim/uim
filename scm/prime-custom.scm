;;; prime-custom.scm: Customization variables for prime.scm
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

(define prime-im-name-label (N_ "PRIME"))
(define prime-im-short-desc (N_ "Japanese predictive input method"))

(define-custom-group 'prime
  prime-im-name-label
  prime-im-short-desc)

(define-custom-group 'prime-advanced
  (N_ "PRIME (advanced)")
  (N_ "Advanced settings for PRIME"))

(define-custom-group 'language
  (N_ "Language choice")
  (N_ "Language settings"))

(define-custom-group 'japanese
  (N_ "Japanese")
  (N_ "Japanese specific settings"))

(define-custom-group 'english
  (N_ "English")
  (N_ "English specific settings"))


(define-custom 'prime-custom-default-language 'Japanese
  '(prime language)
  (list 'choice
	(list 'Japanese (N_ "Japanese") (N_ "Japanese"))
	(list 'English  (N_ "English")  (N_ "English")))
  (N_ "Default language")
  (N_ "long description will be here."))

(define-custom 'prime-language-toggle-key '("F11")
  '(prime language)
  '(key)
  (N_ "Language toggle key")
  (N_ "long description will be here."))

(define-custom 'prime-auto-register-mode? #t
  '(prime)
  '(boolean)
  (N_ "Enable auto register mode")
  (N_ "long description will be here."))

(define-custom 'prime-custom-japanese-space 'wide
  '(prime japanese)
  (list 'choice
	(list 'wide (N_ "Wide width (Zenkaku)") (N_ "Wide width (Zenkaku)"))
	(list 'half (N_ "Half width (Hankaku)") (N_ "Half width (Hankaku)")))
  (N_ "Space character")
  (N_ "long description will be here."))

(define-custom 'prime-altspace-key '("<Control> " "<Alt> ")
  '(prime japanese)
  '(key)
  (N_ "Alternative space character key")
  (N_ "long description will be here."))

(define-custom 'prime-server-setting? 'unixdomain
  '(prime advanced)
  (list 'choice
        (list 'unixdomain
              (N_ "Unix domain")
              (N_ "Use UNIX domain socket to communicate with PRIME."))
        (list 'tcpserver
              (N_ "TCP server")
              (N_ "Use tcp server to communicate with PRIME"))
        (list 'pipe
              (N_ "Pipe")
              (N_ "Use pipe. spawn new prime process to communicate with PRIME")))
  (N_ "Prime connection setting")
  (N_ "long description will be here."))

(define-custom 'prime-command-path "prime"
  '(prime advanced)
  '(pathname regular-file)
  (N_ "Prime command path")
  (N_ "long description will be here."))

(define-custom 'prime-tcpserver-name "localhost"
  '(prime advanced)
  '(string ".*")
  (N_ "PRIME server address")
  (N_ "long description will be here."))

(custom-add-hook 'prime-tcpserver-name
		 'custom-activity-hooks
		 (lambda ()
                   (eq? prime-server-setting?
                        'tcpserver)))

(define-custom 'prime-tcpserver-port 1180
  '(prime advanced)
  '(integer 0 65535)
  (N_ "PRIME server port")
  (N_ "long description will be here."))

(custom-add-hook 'prime-tcpserver-port
		 'custom-activity-hooks
		 (lambda ()
                   (eq? prime-server-setting?
                        'tcpserver)))

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
  (N_ "Number of candidates in candidate window at a time")
  (N_ "long description will be here."))

(define-custom 'prime-always-show-window? #t
  '(prime-advanced candwin)
  '(boolean)
  (N_ "Always showing candidate window")
  (N_ "long description will be here."))

;; If #t a candidate window displays usage examples of candidate words.
(define-custom 'prime-custom-display-usage? #t
  '(prime-advanced annotation)
  '(boolean)
  (N_ "Show usage examples of candidate words")
  (N_ "long description will be here."))

;; If #t a candidate window displays comments of candidate words.
(define-custom 'prime-custom-display-comment? #t
  '(prime-advanced annotation)
  '(boolean)
  (N_ "Show candidate annotations")
  (N_ "long description will be here."))

;; If #t a candidate window displays forms of candidate words such as
;; 'l (small L)', 'I (large i)'.
(define-custom 'prime-custom-display-form? #t
  '(prime-advanced annotation)
  '(boolean)
  (N_ "Show candidate forms")
  (N_ "long description will be here."))

(custom-add-hook 'prime-custom-display-usage?
  'custom-activity-hooks
  (lambda ()
    enable-annotation?))
(custom-add-hook 'prime-custom-display-comment?
  'custom-activity-hooks
  (lambda ()
    enable-annotation?))
(custom-add-hook 'prime-custom-display-form?
  'custom-activity-hooks
  (lambda ()
    enable-annotation?))

;; ------------------------------------------------------------

(define-custom 'prime-custom-number-selection? #f
  '(prime-advanced special-op)
  '(boolean)
  (N_ "Select candidate by numeral keys")
  (N_ "long description will be here."))

(define-custom 'prime-custom-app-mode-vi? #f
  '(prime-advanced special-op)
  '(boolean)
  (N_ "Enable vi-cooperative mode")
  (N_ "long description will be here."))

(custom-add-hook 'prime-custom-app-mode-vi?
  'custom-set-hooks
  (lambda ()
    (prime-configure-app-mode-vi)))

(define-custom 'prime-pseudo-mode-cursor? #f
  '(prime-advanced special-op)
  '(boolean)
  (N_ "Enable pseudo mode cursor")
  (N_ "long description will be here."))

;(define-custom 'prime-mask-pending-preedit? #f
;  '(prime)
;  '(boolean)
;  (N_ "Mask preedit strings (For T-Code users)")
;  (N_ "long description will be here."))

;;
;; toolbar
;;

;; Can't be unified with action definitions in prime.scm until uim
;; 0.4.6.
(define prime-input-mode-indication-alist
  (list
   (list 'action_prime_mode_latin
	 'ja_direct
	 "--"
	 (N_ "Direct input")
	 (N_ "PRIME off"))
   (list 'action_prime_mode_hiragana
	 'ja_hiragana
	 "¤¢"
	 (N_ "Japanese")
	 (N_ "PRIME on"))
   (list 'action_prime_mode_english
	 'ja_halfwidth_alnum
	 "A"
	 (N_ "English")
	 (N_ "PRIME on"))
   (list 'action_prime_mode_wide_latin
	 'ja_fullwidth_alnum
	 "£Á"
	 (N_ "Fullwidth Alphanumeric")
	 (N_ "Fullwidth Alphanumeric input mode"))
   (list 'action_prime_mode_application
	 'prime_mode_application
	 "¡ª"
	 (N_ "Peculiar")
	 (N_ "Application specific input mode"))))

(define prime-widgets '(widget_prime_input_mode))

;;; Input mode

(define default-widget_prime_input_mode 'action_prime_mode_latin)
;; Users don't care this option.
;; (2005-03-03) <Hiroyuki Komatsu>
;(define-custom 'default-widget_prime_input_mode 'action_prime_mode_latin
;  '(prime-advanced toolbar-widget)
;  (cons 'choice
;	(map indication-alist-entry-extract-choice
;	     prime-input-mode-indication-alist))
;  (N_ "Default input mode")
;  (N_ "long description will be here."))

(define prime-input-mode-actions (map car prime-input-mode-indication-alist))
;; Users don't care this option.
;; (2005-03-03) <Hiroyuki Komatsu>
;(define-custom 'prime-input-mode-actions
;               (map car prime-input-mode-indication-alist)
;   '(prime-advanced toolbar-widget)
;  (cons 'ordered-list
;	(map indication-alist-entry-extract-choice
;	     prime-input-mode-indication-alist))
;  (N_ "Input mode menu items")
;  (N_ "long description will be here."))

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
