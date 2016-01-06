;;; sj3-custom.scm: Customization variables for sj3.scm
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


(define sj3-im-name-label (N_ "SJ3"))
(define sj3-im-short-desc (N_ "A multi-segment kana-kanji conversion engine"))

(define-custom-group 'sj3
                     sj3-im-name-label
                     sj3-im-short-desc)

(define-custom-group 'sj3server
		     (N_ "SJ3 server")
		     (N_ "long description will be here."))

(define-custom-group 'sj3-advanced
		     (N_ "SJ3 (advanced)")
		     (N_ "long description will be here."))

(define-custom-group 'sj3-prediction
		     (N_ "Prediction")
		     (N_ "long description will be here."))

;;
;; segment separator
;;

(define-custom 'sj3-show-segment-separator? #f
  '(sj3 segment-sep)
  '(boolean)
  (N_ "Show segment separator")
  (N_ "long description will be here."))

(define-custom 'sj3-segment-separator "|"
  '(sj3 segment-sep)
  '(string ".*")
  (N_ "Segment separator")
  (N_ "long description will be here."))

(custom-add-hook 'sj3-segment-separator
		 'custom-activity-hooks
		 (lambda ()
		   sj3-show-segment-separator?))

;;
;; candidate window
;;

(define-custom 'sj3-use-candidate-window? #t
  '(sj3 candwin)
  '(boolean)
  (N_ "Use candidate window")
  (N_ "long description will be here."))

(define-custom 'sj3-candidate-op-count 1
  '(sj3 candwin)
  '(integer 0 99)
  (N_ "Conversion key press count to show candidate window")
  (N_ "long description will be here."))

(define-custom 'sj3-nr-candidate-max 10
  '(sj3 candwin)
  '(integer 1 20)
  (N_ "Number of candidates in candidate window at a time")
  (N_ "long description will be here."))

(define-custom 'sj3-select-candidate-by-numeral-key? #f
  '(sj3 candwin)
  '(boolean)
  (N_ "Select candidate by numeral keys")
  (N_ "long description will be here."))

;; activity dependency
(custom-add-hook 'sj3-candidate-op-count
		 'custom-activity-hooks
		 (lambda ()
		   sj3-use-candidate-window?))

(custom-add-hook 'sj3-nr-candidate-max
		 'custom-activity-hooks
		 (lambda ()
		   sj3-use-candidate-window?))

(custom-add-hook 'sj3-select-candidate-by-numeral-key?
		 'custom-activity-hooks
		 (lambda ()
		   sj3-use-candidate-window?))

;;
;; toolbar
;;

;; Can't be unified with action definitions in sj3.scm until uim
;; 0.4.6.
(define sj3-input-mode-indication-alist
  (list
   (list 'action_sj3_direct
	 'ja_direct
	 "-"
	 (N_ "Direct input")
	 (N_ "Direct input mode"))
   (list 'action_sj3_hiragana
	 'ja_hiragana
	 "¤¢"
	 (N_ "Hiragana")
	 (N_ "Hiragana input mode"))
   (list 'action_sj3_katakana
	 'ja_katakana
	 "¥¢"
	 (N_ "Katakana")
	 (N_ "Katakana input mode"))
   (list 'action_sj3_halfkana
	 'ja_halfkana
	 "Ž±"
	 (N_ "Halfwidth Katakana")
	 (N_ "Halfwidth Katakana input mode"))
   (list 'action_sj3_halfwidth_alnum
	 'ja_halfwidth_alnum
	 "a"
	 (N_ "Halfwidth Alphanumeric")
	 (N_ "Halfwidth Alphanumeric input mode"))

   (list 'action_sj3_fullwidth_alnum
	 'ja_fullwidth_alnum
	 "£Á"
	 (N_ "Fullwidth Alphanumeric")
	 (N_ "Fullwidth Alphanumeric input mode"))))

(define sj3-kana-input-method-indication-alist
  (list
   (list 'action_sj3_roma
	 'ja_romaji
	 "£Ò"
	 (N_ "Romaji")
	 (N_ "Romaji input mode"))
   (list 'action_sj3_kana
	 'ja_kana
	 "¤«"
	 (N_ "Kana")
	 (N_ "Kana input mode"))
   (list 'action_sj3_azik
	 'ja_azik
	 "£Ú"
	 (N_ "AZIK")
	 (N_ "AZIK extended romaji input mode"))
   (list 'action_sj3_act
	 'ja_act
	 "£Ã"
	 (N_ "ACT")
	 (N_ "ACT extended romaji input mode"))
   (list 'action_sj3_kzik
	 'ja_kzik
	 "£Ë"
	 (N_ "KZIK")
	 (N_ "KZIK extended romaji input mode"))))

;;; Buttons

(define-custom 'sj3-widgets '(widget_sj3_input_mode
				widget_sj3_kana_input_method)
  '(sj3 toolbar-widget)
  (list 'ordered-list
	(list 'widget_sj3_input_mode
	      (N_ "Input mode")
	      (N_ "Input mode"))
	(list 'widget_sj3_kana_input_method
	      (N_ "Kana input method")
	      (N_ "Kana input method")))
  (N_ "Enabled toolbar buttons")
  (N_ "long description will be here."))

;; dynamic reconfiguration
;; sj3-configure-widgets is not defined at this point. So wrapping
;; into lambda.
(custom-add-hook 'sj3-widgets
		 'custom-set-hooks
		 (lambda ()
		   (sj3-configure-widgets)))


;;; Input mode

(define-custom 'default-widget_sj3_input_mode 'action_sj3_direct
  '(sj3 toolbar-widget)
  (cons 'choice
	(map indication-alist-entry-extract-choice
	     sj3-input-mode-indication-alist))
  (N_ "Default input mode")
  (N_ "long description will be here."))

(define-custom 'sj3-input-mode-actions
               (map car sj3-input-mode-indication-alist)
  '(sj3 toolbar-widget)
  (cons 'ordered-list
	(map indication-alist-entry-extract-choice
	     sj3-input-mode-indication-alist))
  (N_ "Input mode menu items")
  (N_ "long description will be here."))

;; value dependency
(if custom-full-featured?
    (custom-add-hook 'sj3-input-mode-actions
		     'custom-set-hooks
		     (lambda ()
		       (custom-choice-range-reflect-olist-val
			'default-widget_sj3_input_mode
			'sj3-input-mode-actions
			sj3-input-mode-indication-alist))))

;; activity dependency
(custom-add-hook 'default-widget_sj3_input_mode
		 'custom-activity-hooks
		 (lambda ()
		   (memq 'widget_sj3_input_mode sj3-widgets)))

(custom-add-hook 'sj3-input-mode-actions
		 'custom-activity-hooks
		 (lambda ()
		   (memq 'widget_sj3_input_mode sj3-widgets)))

;; dynamic reconfiguration
(custom-add-hook 'default-widget_sj3_input_mode
		 'custom-set-hooks
		 (lambda ()
		   (sj3-configure-widgets)))

(custom-add-hook 'sj3-input-mode-actions
		 'custom-set-hooks
		 (lambda ()
		   (sj3-configure-widgets)))

;;; Kana input method

(define-custom 'default-widget_sj3_kana_input_method 'action_sj3_roma
  '(sj3 toolbar-widget)
  (cons 'choice
	(map indication-alist-entry-extract-choice
	     sj3-kana-input-method-indication-alist))
  (N_ "Default kana input method")
  (N_ "long description will be here."))

(define-custom 'sj3-kana-input-method-actions
               (map car sj3-kana-input-method-indication-alist)
  '(sj3 toolbar-widget)
  (cons 'ordered-list
	(map indication-alist-entry-extract-choice
	     sj3-kana-input-method-indication-alist))
  (N_ "Kana input method menu items")
  (N_ "long description will be here."))

;; value dependency
(if custom-full-featured?
    (custom-add-hook 'sj3-kana-input-method-actions
		     'custom-set-hooks
		     (lambda ()
		       (custom-choice-range-reflect-olist-val
			'default-widget_sj3_kana_input_method
			'sj3-kana-input-method-actions
			sj3-kana-input-method-indication-alist))))

;; activity dependency
(custom-add-hook 'default-widget_sj3_kana_input_method
		 'custom-activity-hooks
		 (lambda ()
		   (memq 'widget_sj3_kana_input_method sj3-widgets)))

(custom-add-hook 'sj3-kana-input-method-actions
		 'custom-activity-hooks
		 (lambda ()
		   (memq 'widget_sj3_kana_input_method sj3-widgets)))

;; dynamic reconfiguration
(custom-add-hook 'default-widget_sj3_kana_input_method
		 'custom-set-hooks
		 (lambda ()
		   (sj3-configure-widgets)))

(custom-add-hook 'sj3-kana-input-method-actions
		 'custom-set-hooks
		 (lambda ()
		   (sj3-configure-widgets)))


;;
;; sj3-server-name
;;

(define-custom 'sj3-use-remote-server? #f
  '(sj3-advanced sj3server)
  '(boolean)
  (N_ "Use remote SJ3 server")
  (N_ "long description will be here."))


(define-custom 'sj3-server-name "localhost"
  '(sj3-advanced sj3server)
  '(string ".*")
  (N_ "SJ3 server name")
  (N_ "long description will be here."))

(custom-add-hook 'sj3-server-name
                 'custom-activity-hooks
                 (lambda ()
                   sj3-use-remote-server?))

(define-custom 'sj3-user (or (user-name) "")
  '(sj3-advanced sj3server)
  '(string ".*")
  (N_ "SJ3 user name")
  (N_ "long description will be here."))

(define-custom 'sj3-unix-domain-socket-path "/var/sj3/run/sj3serv.socket"
  '(sj3-advanced sj3server)
  '(string ".*")
  (N_ "SJ3 server socket path")
  (N_ "long description will be here."))

(custom-add-hook 'sj3-unix-domain-socket-path
                 'custom-activity-hooks
                 (lambda ()
                   (not sj3-use-remote-server?)))

(define-custom 'sj3-use-with-vi? #f
  '(sj3-advanced special-op)
  '(boolean)
  (N_ "Enable vi-cooperative mode")
  (N_ "long description will be here."))

(define-custom 'sj3-auto-start-henkan? #f
  '(sj3-advanced special-op)
  '(boolean)
  (N_ "Enable auto conversion with punctuation marks")
  (N_ "long description will be here."))

(define-custom 'sj3-use-mode-transition-keys-in-off-mode? #f
  '(sj3-advanced mode-transition)
  '(boolean)
  (N_ "Enable input mode transition keys in direct (off state) input mode")
  (N_ "long description will be here."))

;; prediction
(define-custom 'sj3-use-prediction? #f
  '(sj3-advanced sj3-prediction)
  '(boolean)
  (N_ "Enable input prediction")
  (N_ "long description will be here."))

(define-custom 'sj3-select-prediction-by-numeral-key? #f
  '(sj3-advanced sj3-prediction)
  '(boolean)
  (N_ "Select prediction candidate by numeral keys")
  (N_ "long description will be here."))

(define-custom 'sj3-use-implicit-commit-prediction? #t
  '(sj3-advanced sj3-prediction)
  '(boolean)
  (N_ "Show selected prediction candidate in preedit area")
  (N_ "long description will be here."))

(define-custom 'sj3-prediction-cache-words 256
  '(sj3-advanced sj3-prediction)
  '(integer 1 65535)
  (N_ "Number of cache of prediction candidates")
  (N_ "long description will be here."))

(define-custom 'sj3-prediction-start-char-count 2
  '(sj3-advanced sj3-prediction)
  '(integer 1 65535)
  (N_ "Character count to start input prediction")
  (N_ "long description will be here."))

(custom-add-hook 'sj3-use-candidate-window?
                 'custom-get-hooks
                 (lambda ()
                   (if (not sj3-use-candidate-window?)
                       (set! sj3-use-prediction? #f))))

(custom-add-hook 'sj3-use-prediction?
                 'custom-activity-hooks
                 (lambda ()
                   sj3-use-candidate-window?))

(custom-add-hook 'sj3-select-prediction-by-numeral-key?
                 'custom-activity-hooks
                 (lambda ()
                   sj3-use-prediction?))

(custom-add-hook 'sj3-use-implicit-commit-prediction?
                 'custom-activity-hooks
                 (lambda ()
                   sj3-use-prediction?))

(custom-add-hook 'sj3-prediction-cache-words
                 'custom-activity-hooks
                 (lambda ()
                   sj3-use-prediction?))

(custom-add-hook 'sj3-prediction-start-char-count
                 'custom-activity-hooks
                 (lambda ()
                   sj3-use-prediction?))
