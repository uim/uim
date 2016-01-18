;;; ajax-ime-custom.scm: Customization variables for ajax-ime.scm
;;;
;;; Copyright (c) 2008-2013 uim Project https://github.com/uim/uim
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


(define ajax-ime-im-name-label (N_ "Ajax-IME"))
(define ajax-ime-im-short-desc (N_ "A multi-segment kana-kanji conversion engine"))

(define-custom-group 'ajax-ime
                     ajax-ime-im-name-label
                     ajax-ime-im-short-desc)

(define-custom-group 'ajax-ime-server
		     (N_ "Ajax-IME server")
		     (N_ "long description will be here."))

(define-custom-group 'ajax-ime-advanced
		     (N_ "Ajax-IME (advanced)")
		     (N_ "long description will be here."))

(define-custom-group 'ajax-ime-prediction
                    (N_ "Prediction")
                    (N_ "long description will be here."))

;;
;; segment separator
;;

(define-custom 'ajax-ime-show-segment-separator? #f
  '(ajax-ime segment-sep)
  '(boolean)
  (N_ "Show segment separator")
  (N_ "long description will be here."))

(define-custom 'ajax-ime-segment-separator "|"
  '(ajax-ime segment-sep)
  '(string ".*")
  (N_ "Segment separator")
  (N_ "long description will be here."))

(custom-add-hook 'ajax-ime-segment-separator
		 'custom-activity-hooks
		 (lambda ()
		   ajax-ime-show-segment-separator?))

;;
;; candidate window
;;

(define-custom 'ajax-ime-use-candidate-window? #t
  '(ajax-ime candwin)
  '(boolean)
  (N_ "Use candidate window")
  (N_ "long description will be here."))

(define-custom 'ajax-ime-candidate-op-count 1
  '(ajax-ime candwin)
  '(integer 0 99)
  (N_ "Conversion key press count to show candidate window")
  (N_ "long description will be here."))

(define-custom 'ajax-ime-nr-candidate-max 10
  '(ajax-ime candwin)
  '(integer 1 20)
  (N_ "Number of candidates in candidate window at a time")
  (N_ "long description will be here."))

(define-custom 'ajax-ime-select-candidate-by-numeral-key? #f
  '(ajax-ime candwin)
  '(boolean)
  (N_ "Select candidate by numeral keys")
  (N_ "long description will be here."))

;; activity dependency
(custom-add-hook 'ajax-ime-candidate-op-count
		 'custom-activity-hooks
		 (lambda ()
		   ajax-ime-use-candidate-window?))

(custom-add-hook 'ajax-ime-nr-candidate-max
		 'custom-activity-hooks
		 (lambda ()
		   ajax-ime-use-candidate-window?))

(custom-add-hook 'ajax-ime-select-candidate-by-numeral-key?
		 'custom-activity-hooks
		 (lambda ()
		   ajax-ime-use-candidate-window?))

;;
;; toolbar
;;

;; Can't be unified with action definitions in ajax-ime.scm until uim
;; 0.4.6.
(define ajax-ime-input-mode-indication-alist
  (list
   (list 'action_ajax-ime_direct
	 'ja_direct
	 "-"
	 (N_ "Direct input")
	 (N_ "Direct input mode"))
   (list 'action_ajax-ime_hiragana
	 'ja_hiragana
	 "¤¢"
	 (N_ "Hiragana")
	 (N_ "Hiragana input mode"))
   (list 'action_ajax-ime_katakana
	 'ja_katakana
	 "¥¢"
	 (N_ "Katakana")
	 (N_ "Katakana input mode"))
   (list 'action_ajax-ime_halfkana
	 'ja_halfkana
	 "Ž±"
	 (N_ "Halfwidth Katakana")
	 (N_ "Halfwidth Katakana input mode"))
   (list 'action_ajax-ime_halfwidth_alnum
	 'ja_halfwidth_alnum
	 "a"
	 (N_ "Halfwidth Alphanumeric")
	 (N_ "Halfwidth Alphanumeric input mode"))

   (list 'action_ajax-ime_fullwidth_alnum
	 'ja_fullwidth_alnum
	 "£Á"
	 (N_ "Fullwidth Alphanumeric")
	 (N_ "Fullwidth Alphanumeric input mode"))))

(define ajax-ime-kana-input-method-indication-alist
  (list
   (list 'action_ajax-ime_roma
	 'ja_romaji
	 "£Ò"
	 (N_ "Romaji")
	 (N_ "Romaji input mode"))
   (list 'action_ajax-ime_kana
	 'ja_kana
	 "¤«"
	 (N_ "Kana")
	 (N_ "Kana input mode"))
   (list 'action_ajax-ime_azik
	 'ja_azik
	 "£Ú"
	 (N_ "AZIK")
	 (N_ "AZIK extended romaji input mode"))
   (list 'action_ajax-ime_act
	 'ja_act
	 "£Ã"
	 (N_ "ACT")
	 (N_ "ACT extended romaji input mode"))
   (list 'action_ajax-ime_kzik
	 'ja_kzik
	 "£Ë"
	 (N_ "KZIK")
	 (N_ "KZIK extended romaji input mode"))))

;;; Buttons

(define-custom 'ajax-ime-widgets '(widget_ajax-ime_input_mode
				widget_ajax-ime_kana_input_method)
  '(ajax-ime toolbar-widget)
  (list 'ordered-list
	(list 'widget_ajax-ime_input_mode
	      (N_ "Input mode")
	      (N_ "Input mode"))
	(list 'widget_ajax-ime_kana_input_method
	      (N_ "Kana input method")
	      (N_ "Kana input method")))
  (N_ "Enabled toolbar buttons")
  (N_ "long description will be here."))

;; dynamic reconfiguration
;; ajax-ime-configure-widgets is not defined at this point. So wrapping
;; into lambda.
(custom-add-hook 'ajax-ime-widgets
		 'custom-set-hooks
		 (lambda ()
		   (ajax-ime-configure-widgets)))


;;; Input mode

(define-custom 'default-widget_ajax-ime_input_mode 'action_ajax-ime_direct
  '(ajax-ime toolbar-widget)
  (cons 'choice
	(map indication-alist-entry-extract-choice
	     ajax-ime-input-mode-indication-alist))
  (N_ "Default input mode")
  (N_ "long description will be here."))

(define-custom 'ajax-ime-input-mode-actions
               (map car ajax-ime-input-mode-indication-alist)
  '(ajax-ime toolbar-widget)
  (cons 'ordered-list
	(map indication-alist-entry-extract-choice
	     ajax-ime-input-mode-indication-alist))
  (N_ "Input mode menu items")
  (N_ "long description will be here."))

;; value dependency
(if custom-full-featured?
    (custom-add-hook 'ajax-ime-input-mode-actions
		     'custom-set-hooks
		     (lambda ()
		       (custom-choice-range-reflect-olist-val
			'default-widget_ajax-ime_input_mode
			'ajax-ime-input-mode-actions
			ajax-ime-input-mode-indication-alist))))

;; activity dependency
(custom-add-hook 'default-widget_ajax-ime_input_mode
		 'custom-activity-hooks
		 (lambda ()
		   (memq 'widget_ajax-ime_input_mode ajax-ime-widgets)))

(custom-add-hook 'ajax-ime-input-mode-actions
		 'custom-activity-hooks
		 (lambda ()
		   (memq 'widget_ajax-ime_input_mode ajax-ime-widgets)))

;; dynamic reconfiguration
(custom-add-hook 'default-widget_ajax-ime_input_mode
		 'custom-set-hooks
		 (lambda ()
		   (ajax-ime-configure-widgets)))

(custom-add-hook 'ajax-ime-input-mode-actions
		 'custom-set-hooks
		 (lambda ()
		   (ajax-ime-configure-widgets)))

;;; Kana input method

(define-custom 'default-widget_ajax-ime_kana_input_method 'action_ajax-ime_roma
  '(ajax-ime toolbar-widget)
  (cons 'choice
	(map indication-alist-entry-extract-choice
	     ajax-ime-kana-input-method-indication-alist))
  (N_ "Default kana input method")
  (N_ "long description will be here."))

(define-custom 'ajax-ime-kana-input-method-actions
               (map car ajax-ime-kana-input-method-indication-alist)
  '(ajax-ime toolbar-widget)
  (cons 'ordered-list
	(map indication-alist-entry-extract-choice
	     ajax-ime-kana-input-method-indication-alist))
  (N_ "Kana input method menu items")
  (N_ "long description will be here."))

;; value dependency
(if custom-full-featured?
    (custom-add-hook 'ajax-ime-kana-input-method-actions
		     'custom-set-hooks
		     (lambda ()
		       (custom-choice-range-reflect-olist-val
			'default-widget_ajax-ime_kana_input_method
			'ajax-ime-kana-input-method-actions
			ajax-ime-kana-input-method-indication-alist))))

;; activity dependency
(custom-add-hook 'default-widget_ajax-ime_kana_input_method
		 'custom-activity-hooks
		 (lambda ()
		   (memq 'widget_ajax-ime_kana_input_method ajax-ime-widgets)))

(custom-add-hook 'ajax-ime-kana-input-method-actions
		 'custom-activity-hooks
		 (lambda ()
		   (memq 'widget_ajax-ime_kana_input_method ajax-ime-widgets)))

;; dynamic reconfiguration
(custom-add-hook 'default-widget_ajax-ime_kana_input_method
		 'custom-set-hooks
		 (lambda ()
		   (ajax-ime-configure-widgets)))

(custom-add-hook 'ajax-ime-kana-input-method-actions
		 'custom-set-hooks
		 (lambda ()
		   (ajax-ime-configure-widgets)))


;;
;; ajax-ime-server
;;

(define-custom 'ajax-ime-url 'ajax-ime
  '(ajax-ime-advanced ajax-ime-server)
  (list 'choice
        (list 'ajax-ime (N_ "Ajax IME") (N_ "Ajax IME")))
  (N_ "Server url of Ajax IME.")
  (N_ "long description will be here."))

(define-custom 'ajax-ime-warn-connection? #t
  '(ajax-ime-advanced ajax-ime-server)
  '(boolean)
  (N_ "Show caveat for the connection")
  (N_ "long description will be here."))

(define-custom 'ajax-ime-use-with-vi? #f
  '(ajax-ime-advanced special-op)
  '(boolean)
  (N_ "Enable vi-cooperative mode")
  (N_ "long description will be here."))

(define-custom 'ajax-ime-auto-start-henkan? #f
 '(ajax-ime-advanced special-op)
 '(boolean)
 (N_ "Enable auto conversion with punctuation marks")
 (N_ "long description will be here."))

(define-custom 'ajax-ime-use-mode-transition-keys-in-off-mode? #f
  '(ajax-ime-advanced mode-transition)
  '(boolean)
  (N_ "Enable input mode transition keys in direct (off state) input mode")
  (N_ "long description will be here."))

;; prediction
(define-custom 'ajax-ime-use-prediction? #f
  '(ajax-ime-advanced ajax-ime-prediction)
  '(boolean)
  (N_ "Enable input prediction")
  (N_ "long description will be here."))

(define-custom 'ajax-ime-select-prediction-by-numeral-key? #f
  '(ajax-ime-advanced ajax-ime-prediction)
  '(boolean)
  (N_ "Select prediction candidate by numeral keys")
  (N_ "long description will be here."))

(define-custom 'ajax-ime-use-implicit-commit-prediction? #t
  '(ajax-ime-advanced ajax-ime-prediction)
  '(boolean)
  (N_ "Show selected prediction candidate in preedit area")
  (N_ "long description will be here."))

(define-custom 'ajax-ime-prediction-cache-words 256
  '(ajax-ime-advanced ajax-ime-prediction)
  '(integer 1 65535)
  (N_ "Number of cache of prediction candidates")
  (N_ "long description will be here."))

(define-custom 'ajax-ime-prediction-start-char-count 2
  '(ajax-ime-advanced ajax-ime-prediction)
  '(integer 1 65535)
  (N_ "Character count to start input prediction")
  (N_ "long description will be here."))

(custom-add-hook 'ajax-ime-use-candidate-window?
                 'custom-get-hooks
                 (lambda ()
                   (if (not ajax-ime-use-candidate-window?)
                       (set! ajax-ime-use-prediction? #f))))

(custom-add-hook 'ajax-ime-use-prediction?
                 'custom-activity-hooks
                 (lambda ()
                   ajax-ime-use-candidate-window?))

(custom-add-hook 'ajax-ime-select-prediction-by-numeral-key?
                 'custom-activity-hooks
                 (lambda ()
                   ajax-ime-use-prediction?))

(custom-add-hook 'ajax-ime-use-implicit-commit-prediction?
                 'custom-activity-hooks
                 (lambda ()
                   ajax-ime-use-prediction?))

(custom-add-hook 'ajax-ime-prediction-cache-words
                 'custom-activity-hooks
                 (lambda ()
                   ajax-ime-use-prediction?))

(custom-add-hook 'ajax-ime-prediction-start-char-count
                 'custom-activity-hooks
                 (lambda ()
                   ajax-ime-use-prediction?))
