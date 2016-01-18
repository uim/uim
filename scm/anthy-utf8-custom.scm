;;; anthy-custom.scm: Customization variables for anthy.scm
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


(define anthy-im-name-label (N_ "Anthy"))
(define anthy-im-short-desc (N_ "A multi-segment kana-kanji conversion engine"))

(define anthy-utf8-im-name-label (N_ "Anthy (UTF-8)"))
(define anthy-utf8-im-short-desc (N_ "A multi-segment kana-kanji conversion engine"))

(define-custom-group 'anthy
                     anthy-im-name-label
                     anthy-im-short-desc)

(define-custom-group 'anthy-utf8
                     anthy-utf8-im-name-label
                     anthy-utf8-im-short-desc)

(define-custom-group 'anthy-advanced
		     (N_ "Anthy (advanced)")
		     (N_ "Advanced settings for Anthy"))

(define-custom-group 'anthy-prediction
		     (N_ "Prediction")
		     (N_ "long description will be here."))

;;
;; segment separator
;;

(define-custom 'anthy-show-segment-separator? #f
  '(anthy segment-sep)
  '(boolean)
  (N_ "Show segment separator")
  (N_ "long description will be here."))

(define-custom 'anthy-segment-separator "|"
  '(anthy segment-sep)
  '(string ".*")
  (N_ "Segment separator")
  (N_ "long description will be here."))

(custom-add-hook 'anthy-segment-separator
		 'custom-activity-hooks
		 (lambda ()
		   anthy-show-segment-separator?))

;;
;; candidate window
;;

(define-custom 'anthy-use-candidate-window? #t
  '(anthy candwin)
  '(boolean)
  (N_ "Use candidate window")
  (N_ "long description will be here."))

(define-custom 'anthy-candidate-op-count 1
  '(anthy candwin)
  '(integer 0 99)
  (N_ "Conversion key press count to show candidate window")
  (N_ "long description will be here."))

(define-custom 'anthy-nr-candidate-max 10
  '(anthy candwin)
  '(integer 1 20)
  (N_ "Number of candidates in candidate window at a time")
  (N_ "long description will be here."))

(define-custom 'anthy-select-candidate-by-numeral-key? #f
  '(anthy candwin)
  '(boolean)
  (N_ "Select candidate by numeral keys")
  (N_ "long description will be here."))

;; activity dependency
(custom-add-hook 'anthy-candidate-op-count
		 'custom-activity-hooks
		 (lambda ()
		   anthy-use-candidate-window?))

(custom-add-hook 'anthy-nr-candidate-max
		 'custom-activity-hooks
		 (lambda ()
		   anthy-use-candidate-window?))

(custom-add-hook 'anthy-select-candidate-by-numeral-key?
		 'custom-activity-hooks
		 (lambda ()
		   anthy-use-candidate-window?))

;;
;; toolbar
;;

;; Can't be unified with action definitions in anthy.scm until uim
;; 0.4.6.
(define anthy-utf8-input-mode-indication-alist
  (list
   (list 'action_anthy_utf8_direct
	 'ja_direct
	 "-"
	 (N_ "Direct input")
	 (N_ "Direct input mode"))
   (list 'action_anthy_utf8_hiragana
	 'ja_hiragana
	 "あ"
	 (N_ "Hiragana")
	 (N_ "Hiragana input mode"))
   (list 'action_anthy_utf8_katakana
	 'ja_katakana
	 "ア"
	 (N_ "Katakana")
	 (N_ "Katakana input mode"))
   (list 'action_anthy_utf8_halfkana
	 'ja_halfkana
	 "ｱ"
	 (N_ "Halfwidth Katakana")
	 (N_ "Halfwidth Katakana input mode"))
   (list 'action_anthy_utf8_halfwidth_alnum
	 'ja_halfwidth_alnum
	 "a"
	 (N_ "Halfwidth Alphanumeric")
	 (N_ "Halfwidth Alphanumeric input mode"))
   (list 'action_anthy_utf8_fullwidth_alnum
	 'ja_fullwidth_alnum
	 "Ａ"
	 (N_ "Fullwidth Alphanumeric")
	 (N_ "Fullwidth Alphanumeric input mode"))))

(define anthy-utf8-kana-input-method-indication-alist
  (list
   (list 'action_anthy_utf8_roma
	 'ja_romaji
	 "Ｒ"
	 (N_ "Romaji")
	 (N_ "Romaji input mode"))
   (list 'action_anthy_utf8_kana
	 'ja_kana
	 "か"
	 (N_ "Kana")
	 (N_ "Kana input mode"))
   (list 'action_anthy_utf8_azik
	 'ja_azik
	 "Ｚ"
	 (N_ "AZIK")
	 (N_ "AZIK extended romaji input mode"))
   (list 'action_anthy_utf8_act
	 'ja_act
	 "Ｃ"
	 (N_ "ACT")
	 (N_ "ACT extended romaji input mode"))
   (list 'action_anthy_utf8_kzik
	 'ja_kzik
	 "Ｋ"
	 (N_ "KZIK")
	 (N_ "KZIK extended romaji input mode"))))

;;; Buttons

(define-custom 'anthy-utf8-widgets '(widget_anthy_utf8_input_mode
				widget_anthy_utf8_kana_input_method)
  '(anthy-utf8 toolbar-widget)
  (list 'ordered-list
	(list 'widget_anthy_utf8_input_mode
	      (N_ "Input mode")
	      (N_ "Input mode"))
	(list 'widget_anthy_utf8_kana_input_method
	      (N_ "Kana input method")
	      (N_ "Kana input method")))
  (N_ "Enabled toolbar buttons")
  (N_ "long description will be here."))

;; dynamic reconfiguration
;; anthy-configure-widgets is not defined at this point. So wrapping
;; into lambda.
(custom-add-hook 'anthy-utf8-widgets
		 'custom-set-hooks
		 (lambda ()
		   (anthy-utf8-configure-widgets)))


;;; Input mode

(define-custom 'default-widget_anthy_utf8_input_mode 'action_anthy_utf8_direct
  '(anthy-utf8 toolbar-widget)
  (cons 'choice
	(map indication-alist-entry-extract-choice
	     anthy-utf8-input-mode-indication-alist))
  (N_ "Default input mode")
  (N_ "long description will be here."))

(define-custom 'anthy-utf8-input-mode-actions
               (map car anthy-utf8-input-mode-indication-alist)
  '(anthy-utf8 toolbar-widget)
  (cons 'ordered-list
	(map indication-alist-entry-extract-choice
	     anthy-utf8-input-mode-indication-alist))
  (N_ "Input mode menu items")
  (N_ "long description will be here."))

;; value dependency
(if custom-full-featured?
    (custom-add-hook 'anthy-utf8-input-mode-actions
		     'custom-set-hooks
		     (lambda ()
		       (custom-choice-range-reflect-olist-val
			'default-widget_anthy_utf8_input_mode
			'anthy-utf8-input-mode-actions
			anthy-utf8-input-mode-indication-alist))))

;; activity dependency
(custom-add-hook 'default-widget_anthy_utf8_input_mode
		 'custom-activity-hooks
		 (lambda ()
		   (memq 'widget_anthy_utf8_input_mode anthy-utf8-widgets)))

(custom-add-hook 'anthy-utf8-input-mode-actions
		 'custom-activity-hooks
		 (lambda ()
		   (memq 'widget_anthy_utf8_input_mode anthy-utf8-widgets)))

;; dynamic reconfiguration
(custom-add-hook 'default-widget_anthy_utf8_input_mode
		 'custom-set-hooks
		 (lambda ()
		   (anthy-utf8-configure-widgets)))

(custom-add-hook 'anthy-utf8-input-mode-actions
		 'custom-set-hooks
		 (lambda ()
		   (anthy-utf8-configure-widgets)))

;;; Kana input method

(define-custom 'default-widget_anthy_utf8_kana_input_method 'action_anthy_utf8_roma
  '(anthy-utf8 toolbar-widget)
  (cons 'choice
	(map indication-alist-entry-extract-choice
	     anthy-utf8-kana-input-method-indication-alist))
  (N_ "Default kana input method")
  (N_ "long description will be here."))

(define-custom 'anthy-utf8-kana-input-method-actions
               (map car anthy-utf8-kana-input-method-indication-alist)
  '(anthy-utf8 toolbar-widget)
  (cons 'ordered-list
	(map indication-alist-entry-extract-choice
	     anthy-utf8-kana-input-method-indication-alist))
  (N_ "Kana input method menu items")
  (N_ "long description will be here."))

;; value dependency
(if custom-full-featured?
    (custom-add-hook 'anthy-utf8-kana-input-method-actions
		     'custom-set-hooks
		     (lambda ()
		       (custom-choice-range-reflect-olist-val
			'default-widget_anthy_utf8_kana_input_method
			'anthy-utf8-kana-input-method-actions
			anthy-utf8-kana-input-method-indication-alist))))

;; activity dependency
(custom-add-hook 'default-widget_anthy_utf8_kana_input_method
		 'custom-activity-hooks
		 (lambda ()
		   (memq 'widget_anthy_utf8_kana_input_method anthy-utf8-widgets)))

(custom-add-hook 'anthy-utf8-kana-input-method-actions
		 'custom-activity-hooks
		 (lambda ()
		   (memq 'widget_anthy_utf8_kana_input_method anthy-utf8-widgets)))

;; dynamic reconfiguration
(custom-add-hook 'default-widget_anthy_utf8_kana_input_method
		 'custom-set-hooks
		 (lambda ()
		   (anthy-utf8-configure-widgets)))

(custom-add-hook 'anthy-utf8-kana-input-method-actions
		 'custom-set-hooks
		 (lambda ()
		   (anthy-utf8-configure-widgets)))

(define-custom 'anthy-use-prediction? #f
  '(anthy-advanced anthy-prediction)
  '(boolean)
  (N_ "Enable input prediction")
  (N_ "long description will be here."))

(define-custom 'anthy-select-prediction-by-numeral-key? #f
  '(anthy-advanced anthy-prediction)
  '(boolean)
  (N_ "Select prediction candidate by numeral keys")
  (N_ "long description will be here."))

(define-custom 'anthy-use-implicit-commit-prediction? #t
  '(anthy-advanced anthy-prediction)
  '(boolean)
  (N_ "Show selected prediction candidate in preedit area")
  (N_ "long description will be here."))

(define-custom 'anthy-prediction-start-char-count 1
  '(anthy-advanced anthy-prediction)
  '(integer 1 65535)
  (N_ "Character count to start input prediction")
  (N_ "long description will be here."))

(custom-add-hook 'anthy-use-candidate-window?
		 'custom-get-hooks
		 (lambda ()
		   (if (not anthy-use-candidate-window?)
		       (set! anthy-use-prediction? #f))))

(custom-add-hook 'anthy-use-prediction?
		 'custom-activity-hooks
		 (lambda ()
		   anthy-use-candidate-window?))

(custom-add-hook 'anthy-select-prediction-by-numeral-key?
		 'custom-activity-hooks
		 (lambda ()
		   anthy-use-prediction?))

(custom-add-hook 'anthy-use-implicit-commit-prediction?
		 'custom-activity-hooks
		 (lambda ()
		   anthy-use-prediction?))

(custom-add-hook 'anthy-prediction-start-char-count
		 'custom-activity-hooks
		 (lambda ()
		   anthy-use-prediction?))

(define-custom 'anthy-use-with-vi? #f
  '(anthy-advanced special-op)
  '(boolean)
  (N_ "Enable vi-cooperative mode")
  (N_ "long description will be here."))

(define-custom 'anthy-auto-start-henkan? #f
  '(anthy-advanced special-op)
  '(boolean)
  (N_ "Enable auto conversion with punctuation marks")
  (N_ "long description will be here."))

(define-custom 'anthy-use-mode-transition-keys-in-off-mode? #f
  '(anthy-advanced mode-transition)
  '(boolean)
  (N_ "Enable input mode transition keys in direct (off state) input mode")
  (N_ "long description will be here."))
