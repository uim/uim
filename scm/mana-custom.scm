;;; mana-custom.scm: Customization variables for mana.scm
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


(define mana-im-name-label (N_ "Mana"))
(define mana-im-short-desc (N_ "A multi-segment kana-kanji conversion engine"))

(define-custom-group 'mana
                     mana-im-name-label
                     mana-im-short-desc)

(define-custom-group 'mana-advanced
                     (N_ "Mana (advanced)")
                     (N_ "Advanced settings for Mana"))

;;
;; segment separator
;;

(define-custom 'mana-show-segment-separator? #f
  '(mana segment-sep)
  '(boolean)
  (N_ "Show segment separator")
  (N_ "long description will be here."))

(define-custom 'mana-segment-separator "|"
  '(mana segment-sep)
  '(string ".*")
  (N_ "Segment separator")
  (N_ "long description will be here."))

(custom-add-hook 'mana-segment-separator
		 'custom-activity-hooks
		 (lambda ()
		   mana-show-segment-separator?))

;;
;; candidate window
;;

(define-custom 'mana-use-candidate-window? #t
  '(mana candwin)
  '(boolean)
  (N_ "Use candidate window")
  (N_ "long description will be here."))

(define-custom 'mana-candidate-op-count 1
  '(mana candwin)
  '(integer 0 99)
  (N_ "Conversion key press count to show candidate window")
  (N_ "long description will be here."))

(define-custom 'mana-nr-candidate-max 10
  '(mana candwin)
  '(integer 1 20)
  (N_ "Number of candidates in candidate window at a time")
  (N_ "long description will be here."))

(define-custom 'mana-select-candidate-by-numeral-key? #f
  '(mana candwin)
  '(boolean)
  (N_ "Select candidate by numeral keys")
  (N_ "long description will be here."))

;; activity dependency
(custom-add-hook 'mana-candidate-op-count
		 'custom-activity-hooks
		 (lambda ()
		   mana-use-candidate-window?))

(custom-add-hook 'mana-nr-candidate-max
		 'custom-activity-hooks
		 (lambda ()
		   mana-use-candidate-window?))

(custom-add-hook 'mana-select-candidate-by-numeral-key?
		 'custom-activity-hooks
		 (lambda ()
		   mana-use-candidate-window?))

;;
;; toolbar
;;

;; Can't be unified with action definitions in mana.scm until uim
;; 0.4.6.
(define mana-input-mode-indication-alist
  (list
   (list 'action_mana_direct
	 'ja_direct
	 "-"
	 (N_ "Direct input")
	 (N_ "Direct input mode"))
   (list 'action_mana_hiragana
	 'ja_hiragana
	 "¤¢"
	 (N_ "Hiragana")
	 (N_ "Hiragana input mode"))
   (list 'action_mana_katakana
	 'ja_katakana
	 "¥¢"
	 (N_ "Katakana")
	 (N_ "Katakana input mode"))
   (list 'action_mana_halfkana
	 'ja_halfkana
	 "Ž±"
	 (N_ "Halfwidth Katakana")
	 (N_ "Halfwidth Katakana input mode"))
   (list 'action_mana_halfwidth_alnum
	 'ja_halfwidth_alnum
	 "aA"
	 (N_ "Halfwidth Alphanumeric")
	 (N_ "Halfwidth Alphanumeric input mode"))
   (list 'action_mana_fullwidth_alnum
	 'ja_fullwidth_alnum
	 "£Á"
	 (N_ "Fullwidth Alphanumeric")
	 (N_ "Fullwidth Alphanumeric input mode"))))

(define mana-kana-input-method-indication-alist
  (list
   (list 'action_mana_roma
	 'ja_romaji
	 "£Ò"
	 (N_ "Romaji")
	 (N_ "Romaji input mode"))
   (list 'action_mana_kana
	 'ja_kana
	 "¤«"
	 (N_ "Kana")
	 (N_ "Kana input mode"))
   (list 'action_mana_azik
	 'ja_azik
	 "£Ú"
	 (N_ "AZIK")
	 (N_ "AZIK extended romaji input mode"))
   (list 'action_mana_act
	 'ja_act
	 "£Ã"
	 (N_ "ACT")
	 (N_ "ACT extended romaji input mode"))
   (list 'action_mana_kzik
	 'ja_kzik
	 "£Ë"
	 (N_ "KZIK")
	 (N_ "KZIK extended romaji input mode"))))
 
;;; Buttons

(define-custom 'mana-widgets '(widget_mana_input_mode
				widget_mana_kana_input_method)
  '(mana toolbar-widget)
  (list 'ordered-list
	(list 'widget_mana_input_mode
	      (N_ "Input mode")
	      (N_ "Input mode"))
	(list 'widget_mana_kana_input_method
	      (N_ "Kana input method")
	      (N_ "Kana input method")))
  (N_ "Enabled toolbar buttons")
  (N_ "long description will be here."))

;; dynamic reconfiguration
;; mana-configure-widgets is not defined at this point. So wrapping
;; into lambda.
(custom-add-hook 'mana-widgets
		 'custom-set-hooks
		 (lambda ()
		   (mana-configure-widgets)))


;;; Input mode

(define-custom 'default-widget_mana_input_mode 'action_mana_direct
  '(mana toolbar-widget)
  (cons 'choice
	(map indication-alist-entry-extract-choice
	     mana-input-mode-indication-alist))
  (N_ "Default input mode")
  (N_ "long description will be here."))

(define-custom 'mana-input-mode-actions
               (map car mana-input-mode-indication-alist)
  '(mana toolbar-widget)
  (cons 'ordered-list
	(map indication-alist-entry-extract-choice
	     mana-input-mode-indication-alist))
  (N_ "Input mode menu items")
  (N_ "long description will be here."))

;; value dependency
(if custom-full-featured?
    (custom-add-hook 'mana-input-mode-actions
		     'custom-set-hooks
		     (lambda ()
		       (custom-choice-range-reflect-olist-val
			'default-widget_mana_input_mode
			'mana-input-mode-actions
			mana-input-mode-indication-alist))))

;; activity dependency
(custom-add-hook 'default-widget_mana_input_mode
		 'custom-activity-hooks
		 (lambda ()
		   (memq 'widget_mana_input_mode mana-widgets)))

(custom-add-hook 'mana-input-mode-actions
		 'custom-activity-hooks
		 (lambda ()
		   (memq 'widget_mana_input_mode mana-widgets)))

;; dynamic reconfiguration
(custom-add-hook 'default-widget_mana_input_mode
		 'custom-set-hooks
		 (lambda ()
		   (mana-configure-widgets)))

(custom-add-hook 'mana-input-mode-actions
		 'custom-set-hooks
		 (lambda ()
		   (mana-configure-widgets)))

;;; Kana input method

(define-custom 'default-widget_mana_kana_input_method 'action_mana_roma
  '(mana toolbar-widget)
  (cons 'choice
	(map indication-alist-entry-extract-choice
	     mana-kana-input-method-indication-alist))
  (N_ "Default kana input method")
  (N_ "long description will be here."))

(define-custom 'mana-kana-input-method-actions
               (map car mana-kana-input-method-indication-alist)
  '(mana toolbar-widget)
  (cons 'ordered-list
	(map indication-alist-entry-extract-choice
	     mana-kana-input-method-indication-alist))
  (N_ "Kana input method menu items")
  (N_ "long description will be here."))

;; value dependency
(if custom-full-featured?
    (custom-add-hook 'mana-kana-input-method-actions
		     'custom-set-hooks
		     (lambda ()
		       (custom-choice-range-reflect-olist-val
			'default-widget_mana_kana_input_method
			'mana-kana-input-method-actions
			mana-kana-input-method-indication-alist))))

;; activity dependency
(custom-add-hook 'default-widget_mana_kana_input_method
		 'custom-activity-hooks
		 (lambda ()
		   (memq 'widget_mana_kana_input_method mana-widgets)))

(custom-add-hook 'mana-kana-input-method-actions
		 'custom-activity-hooks
		 (lambda ()
		   (memq 'widget_mana_kana_input_method mana-widgets)))

;; dynamic reconfiguration
(custom-add-hook 'default-widget_mana_kana_input_method
		 'custom-set-hooks
		 (lambda ()
		   (mana-configure-widgets)))

(custom-add-hook 'mana-kana-input-method-actions
		 'custom-set-hooks
		 (lambda ()
		   (mana-configure-widgets)))

(define-custom 'mana-use-with-vi? #f
  '(mana-advanced special-op)
  '(boolean)
  (N_ "Enable vi-cooperative mode")
  (N_ "long description will be here."))

(define-custom 'mana-auto-start-henkan? #f
  '(mana-advanced special-op)
  '(boolean)
  (N_ "Enable auto conversion with punctuation marks")
  (N_ "long description will be here."))

(define-custom 'mana-use-mode-transition-keys-in-off-mode? #f
  '(mana-advanced mode-transition)
  '(boolean)
  (N_ "Enable input mode transition keys in direct (off state) input mode")
  (N_ "long description will be here."))
