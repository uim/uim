;;; baidu-olime-jp-custom.scm: Customization variables for baidu-olime-jp.scm
;;;
;;; Copyright (c) 2012- uim Project https://github.com/uim/uim
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
(require "openssl.scm")

(define baidu-olime-jp-im-name-label (N_ "Baidu-OnlineIME-Jp"))
(define baidu-olime-jp-im-short-desc (N_ "A multi-segment kana-kanji conversion engine"))

(define-custom-group 'baidu-olime-jp
                     baidu-olime-jp-im-name-label
                     baidu-olime-jp-im-short-desc)

(define-custom-group 'baidu-olime-jp-server
		     (N_ "Baidu-OnlineIME-Jp server")
		     (N_ "long description will be here."))

(define-custom-group 'baidu-olime-jp-advanced
		     (N_ "Baidu-OnlineIME-Jp (advanced)")
		     (N_ "long description will be here."))

(define-custom-group 'baidu-olime-jp-prediction
  (N_ "Prediction")
  (N_ "long description will be here."))

;;
;; segment separator
;;

(define-custom 'baidu-olime-jp-show-segment-separator? #f
  '(baidu-olime-jp segment-sep)
  '(boolean)
  (N_ "Show segment separator")
  (N_ "long description will be here."))

(define-custom 'baidu-olime-jp-segment-separator "|"
  '(baidu-olime-jp segment-sep)
  '(string ".*")
  (N_ "Segment separator")
  (N_ "long description will be here."))

(custom-add-hook 'baidu-olime-jp-segment-separator
		 'custom-activity-hooks
		 (lambda ()
		   baidu-olime-jp-show-segment-separator?))

;;
;; candidate window
;;

(define-custom 'baidu-olime-jp-use-candidate-window? #t
  '(baidu-olime-jp candwin)
  '(boolean)
  (N_ "Use candidate window")
  (N_ "long description will be here."))

(define-custom 'baidu-olime-jp-candidate-op-count 1
  '(baidu-olime-jp candwin)
  '(integer 0 99)
  (N_ "Conversion key press count to show candidate window")
  (N_ "long description will be here."))

(define-custom 'baidu-olime-jp-nr-candidate-max 10
  '(baidu-olime-jp candwin)
  '(integer 1 20)
  (N_ "Number of candidates in candidate window at a time")
  (N_ "long description will be here."))

(define-custom 'baidu-olime-jp-select-candidate-by-numeral-key? #f
  '(baidu-olime-jp candwin)
  '(boolean)
  (N_ "Select candidate by numeral keys")
  (N_ "long description will be here."))

;; activity dependency
(custom-add-hook 'baidu-olime-jp-candidate-op-count
		 'custom-activity-hooks
		 (lambda ()
		   baidu-olime-jp-use-candidate-window?))

(custom-add-hook 'baidu-olime-jp-nr-candidate-max
		 'custom-activity-hooks
		 (lambda ()
		   baidu-olime-jp-use-candidate-window?))

(custom-add-hook 'baidu-olime-jp-select-candidate-by-numeral-key?
		 'custom-activity-hooks
		 (lambda ()
		   baidu-olime-jp-use-candidate-window?))

;;
;; toolbar
;;

;; Can't be unified with action definitions in baidu-olime-jp.scm until uim
;; 0.4.6.
(define baidu-olime-jp-input-mode-indication-alist
  (list
   (list 'action_baidu-olime-jp_direct
	 'ja_direct
	 "-"
	 (N_ "Direct input")
	 (N_ "Direct input mode"))
   (list 'action_baidu-olime-jp_hiragana
	 'ja_hiragana
	 "¤¢"
	 (N_ "Hiragana")
	 (N_ "Hiragana input mode"))
   (list 'action_baidu-olime-jp_katakana
	 'ja_katakana
	 "¥¢"
	 (N_ "Katakana")
	 (N_ "Katakana input mode"))
   (list 'action_baidu-olime-jp_halfkana
	 'ja_halfkana
	 "Ž±"
	 (N_ "Halfwidth Katakana")
	 (N_ "Halfwidth Katakana input mode"))
   (list 'action_baidu-olime-jp_halfwidth_alnum
	 'ja_halfwidth_alnum
	 "a"
	 (N_ "Halfwidth Alphanumeric")
	 (N_ "Halfwidth Alphanumeric input mode"))

   (list 'action_baidu-olime-jp_fullwidth_alnum
	 'ja_fullwidth_alnum
	 "£Á"
	 (N_ "Fullwidth Alphanumeric")
	 (N_ "Fullwidth Alphanumeric input mode"))))

(define baidu-olime-jp-kana-input-method-indication-alist
  (list
   (list 'action_baidu-olime-jp_roma
	 'ja_romaji
	 "£Ò"
	 (N_ "Romaji")
	 (N_ "Romaji input mode"))
   (list 'action_baidu-olime-jp_kana
	 'ja_kana
	 "¤«"
	 (N_ "Kana")
	 (N_ "Kana input mode"))
   (list 'action_baidu-olime-jp_azik
	 'ja_azik
	 "£Ú"
	 (N_ "AZIK")
	 (N_ "AZIK extended romaji input mode"))
   (list 'action_baidu-olime-jp_act
	 'ja_act
	 "£Ã"
	 (N_ "ACT")
	 (N_ "ACT extended romaji input mode"))
   (list 'action_baidu-olime-jp_kzik
	 'ja_kzik
	 "£Ë"
	 (N_ "KZIK")
	 (N_ "KZIK extended romaji input mode"))))

;;; Buttons

(define-custom 'baidu-olime-jp-widgets '(widget_baidu-olime-jp_input_mode
				widget_baidu-olime-jp_kana_input_method)
  '(baidu-olime-jp toolbar-widget)
  (list 'ordered-list
	(list 'widget_baidu-olime-jp_input_mode
	      (N_ "Input mode")
	      (N_ "Input mode"))
	(list 'widget_baidu-olime-jp_kana_input_method
	      (N_ "Kana input method")
	      (N_ "Kana input method")))
  (N_ "Enabled toolbar buttons")
  (N_ "long description will be here."))

;; dynamic reconfiguration
;; baidu-olime-jp-configure-widgets is not defined at this point. So wrapping
;; into lambda.
(custom-add-hook 'baidu-olime-jp-widgets
		 'custom-set-hooks
		 (lambda ()
		   (baidu-olime-jp-configure-widgets)))


;;; Input mode

(define-custom 'default-widget_baidu-olime-jp_input_mode 'action_baidu-olime-jp_direct
  '(baidu-olime-jp toolbar-widget)
  (cons 'choice
	(map indication-alist-entry-extract-choice
	     baidu-olime-jp-input-mode-indication-alist))
  (N_ "Default input mode")
  (N_ "long description will be here."))

(define-custom 'baidu-olime-jp-input-mode-actions
               (map car baidu-olime-jp-input-mode-indication-alist)
  '(baidu-olime-jp toolbar-widget)
  (cons 'ordered-list
	(map indication-alist-entry-extract-choice
	     baidu-olime-jp-input-mode-indication-alist))
  (N_ "Input mode menu items")
  (N_ "long description will be here."))

;; value dependency
(if custom-full-featured?
    (custom-add-hook 'baidu-olime-jp-input-mode-actions
		     'custom-set-hooks
		     (lambda ()
		       (custom-choice-range-reflect-olist-val
			'default-widget_baidu-olime-jp_input_mode
			'baidu-olime-jp-input-mode-actions
			baidu-olime-jp-input-mode-indication-alist))))

;; activity dependency
(custom-add-hook 'default-widget_baidu-olime-jp_input_mode
		 'custom-activity-hooks
		 (lambda ()
		   (memq 'widget_baidu-olime-jp_input_mode baidu-olime-jp-widgets)))

(custom-add-hook 'baidu-olime-jp-input-mode-actions
		 'custom-activity-hooks
		 (lambda ()
		   (memq 'widget_baidu-olime-jp_input_mode baidu-olime-jp-widgets)))

;; dynamic reconfiguration
(custom-add-hook 'default-widget_baidu-olime-jp_input_mode
		 'custom-set-hooks
		 (lambda ()
		   (baidu-olime-jp-configure-widgets)))

(custom-add-hook 'baidu-olime-jp-input-mode-actions
		 'custom-set-hooks
		 (lambda ()
		   (baidu-olime-jp-configure-widgets)))

;;; Kana input method

(define-custom 'default-widget_baidu-olime-jp_kana_input_method 'action_baidu-olime-jp_roma
  '(baidu-olime-jp toolbar-widget)
  (cons 'choice
	(map indication-alist-entry-extract-choice
	     baidu-olime-jp-kana-input-method-indication-alist))
  (N_ "Default kana input method")
  (N_ "long description will be here."))

(define-custom 'baidu-olime-jp-kana-input-method-actions
               (map car baidu-olime-jp-kana-input-method-indication-alist)
  '(baidu-olime-jp toolbar-widget)
  (cons 'ordered-list
	(map indication-alist-entry-extract-choice
	     baidu-olime-jp-kana-input-method-indication-alist))
  (N_ "Kana input method menu items")
  (N_ "long description will be here."))

;; value dependency
(if custom-full-featured?
    (custom-add-hook 'baidu-olime-jp-kana-input-method-actions
		     'custom-set-hooks
		     (lambda ()
		       (custom-choice-range-reflect-olist-val
			'default-widget_baidu-olime-jp_kana_input_method
			'baidu-olime-jp-kana-input-method-actions
			baidu-olime-jp-kana-input-method-indication-alist))))

;; activity dependency
(custom-add-hook 'default-widget_baidu-olime-jp_kana_input_method
		 'custom-activity-hooks
		 (lambda ()
		   (memq 'widget_baidu-olime-jp_kana_input_method baidu-olime-jp-widgets)))

(custom-add-hook 'baidu-olime-jp-kana-input-method-actions
		 'custom-activity-hooks
		 (lambda ()
		   (memq 'widget_baidu-olime-jp_kana_input_method baidu-olime-jp-widgets)))

;; dynamic reconfiguration
(custom-add-hook 'default-widget_baidu-olime-jp_kana_input_method
		 'custom-set-hooks
		 (lambda ()
		   (baidu-olime-jp-configure-widgets)))

(custom-add-hook 'baidu-olime-jp-kana-input-method-actions
		 'custom-set-hooks
		 (lambda ()
		   (baidu-olime-jp-configure-widgets)))


;;
;; baidu-olime-jp-server
;;

(define-custom 'baidu-olime-jp-server "cloud.ime.baidu.jp"
  '(baidu-olime-jp-advanced baidu-olime-jp-server)
  '(string ".*")
  (N_ "Baidu-OnlineIME-Jp server address")
  (N_ "long description will be here."))

(define-custom 'baidu-olime-jp-use-ssl? (provided? "openssl")
  '(baidu-olime-jp-advanced baidu-olime-jp-server)
  '(boolean)
  (N_ "Use SSL")
  (N_ "long description will be here."))

(custom-add-hook 'baidu-olime-jp-use-ssl?
                 'custom-activity-hooks
                 (lambda ()
                   ;; SSL is forced
                   #f
                   ))

(define-custom 'baidu-olime-jp-use-with-vi? #f
  '(baidu-olime-jp-advanced special-op)
  '(boolean)
  (N_ "Enable vi-cooperative mode")
  (N_ "long description will be here."))

(define-custom 'baidu-olime-jp-auto-start-henkan? #f
  '(baidu-olime-jp-advanced special-op)
  '(boolean)
  (N_ "Enable auto conversion with punctuation marks")
  (N_ "long description will be here."))

(define-custom 'baidu-olime-jp-use-mode-transition-keys-in-off-mode? #f
  '(baidu-olime-jp-advanced mode-transition)
  '(boolean)
  (N_ "Enable input mode transition keys in direct (off state) input mode")
  (N_ "long description will be here."))

;; prediction
(define-custom 'baidu-olime-jp-use-prediction? #f
  '(baidu-olime-jp-advanced baidu-olime-jp-prediction)
  '(boolean)
  (N_ "Enable input prediction")
  (N_ "long description will be here."))

(define-custom 'baidu-olime-jp-select-prediction-by-numeral-key? #f
  '(baidu-olime-jp-advanced baidu-olime-jp-prediction)
  '(boolean)
  (N_ "Select prediction candidate by numeral keys")
  (N_ "long description will be here."))

(define-custom 'baidu-olime-jp-use-implicit-commit-prediction? #t
  '(baidu-olime-jp-advanced baidu-olime-jp-prediction)
  '(boolean)
  (N_ "Show selected prediction candidate in preedit area")
  (N_ "long description will be here."))

(custom-add-hook 'baidu-olime-jp-use-candidate-window?
                 'custom-get-hooks
                 (lambda ()
                   (if (not baidu-olime-jp-use-candidate-window?)
                       (set! baidu-olime-jp-use-prediction? #f))))

(custom-add-hook 'baidu-olime-jp-use-prediction?
                 'custom-activity-hooks
                 (lambda ()
                   baidu-olime-jp-use-candidate-window?))

(define-custom 'baidu-olime-jp-prediction-cache-words 256
  '(baidu-olime-jp-advanced baidu-olime-jp-prediction)
  '(integer 1 65535)
  (N_ "Number of cache of prediction candidates")
  (N_ "long description will be here."))

(define-custom 'baidu-olime-jp-prediction-start-char-count 2
  '(baidu-olime-jp-advanced baidu-olime-jp-prediction)
  '(integer 1 65535)
  (N_ "Character count to start input prediction")
  (N_ "long description will be here."))

(custom-add-hook 'baidu-olime-jp-use-candidate-window?
                 'custom-get-hooks
                 (lambda ()
                   (if (not baidu-olime-jp-use-candidate-window?)
                       (set! baidu-olime-jp-use-prediction? #f))))

(custom-add-hook 'baidu-olime-jp-use-prediction?
                 'custom-activity-hooks
                 (lambda ()
                   baidu-olime-jp-use-candidate-window?))

(custom-add-hook 'baidu-olime-jp-select-prediction-by-numeral-key?
                 'custom-activity-hooks
                 (lambda ()
                   baidu-olime-jp-use-prediction?))

(custom-add-hook 'baidu-olime-jp-use-implicit-commit-prediction?
                 'custom-activity-hooks
                 (lambda ()
                   baidu-olime-jp-use-prediction?))

(custom-add-hook 'baidu-olime-jp-prediction-cache-words
                 'custom-activity-hooks
                 (lambda ()
                   baidu-olime-jp-use-prediction?))

(custom-add-hook 'baidu-olime-jp-prediction-start-char-count
                 'custom-activity-hooks
                 (lambda ()
                   baidu-olime-jp-use-prediction?))
