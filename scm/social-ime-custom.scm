;;; social-ime-custom.scm: Customization variables for social-ime.scm
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


(define social-ime-im-name-label (N_ "Social-IME"))
(define social-ime-im-short-desc (N_ "A multi-segment kana-kanji conversion engine"))

(define-custom-group 'social-ime
                     social-ime-im-name-label
                     social-ime-im-short-desc)

(define-custom-group 'social-ime-server
		     (N_ "Social-IME server")
		     (N_ "long description will be here."))

(define-custom-group 'social-ime-advanced
		     (N_ "Social-IME (advanced)")
		     (N_ "long description will be here."))

(define-custom-group 'social-ime-prediction
		     (N_ "Prediction")
		     (N_ "long description will be here."))
;;
;; segment separator
;;

(define-custom 'social-ime-show-segment-separator? #f
  '(social-ime segment-sep)
  '(boolean)
  (N_ "Show segment separator")
  (N_ "long description will be here."))

(define-custom 'social-ime-segment-separator "|"
  '(social-ime segment-sep)
  '(string ".*")
  (N_ "Segment separator")
  (N_ "long description will be here."))

(custom-add-hook 'social-ime-segment-separator
		 'custom-activity-hooks
		 (lambda ()
		   social-ime-show-segment-separator?))

;;
;; candidate window
;;

(define-custom 'social-ime-use-candidate-window? #t
  '(social-ime candwin)
  '(boolean)
  (N_ "Use candidate window")
  (N_ "long description will be here."))

(define-custom 'social-ime-candidate-op-count 1
  '(social-ime candwin)
  '(integer 0 99)
  (N_ "Conversion key press count to show candidate window")
  (N_ "long description will be here."))

(define-custom 'social-ime-nr-candidate-max 10
  '(social-ime candwin)
  '(integer 1 20)
  (N_ "Number of candidates in candidate window at a time")
  (N_ "long description will be here."))

(define-custom 'social-ime-select-candidate-by-numeral-key? #f
  '(social-ime candwin)
  '(boolean)
  (N_ "Select candidate by numeral keys")
  (N_ "long description will be here."))

;; activity dependency
(custom-add-hook 'social-ime-candidate-op-count
		 'custom-activity-hooks
		 (lambda ()
		   social-ime-use-candidate-window?))

(custom-add-hook 'social-ime-nr-candidate-max
		 'custom-activity-hooks
		 (lambda ()
		   social-ime-use-candidate-window?))

(custom-add-hook 'social-ime-select-candidate-by-numeral-key?
		 'custom-activity-hooks
		 (lambda ()
		   social-ime-use-candidate-window?))

;;
;; toolbar
;;

;; Can't be unified with action definitions in social-ime.scm until uim
;; 0.4.6.
(define social-ime-input-mode-indication-alist
  (list
   (list 'action_social-ime_direct
	 'ja_direct
	 "-"
	 (N_ "Direct input")
	 (N_ "Direct input mode"))
   (list 'action_social-ime_hiragana
	 'ja_hiragana
	 "¤¢"
	 (N_ "Hiragana")
	 (N_ "Hiragana input mode"))
   (list 'action_social-ime_katakana
	 'ja_katakana
	 "¥¢"
	 (N_ "Katakana")
	 (N_ "Katakana input mode"))
   (list 'action_social-ime_halfkana
	 'ja_halfkana
	 "Ž±"
	 (N_ "Halfwidth Katakana")
	 (N_ "Halfwidth Katakana input mode"))
   (list 'action_social-ime_halfwidth_alnum
	 'ja_halfwidth_alnum
	 "a"
	 (N_ "Halfwidth Alphanumeric")
	 (N_ "Halfwidth Alphanumeric input mode"))

   (list 'action_social-ime_fullwidth_alnum
	 'ja_fullwidth_alnum
	 "£Á"
	 (N_ "Fullwidth Alphanumeric")
	 (N_ "Fullwidth Alphanumeric input mode"))))

(define social-ime-kana-input-method-indication-alist
  (list
   (list 'action_social-ime_roma
	 'ja_romaji
	 "£Ò"
	 (N_ "Romaji")
	 (N_ "Romaji input mode"))
   (list 'action_social-ime_kana
	 'ja_kana
	 "¤«"
	 (N_ "Kana")
	 (N_ "Kana input mode"))
   (list 'action_social-ime_azik
	 'ja_azik
	 "£Ú"
	 (N_ "AZIK")
	 (N_ "AZIK extended romaji input mode"))
   (list 'action_social-ime_act
	 'ja_act
	 "£Ã"
	 (N_ "ACT")
	 (N_ "ACT extended romaji input mode"))
   (list 'action_social-ime_kzik
	 'ja_kzik
	 "£Ë"
	 (N_ "KZIK")
	 (N_ "KZIK extended romaji input mode"))))

;;; Buttons

(define-custom 'social-ime-widgets '(widget_social-ime_input_mode
				widget_social-ime_kana_input_method)
  '(social-ime toolbar-widget)
  (list 'ordered-list
	(list 'widget_social-ime_input_mode
	      (N_ "Input mode")
	      (N_ "Input mode"))
	(list 'widget_social-ime_kana_input_method
	      (N_ "Kana input method")
	      (N_ "Kana input method")))
  (N_ "Enabled toolbar buttons")
  (N_ "long description will be here."))

;; dynamic reconfiguration
;; social-ime-configure-widgets is not defined at this point. So wrapping
;; into lambda.
(custom-add-hook 'social-ime-widgets
		 'custom-set-hooks
		 (lambda ()
		   (social-ime-configure-widgets)))


;;; Input mode

(define-custom 'default-widget_social-ime_input_mode 'action_social-ime_direct
  '(social-ime toolbar-widget)
  (cons 'choice
	(map indication-alist-entry-extract-choice
	     social-ime-input-mode-indication-alist))
  (N_ "Default input mode")
  (N_ "long description will be here."))

(define-custom 'social-ime-input-mode-actions
               (map car social-ime-input-mode-indication-alist)
  '(social-ime toolbar-widget)
  (cons 'ordered-list
	(map indication-alist-entry-extract-choice
	     social-ime-input-mode-indication-alist))
  (N_ "Input mode menu items")
  (N_ "long description will be here."))

;; value dependency
(if custom-full-featured?
    (custom-add-hook 'social-ime-input-mode-actions
		     'custom-set-hooks
		     (lambda ()
		       (custom-choice-range-reflect-olist-val
			'default-widget_social-ime_input_mode
			'social-ime-input-mode-actions
			social-ime-input-mode-indication-alist))))

;; activity dependency
(custom-add-hook 'default-widget_social-ime_input_mode
		 'custom-activity-hooks
		 (lambda ()
		   (memq 'widget_social-ime_input_mode social-ime-widgets)))

(custom-add-hook 'social-ime-input-mode-actions
		 'custom-activity-hooks
		 (lambda ()
		   (memq 'widget_social-ime_input_mode social-ime-widgets)))

;; dynamic reconfiguration
(custom-add-hook 'default-widget_social-ime_input_mode
		 'custom-set-hooks
		 (lambda ()
		   (social-ime-configure-widgets)))

(custom-add-hook 'social-ime-input-mode-actions
		 'custom-set-hooks
		 (lambda ()
		   (social-ime-configure-widgets)))

;;; Kana input method

(define-custom 'default-widget_social-ime_kana_input_method 'action_social-ime_roma
  '(social-ime toolbar-widget)
  (cons 'choice
	(map indication-alist-entry-extract-choice
	     social-ime-kana-input-method-indication-alist))
  (N_ "Default kana input method")
  (N_ "long description will be here."))

(define-custom 'social-ime-kana-input-method-actions
               (map car social-ime-kana-input-method-indication-alist)
  '(social-ime toolbar-widget)
  (cons 'ordered-list
	(map indication-alist-entry-extract-choice
	     social-ime-kana-input-method-indication-alist))
  (N_ "Kana input method menu items")
  (N_ "long description will be here."))

;; value dependency
(if custom-full-featured?
    (custom-add-hook 'social-ime-kana-input-method-actions
		     'custom-set-hooks
		     (lambda ()
		       (custom-choice-range-reflect-olist-val
			'default-widget_social-ime_kana_input_method
			'social-ime-kana-input-method-actions
			social-ime-kana-input-method-indication-alist))))

;; activity dependency
(custom-add-hook 'default-widget_social-ime_kana_input_method
		 'custom-activity-hooks
		 (lambda ()
		   (memq 'widget_social-ime_kana_input_method social-ime-widgets)))

(custom-add-hook 'social-ime-kana-input-method-actions
		 'custom-activity-hooks
		 (lambda ()
		   (memq 'widget_social-ime_kana_input_method social-ime-widgets)))

;; dynamic reconfiguration
(custom-add-hook 'default-widget_social-ime_kana_input_method
		 'custom-set-hooks
		 (lambda ()
		   (social-ime-configure-widgets)))

(custom-add-hook 'social-ime-kana-input-method-actions
		 'custom-set-hooks
		 (lambda ()
		   (social-ime-configure-widgets)))


;;
;; social-ime-server
;;

(define-custom 'social-ime-server "www.social-ime.com"
  '(social-ime-advanced social-ime-server)
  '(string ".*")
  (N_ "Social-IME server address")
  (N_ "long description will be here."))

(define-custom 'social-ime-path "/api/"
  '(social-ime-advanced social-ime-server)
  '(string ".*")
  (N_ "Social-IME server path")
  (N_ "long description will be here."))

(define-custom 'social-ime-prediction-api-path "/api2/predict.php"
  '(social-ime-advanced social-ime-server)
  '(string ".*")
  (N_ "Social-IME server prediction API path")
  (N_ "long description will be here."))

(define-custom 'social-ime-user ""
  '(social-ime-advanced social-ime-server)
  '(string ".*")
  (N_ "Social-IME user name")
  (N_ "long description will be here."))

(define-custom 'social-ime-use-with-vi? #f
  '(social-ime-advanced special-op)
  '(boolean)
  (N_ "Enable vi-cooperative mode")
  (N_ "long description will be here."))

(define-custom 'social-ime-auto-start-henkan? #f
  '(social-ime-advanced special-op)
  '(boolean)
  (N_ "Enable auto conversion with punctuation marks")
  (N_ "long description will be here."))

(define-custom 'social-ime-use-mode-transition-keys-in-off-mode? #f
  '(social-ime-advanced mode-transition)
  '(boolean)
  (N_ "Enable input mode transition keys in direct (off state) input mode")
  (N_ "long description will be here."))

;; prediction
(define-custom 'social-ime-use-prediction? #f
  '(social-ime-advanced social-ime-prediction)
  '(boolean)
  (N_ "Enable input prediction")
  (N_ "long description will be here."))

(define-custom 'social-ime-prediction-type 'www
  '(social-ime-advanced social-ime-prediction)
  (list 'choice
        (list 'www (N_ "Social-IME Server") (N_ "Social-IME Server"))
        (list 'uim (N_ "uim")  (N_ "uim")))
  (N_ "Prediction type")
  (N_ "long description will be here."))

(define-custom 'social-ime-select-prediction-by-numeral-key? #f
  '(social-ime-advanced social-ime-prediction)
  '(boolean)
  (N_ "Select prediction candidate by numeral keys")
  (N_ "long description will be here."))

(define-custom 'social-ime-use-implicit-commit-prediction? #t
  '(social-ime-advanced social-ime-prediction)
  '(boolean)
  (N_ "Show selected prediction candidate in preedit area")
  (N_ "long description will be here."))

(define-custom 'social-ime-prediction-cache-words 256
  '(social-ime-advanced social-ime-prediction)
  '(integer 1 65535)
  (N_ "Number of cache of prediction candidates")
  (N_ "long description will be here."))

(define-custom 'social-ime-prediction-start-char-count 2
  '(social-ime-advanced social-ime-prediction)
  '(integer 1 65535)
  (N_ "Character count to start input prediction")
  (N_ "long description will be here."))

(define-custom 'social-ime-warn-connection? #t
  '(social-ime-advanced social-ime-server)
  '(boolean)
  (N_ "Show caveat for the connection")
  (N_ "long description will be here."))

(custom-add-hook 'social-ime-use-candidate-window?
                 'custom-get-hooks
                 (lambda ()
                   (if (not social-ime-use-candidate-window?)
                       (set! social-ime-use-prediction? #f))))

(custom-add-hook 'social-ime-use-prediction?
                 'custom-activity-hooks
                 (lambda ()
                   social-ime-use-candidate-window?))

(custom-add-hook 'social-ime-prediction-type
                 'custom-activity-hooks
                 (lambda ()
                   social-ime-use-prediction?))

(custom-add-hook 'social-ime-select-prediction-by-numeral-key?
                 'custom-activity-hooks
                 (lambda ()
                   social-ime-use-prediction?))

(custom-add-hook 'social-ime-use-implicit-commit-prediction?
                 'custom-activity-hooks
                 (lambda ()
                   social-ime-use-prediction?))

(custom-add-hook 'social-ime-prediction-cache-words
                 'custom-activity-hooks
                 (lambda ()
                   social-ime-use-prediction?))

(custom-add-hook 'social-ime-prediction-start-char-count
                 'custom-activity-hooks
                 (lambda ()
                   social-ime-use-prediction?))
