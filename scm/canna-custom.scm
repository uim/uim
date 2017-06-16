;;; canna-custom.scm: Customization variables for canna.scm
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


(define canna-im-name-label (N_ "Canna"))
(define canna-im-short-desc (N_ "A multi-segment kana-kanji conversion engine"))

(define-custom-group 'canna
                     canna-im-name-label
                     canna-im-short-desc)

(define-custom-group 'cannaserver
		     (N_ "Canna server")
		     (N_ "long description will be here."))

(define-custom-group 'canna-advanced
		     (N_ "Canna (advanced)")
		     (N_ "long description will be here."))

(define-custom-group 'canna-prediction
		     (N_ "Prediction")
		     (N_ "long description will be here."))

;;
;; segment separator
;;

(define-custom 'canna-show-segment-separator? #f
  '(canna segment-sep)
  '(boolean)
  (N_ "Show segment separator")
  (N_ "long description will be here."))

(define-custom 'canna-segment-separator "|"
  '(canna segment-sep)
  '(string ".*")
  (N_ "Segment separator")
  (N_ "long description will be here."))

(custom-add-hook 'canna-segment-separator
		 'custom-activity-hooks
		 (lambda ()
		   canna-show-segment-separator?))

;;
;; candidate window
;;

(define-custom 'canna-use-candidate-window? #t
  '(canna candwin)
  '(boolean)
  (N_ "Use candidate window")
  (N_ "long description will be here."))

(define-custom 'canna-candidate-op-count 1
  '(canna candwin)
  '(integer 0 99)
  (N_ "Conversion key press count to show candidate window")
  (N_ "long description will be here."))

(define-custom 'canna-nr-candidate-max 10
  '(canna candwin)
  '(integer 1 20)
  (N_ "Number of candidates in candidate window at a time")
  (N_ "long description will be here."))

(define-custom 'canna-select-candidate-by-numeral-key? #f
  '(canna candwin)
  '(boolean)
  (N_ "Select candidate by numeral keys")
  (N_ "long description will be here."))

;; activity dependency
(custom-add-hook 'canna-candidate-op-count
		 'custom-activity-hooks
		 (lambda ()
		   canna-use-candidate-window?))

(custom-add-hook 'canna-nr-candidate-max
		 'custom-activity-hooks
		 (lambda ()
		   canna-use-candidate-window?))

(custom-add-hook 'canna-select-candidate-by-numeral-key?
		 'custom-activity-hooks
		 (lambda ()
		   canna-use-candidate-window?))

;;
;; toolbar
;;

;; Can't be unified with action definitions in canna.scm until uim
;; 0.4.6.
(define canna-input-mode-indication-alist
  (list
   (list 'action_canna_direct
	 'ja_direct
	 "-"
	 (N_ "Direct input")
	 (N_ "Direct input mode"))
   (list 'action_canna_hiragana
	 'ja_hiragana
	 "¤¢"
	 (N_ "Hiragana")
	 (N_ "Hiragana input mode"))
   (list 'action_canna_katakana
	 'ja_katakana
	 "¥¢"
	 (N_ "Katakana")
	 (N_ "Katakana input mode"))
   (list 'action_canna_halfkana
	 'ja_halfkana
	 "Ž±"
	 (N_ "Halfwidth Katakana")
	 (N_ "Halfwidth Katakana input mode"))
   (list 'action_canna_halfwidth_alnum
	 'ja_halfwidth_alnum
	 "a"
	 (N_ "Halfwidth Alphanumeric")
	 (N_ "Halfwidth Alphanumeric input mode"))

   (list 'action_canna_fullwidth_alnum
	 'ja_fullwidth_alnum
	 "£Á"
	 (N_ "Fullwidth Alphanumeric")
	 (N_ "Fullwidth Alphanumeric input mode"))))

(define canna-kana-input-method-indication-alist
  (list
   (list 'action_canna_roma
	 'ja_romaji
	 "£Ò"
	 (N_ "Romaji")
	 (N_ "Romaji input mode"))
   (list 'action_canna_kana
	 'ja_kana
	 "¤«"
	 (N_ "Kana")
	 (N_ "Kana input mode"))
   (list 'action_canna_azik
	 'ja_azik
	 "£Ú"
	 (N_ "AZIK")
	 (N_ "AZIK extended romaji input mode"))
   (list 'action_canna_act
	 'ja_act
	 "£Ã"
	 (N_ "ACT")
	 (N_ "ACT extended romaji input mode"))
   (list 'action_canna_kzik
	 'ja_kzik
	 "£Ë"
	 (N_ "KZIK")
	 (N_ "KZIK extended romaji input mode"))))

;;; Buttons

(define-custom 'canna-widgets '(widget_canna_input_mode
				widget_canna_kana_input_method)
  '(canna toolbar-widget)
  (list 'ordered-list
	(list 'widget_canna_input_mode
	      (N_ "Input mode")
	      (N_ "Input mode"))
	(list 'widget_canna_kana_input_method
	      (N_ "Kana input method")
	      (N_ "Kana input method")))
  (N_ "Enabled toolbar buttons")
  (N_ "long description will be here."))

;; dynamic reconfiguration
;; canna-configure-widgets is not defined at this point. So wrapping
;; into lambda.
(custom-add-hook 'canna-widgets
		 'custom-set-hooks
		 (lambda ()
		   (canna-configure-widgets)))


;;; Input mode

(define-custom 'default-widget_canna_input_mode 'action_canna_direct
  '(canna toolbar-widget)
  (cons 'choice
	(map indication-alist-entry-extract-choice
	     canna-input-mode-indication-alist))
  (N_ "Default input mode")
  (N_ "long description will be here."))

(define-custom 'canna-input-mode-actions
               (map car canna-input-mode-indication-alist)
  '(canna toolbar-widget)
  (cons 'ordered-list
	(map indication-alist-entry-extract-choice
	     canna-input-mode-indication-alist))
  (N_ "Input mode menu items")
  (N_ "long description will be here."))

;; value dependency
(if custom-full-featured?
    (custom-add-hook 'canna-input-mode-actions
		     'custom-set-hooks
		     (lambda ()
		       (custom-choice-range-reflect-olist-val
			'default-widget_canna_input_mode
			'canna-input-mode-actions
			canna-input-mode-indication-alist))))

;; activity dependency
(custom-add-hook 'default-widget_canna_input_mode
		 'custom-activity-hooks
		 (lambda ()
		   (memq 'widget_canna_input_mode canna-widgets)))

(custom-add-hook 'canna-input-mode-actions
		 'custom-activity-hooks
		 (lambda ()
		   (memq 'widget_canna_input_mode canna-widgets)))

;; dynamic reconfiguration
(custom-add-hook 'default-widget_canna_input_mode
		 'custom-set-hooks
		 (lambda ()
		   (canna-configure-widgets)))

(custom-add-hook 'canna-input-mode-actions
		 'custom-set-hooks
		 (lambda ()
		   (canna-configure-widgets)))

;;; Kana input method

(define-custom 'default-widget_canna_kana_input_method 'action_canna_roma
  '(canna toolbar-widget)
  (cons 'choice
	(map indication-alist-entry-extract-choice
	     canna-kana-input-method-indication-alist))
  (N_ "Default kana input method")
  (N_ "long description will be here."))

(define-custom 'canna-kana-input-method-actions
               (map car canna-kana-input-method-indication-alist)
  '(canna toolbar-widget)
  (cons 'ordered-list
	(map indication-alist-entry-extract-choice
	     canna-kana-input-method-indication-alist))
  (N_ "Kana input method menu items")
  (N_ "long description will be here."))

;; value dependency
(if custom-full-featured?
    (custom-add-hook 'canna-kana-input-method-actions
		     'custom-set-hooks
		     (lambda ()
		       (custom-choice-range-reflect-olist-val
			'default-widget_canna_kana_input_method
			'canna-kana-input-method-actions
			canna-kana-input-method-indication-alist))))

;; activity dependency
(custom-add-hook 'default-widget_canna_kana_input_method
		 'custom-activity-hooks
		 (lambda ()
		   (memq 'widget_canna_kana_input_method canna-widgets)))

(custom-add-hook 'canna-kana-input-method-actions
		 'custom-activity-hooks
		 (lambda ()
		   (memq 'widget_canna_kana_input_method canna-widgets)))

;; dynamic reconfiguration
(custom-add-hook 'default-widget_canna_kana_input_method
		 'custom-set-hooks
		 (lambda ()
		   (canna-configure-widgets)))

(custom-add-hook 'canna-kana-input-method-actions
		 'custom-set-hooks
		 (lambda ()
		   (canna-configure-widgets)))


;;
;; canna-server-name
;;

; TODO: support cannaserver on other host
(define canna-server-name #f)
;(define canna-server-name "localhost")
;(define canna-server-name "127.0.0.1")

;; warning: must be defined before custom-preserved-canna-server-name
(define-custom 'custom-activate-canna-server-name? #f
  '(canna-advanced cannaserver)
  '(boolean)
  (N_ "Specify Canna server")
  (N_ "long description will be here."))

(define-custom 'custom-preserved-canna-server-name ""
  '(canna-advanced cannaserver)
  '(string ".*")
  (N_ "Canna server name")
  (N_ "long description will be here."))

(define-custom 'canna-user-name (user-name)
  '(canna-advanced cannaserver)
  '(string ".*")
  (N_ "Canna user name")
  (N_ "long description will be here."))

;; activity dependency
(custom-add-hook 'custom-preserved-canna-server-name
		 'custom-activity-hooks
		 (lambda ()
		   custom-activate-canna-server-name?))

(define custom-hook-get-canna-server-name
  (lambda ()
    (set! custom-activate-canna-server-name? canna-server-name)
    (set! custom-preserved-canna-server-name (or canna-server-name
						 custom-preserved-canna-server-name
						 ""))))

(custom-add-hook 'canna-user-name
		 'custom-activity-hooks
		 (lambda ()
		   custom-activate-canna-server-name?))

;; decode #f from canna-server-name
(custom-add-hook 'custom-activate-canna-server-name?
		 'custom-get-hooks
		 custom-hook-get-canna-server-name)
(custom-add-hook 'canna-server-name
		 'custom-get-hooks
		 custom-hook-get-canna-server-name)

(define custom-hook-set-canna-server-name
  (lambda ()
    (set! canna-server-name
	  (and custom-activate-canna-server-name?
	       custom-preserved-canna-server-name))))

;; encode #f into canna-server-name
(custom-add-hook 'custom-activate-canna-server-name?
		 'custom-set-hooks
		 custom-hook-set-canna-server-name)
(custom-add-hook 'custom-preserved-canna-server-name
		 'custom-set-hooks
		 custom-hook-set-canna-server-name)

(define custom-hook-literalize-preserved-canna-server-name
  (lambda ()
    (string-append
     "(define custom-preserved-canna-server-name "
     (custom-value-as-literal 'custom-preserved-canna-server-name)
     ")\n"
     "(define canna-server-name "
     (if canna-server-name
	 (string-append "\"" canna-server-name "\"")
	 "#f")
     ")")))

(custom-add-hook 'custom-preserved-canna-server-name
		 'custom-literalize-hooks
		 custom-hook-literalize-preserved-canna-server-name)

(define-custom 'canna-use-with-vi? #f
  '(canna-advanced special-op)
  '(boolean)
  (N_ "Enable vi-cooperative mode")
  (N_ "long description will be here."))

(define-custom 'canna-auto-start-henkan? #f
 '(canna-advanced special-op)
 '(boolean)
 (N_ "Enable auto conversion with punctuation marks")
 (N_ "long description will be here."))

(define-custom 'canna-use-mode-transition-keys-in-off-mode? #f
  '(canna-advanced mode-transition)
  '(boolean)
  (N_ "Enable input mode transition keys in direct (off state) input mode")
  (N_ "long description will be here."))

;; prediction
(define-custom 'canna-use-prediction? #f
  '(canna-advanced canna-prediction)
  '(boolean)
  (N_ "Enable input prediction")
  (N_ "long description will be here."))

(define-custom 'canna-select-prediction-by-numeral-key? #f
  '(canna-advanced canna-prediction)
  '(boolean)
  (N_ "Select prediction candidate by numeral keys")
  (N_ "long description will be here."))

(define-custom 'canna-use-implicit-commit-prediction? #t
  '(canna-advanced canna-prediction)
  '(boolean)
  (N_ "Show selected prediction candidate in preedit area")
  (N_ "long description will be here."))

(define-custom 'canna-prediction-cache-words 256
  '(canna-advanced canna-prediction)
  '(integer 1 65535)
  (N_ "Number of cache of prediction candidates")
  (N_ "long description will be here."))

(define-custom 'canna-prediction-start-char-count 2
  '(canna-advanced canna-prediction)
  '(integer 1 65535)
  (N_ "Character count to start input prediction")
  (N_ "long description will be here."))

(custom-add-hook 'canna-use-candidate-window?
                 'custom-get-hooks
                 (lambda ()
                   (if (not canna-use-candidate-window?)
                       (set! canna-use-prediction? #f))))

(custom-add-hook 'canna-use-prediction?
                 'custom-activity-hooks
                 (lambda ()
                   canna-use-candidate-window?))

(custom-add-hook 'canna-select-prediction-by-numeral-key?
                 'custom-activity-hooks
                 (lambda ()
                   canna-use-prediction?))

(custom-add-hook 'canna-use-implicit-commit-prediction?
                 'custom-activity-hooks
                 (lambda ()
                   canna-use-prediction?))

(custom-add-hook 'canna-prediction-cache-words
                 'custom-activity-hooks
                 (lambda ()
                   canna-use-prediction?))

(custom-add-hook 'canna-prediction-start-char-count
                 'custom-activity-hooks
                 (lambda ()
                   canna-use-prediction?))
