;;; skk-custom.scm: Customization variables for skk.scm
;;;
;;; Copyright (c) 2003-2006 uim Project http://uim.freedesktop.org/
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


(define skk-im-name-label (N_ "SKK"))
(define skk-im-short-desc (N_ "uim version of SKK input method"))

(define-custom-group 'skk
                     (ugettext skk-im-name-label)
                     (ugettext skk-im-short-desc))

(define-custom-group 'skk-dict
                     (_ "SKK dictionaries")
                     (_ "Dictionary settings for SKK"))

(define-custom-group 'skk-advanced
                     (_ "SKK (advanced)")
                     (_ "Advanced settings for SKK"))


;;
;; candidate window
;;

(define-custom 'skk-use-candidate-window? #t
  '(skk candwin)
  '(boolean)
  (_ "Use candidate window")
  (_ "long description will be here."))

(define-custom 'skk-commit-candidate-by-label-key? #t
  '(skk candwin)
  '(boolean)
  (_ "Commit candidate by heading label keys")
  (_ "long description will be here."))

(define-custom 'skk-candidate-selection-style 'ddskk-like
  '(skk candwin)
  (list 'choice
	(list 'uim (_ "uim") (_ "uim native"))
	(list 'ddskk-like (_ "ddskk-like") (_ "Similar to ddskk")))
  (_ "Candidate selection style")
  (_ "long description will be here."))

(define-custom 'skk-use-manual-candwin-setting? #f
  '(skk candwin)
  '(boolean)
  (_ "Set candidate window behavior manually")
  (_ "long description will be here."))

(define-custom 'skk-candidate-op-count 5
  '(skk candwin)
  '(integer 0 99)
  (_ "Conversion key press count to show candidate window")
  (_ "long description will be here."))

(define-custom 'skk-nr-candidate-max 7
  '(skk candwin)
  '(integer 1 20)
  (_ "Number of candidates in candidate window at a time")
  (_ "long description will be here."))

;; activity dependency
(custom-add-hook 'skk-commit-candidate-by-label-key?
		 'custom-activity-hooks
		 (lambda ()
		   skk-use-candidate-window?))

(custom-add-hook 'skk-candidate-selection-style
		 'custom-activity-hooks
		 (lambda ()
		   skk-use-candidate-window?))

(custom-add-hook 'skk-use-manual-candwin-setting?
		 'custom-activity-hooks
		 (lambda ()
		   skk-use-candidate-window?))

(custom-add-hook 'skk-candidate-op-count
		 'custom-activity-hooks
		 (lambda ()
		   skk-use-candidate-window?))

(custom-add-hook 'skk-nr-candidate-max
		 'custom-activity-hooks
		 (lambda ()
		   skk-use-candidate-window?))

(custom-add-hook 'skk-use-candidate-window?
		 'custom-get-hooks
		 (lambda ()
		   (if (not skk-use-candidate-window?)
		       (begin
		         (set! skk-candidate-selection-style 'uim)
			 (set! skk-use-manual-candwin-setting? #f)))))

(custom-add-hook 'skk-candidate-op-count
		 'custom-activity-hooks
		 (lambda ()
		   skk-use-manual-candwin-setting?))

(custom-add-hook 'skk-nr-candidate-max
		 'custom-activity-hooks
		 (lambda ()
		   skk-use-manual-candwin-setting?))
		   
(custom-add-hook 'skk-candidate-selection-style
		 'custom-set-hooks
		 (lambda ()
		   (if (not skk-use-manual-candwin-setting?)
		       (cond
			((eq? skk-candidate-selection-style 'ddskk-like)
			    (custom-set-value! 'skk-candidate-op-count 5)
			    (custom-set-value! 'skk-nr-candidate-max 7))
		        ((eq? skk-candidate-selection-style 'uim)
			    (custom-set-value! 'skk-candidate-op-count 2)
			    (custom-set-value! 'skk-nr-candidate-max 10))))))

(custom-add-hook 'skk-use-manual-candwin-setting?
		 'custom-set-hooks
		 (lambda ()
		   (if (not skk-use-manual-candwin-setting?)
		       (custom-set-value!
		        'skk-candidate-selection-style
		        skk-candidate-selection-style))))

;;
;; toolbar
;;

;; Can't be unified with action definitions in skk.scm until uim
;; 0.4.6.
(define skk-input-mode-indication-alist
  (list
   (list 'action_skk_latin
	 'ja_direct
	 "a"
	 (N_ "Direct input")
	 (N_ "Direct input mode"))
   (list 'action_skk_hiragana
	 'ja_hiragana
	 "¤¢"
	 (N_ "Hiragana")
	 (N_ "Hiragana input mode"))
   (list 'action_skk_katakana
	 'ja_katakana
	 "¥¢"
	 (N_ "Katakana")
	 (N_ "Katakana input mode"))
   (list 'action_skk_hankana
	 'ja_halfwidth_katakana
	 "Ž±"
	 (N_ "Halfwidth Katakana")
	 (N_ "Halfwidth Katakana input mode"))
   (list 'action_skk_wide_latin
	 'ja_fullwidth_alnum
	 "£Á"
	 (N_ "Fullwidth Alphanumeric")
	 (N_ "Fullwidth Alphanumeric input mode"))))

(define skk-kana-input-method-indication-alist
  (list
   (list 'action_skk_roma
	 'ja_romaji
	 "£Ò"
	 (N_ "Romaji")
	 (N_ "Romaji input mode"))
   (list 'action_skk_azik
	 'ja_azik
	 "£Ú"
	 (N_ "AZIK")
	 (N_ "AZIK extended romaji input mode"))))

;;; Buttons

(define-custom 'skk-widgets '(widget_skk_input_mode
			      widget_skk_kana_input_method)
  '(skk toolbar)
  (list 'ordered-list
	(list 'widget_skk_input_mode
	      (_ "Input mode")
	      (_ "Input mode"))
	(list 'widget_skk_kana_input_method
	      (_ "Kana input method")
	      (_ "Kana input method")))
  (_ "Enabled toolbar buttons")
  (_ "long description will be here."))

;; dynamic reconfiguration
;; skk-configure-widgets is not defined at this point. So wrapping
;; into lambda.
(custom-add-hook 'skk-widgets
		 'custom-set-hooks
		 (lambda ()
		   (skk-configure-widgets)))


;;; Input mode

(define-custom 'default-widget_skk_input_mode 'action_skk_latin
  '(skk toolbar)
  (cons 'choice
	(map indication-alist-entry-extract-choice
	     skk-input-mode-indication-alist))
  (_ "Default input mode")
  (_ "long description will be here."))

(define-custom 'skk-input-mode-actions
               (map car skk-input-mode-indication-alist)
  '(skk toolbar)
  (cons 'ordered-list
	(map indication-alist-entry-extract-choice
	     skk-input-mode-indication-alist))
  (_ "Input mode menu items")
  (_ "long description will be here."))

;; value dependency
(if custom-full-featured?
    (custom-add-hook 'skk-input-mode-actions
		     'custom-set-hooks
		     (lambda ()
		       (custom-choice-range-reflect-olist-val
			'default-widget_skk_input_mode
			'skk-input-mode-actions
			skk-input-mode-indication-alist))))

;; dynamic reconfiguration
(custom-add-hook 'default-widget_skk_input_mode
		 'custom-set-hooks
		 (lambda ()
		   (skk-configure-widgets)))

(custom-add-hook 'skk-input-mode-actions
		 'custom-set-hooks
		 (lambda ()
		   (skk-configure-widgets)))

;;; Kana input method

(define-custom 'default-widget_skk_kana_input_method 'action_skk_roma
  '(skk toolbar)
  (cons 'choice
	(map indication-alist-entry-extract-choice
	     skk-kana-input-method-indication-alist))
  (_ "Default kana input method")
  (_ "long description will be here."))

(define-custom 'skk-kana-input-method-actions
               (map car skk-kana-input-method-indication-alist)
  '(skk toolbar)
  (cons 'ordered-list
	(map indication-alist-entry-extract-choice
	     skk-kana-input-method-indication-alist))
  (_ "Kana input method menu items")
  (_ "long description will be here."))

;; value dependency
(if custom-full-featured?
    (custom-add-hook 'skk-kana-input-method-actions
		     'custom-set-hooks
		     (lambda ()
		       (custom-choice-range-reflect-olist-val
			'default-widget_skk_kana_input_method
			'skk-kana-input-method-actions
			skk-kana-input-method-indication-alist))))

;; activity dependency
(custom-add-hook 'default-widget_skk_kana_input_method
		 'custom-activity-hooks
		 (lambda ()
		   (memq 'widget_skk_kana_input_method skk-widgets)))

(custom-add-hook 'skk-kana-input-method-actions
		 'custom-activity-hooks
		 (lambda ()
		   (memq 'widget_skk_kana_input_method skk-widgets)))

;; dynamic reconfiguration
(custom-add-hook 'default-widget_skk_kana_input_method
		 'custom-set-hooks
		 (lambda ()
		   (skk-configure-widgets)))

(custom-add-hook 'skk-kana-input-method-actions
		 'custom-set-hooks
		 (lambda ()
		   (skk-configure-widgets)))


;;
;; dictionary
;;

(define-custom 'skk-use-skkserv? #f
  '(skk-dict)
  '(boolean)
  (_ "Use skkserv instead of SKK-JISYO")
  (_ "long description will be here."))

(define-custom 'skk-skkserv-portnum 1178
  '(skk-dict)
  '(integer 0 65535)
  (_ "Port number of skkserv")
  (_ "long description will be here."))

(custom-add-hook 'skk-skkserv-portnum
		 'custom-activity-hooks
		 (lambda ()
		   skk-use-skkserv?))

(define-custom 'skk-dic-file-name (string-append (sys-datadir)
						 "/skk/SKK-JISYO.L")
  '(skk-dict)
  '(pathname regular-file)
  (_ "Dictionary file")
  (_ "long description will be here."))

(define-custom 'skk-personal-dic-filename
  (string-append (getenv "HOME") "/.skk-jisyo")
  '(skk-dict)
  '(pathname regular-file)
  (_ "Personal dictionary file")
  (_ "long description will be here."))

(define-custom 'skk-uim-personal-dic-filename
  (string-append (getenv "HOME") "/.skk-uim-jisyo")
  '(skk-dict)
  '(pathname regular-file)
  (_ "Personal dictionary file (dedicated to uim)")
  (_ "long description will be here."))

(custom-add-hook 'skk-dic-file-name
		 'custom-activity-hooks
		 (lambda ()
		   (not skk-use-skkserv?)))

;;
;; advanced
;;

(define-custom 'skk-style 'skk-style-ddskk-like
  '(skk-advanced)
  (list 'choice
	(list 'skk-style-ddskk-like (_ "ddskk") (_ "Similar to ddskk"))
	(list 'skk-style-uim (_ "uim") (_ "uim native")))
  (_ "Visual style")
  (_ "long description will be here."))

(define-custom 'skk-use-recursive-learning? #t
  '(skk-advanced)
  '(boolean)
  (_ "Use recursive learning")
  (_ "long description will be here."))

(define-custom 'skk-use-numeric-conversion? #t
  '(skk-advanced)
  '(boolean)
  (_ "Use numeric conversion")
  (_ "long description will be here."))

(define-custom 'skk-auto-start-henkan? #t
  '(skk-advanced)
  '(boolean)
  (_ "Enable auto conversion with punctuation marks")
  (_ "long description will be here."))

(define-custom 'skk-dcomp-activate? #f
  '(skk-advanced)
  '(boolean)
  (_ "Enable dynamic completion")
  (_ "long description will be here."))

(define-custom 'skk-use-look? #f
  '(skk-advanced)
  '(boolean)
  (_ "Use UNIX look command for completion")
  (_ "long description will be here."))
;;
;; annotation
;;

(define-custom 'skk-show-annotation? #t
  '(skk-advanced annotation)
  '(boolean)
  (_ "Show annotation of candidate word")
  (_ "long description will be here."))

(define-custom 'skk-show-annotation-in-preedit? #f
  '(skk-advanced annotation)
  '(boolean)
  (_ "Show annotation also in preedit area")
  (_ "long description will be here."))

(custom-add-hook 'skk-show-annotation-in-preedit?
		 'custom-activity-hooks
		 (lambda ()
		   skk-show-annotation?))

(custom-add-hook 'skk-show-annotation?
		 'custom-get-hooks
		 (lambda ()
		   (if (not skk-show-annotation?)
		       (set! skk-show-annotation-in-preedit? #f))))

;;
;; special operations
;;

(define-custom 'skk-use-with-vi? #f
  '(skk-advanced special-op)
  '(boolean)
  (_ "Enable vi-cooperative mode")
  (_ "long description will be here."))

(define-custom 'skk-egg-like-newline? #f
  '(skk-advanced special-op)
  '(boolean)
  (_ "Use Enter key as just committing (egg-like operation)")
  (_ "long description will be here."))

;; should be removed if there is no usage
(define-custom 'skk-commit-newline-explicitly? #f
  '(skk-advanced special-op)
  '(boolean)
  (_ "Commit newline as ASCII string instead of native key-event")
  (_ "long description will be here."))
