;;; skk-custom.scm: Customization variables for skk.scm
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


(define skk-im-name-label (N_ "SKK"))
(define skk-im-short-desc (N_ "uim version of SKK input method"))

(define-custom-group 'skk
                     skk-im-name-label
                     skk-im-short-desc)

(define-custom-group 'skk-dict
                     (N_ "SKK dictionaries")
                     (N_ "Dictionary settings for SKK"))

(define-custom-group 'skk-advanced
                     (N_ "SKK (advanced)")
                     (N_ "Advanced settings for SKK"))

;; subgroup
(define-custom-group 'skkserv
		     (N_ "SKK server")
		     (N_ "long description will be here."))

;; subgroup
(define-custom-group 'dict-files
		     (N_ "Dictionary files")
		     (N_ "long description will be here."))


;;
;; candidate window
;;

(define-custom 'skk-use-candidate-window? #t
  '(skk candwin)
  '(boolean)
  (N_ "Use candidate window")
  (N_ "long description will be here."))

(define-custom 'skk-commit-candidate-by-label-key? #t
  '(skk candwin)
  '(boolean)
  (N_ "Commit candidate by heading label keys")
  (N_ "long description will be here."))

(define-custom 'skk-candidate-selection-style 'ddskk-like
  '(skk candwin)
  (list 'choice
	(list 'uim (N_ "uim") (N_ "uim native"))
	(list 'ddskk-like (N_ "ddskk-like") (N_ "Similar to ddskk")))
  (N_ "Candidate selection style")
  (N_ "long description will be here."))

(define-custom 'skk-use-manual-candwin-setting? #f
  '(skk candwin)
  '(boolean)
  (N_ "Set candidate window behavior manually")
  (N_ "long description will be here."))

(define-custom 'skk-candidate-op-count 5
  '(skk candwin)
  '(integer 0 99)
  (N_ "Conversion key press count to show candidate window")
  (N_ "long description will be here."))

(define-custom 'skk-nr-candidate-max 7
  '(skk candwin)
  '(integer 1 20)
  (N_ "Number of candidates in candidate window at a time")
  (N_ "long description will be here."))

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
	 'ja_halfwidth_alnum
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
	 'ja_halfkana
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
	 (N_ "AZIK extended romaji input mode"))

   (list 'action_skk_act
	 'ja_act
	 "£Ã"
	 (N_ "ACT")
	 (N_ "ACT extended romaji input mode"))

   (list 'action_skk_kzik
	 'ja_kzik
	 "£Ë"
	 (N_ "KZIK")
	 (N_ "KZIK extended romaji input mode"))))
    
;;; Buttons

(define-custom 'skk-widgets '(widget_skk_input_mode
			      widget_skk_kana_input_method)
  '(skk toolbar-widget)
  (list 'ordered-list
	(list 'widget_skk_input_mode
	      (N_ "Input mode")
	      (N_ "Input mode"))
	(list 'widget_skk_kana_input_method
	      (N_ "Kana input method")
	      (N_ "Kana input method")))
  (N_ "Enabled toolbar buttons")
  (N_ "long description will be here."))

;; dynamic reconfiguration
;; skk-configure-widgets is not defined at this point. So wrapping
;; into lambda.
(custom-add-hook 'skk-widgets
		 'custom-set-hooks
		 (lambda ()
		   (skk-configure-widgets)))


;;; Input mode

(define-custom 'default-widget_skk_input_mode 'action_skk_latin
  '(skk toolbar-widget)
  (cons 'choice
	(map indication-alist-entry-extract-choice
	     skk-input-mode-indication-alist))
  (N_ "Default input mode")
  (N_ "long description will be here."))

(define-custom 'skk-input-mode-actions
               (map car skk-input-mode-indication-alist)
  '(skk toolbar-widget)
  (cons 'ordered-list
	(map indication-alist-entry-extract-choice
	     skk-input-mode-indication-alist))
  (N_ "Input mode menu items")
  (N_ "long description will be here."))

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
  '(skk toolbar-widget)
  (cons 'choice
	(map indication-alist-entry-extract-choice
	     skk-kana-input-method-indication-alist))
  (N_ "Default kana input method")
  (N_ "long description will be here."))

(define-custom 'skk-kana-input-method-actions
               (map car skk-kana-input-method-indication-alist)
  '(skk toolbar-widget)
  (cons 'ordered-list
	(map indication-alist-entry-extract-choice
	     skk-kana-input-method-indication-alist))
  (N_ "Kana input method menu items")
  (N_ "long description will be here."))

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
  '(skk-dict skkserv)
  '(boolean)
  (N_ "Use skkserv instead of SKK-JISYO")
  (N_ "long description will be here."))

(define-custom 'skk-skkserv-enable-completion? #f
  '(skk-dict skkserv)
  '(boolean)
  (N_ "Enable skkserv completion")
  (N_ "long description will be here."))

(custom-add-hook 'skk-skkserv-enable-completion?
		 'custom-activity-hooks
		 (lambda ()
		   skk-use-skkserv?))

(define-custom 'skk-skkserv-completion-timeout 2000
  '(skk-dict skkserv)
  '(integer -1 65535)
  (N_ "Timeout for skkserv completion (msec)")
  (N_ "long description will be here."))

(custom-add-hook 'skk-skkserv-completion-timeout
		 'custom-activity-hooks
		 (lambda ()
		   skk-skkserv-enable-completion?))

(define-custom 'skk-skkserv-use-env? #t
  '(skk-dict skkserv)
  '(boolean)
  (N_ "Use value of environment variable SKKSERVER")
  (N_ "long description will be here."))

(custom-add-hook 'skk-skkserv-use-env?
		 'custom-activity-hooks
		 (lambda ()
		   skk-use-skkserv?))

(define-custom 'skk-skkserv-hostname "localhost"
  '(skk-dict skkserv)
  '(string ".*")
  (N_ "Hostname of skkserv")
  (N_ "long description will be here."))

(custom-add-hook 'skk-skkserv-hostname
		 'custom-activity-hooks
		 (lambda ()
		   (not skk-skkserv-use-env?)))

(define-custom 'skk-skkserv-portnum 1178
  '(skk-dict skkserv)
  '(integer 0 65535)
  (N_ "Port number of skkserv")
  (N_ "long description will be here."))

(custom-add-hook 'skk-skkserv-portnum
		 'custom-activity-hooks
		 (lambda ()
		   skk-use-skkserv?))

(define-custom 'skk-skkserv-address-family 'unspecified
  '(skk-dict skkserv)
  (list 'choice
	(list 'unspecified (N_ "Auto") (N_ "Auto"))
	(list 'inet  (N_ "IPv4")
	      (N_ "Forces skkserv to use IPv4 addresses only"))
	(list 'inet6 (N_ "IPv6")
	      (N_ "Forces skkserv to use IPv6 addresses only")))
  (N_ "Address family of skkserv")
  (N_ "long description will be here."))

(custom-add-hook 'skk-skkserv-address-family
	 'custom-activity-hooks
	 (lambda ()
	   skk-use-skkserv?))

(define-custom 'skk-dic-file-name (string-append (sys-datadir)
						 "/skk/SKK-JISYO.L")
  '(skk-dict dict-files)
  '(pathname regular-file)
  (N_ "System dictionary file")
  (N_ "long description will be here."))

(define-custom 'skk-personal-dic-filename
  (string-append (or (home-directory (user-name)) "") "/.skk-jisyo")
  '(skk-dict dict-files)
  '(pathname regular-file)
  (N_ "Personal dictionary file")
  (N_ "long description will be here."))

(define-custom 'skk-uim-personal-dic-filename
  (string-append (or (home-directory (user-name)) "") "/.skk-uim-jisyo")
  '(skk-dict dict-files)
  '(pathname regular-file)
  (N_ "Personal dictionary file (dedicated to uim)")
  (N_ "long description will be here."))

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
	(list 'skk-style-ddskk-like (N_ "ddskk") (N_ "Similar to ddskk"))
	(list 'skk-style-uim (N_ "uim") (N_ "uim native")))
  (N_ "Visual style")
  (N_ "long description will be here."))

(define-custom 'skk-use-recursive-learning? #t
  '(skk-advanced)
  '(boolean)
  (N_ "Use recursive learning")
  (N_ "long description will be here."))

(define-custom 'skk-use-numeric-conversion? #t
  '(skk-advanced)
  '(boolean)
  (N_ "Use numeric conversion")
  (N_ "long description will be here."))

(define-custom 'skk-auto-start-henkan? #t
  '(skk-advanced)
  '(boolean)
  (N_ "Enable auto conversion with punctuation marks")
  (N_ "long description will be here."))

(define-custom 'skk-dcomp-activate? #f
  '(skk-advanced)
  '(boolean)
  (N_ "Enable dynamic completion")
  (N_ "long description will be here."))

(define-custom 'skk-use-look? #f
  '(skk-advanced)
  '(boolean)
  (N_ "Use UNIX look command for completion")
  (N_ "long description will be here."))

(custom-add-hook 'skk-use-look?
                 'custom-set-hooks
                 (lambda ()
                   (if skk-use-look?
                     (skk-lib-look-open skk-look-dict))))

(define-custom 'skk-look-dict "/usr/share/dict/words"
  '(skk-advanced)
  '(pathname regular-file)
  (N_ "Use UNIX look dictionary file")
  (N_ "long description will be here."))

(custom-add-hook 'skk-look-dict
		 'custom-activity-hooks
		 (lambda ()
		   skk-use-look?))

;;
;; annotation
;;

(define-custom 'skk-show-annotation? #t
  '(skk-advanced annotation)
  '(boolean)
  (N_ "Show annotation of candidate word")
  (N_ "long description will be here."))

(define-custom 'skk-show-annotation-in-preedit? #f
  '(skk-advanced annotation)
  '(boolean)
  (N_ "Show annotation also in preedit area")
  (N_ "long description will be here."))

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
  (N_ "Enable vi-cooperative mode")
  (N_ "long description will be here."))

(define-custom 'skk-egg-like-newline? #f
  '(skk-advanced special-op)
  '(boolean)
  (N_ "Use Enter key as just committing (egg-like operation)")
  (N_ "long description will be here."))

;; should be removed if there is no usage
(define-custom 'skk-commit-newline-explicitly? #f
  '(skk-advanced special-op)
  '(boolean)
  (N_ "Commit newline as ASCII string instead of native key-event")
  (N_ "long description will be here."))
