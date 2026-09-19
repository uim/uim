;;;
;;; Copyright (c) 2010-2012 uim Project https://github.com/uim/uim
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

;;; This file originates from the MacUIM project by Etsushi Kato
;;; (https://github.com/e-kato/macuim, Mozc/scm/mozc-custom.scm) and
;;; carries its BSD-3-Clause license above. Only the uim-mozc-helper
;;; customs were added for uim.

(require "i18n.scm")

(define mozc-im-name-label (N_ "Mozc"))
(define mozc-im-short-desc (N_ "Mozc Japanese engine"))

(define-custom-group 'mozc
		     (ugettext mozc-im-name-label)
		     (ugettext mozc-im-short-desc))

;;
;; segment separator
;;

(define-custom 'mozc-show-segment-separator? #f
  '(mozc segment-sep)
  '(boolean)
  (N_ "Show segment separator")
  (N_ "long description will be here."))

(define-custom 'mozc-segment-separator "|"
  '(mozc segment-sep)
  '(string ".*")
  (N_ "Segment separator")
  (N_ "long description will be here."))

(custom-add-hook 'mozc-segment-separator
                 'custom-activity-hooks
                 (lambda ()
                   mozc-show-segment-separator?))

;;
;; toolbar
;;

;; Can't be unified with action definitions in mozc.scm until uim
;; 0.4.6.
(define mozc-input-mode-indication-alist
  (list
   (list 'action_mozc_direct
	 'ja_direct
	 "-"
	 (N_ "Direct input")
	 (N_ "Direct input mode"))
   (list 'action_mozc_hiragana
	 'ja_hiragana
	 "あ"
	 (N_ "Hiragana")
	 (N_ "Hiragana input mode"))
   (list 'action_mozc_katakana
	 'ja_katakana
	 "ア"
	 (N_ "Katakana")
	 (N_ "Katakana input mode"))
   (list 'action_mozc_halfkana
	 'ja_halfkana
	 "ｱ"
	 (N_ "Halfwidth Katakana")
	 (N_ "Halfwidth Katakana input mode"))
   (list 'action_mozc_halfwidth_alnum
	 'ja_halfwidth_alnum
	 "a"
	 (N_ "Halfwidth Alphanumeric")
	 (N_ "Halfwidth Alphanumeric input mode"))
   (list 'action_mozc_fullwidth_alnum
	 'ja_fullwidth_alnum
	 "Ａ"
	 (N_ "Fullwidth Alphanumeric")
	 (N_ "Fullwidth Alphanumeric input mode"))))

(define mozc-kana-input-method-indication-alist
  (list
   (list 'action_mozc_roma
         'ja_romaji
         "Ｒ"
         (N_ "Romaji")
         (N_ "Romaji input mode"))
   (list 'action_mozc_kana
         'ja_kana
         "か"
         (N_ "Kana")
         (N_ "Kana input mode"))))

(define mozc-tool-indication-alist
  (list
   (list 'action_mozc_tool_selector
         'mozc_tool_selector
         "T"
         (N_ "MozcTool selector")
         (N_ "MozcTool selector"))
   (list 'action_mozc_tool_about_dialog
         'mozc_tool_about_dialog
         "A"
         (N_ "About")
         (N_ "About"))
   (list 'action_mozc_tool_config_dialog
         'mozc_tool_config_dialog
         "C"
         (N_ "Config dialog")
         (N_ "Config dialog"))
   (list 'action_mozc_tool_dictionary_tool
         'mozc_tool_dictionary_tool
         "D"
         (N_ "Dictionary tool")
         (N_ "Dictionary tool"))
   (list 'action_mozc_tool_word_register_dialog
         'mozc_tool_word_register_dialog
         "W"
         (N_ "Word register dialog")
         (N_ "Word register dialog"))
   (list 'action_mozc_tool_character_palette
         'mozc_tool_character_palette
         "P"
         (N_ "Character palette")
         (N_ "Character palette"))
   (list 'action_mozc_tool_hand_writing
         'mozc_tool_hand_writing
         "H"
         (N_ "Hand writing")
         (N_ "Hand writing"))
   (list 'action_mozc_reconvert
         'mozc_reconvert
         "R"
         (N_ "Reconvert")
         (N_ "Reconvert"))))


;;; Buttons

(define-custom 'mozc-widgets '(widget_mozc_input_mode
                               widget_mozc_kana_input_method
                               widget_mozc_tool)
  '(mozc toolbar)
  (list 'ordered-list
	(list 'widget_mozc_input_mode
	      (_ "Input mode")
	      (_ "Input mode"))
	(list 'widget_mozc_kana_input_method
	      (_ "Kana input method")
	      (_ "Kana input method"))
	(list 'widget_mozc_tool
	      (_ "Mozc tool")
	      (_ "Mozc tool")))
  (_ "Enabled toolbar buttons")
  (_ "long description will be here."))

;; dynamic reconfiguration
;; mozc-configure-widgets is not defined at this point. So wrapping
;; into lambda.
(custom-add-hook 'mozc-widgets
		 'custom-set-hooks
		 (lambda ()
		   (mozc-configure-widgets)))


;;; Input mode

(define-custom 'default-widget_mozc_input_mode 'action_mozc_direct
  '(mozc toolbar)
  (cons 'choice
	(map indication-alist-entry-extract-choice
	     mozc-input-mode-indication-alist))
  (_ "Default input mode")
  (_ "long description will be here."))

(define-custom 'mozc-input-mode-actions
	       (map car mozc-input-mode-indication-alist)
  '(mozc toolbar)
  (cons 'ordered-list
	(map indication-alist-entry-extract-choice
	     mozc-input-mode-indication-alist))
  (_ "Input mode menu items")
  (_ "long description will be here."))

;; value dependency
(if custom-full-featured?
    (custom-add-hook 'mozc-input-mode-actions
		     'custom-set-hooks
		     (lambda ()
		       (custom-choice-range-reflect-olist-val
			'default-widget_mozc_input_mode
			'mozc-input-mode-actions
			mozc-input-mode-indication-alist))))

;; activity dependency
(custom-add-hook 'default-widget_mozc_input_mode
		 'custom-activity-hooks
		 (lambda ()
		   (memq 'widget_mozc_input_mode mozc-widgets)))

(custom-add-hook 'mozc-input-mode-actions
		 'custom-activity-hooks
		 (lambda ()
		   (memq 'widget_mozc_input_mode mozc-widgets)))

;; dynamic reconfiguration
(custom-add-hook 'default-widget_mozc_input_mode
		 'custom-set-hooks
		 (lambda ()
		   (mozc-configure-widgets)))

(custom-add-hook 'mozc-input-mode-actions
		 'custom-set-hooks
		 (lambda ()
		   (mozc-configure-widgets)))

;;; Kana input method

(define-custom 'default-widget_mozc_kana_input_method 'action_mozc_roma
  '(mozc toolbar)
  (cons 'choice
        (map indication-alist-entry-extract-choice
             mozc-kana-input-method-indication-alist))
  (N_ "Default kana input method")
  (N_ "long description will be here."))

(define-custom 'mozc-kana-input-method-actions
               (map car mozc-kana-input-method-indication-alist)
  '(mozc toolbar)
  (cons 'ordered-list
        (map indication-alist-entry-extract-choice
             mozc-kana-input-method-indication-alist))
  (N_ "Kana input method menu items")
  (N_ "long description will be here."))

;; value dependency
(if custom-full-featured?
    (custom-add-hook 'mozc-kana-input-method-actions
                     'custom-set-hooks
                     (lambda ()
                       (custom-choice-range-reflect-olist-val
                        'default-widget_mozc_kana_input_method
                        'mozc-kana-input-method-actions
                        mozc-kana-input-method-indication-alist))))

;; activity dependency
(custom-add-hook 'default-widget_mozc_kana_input_method
                 'custom-activity-hooks
                 (lambda ()
                   (memq 'widget_mozc_kana_input_method mozc-widgets
)))

(custom-add-hook 'mozc-kana-input-method-actions
                 'custom-activity-hooks
                 (lambda ()
                   (memq 'widget_mozc_kana_input_method mozc-widgets
)))

;; dynamic reconfiguration
(custom-add-hook 'default-widget_mozc_kana_input_method
                 'custom-set-hooks
                 (lambda ()
                   (mozc-configure-widgets)))

(custom-add-hook 'mozc-kana-input-method-actions
                 'custom-set-hooks
                 (lambda ()
                   (mozc-configure-widgets)))


;;; Mozc tool
(define mozc-tool-actions (map car mozc-tool-indication-alist))





(define-custom 'mozc-use-with-vi? #f
  '(mozc special-op)
  '(boolean)
  (N_ "Enable vi-cooperative mode")
  (N_ "long description will be here."))

(define-custom 'mozc-use-context-aware-conversion? #f
  '(mozc special-op)
  '(boolean)
  (N_ "Use text input with context awareness")
  (N_ "long description will be here."))

(define-custom 'mozc-keyboard-type-for-kana-input-method 'jp-keyboard
  '(mozc)
  (list 'choice
        (list 'jp-keyboard
              (N_ "Japanese keyboard")
              (N_ "long description will be here."))
        (list 'us-keyboard
              (N_ "US keyboard")
              (N_ "long description will be here.")))
  (N_ "Keyboard type for kana input method")
  (N_ "long description will be here."))


;;
;; helper processes
;;
(define-custom-group 'mozc-helper
		     (N_ "Mozc helper")
		     (N_ "Helper process settings"))

(define-custom 'mozc-helper-command "uim-mozc-helper"
               '(mozc-helper)
               '(string ".*")
               (N_ "uim-mozc-helper command")
               (N_ "Command name or path of uim-mozc-helper that talks to mozc_server"))

(define-custom 'mozc-emacs-helper-command "mozc_emacs_helper"
               '(mozc-helper)
               '(string ".*")
               (N_ "mozc_emacs_helper command")
               (N_ "Command name or path of mozc_emacs_helper used to launch mozc_server"))

(define-custom-group 'mozc-tool
		     (N_ "MozcTool")
		     (N_ "MozcTool settings"))

(define-custom 'mozc-tool-about-dialog-cmd "/usr/lib/mozc/mozc_tool"
               '(mozc-tool)
               '(pathname regular-file)
               (N_ "Path of about dialog command")
               (N_ "long description will be here."))

(define-custom 'mozc-tool-about-dialog-cmd-option "--mode=about_dialog"
               '(mozc-tool)
               '(string ".*")
               (N_ "Option for about dialog command")
               (N_ "long description will be here."))

(define-custom 'mozc-tool-config-dialog-cmd "/usr/lib/mozc/mozc_tool"
               '(mozc-tool)
               '(pathname regular-file)
               (N_ "Path of config dialog command")
               (N_ "long description will be here."))

(define-custom 'mozc-tool-config-dialog-cmd-option "--mode=config_dialog"
               '(mozc-tool)
               '(string ".*")
               (N_ "Option for config dialog command")
               (N_ "long description will be here."))

(define-custom 'mozc-tool-dictionary-tool-cmd "/usr/lib/mozc/mozc_tool"
               '(mozc-tool)
               '(pathname regular-file)
               (N_ "Path of dictionary tool command")
               (N_ "long description will be here."))

(define-custom 'mozc-tool-dictionary-tool-cmd-option "--mode=dictionary_tool"
               '(mozc-tool)
               '(string ".*")
               (N_ "Option for dictionary tool command")
               (N_ "long description will be here."))

(define-custom 'mozc-tool-word-register-dialog-cmd "/usr/lib/mozc/mozc_tool"
               '(mozc-tool)
               '(pathname regular-file)
               (N_ "Path of word register dialog command")
               (N_ "long description will be here."))

(define-custom 'mozc-tool-word-register-dialog-cmd-option "--mode=word_register_dialog"
               '(mozc-tool)
               '(string ".*")
               (N_ "Option for word register dialog command")
               (N_ "long description will be here."))

(define-custom 'mozc-tool-character-palette-cmd "/usr/lib/mozc/mozc_tool"
               '(mozc-tool)
               '(pathname regular-file)
               (N_ "Path of character palette command")
               (N_ "long description will be here."))

(define-custom 'mozc-tool-character-palette-cmd-option "--mode=character_palette"
               '(mozc-tool)
               '(string ".*")
               (N_ "Option for character palette command")
               (N_ "long description will be here."))

(define-custom 'mozc-tool-hand-writing-cmd "/usr/lib/mozc/mozc_tool"
               '(mozc-tool)
               '(pathname regular-file)
               (N_ "Path of hand writing command")
               (N_ "long description will be here."))

(define-custom 'mozc-tool-hand-writing-cmd-option "--mode=hand_writing"
               '(mozc-tool)
               '(string ".*")
               (N_ "Option for hand writing command")
               (N_ "long description will be here."))
