;;; skk-custom.scm: Customization variables for skk.scm
;;;
;;; Copyright (c) 2003-2005 uim Project http://uim.freedesktop.org/
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
;;; THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
;;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
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
(define skk-im-short-desc (N_ "uim's SKK like input method"))

(define-custom-group 'skk
                     (ugettext skk-im-name-label)
                     (ugettext skk-im-short-desc))

(define-custom 'skk-use-candidate-window? #t
  '(skk candwin)
  '(boolean)
  (_ "Use candidate window")
  (_ "long description will be here."))

(define-custom 'skk-candidate-op-count 2
  '(skk candwin)
  '(integer 0 99)
  (_ "Conversion key press count to show candidate window")
  (_ "long description will be here."))

(define-custom 'skk-nr-candidate-max 10
  '(skk candwin)
  '(integer 1 20)
  (_ "Number of candidates in candidate window at a time")
  (_ "long description will be here."))

(define-custom 'skk-commit-candidate-by-label-key? #t
  '(skk candwin advanced)
  '(boolean)
  (_ "Commit candidate by heading label keys")
  (_ "long description will be here."))

;; activity dependency
(custom-add-hook 'skk-candidate-op-count
		 'custom-activity-hooks
		 (lambda ()
		   skk-use-candidate-window?))

(custom-add-hook 'skk-nr-candidate-max
		 'custom-activity-hooks
		 (lambda ()
		   skk-use-candidate-window?))

(custom-add-hook 'skk-commit-candidate-by-label-key?
		 'custom-activity-hooks
		 (lambda ()
		   skk-use-candidate-window?))

(define-custom 'skk-style 'skk-style-ddskk-like
  '(skk advanced)
  (list 'choice
	(list 'skk-style-ddskk-like (_ "ddskk") (_ "Similar to ddskk"))
	(list 'skk-style-uim (_ "uim") (_ "uim native")))
  (_ "Visual style")
  (_ "long description will be here."))

(define-custom 'skk-use-recursive-learning? #t
  '(skk advanced)
  '(boolean)
  (_ "Use recursive learning")
  (_ "long description will be here."))

(define-custom 'skk-egg-like-newline? #f
  '(skk advanced)
  '(boolean)
  (_ "Use Enter key as just committing (egg-like operation)")
  (_ "long description will be here."))

;; should be removed if there is no usage
(define-custom 'skk-commit-newline-explicitly? #f
  '(skk advanced)
  '(boolean)
  (_ "Commit newline as ASCII string instead of native key-event")
  (_ "long description will be here."))

(define-custom 'skk-use-numeric-conversion? #t
  '(skk advanced)
  '(boolean)
  (_ "Use numeric conversion")
  (_ "long description will be here."))

(define-custom 'skk-use-with-vi? #f
  '(skk advanced)
  '(boolean)
  (_ "Friendly for vi user")
  (_ "long description will be here."))

(define-custom 'skk-auto-start-henkan? #t
  '(skk advanced)
  '(boolean)
  (_ "Enable auto conversion with punctuation marks")
  (_ "long description will be here."))

(define-custom 'skk-show-annotation? #t
  '(skk advanced)
  '(boolean)
  (_ "Show annotation in candidate window")
  (_ "long description will be here."))

(define-custom 'skk-show-annotation-in-preedit? #f
  '(skk advanced)
  '(boolean)
  (_ "Show annotation in preedit area")
  (_ "long description will be here."))

(custom-add-hook 'skk-show-annotation-in-preedit?
		 'custom-activity-hooks
		 (lambda ()
		   skk-show-annotation?))

(define-custom 'skk-dic-file-name (string-append (sys-datadir)
						 "/skk/SKK-JISYO.L")
  '(skk)
  '(pathname)
  (_ "Dictionary file")
  (_ "long description will be here."))

(define-custom 'skk-personal-dic-filename
  (string-append (getenv "HOME") "/.skk-jisyo")
  '(skk)
  '(pathname)
  (_ "Personal dictionary file")
  (_ "long description will be here."))

(define-custom 'skk-uim-personal-dic-filename
  (string-append (getenv "HOME") "/.skk-uim-jisyo")
  '(skk)
  '(pathname)
  (_ "Personal dictionary file (dedicated to uim)")
  (_ "long description will be here."))


;;
;; toolbar
;;

;; Can't be unified with action definitions in skk.scm until uim
;; 0.4.6.
(define skk-input-mode-indication-alist
  (list
   (list 'action_skk_latin
	 'figure_ja_latin
	 "s"
	 (N_ "Direct input")
	 (N_ "Direct input mode"))
   (list 'action_skk_hiragana
	 'figure_ja_hiragana
	 "¤¨"
	 (N_ "Hiragana")
	 (N_ "Hiragana input mode"))
   (list 'action_skk_katakana
	 'figure_ja_katakana
	 "¥¨"
	 (N_ "Katakana")
	 (N_ "Katakana input mode"))
   (list 'action_skk_hankana
	 'figure_ja_hankana
	 "Ž´"
	 (N_ "Halfwidth Katakana")
	 (N_ "Halfwidth Katakana input mode"))
   (list 'action_skk_wide_latin
	 'figure_ja_wide_latin
	 "£Ó"
	 (N_ "Fullwidth Alphanumeric")
	 (N_ "Fullwidth Alphanumeric input mode"))))

(define skk-widgets '(widget_skk_input_mode))

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
(custom-add-hook 'skk-input-mode-actions
		 'custom-set-hooks
		 (lambda ()
		   (custom-choice-range-reflect-olist-val
		    'default-widget_skk_input_mode
		    'skk-input-mode-actions
		    skk-input-mode-indication-alist)))

;; dynamic reconfiguration
(custom-add-hook 'default-widget_skk_input_mode
		 'custom-set-hooks
		 (lambda ()
		   skk-configure-widgets))

(custom-add-hook 'skk-input-mode-actions
		 'custom-set-hooks
		 (lambda ()
		   skk-configure-widgets))

