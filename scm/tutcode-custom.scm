;;; tutcode-custom.scm: Customization variables for tutcode.scm
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


(define tutcode-im-name-label (N_ "TUT-Code"))
(define tutcode-im-short-desc (N_ "uim version of TUT-Code input method"))

(define-custom-group 'tutcode
                     tutcode-im-name-label
                     tutcode-im-short-desc)

(define-custom-group 'tutcode-dict
                     (N_ "TUT-Code dictionaries")
                     (N_ "Dictionary settings for TUT-Code"))

(define-custom-group 'tutcode-bushu
                     (N_ "Bushu conversion")
                     (N_ "Bushu conversion settings for TUT-Code"))

(define-custom-group 'tutcode-mazegaki
                     (N_ "Mazegaki conversion")
                     (N_ "Mazegaki conversion settings for TUT-Code"))

(define-custom-group 'tutcode-prediction
                    (N_ "Prediction")
                    (N_ "long description will be here."))

;;
;; dictionary
;;

(define-custom 'tutcode-dic-filename (string-append (sys-datadir)
						 "/tc/mazegaki.dic")
  '(tutcode tutcode-dict)
  '(pathname regular-file)
  (N_ "Mazegaki dictionary file")
  (N_ "long description will be here."))

(define-custom 'tutcode-personal-dic-filename
  (string-append (or (home-directory (user-name)) "") "/.mazegaki.dic")
  '(tutcode tutcode-dict)
  '(pathname regular-file)
  (N_ "Personal mazegaki dictionary file")
  (N_ "long description will be here."))

(define-custom 'tutcode-rule-filename
  (string-append (sys-pkgdatadir) "/tutcode-rule.scm")
  '(tutcode)
  '(pathname regular-file)
  (N_ "Code table file")
  (N_ "Code table name is 'filename-rule' when code table file name is 'filename.scm'."))

(define-custom 'tutcode-enable-mazegaki-learning? #t
  '(tutcode tutcode-mazegaki)
  '(boolean)
  (N_ "Enable learning in mazegaki conversion")
  (N_ "long description will be here."))

(define-custom 'tutcode-mazegaki-fixed-priority-count 0
  '(tutcode tutcode-mazegaki)
  '(integer 0 65535)
  (N_ "Number of candidates to be excluded from mazegaki learning")
  (N_ "long description will be here."))

(define-custom 'tutcode-use-recursive-learning? #t
  '(tutcode tutcode-mazegaki)
  '(boolean)
  (N_ "Use recursive learning")
  (N_ "long description will be here."))

(define-custom 'tutcode-use-with-vi? #f
  '(tutcode)
  '(boolean)
  (N_ "Enable vi-cooperative mode")
  (N_ "long description will be here."))

(define-custom 'tutcode-show-pending-rk? #f
  '(tutcode)
  '(boolean)
  (N_ "Show pending key sequences")
  (N_ "long description will be here."))

(define-custom 'tutcode-use-dvorak? #f
  '(tutcode)
  '(boolean)
  (N_ "Use Dvorak keyboard")
  (N_ "long description will be here."))

(define-custom 'tutcode-use-kigou2-mode? #f
  '(tutcode)
  '(boolean)
  (N_ "Enable two stroke kigou mode")
  (N_ "long description will be here."))

(define-custom 'tutcode-enable-fallback-surrounding-text? #f
  '(tutcode)
  '(boolean)
  (N_ "Enable fallback of surrounding text API")
  (N_ "long description will be here."))

(define-custom 'tutcode-keep-illegal-sequence? #f
  '(tutcode)
  '(boolean)
  (N_ "Keep key sequence not convertible to Kanji")
  (N_ "long description will be here."))

(define-custom 'tutcode-delete-leading-delimiter-on-postfix-kanji2seq? #f
  '(tutcode)
  '(boolean)
  (N_ "Delete leading delimiter on postfix kanji to sequence conversion")
  (N_ "long description will be here."))

(define-custom 'tutcode-history-size 0
  '(tutcode)
  '(integer 0 65535)
  (N_ "History size")
  (N_ "long description will be here."))

(define-custom 'tutcode-mazegaki-yomi-max 10
  '(tutcode tutcode-mazegaki)
  '(integer 1 99)
  (N_ "Maximum length of yomi for postfix mazegaki conversion")
  (N_ "long description will be here."))

(define-custom 'tutcode-mazegaki-enable-inflection? #f
  '(tutcode tutcode-mazegaki)
  '(boolean)
  (N_ "Enable inflection in mazegaki conversion")
  (N_ "long description will be here."))

(define-custom 'tutcode-mazegaki-suffix-max 4
  '(tutcode tutcode-mazegaki)
  '(integer 1 99)
  (N_ "Maximum length of yomi suffix for mazegaki conversion")
  (N_ "long description will be here."))

(define-custom 'tutcode-bushu-conversion-algorithm 'tc-2.1+ml1925
  '(tutcode tutcode-bushu)
  (list 'choice
    (list 'tc-2.1+ml1925
          (N_ "tc-2.1+[tcode-ml:1925]") (N_ "tc-2.1+[tcode-ml:1925]"))
    (list 'kw-yamanobe
          (N_ "Kanchoku Win YAMANOBE") (N_ "Kanchoku Win YAMANOBE"))
    (list 'tc-2.3.1-22.6 (N_ "tc-2.3.1-22.6") (N_ "tc-2.3.1-22.6")))
  (N_ "Bushu conversion algorithm")
  (N_ "long description will be here."))

(define-custom 'tutcode-use-interactive-bushu-conversion? #f
  '(tutcode tutcode-bushu)
  '(boolean)
  (N_ "Enable interactive bushu conversion")
  (N_ "long description will be here."))

(define-custom 'tutcode-bushu-index2-filename (string-append (sys-datadir)
						 "/tc/bushu.index2")
  '(tutcode tutcode-bushu)
  '(pathname regular-file)
  (N_ "bushu.index2 file")
  (N_ "long description will be here."))

(define-custom 'tutcode-bushu-expand-filename (string-append (sys-datadir)
						 "/tc/bushu.expand")
  '(tutcode tutcode-bushu)
  '(pathname regular-file)
  (N_ "bushu.expand file")
  (N_ "long description will be here."))

(define-custom 'tutcode-bushu-help-filename ""
  '(tutcode tutcode-bushu)
  '(pathname regular-file)
  (N_ "bushu.help file")
  (N_ "long description will be here."))

;;
;; candidate window
;;

(define-custom 'tutcode-use-candidate-window? #t
  '(tutcode candwin)
  '(boolean)
  (N_ "Use candidate window")
  (N_ "long description will be here."))

(define-custom 'tutcode-use-pseudo-table-style? #f
  '(tutcode candwin)
  '(boolean)
  (N_ "Use pseudo table style layout")
  (N_ "long description will be here."))

(define-custom 'tutcode-candidate-window-table-layout 'qwerty-jis
  '(tutcode candwin)
  (list 'choice
	(list 'qwerty-jis (N_ "qwerty-jis") (N_ "Qwerty JIS"))
	(list 'qwerty-us (N_ "qwerty-us") (N_ "Qwerty US"))
	(list 'dvorak (N_ "dvorak") (N_ "Dvorak")))
  (N_ "Key layout of table style candidate window")
  (N_ "long description will be here."))

(define-custom 'tutcode-commit-candidate-by-label-key 'always
  '(tutcode candwin)
  (list 'choice
	(list 'always (N_ "always") (N_ "All keys as label key"))
	(list 'havecand (N_ "which have candidate")
          (N_ "Enable keys which have candidate"))
	(list 'candwin (N_ "while candidate window is shown")
          (N_ "Enable while candidate window is shown"))
	(list 'never (N_ "never") (N_ "Never")))
  (N_ "Commit candidate by heading label keys")
  (N_ "long description will be here."))

(define-custom 'tutcode-candidate-op-count 5
  '(tutcode candwin)
  '(integer 0 99)
  (N_ "Conversion key press count to show candidate window")
  (N_ "long description will be here."))

(define-custom 'tutcode-nr-candidate-max 10
  '(tutcode candwin)
  '(integer 1 99)
  (N_ "Number of candidates in candidate window at a time")
  (N_ "long description will be here."))

(define-custom 'tutcode-nr-candidate-max-for-kigou-mode 10
  '(tutcode candwin)
  '(integer 1 99)
  (N_ "Number of candidates in candidate window at a time for kigou mode")
  (N_ "long description will be here."))

(define-custom 'tutcode-nr-candidate-max-for-prediction 10
  '(tutcode candwin)
  '(integer 1 99)
  (N_ "Number of candidates in candidate window at a time for prediction")
  (N_ "long description will be here."))

(define-custom 'tutcode-nr-candidate-max-for-guide 10
  '(tutcode candwin)
  '(integer 1 99)
  (N_ "Number of candidates in candidate window at a time for kanji combination guide")
  (N_ "long description will be here."))

(define-custom 'tutcode-nr-candidate-max-for-history 10
  '(tutcode candwin)
  '(integer 1 99)
  (N_ "Number of candidates in candidate window at a time for history")
  (N_ "long description will be here."))

(define-custom 'tutcode-use-stroke-help-window? #f
  '(tutcode candwin)
  '(boolean)
  (N_ "Use stroke help window")
  (N_ "long description will be here."))

(define-custom 'tutcode-show-stroke-help-window-on-no-input? #t
  '(tutcode candwin)
  '(boolean)
  (N_ "Show stroke help window on no input")
  (N_ "long description will be here."))

(define-custom 'tutcode-use-auto-help-window? #f
  '(tutcode candwin)
  '(boolean)
  (N_ "Use auto help window")
  (N_ "long description will be here."))

(define-custom 'tutcode-auto-help-with-real-keys? #f
  '(tutcode candwin)
  '(boolean)
  (N_ "Show real keys on auto help window")
  (N_ "long description will be here."))

(define-custom 'tutcode-candidate-window-use-delay? #f
  '(tutcode candwin)
  '(boolean)
  (N_ "Use delay showing candidate window")
  (N_ "long description will be here."))

(define-custom 'tutcode-candidate-window-activate-delay-for-mazegaki 0
  '(tutcode candwin)
  '(integer 0 65535)
  (N_ "Delay before showing candidate window for mazegaki [s]")
  (N_ "long description will be here."))

(define-custom 'tutcode-candidate-window-activate-delay-for-stroke-help 2
  '(tutcode candwin)
  '(integer 0 65535)
  (N_ "Delay before showing candidate window for stroke help [s]")
  (N_ "long description will be here."))

(define-custom 'tutcode-candidate-window-activate-delay-for-auto-help 1
  '(tutcode candwin)
  '(integer 0 65535)
  (N_ "Delay before showing candidate window for auto help [s]")
  (N_ "long description will be here."))

(define-custom 'tutcode-candidate-window-activate-delay-for-completion 2
  '(tutcode candwin)
  '(integer 0 65535)
  (N_ "Delay before showing candidate window for completion [s]")
  (N_ "long description will be here."))

(define-custom 'tutcode-candidate-window-activate-delay-for-prediction 2
  '(tutcode candwin)
  '(integer 0 65535)
  (N_ "Delay before showing candidate window for prediction [s]")
  (N_ "long description will be here."))

(define-custom 'tutcode-candidate-window-activate-delay-for-bushu-prediction 2
  '(tutcode candwin)
  '(integer 0 65535)
  (N_ "Delay before showing candidate window for bushu prediction [s]")
  (N_ "long description will be here."))

(define-custom 'tutcode-candidate-window-activate-delay-for-interactive-bushu 1
  '(tutcode candwin)
  '(integer 0 65535)
  (N_ "Delay before showing candidate window for interactive bushu conversion [s]")
  (N_ "long description will be here."))

;; prediction/completion
(define-custom 'tutcode-use-completion? #f
  '(tutcode tutcode-prediction)
  '(boolean)
  (N_ "Enable completion")
  (N_ "long description will be here."))

(define-custom 'tutcode-completion-chars-min 2
  '(tutcode tutcode-prediction)
  '(integer 0 65535)
  (N_ "Minimum character length for completion")
  (N_ "long description will be here."))

(define-custom 'tutcode-completion-chars-max 5
  '(tutcode tutcode-prediction)
  '(integer 1 65535)
  (N_ "Maximum character length for completion")
  (N_ "long description will be here."))

(define-custom 'tutcode-use-prediction? #f
  '(tutcode tutcode-prediction)
  '(boolean)
  (N_ "Enable input prediction for mazegaki conversion")
  (N_ "long description will be here."))

(define-custom 'tutcode-prediction-start-char-count 2
  '(tutcode tutcode-prediction)
  '(integer 0 65535)
  (N_ "Character count to start input prediction")
  (N_ "long description will be here."))

(define-custom 'tutcode-use-kanji-combination-guide? #f
  '(tutcode tutcode-prediction)
  '(boolean)
  (N_ "Enable Kanji combination guide")
  (N_ "long description will be here."))

(define-custom 'tutcode-stroke-help-with-kanji-combination-guide 'disable
  '(tutcode tutcode-prediction)
  (list 'choice
    (list 'full (N_ "Full stroke help") (N_ "Full stroke help"))
    (list 'guide-only (N_ "Guide only") (N_ "Guide only"))
    (list 'disable (N_ "Disable") (N_ "Disable")))
  (N_ "Show stroke help temporarily by keys in kanji combination guide")
  (N_ "long description will be here."))

(define-custom 'tutcode-use-bushu-prediction? #f
  '(tutcode tutcode-prediction)
  '(boolean)
  (N_ "Enable input prediction for bushu conversion")
  (N_ "long description will be here."))

;; activity dependency
(custom-add-hook 'tutcode-mazegaki-fixed-priority-count
                 'custom-activity-hooks
                 (lambda ()
                   tutcode-enable-mazegaki-learning?))

(custom-add-hook 'tutcode-candidate-op-count
		 'custom-activity-hooks
		 (lambda ()
		   tutcode-use-candidate-window?))

(custom-add-hook 'tutcode-nr-candidate-max
		 'custom-activity-hooks
		 (lambda ()
		   tutcode-use-candidate-window?))

(custom-add-hook 'tutcode-nr-candidate-max-for-kigou-mode
		 'custom-activity-hooks
		 (lambda ()
		   tutcode-use-candidate-window?))

(custom-add-hook 'tutcode-nr-candidate-max-for-history
		 'custom-activity-hooks
		 (lambda ()
		   tutcode-use-candidate-window?))

(custom-add-hook 'tutcode-auto-help-with-real-keys?
		 'custom-activity-hooks
		 (lambda ()
		   tutcode-use-auto-help-window?))

(define (tutcode-custom-adjust-nr-candidate-max)
  (if (or (eq? candidate-window-style 'table) tutcode-use-pseudo-table-style?)
    (begin
      (custom-set-value! 'tutcode-nr-candidate-max
        (length tutcode-table-heading-label-char-list))
      (custom-set-value!
        'tutcode-nr-candidate-max-for-kigou-mode
        (length tutcode-table-heading-label-char-list-for-kigou-mode))
      (custom-set-value!
        'tutcode-nr-candidate-max-for-prediction
        (length tutcode-heading-label-char-list-for-prediction))
      (custom-set-value!
        'tutcode-nr-candidate-max-for-guide
        (- (length tutcode-table-heading-label-char-list-for-kigou-mode)
           (length tutcode-heading-label-char-list-for-prediction)))
      (custom-set-value!
        'tutcode-nr-candidate-max-for-history
        (length tutcode-table-heading-label-char-list)))
    (begin
      (custom-set-value! 'tutcode-nr-candidate-max 10)
      (custom-set-value! 'tutcode-nr-candidate-max-for-kigou-mode 10)
      (custom-set-value! 'tutcode-nr-candidate-max-for-prediction 10)
      (custom-set-value! 'tutcode-nr-candidate-max-for-guide 10)
      (custom-set-value! 'tutcode-nr-candidate-max-for-history 10))))

(custom-add-hook 'candidate-window-style
  'custom-set-hooks
  tutcode-custom-adjust-nr-candidate-max)

(custom-add-hook 'tutcode-use-pseudo-table-style?
  'custom-set-hooks
  tutcode-custom-adjust-nr-candidate-max)

(custom-add-hook 'tutcode-candidate-window-table-layout
		 'custom-activity-hooks
		 (lambda ()
                   (eq? candidate-window-style 'table)))

(custom-add-hook 'tutcode-bushu-index2-filename
		 'custom-activity-hooks
		 (lambda ()
                   (or
                     tutcode-use-interactive-bushu-conversion?
                     (eq? tutcode-bushu-conversion-algorithm 'tc-2.3.1-22.6))))

(custom-add-hook 'tutcode-bushu-expand-filename
		 'custom-activity-hooks
		 (lambda ()
                   (or
                     tutcode-use-interactive-bushu-conversion?
                     (eq? tutcode-bushu-conversion-algorithm 'tc-2.3.1-22.6))))
