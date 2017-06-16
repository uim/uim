;;; google-cgiapi-jp-custom.scm: Customization variables for google-cgiapi-jp.scm
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

(define-custom-group 'google-cgiapi-jp-keys1
		     (N_ "Google-CGIAPI-Jp key bindings 1")
		     (N_ "long description will be here."))

(define-custom-group 'google-cgiapi-jp-keys2
		     (N_ "Google-CGIAPI-Jp key bindings 2")
		     (N_ "long description will be here."))

(define-custom-group 'google-cgiapi-jp-keys3
		     (N_ "Google-CGIAPI-Jp key bindings 3")
		     (N_ "long description will be here."))

(define-custom-group 'google-cgiapi-jp-keys4
		     (N_ "Google-CGIAPI-Jp key bindings 4")
		     (N_ "long description will be here."))

(define-custom 'google-cgiapi-jp-next-segment-key '(generic-go-right-key)
               '(google-cgiapi-jp-keys1)
	       '(key)
	       (N_ "[Google-CGIAPI-Jp] next segment")
	       (N_ "long description will be here"))

(define-custom 'google-cgiapi-jp-prev-segment-key '(generic-go-left-key)
               '(google-cgiapi-jp-keys1)
	       '(key)
	       (N_ "[Google-CGIAPI-Jp] previous segment")
	       (N_ "long description will be here"))

(define-custom 'google-cgiapi-jp-extend-segment-key '("<IgnoreCase><Control>o" "<Shift>right")
               '(google-cgiapi-jp-keys1)
	       '(key)
	       (N_ "[Google-CGIAPI-Jp] extend segment")
	       (N_ "long description will be here"))

(define-custom 'google-cgiapi-jp-shrink-segment-key '("<IgnoreCase><Control>i" "<Shift>left")
               '(google-cgiapi-jp-keys1)
	       '(key)
	       (N_ "[Google-CGIAPI-Jp] shrink segment")
	       (N_ "long description will be here"))

(define-custom 'google-cgiapi-jp-transpose-as-hiragana-key '("F6" "Muhenkan")
               '(google-cgiapi-jp-keys1)
	       '(key)
	       (N_ "[Google-CGIAPI-Jp] convert to hiragana")
	       (N_ "long description will be here"))

(define-custom 'google-cgiapi-jp-transpose-as-katakana-key '("F7" "Muhenkan")
               '(google-cgiapi-jp-keys1)
	       '(key)
	       (N_ "[Google-CGIAPI-Jp] convert to katakana")
	       (N_ "long description will be here"))

(define-custom 'google-cgiapi-jp-transpose-as-halfkana-key '("F8" "Muhenkan")
               '(google-cgiapi-jp-keys1)
	       '(key)
	       (N_ "[Google-CGIAPI-Jp] convert to halfwidth katakana")
	       (N_ "long description will be here"))

(define-custom 'google-cgiapi-jp-transpose-as-halfwidth-alnum-key '("F10")
	       '(google-cgiapi-jp-keys1)
	       '(key)
	       (N_ "[Google-CGIAPI-Jp] convert to halfwidth alphanumeric")
	       (N_ "long description will be here"))

(define-custom 'google-cgiapi-jp-transpose-as-fullwidth-alnum-key '("F9")
	       '(google-cgiapi-jp-keys1)
	       '(key)
	       (N_ "[Google-CGIAPI-Jp] convert to fullwidth alphanumeric")
	       (N_ "long description will be here"))

(define-custom 'google-cgiapi-jp-commit-as-opposite-kana-key '("<IgnoreCase><Shift>q")  ;; "Q"
               '(google-cgiapi-jp-keys1)
	       '(key)
	       (N_ "[Google-CGIAPI-Jp] commit as transposed kana")
	       (N_ "long description will be here"))

;;
;; overriding generic keys
;;
(define-custom 'google-cgiapi-jp-on-key '("<Control>\\" generic-on-key)
               '(google-cgiapi-jp-keys2)
	       '(key)
	       (N_ "[Google-CGIAPI-Jp] on")
	       (N_ "long description will be here"))

;;(define-custom 'google-cgiapi-jp-off-key '("l" generic-on-key)
(define-custom 'google-cgiapi-jp-off-key '("<Control>\\" generic-off-key)
               '(google-cgiapi-jp-keys2)
	       '(key)
	       (N_ "[Google-CGIAPI-Jp] off")
	       (N_ "long description will be here"))

(define-custom 'google-cgiapi-jp-begin-conv-key '(generic-begin-conv-key)
               '(google-cgiapi-jp-keys2)
	       '(key)
	       (N_ "[Google-CGIAPI-Jp] begin conversion")
	       (N_ "long description will be here"))

(define-custom 'google-cgiapi-jp-commit-key '(generic-commit-key)
               '(google-cgiapi-jp-keys2)
	       '(key)
	       (N_ "[Google-CGIAPI-Jp] commit")
	       (N_ "long description will be here"))

(define-custom 'google-cgiapi-jp-cancel-key '(generic-cancel-key)
               '(google-cgiapi-jp-keys2)
	       '(key)
	       (N_ "[Google-CGIAPI-Jp] cancel")
	       (N_ "long description will be here"))

(define-custom 'google-cgiapi-jp-next-candidate-key '(generic-next-candidate-key)
               '(google-cgiapi-jp-keys2)
	       '(key)
	       (N_ "[Google-CGIAPI-Jp] next candidate")
	       (N_ "long description will be here"))

(define-custom 'google-cgiapi-jp-prev-candidate-key '(generic-prev-candidate-key)
               '(google-cgiapi-jp-keys2)
	       '(key)
	       (N_ "[Google-CGIAPI-Jp] previous candidate")
	       (N_ "long description will be here"))

(define-custom 'google-cgiapi-jp-next-page-key '(generic-next-page-key)
             '(google-cgiapi-jp-keys2)
	       '(key)
	       (N_ "[Google-CGIAPI-Jp] next page of candidate window")
	       (N_ "long description will be here"))

(define-custom 'google-cgiapi-jp-prev-page-key '(generic-prev-page-key)
               '(google-cgiapi-jp-keys2)
	       '(key)
	       (N_ "[Google-CGIAPI-Jp] previous page of candidate window")
	       (N_ "long description will be here"))

;;
;; overriding generic keys (advanced)
;;
(define-custom 'google-cgiapi-jp-beginning-of-preedit-key '(generic-beginning-of-preedit-key)
               '(google-cgiapi-jp-keys3)
	       '(key)
	       (N_ "[Google-CGIAPI-Jp] beginning of preedit")
	       (N_ "long description will be here"))

(define-custom 'google-cgiapi-jp-end-of-preedit-key '(generic-end-of-preedit-key)
               '(google-cgiapi-jp-keys3)
	       '(key)
	       (N_ "[Google-CGIAPI-Jp] end of preedit")
	       (N_ "long description will be here"))

(define-custom 'google-cgiapi-jp-kill-key '(generic-kill-key)
               '(google-cgiapi-jp-keys3)
	       '(key)
	       (N_ "[Google-CGIAPI-Jp] erase after cursor")
	       (N_ "long description will be here"))

(define-custom 'google-cgiapi-jp-kill-backward-key '(generic-kill-backward-key)
               '(google-cgiapi-jp-keys3)
	       '(key)
	       (N_ "[Google-CGIAPI-Jp] erase before cursor")
	       (N_ "long description will be here"))

(define-custom 'google-cgiapi-jp-backspace-key '(generic-backspace-key)
               '(google-cgiapi-jp-keys3)
	       '(key)
	       (N_ "[Google-CGIAPI-Jp] backspace")
	       (N_ "long description will be here"))

(define-custom 'google-cgiapi-jp-delete-key '(generic-delete-key)
               '(google-cgiapi-jp-keys3)
	       '(key)
	       (N_ "[Google-CGIAPI-Jp] delete")
	       (N_ "long description will be here"))

(define-custom 'google-cgiapi-jp-go-left-key '(generic-go-left-key)
               '(google-cgiapi-jp-keys3)
	       '(key)
	       (N_ "[Google-CGIAPI-Jp] go left")
	       (N_ "long description will be here"))

(define-custom 'google-cgiapi-jp-go-right-key '(generic-go-right-key)
               '(google-cgiapi-jp-keys3)
	       '(key)
	       (N_ "[Google-CGIAPI-Jp] go right")
	       (N_ "long description will be here"))

(define-custom 'google-cgiapi-jp-vi-escape-key '("escape" "<Control>[")
               '(google-cgiapi-jp-keys3)
	       '(key)
	       (N_ "[Google-CGIAPI-Jp] ESC keys on vi-cooperative mode")
	       (N_ "long description will be here"))

;;
;; ja advanced
;;

(define-custom 'google-cgiapi-jp-hiragana-key '("<Shift>F6")
	       '(google-cgiapi-jp-keys4 mode-transition)
	       '(key)
	       (N_ "[Google-CGIAPI-Jp] hiragana mode")
	       (N_ "long description will be here"))

(define-custom 'google-cgiapi-jp-katakana-key '("<Shift>F7")
	       '(google-cgiapi-jp-keys4 mode-transition)
	       '(key)
	       (N_ "[Google-CGIAPI-Jp] katakana mode")
	       (N_ "long description will be here"))

(define-custom 'google-cgiapi-jp-halfkana-key '("<Shift>F8")
	       '(google-cgiapi-jp-keys4 mode-transition)
	       '(key)
	       (N_ "[Google-CGIAPI-Jp] halfwidth katakana mode")
	       (N_ "long description will be here"))

(define-custom 'google-cgiapi-jp-halfwidth-alnum-key '("<Shift>F10")
	       '(google-cgiapi-jp-keys4 mode-transition)
	       '(key)
	       (N_ "[Google-CGIAPI-Jp] halfwidth alphanumeric mode")
	       (N_ "long description will be here"))

(define-custom 'google-cgiapi-jp-fullwidth-alnum-key '("<Shift>F9")
	       '(google-cgiapi-jp-keys4 mode-transition)
	       '(key)
	       (N_ "[Google-CGIAPI-Jp] fullwidth alphanumeric mode")
	       (N_ "long description will be here"))

(define-custom 'google-cgiapi-jp-kana-toggle-key '()
               '(google-cgiapi-jp-keys4 advanced)
	       '(key)
	       (N_ "[Google-CGIAPI-Jp] toggle hiragana/katakana mode")
	       (N_ "long description will be here"))

(define-custom 'google-cgiapi-jp-alkana-toggle-key '()
	       '(google-cgiapi-jp-keys4 advanced)
	       '(key)
	       (N_ "[Google-CGIAPI-Jp] toggle kana/alphanumeric mode")
	       (N_ "long description will be here"))

(define-custom 'google-cgiapi-jp-next-prediction-key '("tab" "down" "<IgnoreCase><Control>n" "<IgnoreCase><Control>i")
               '(google-cgiapi-jp-keys4 google-cgiapi-jp-prediction)
               '(key)
               (N_ "[Google-CGIAPI-Jp] Next prediction candidate")
               (N_ "long description will be here"))

(define-custom 'google-cgiapi-jp-prev-prediction-key '(generic-prev-candidate-key)
               '(google-cgiapi-jp-keys4 google-cgiapi-jp-prediction)
               '(key)
               (N_ "[Google-CGIAPI-Jp] Previous prediction candidate")
               (N_ "long description will be here"))
