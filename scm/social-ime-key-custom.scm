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

(define-custom-group 'social-ime-keys1
		     (N_ "Social-IME key bindings 1")
		     (N_ "long description will be here."))

(define-custom-group 'social-ime-keys2
		     (N_ "Social-IME key bindings 2")
		     (N_ "long description will be here."))

(define-custom-group 'social-ime-keys3
		     (N_ "Social-IME key bindings 3")
		     (N_ "long description will be here."))

(define-custom-group 'social-ime-keys4
		     (N_ "Social-IME key bindings 4")
		     (N_ "long description will be here."))

(define-custom 'social-ime-next-segment-key '(generic-go-right-key)
               '(social-ime-keys1)
	       '(key)
	       (N_ "[Social-IME] next segment")
	       (N_ "long description will be here"))

(define-custom 'social-ime-prev-segment-key '(generic-go-left-key)
               '(social-ime-keys1)
	       '(key)
	       (N_ "[Social-IME] previous segment")
	       (N_ "long description will be here"))

(define-custom 'social-ime-extend-segment-key '("<IgnoreCase><Control>o" "<Shift>right")
               '(social-ime-keys1)
	       '(key)
	       (N_ "[Social-IME] extend segment")
	       (N_ "long description will be here"))

(define-custom 'social-ime-shrink-segment-key '("<IgnoreCase><Control>i" "<Shift>left")
               '(social-ime-keys1)
	       '(key)
	       (N_ "[Social-IME] shrink segment")
	       (N_ "long description will be here"))

(define-custom 'social-ime-transpose-as-hiragana-key '("F6" "Muhenkan")
               '(social-ime-keys1)
	       '(key)
	       (N_ "[Social-IME] convert to hiragana")
	       (N_ "long description will be here"))

(define-custom 'social-ime-transpose-as-katakana-key '("F7" "Muhenkan")
               '(social-ime-keys1)
	       '(key)
	       (N_ "[Social-IME] convert to katakana")
	       (N_ "long description will be here"))

(define-custom 'social-ime-transpose-as-halfkana-key '("F8" "Muhenkan")
               '(social-ime-keys1)
	       '(key)
	       (N_ "[Social-IME] convert to halfwidth katakana")
	       (N_ "long description will be here"))

(define-custom 'social-ime-transpose-as-halfwidth-alnum-key '("F10")
	       '(social-ime-keys1)
	       '(key)
	       (N_ "[Social-IME] convert to halfwidth alphanumeric")
	       (N_ "long description will be here"))

(define-custom 'social-ime-transpose-as-fullwidth-alnum-key '("F9")
	       '(social-ime-keys1)
	       '(key)
	       (N_ "[Social-IME] convert to fullwidth alphanumeric")
	       (N_ "long description will be here"))

(define-custom 'social-ime-commit-as-opposite-kana-key '("<IgnoreCase><Shift>q")  ;; "Q"
               '(social-ime-keys1)
	       '(key)
	       (N_ "[Social-IME] commit as transposed kana")
	       (N_ "long description will be here"))

;;
;; overriding generic keys
;;
(define-custom 'social-ime-on-key '("<Control>\\" generic-on-key)
               '(social-ime-keys2)
	       '(key)
	       (N_ "[Social-IME] on")
	       (N_ "long description will be here"))

;;(define-custom 'social-ime-off-key '("l" generic-on-key)
(define-custom 'social-ime-off-key '("<Control>\\" generic-off-key)
               '(social-ime-keys2)
	       '(key)
	       (N_ "[Social-IME] off")
	       (N_ "long description will be here"))

(define-custom 'social-ime-begin-conv-key '(generic-begin-conv-key)
               '(social-ime-keys2)
	       '(key)
	       (N_ "[Social-IME] begin conversion")
	       (N_ "long description will be here"))

(define-custom 'social-ime-commit-key '(generic-commit-key)
               '(social-ime-keys2)
	       '(key)
	       (N_ "[Social-IME] commit")
	       (N_ "long description will be here"))

(define-custom 'social-ime-cancel-key '(generic-cancel-key)
               '(social-ime-keys2)
	       '(key)
	       (N_ "[Social-IME] cancel")
	       (N_ "long description will be here"))

(define-custom 'social-ime-next-candidate-key '(generic-next-candidate-key)
               '(social-ime-keys2)
	       '(key)
	       (N_ "[Social-IME] next candidate")
	       (N_ "long description will be here"))

(define-custom 'social-ime-prev-candidate-key '(generic-prev-candidate-key)
               '(social-ime-keys2)
	       '(key)
	       (N_ "[Social-IME] previous candidate")
	       (N_ "long description will be here"))

(define-custom 'social-ime-next-page-key '(generic-next-page-key)
             '(social-ime-keys2)
	       '(key)
	       (N_ "[Social-IME] next page of candidate window")
	       (N_ "long description will be here"))

(define-custom 'social-ime-prev-page-key '(generic-prev-page-key)
               '(social-ime-keys2)
	       '(key)
	       (N_ "[Social-IME] previous page of candidate window")
	       (N_ "long description will be here"))

;;
;; overriding generic keys (advanced)
;;
(define-custom 'social-ime-beginning-of-preedit-key '(generic-beginning-of-preedit-key)
               '(social-ime-keys3)
	       '(key)
	       (N_ "[Social-IME] beginning of preedit")
	       (N_ "long description will be here"))

(define-custom 'social-ime-end-of-preedit-key '(generic-end-of-preedit-key)
               '(social-ime-keys3)
	       '(key)
	       (N_ "[Social-IME] end of preedit")
	       (N_ "long description will be here"))

(define-custom 'social-ime-kill-key '(generic-kill-key)
               '(social-ime-keys3)
	       '(key)
	       (N_ "[Social-IME] erase after cursor")
	       (N_ "long description will be here"))

(define-custom 'social-ime-kill-backward-key '(generic-kill-backward-key)
               '(social-ime-keys3)
	       '(key)
	       (N_ "[Social-IME] erase before cursor")
	       (N_ "long description will be here"))

(define-custom 'social-ime-backspace-key '(generic-backspace-key)
               '(social-ime-keys3)
	       '(key)
	       (N_ "[Social-IME] backspace")
	       (N_ "long description will be here"))

(define-custom 'social-ime-delete-key '(generic-delete-key)
               '(social-ime-keys3)
	       '(key)
	       (N_ "[Social-IME] delete")
	       (N_ "long description will be here"))

(define-custom 'social-ime-go-left-key '(generic-go-left-key)
               '(social-ime-keys3)
	       '(key)
	       (N_ "[Social-IME] go left")
	       (N_ "long description will be here"))

(define-custom 'social-ime-go-right-key '(generic-go-right-key)
               '(social-ime-keys3)
	       '(key)
	       (N_ "[Social-IME] go right")
	       (N_ "long description will be here"))

(define-custom 'social-ime-vi-escape-key '("escape" "<Control>[")
               '(social-ime-keys3)
	       '(key)
	       (N_ "[Social-IME] ESC keys on vi-cooperative mode")
	       (N_ "long description will be here"))

;;
;; ja advanced
;;

(define-custom 'social-ime-hiragana-key '("<Shift>F6")
	       '(social-ime-keys4 mode-transition)
	       '(key)
	       (N_ "[Social-IME] hiragana mode")
	       (N_ "long description will be here"))

(define-custom 'social-ime-katakana-key '("<Shift>F7")
	       '(social-ime-keys4 mode-transition)
	       '(key)
	       (N_ "[Social-IME] katakana mode")
	       (N_ "long description will be here"))

(define-custom 'social-ime-halfkana-key '("<Shift>F8")
	       '(social-ime-keys4 mode-transition)
	       '(key)
	       (N_ "[Social-IME] halfwidth katakana mode")
	       (N_ "long description will be here"))

(define-custom 'social-ime-halfwidth-alnum-key '("<Shift>F10")
	       '(social-ime-keys4 mode-transition)
	       '(key)
	       (N_ "[Social-IME] halfwidth alphanumeric mode")
	       (N_ "long description will be here"))

(define-custom 'social-ime-fullwidth-alnum-key '("<Shift>F9")
	       '(social-ime-keys4 mode-transition)
	       '(key)
	       (N_ "[Social-IME] fullwidth alphanumeric mode")
	       (N_ "long description will be here"))

(define-custom 'social-ime-kana-toggle-key '()
               '(social-ime-keys4 advanced)
	       '(key)
	       (N_ "[Social-IME] toggle hiragana/katakana mode")
	       (N_ "long description will be here"))

(define-custom 'social-ime-alkana-toggle-key '()
	       '(social-ime-keys4 advanced)
	       '(key)
	       (N_ "[Social-IME] toggle kana/alphanumeric mode")
	       (N_ "long description will be here"))

(define-custom 'social-ime-next-prediction-key '("tab" "down" "<IgnoreCase><Control>n" "<IgnoreCase><Control>i")
               '(social-ime-keys4 social-ime-prediction)
               '(key)
               (N_ "[Social-IME] Next prediction candidate")
               (N_ "long description will be here"))

(define-custom 'social-ime-prev-prediction-key '(generic-prev-candidate-key)
               '(social-ime-keys4 social-ime-prediction)
               '(key)
               (N_ "[Social-IME] Previous prediction candidate")
               (N_ "long description will be here"))
