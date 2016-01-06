;;; ajax-ime-custom.scm: Customization variables for ajax-ime.scm
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

(define-custom-group 'ajax-ime-keys1
		     (N_ "Ajax-IME key bindings 1")
		     (N_ "long description will be here."))

(define-custom-group 'ajax-ime-keys2
		     (N_ "Ajax-IME key bindings 2")
		     (N_ "long description will be here."))

(define-custom-group 'ajax-ime-keys3
		     (N_ "Ajax-IME key bindings 3")
		     (N_ "long description will be here."))

(define-custom-group 'ajax-ime-keys4
		     (N_ "Ajax-IME key bindings 4")
		     (N_ "long description will be here."))

(define-custom 'ajax-ime-next-segment-key '(generic-go-right-key)
               '(ajax-ime-keys1)
	       '(key)
	       (N_ "[Ajax-IME] next segment")
	       (N_ "long description will be here"))

(define-custom 'ajax-ime-prev-segment-key '(generic-go-left-key)
               '(ajax-ime-keys1)
	       '(key)
	       (N_ "[Ajax-IME] previous segment")
	       (N_ "long description will be here"))

(define-custom 'ajax-ime-extend-segment-key '("<IgnoreCase><Control>o" "<Shift>right")
               '(ajax-ime-keys1)
	       '(key)
	       (N_ "[Ajax-IME] extend segment")
	       (N_ "long description will be here"))

(define-custom 'ajax-ime-shrink-segment-key '("<IgnoreCase><Control>i" "<Shift>left")
               '(ajax-ime-keys1)
	       '(key)
	       (N_ "[Ajax-IME] shrink segment")
	       (N_ "long description will be here"))

(define-custom 'ajax-ime-transpose-as-hiragana-key '("F6" "Muhenkan")
               '(ajax-ime-keys1)
	       '(key)
	       (N_ "[Ajax-IME] convert to hiragana")
	       (N_ "long description will be here"))

(define-custom 'ajax-ime-transpose-as-katakana-key '("F7" "Muhenkan")
               '(ajax-ime-keys1)
	       '(key)
	       (N_ "[Ajax-IME] convert to katakana")
	       (N_ "long description will be here"))

(define-custom 'ajax-ime-transpose-as-halfkana-key '("F8" "Muhenkan")
               '(ajax-ime-keys1)
	       '(key)
	       (N_ "[Ajax-IME] convert to halfwidth katakana")
	       (N_ "long description will be here"))

(define-custom 'ajax-ime-transpose-as-halfwidth-alnum-key '("F10")
	       '(ajax-ime-keys1)
	       '(key)
	       (N_ "[Ajax-IME] convert to halfwidth alphanumeric")
	       (N_ "long description will be here"))

(define-custom 'ajax-ime-transpose-as-fullwidth-alnum-key '("F9")
	       '(ajax-ime-keys1)
	       '(key)
	       (N_ "[Ajax-IME] convert to fullwidth alphanumeric")
	       (N_ "long description will be here"))

(define-custom 'ajax-ime-commit-as-opposite-kana-key '("<IgnoreCase><Shift>q")  ;; "Q"
               '(ajax-ime-keys1)
	       '(key)
	       (N_ "[Ajax-IME] commit as transposed kana")
	       (N_ "long description will be here"))

;;
;; overriding generic keys
;;
(define-custom 'ajax-ime-on-key '("<Control>\\" generic-on-key)
               '(ajax-ime-keys2)
	       '(key)
	       (N_ "[Ajax-IME] on")
	       (N_ "long description will be here"))

;;(define-custom 'ajax-ime-off-key '("l" generic-on-key)
(define-custom 'ajax-ime-off-key '("<Control>\\" generic-off-key)
               '(ajax-ime-keys2)
	       '(key)
	       (N_ "[Ajax-IME] off")
	       (N_ "long description will be here"))

(define-custom 'ajax-ime-begin-conv-key '(generic-begin-conv-key)
               '(ajax-ime-keys2)
	       '(key)
	       (N_ "[Ajax-IME] begin conversion")
	       (N_ "long description will be here"))

(define-custom 'ajax-ime-commit-key '(generic-commit-key)
               '(ajax-ime-keys2)
	       '(key)
	       (N_ "[Ajax-IME] commit")
	       (N_ "long description will be here"))

(define-custom 'ajax-ime-cancel-key '(generic-cancel-key)
               '(ajax-ime-keys2)
	       '(key)
	       (N_ "[Ajax-IME] cancel")
	       (N_ "long description will be here"))

(define-custom 'ajax-ime-next-candidate-key '(generic-next-candidate-key)
               '(ajax-ime-keys2)
	       '(key)
	       (N_ "[Ajax-IME] next candidate")
	       (N_ "long description will be here"))

(define-custom 'ajax-ime-prev-candidate-key '(generic-prev-candidate-key)
               '(ajax-ime-keys2)
	       '(key)
	       (N_ "[Ajax-IME] previous candidate")
	       (N_ "long description will be here"))

(define-custom 'ajax-ime-next-page-key '(generic-next-page-key)
             '(ajax-ime-keys2)
	       '(key)
	       (N_ "[Ajax-IME] next page of candidate window")
	       (N_ "long description will be here"))

(define-custom 'ajax-ime-prev-page-key '(generic-prev-page-key)
               '(ajax-ime-keys2)
	       '(key)
	       (N_ "[Ajax-IME] previous page of candidate window")
	       (N_ "long description will be here"))

;;
;; overriding generic keys (advanced)
;;
(define-custom 'ajax-ime-beginning-of-preedit-key '(generic-beginning-of-preedit-key)
               '(ajax-ime-keys3)
	       '(key)
	       (N_ "[Ajax-IME] beginning of preedit")
	       (N_ "long description will be here"))

(define-custom 'ajax-ime-end-of-preedit-key '(generic-end-of-preedit-key)
               '(ajax-ime-keys3)
	       '(key)
	       (N_ "[Ajax-IME] end of preedit")
	       (N_ "long description will be here"))

(define-custom 'ajax-ime-kill-key '(generic-kill-key)
               '(ajax-ime-keys3)
	       '(key)
	       (N_ "[Ajax-IME] erase after cursor")
	       (N_ "long description will be here"))

(define-custom 'ajax-ime-kill-backward-key '(generic-kill-backward-key)
               '(ajax-ime-keys3)
	       '(key)
	       (N_ "[Ajax-IME] erase before cursor")
	       (N_ "long description will be here"))

(define-custom 'ajax-ime-backspace-key '(generic-backspace-key)
               '(ajax-ime-keys3)
	       '(key)
	       (N_ "[Ajax-IME] backspace")
	       (N_ "long description will be here"))

(define-custom 'ajax-ime-delete-key '(generic-delete-key)
               '(ajax-ime-keys3)
	       '(key)
	       (N_ "[Ajax-IME] delete")
	       (N_ "long description will be here"))

(define-custom 'ajax-ime-go-left-key '(generic-go-left-key)
               '(ajax-ime-keys3)
	       '(key)
	       (N_ "[Ajax-IME] go left")
	       (N_ "long description will be here"))

(define-custom 'ajax-ime-go-right-key '(generic-go-right-key)
               '(ajax-ime-keys3)
	       '(key)
	       (N_ "[Ajax-IME] go right")
	       (N_ "long description will be here"))

(define-custom 'ajax-ime-vi-escape-key '("escape" "<Control>[")
               '(ajax-ime-keys3)
	       '(key)
	       (N_ "[Ajax-IME] ESC keys on vi-cooperative mode")
	       (N_ "long description will be here"))

;;
;; ja advanced
;;

(define-custom 'ajax-ime-hiragana-key '("<Shift>F6")
	       '(ajax-ime-keys4 mode-transition)
	       '(key)
	       (N_ "[Ajax-IME] hiragana mode")
	       (N_ "long description will be here"))

(define-custom 'ajax-ime-katakana-key '("<Shift>F7")
	       '(ajax-ime-keys4 mode-transition)
	       '(key)
	       (N_ "[Ajax-IME] katakana mode")
	       (N_ "long description will be here"))

(define-custom 'ajax-ime-halfkana-key '("<Shift>F8")
	       '(ajax-ime-keys4 mode-transition)
	       '(key)
	       (N_ "[Ajax-IME] halfwidth katakana mode")
	       (N_ "long description will be here"))

(define-custom 'ajax-ime-halfwidth-alnum-key '("<Shift>F10")
	       '(ajax-ime-keys4 mode-transition)
	       '(key)
	       (N_ "[Ajax-IME] halfwidth alphanumeric mode")
	       (N_ "long description will be here"))

(define-custom 'ajax-ime-fullwidth-alnum-key '("<Shift>F9")
	       '(ajax-ime-keys4 mode-transition)
	       '(key)
	       (N_ "[Ajax-IME] fullwidth alphanumeric mode")
	       (N_ "long description will be here"))

(define-custom 'ajax-ime-kana-toggle-key '()
               '(ajax-ime-keys4 advanced)
	       '(key)
	       (N_ "[Ajax-IME] toggle hiragana/katakana mode")
	       (N_ "long description will be here"))

(define-custom 'ajax-ime-alkana-toggle-key '()
	       '(ajax-ime-keys4 advanced)
	       '(key)
	       (N_ "[Ajax-IME] toggle kana/alphanumeric mode")
	       (N_ "long description will be here"))

(define-custom 'ajax-ime-next-prediction-key '("tab" "down" "<IgnoreCase><Control>n" "<IgnoreCase><Control>i")
               '(ajax-ime-keys4 ajax-ime-prediction)
               '(key)
               (N_ "[Ajax-IME] Next prediction candidate")
               (N_ "long description will be here"))

(define-custom 'ajax-ime-prev-prediction-key '(generic-prev-candidate-key)
               '(ajax-ime-keys4 ajax-ime-prediction)
               '(key)
               (N_ "[Ajax-IME] Previous prediction candidate")
               (N_ "long description will be here"))
