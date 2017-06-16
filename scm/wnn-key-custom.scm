;;; wnn-custom.scm: Customization variables for wnn.scm
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

(define-custom-group 'wnn-keys1
		     (N_ "Wnn key bindings 1")
		     (N_ "long description will be here."))

(define-custom-group 'wnn-keys2
		     (N_ "Wnn key bindings 2")
		     (N_ "long description will be here."))

(define-custom-group 'wnn-keys3
		     (N_ "Wnn key bindings 3")
		     (N_ "long description will be here."))

(define-custom-group 'wnn-keys4
		     (N_ "Wnn key bindings 4")
		     (N_ "long description will be here."))

(define-custom 'wnn-next-segment-key '(generic-go-right-key)
               '(wnn-keys1)
	       '(key)
	       (N_ "[Wnn] next segment")
	       (N_ "long description will be here"))

(define-custom 'wnn-prev-segment-key '(generic-go-left-key)
               '(wnn-keys1)
	       '(key)
	       (N_ "[Wnn] previous segment")
	       (N_ "long description will be here"))

(define-custom 'wnn-extend-segment-key '("<IgnoreCase><Control>o" "<Shift>right")
               '(wnn-keys1)
	       '(key)
	       (N_ "[Wnn] extend segment")
	       (N_ "long description will be here"))

(define-custom 'wnn-shrink-segment-key '("<IgnoreCase><Control>i" "<Shift>left")
               '(wnn-keys1)
	       '(key)
	       (N_ "[Wnn] shrink segment")
	       (N_ "long description will be here"))

(define-custom 'wnn-transpose-as-hiragana-key '("F6" "Muhenkan")
               '(wnn-keys1)
	       '(key)
	       (N_ "[Wnn] convert to hiragana")
	       (N_ "long description will be here"))

(define-custom 'wnn-transpose-as-katakana-key '("F7" "Muhenkan")
               '(wnn-keys1)
	       '(key)
	       (N_ "[Wnn] convert to katakana")
	       (N_ "long description will be here"))

(define-custom 'wnn-transpose-as-halfkana-key '("F8" "Muhenkan")
               '(wnn-keys1)
	       '(key)
	       (N_ "[Wnn] convert to halfwidth katakana")
	       (N_ "long description will be here"))

(define-custom 'wnn-transpose-as-halfwidth-alnum-key '("F10")
	       '(wnn-keys1)
	       '(key)
	       (N_ "[Wnn] convert to halfwidth alphanumeric")
	       (N_ "long description will be here"))

(define-custom 'wnn-transpose-as-fullwidth-alnum-key '("F9")
	       '(wnn-keys1)
	       '(key)
	       (N_ "[Wnn] convert to fullwidth alphanumeric")
	       (N_ "long description will be here"))

(define-custom 'wnn-commit-as-opposite-kana-key '("<IgnoreCase><Shift>q")  ;; "Q"
               '(wnn-keys1)
	       '(key)
	       (N_ "[Wnn] commit as transposed kana")
	       (N_ "long description will be here"))

;;
;; overriding generic keys
;;
(define-custom 'wnn-on-key '("<Control>\\" generic-on-key)
               '(wnn-keys2)
	       '(key)
	       (N_ "[Wnn] on")
	       (N_ "long description will be here"))

;;(define-custom 'wnn-off-key '("l" generic-on-key)
(define-custom 'wnn-off-key '("<Control>\\" generic-off-key)
               '(wnn-keys2)
	       '(key)
	       (N_ "[Wnn] off")
	       (N_ "long description will be here"))

(define-custom 'wnn-begin-conv-key '(generic-begin-conv-key)
               '(wnn-keys2)
	       '(key)
	       (N_ "[Wnn] begin conversion")
	       (N_ "long description will be here"))

(define-custom 'wnn-commit-key '(generic-commit-key)
               '(wnn-keys2)
	       '(key)
	       (N_ "[Wnn] commit")
	       (N_ "long description will be here"))

(define-custom 'wnn-cancel-key '(generic-cancel-key)
               '(wnn-keys2)
	       '(key)
	       (N_ "[Wnn] cancel")
	       (N_ "long description will be here"))

(define-custom 'wnn-next-candidate-key '(generic-next-candidate-key)
               '(wnn-keys2)
	       '(key)
	       (N_ "[Wnn] next candidate")
	       (N_ "long description will be here"))

(define-custom 'wnn-prev-candidate-key '(generic-prev-candidate-key)
               '(wnn-keys2)
	       '(key)
	       (N_ "[Wnn] previous candidate")
	       (N_ "long description will be here"))

(define-custom 'wnn-next-page-key '(generic-next-page-key)
             '(wnn-keys2)
	       '(key)
	       (N_ "[Wnn] next page of candidate window")
	       (N_ "long description will be here"))

(define-custom 'wnn-prev-page-key '(generic-prev-page-key)
               '(wnn-keys2)
	       '(key)
	       (N_ "[Wnn] previous page of candidate window")
	       (N_ "long description will be here"))

;;
;; overriding generic keys (advanced)
;;
(define-custom 'wnn-beginning-of-preedit-key '(generic-beginning-of-preedit-key)
               '(wnn-keys3)
	       '(key)
	       (N_ "[Wnn] beginning of preedit")
	       (N_ "long description will be here"))

(define-custom 'wnn-end-of-preedit-key '(generic-end-of-preedit-key)
               '(wnn-keys3)
	       '(key)
	       (N_ "[Wnn] end of preedit")
	       (N_ "long description will be here"))

(define-custom 'wnn-kill-key '(generic-kill-key)
               '(wnn-keys3)
	       '(key)
	       (N_ "[Wnn] erase after cursor")
	       (N_ "long description will be here"))

(define-custom 'wnn-kill-backward-key '(generic-kill-backward-key)
               '(wnn-keys3)
	       '(key)
	       (N_ "[Wnn] erase before cursor")
	       (N_ "long description will be here"))

(define-custom 'wnn-backspace-key '(generic-backspace-key)
               '(wnn-keys3)
	       '(key)
	       (N_ "[Wnn] backspace")
	       (N_ "long description will be here"))

(define-custom 'wnn-delete-key '(generic-delete-key)
               '(wnn-keys3)
	       '(key)
	       (N_ "[Wnn] delete")
	       (N_ "long description will be here"))

(define-custom 'wnn-go-left-key '(generic-go-left-key)
               '(wnn-keys3)
	       '(key)
	       (N_ "[Wnn] go left")
	       (N_ "long description will be here"))

(define-custom 'wnn-go-right-key '(generic-go-right-key)
               '(wnn-keys3)
	       '(key)
	       (N_ "[Wnn] go right")
	       (N_ "long description will be here"))

(define-custom 'wnn-vi-escape-key '("escape" "<Control>[")
               '(wnn-keys3)
	       '(key)
	       (N_ "[Wnn] ESC keys on vi-cooperative mode")
	       (N_ "long description will be here"))

;;
;; ja advanced
;;

(define-custom 'wnn-hiragana-key '("<Shift>F6")
	       '(wnn-keys4 mode-transition)
	       '(key)
	       (N_ "[Wnn] hiragana mode")
	       (N_ "long description will be here"))

(define-custom 'wnn-katakana-key '("<Shift>F7")
	       '(wnn-keys4 mode-transition)
	       '(key)
	       (N_ "[Wnn] katakana mode")
	       (N_ "long description will be here"))

(define-custom 'wnn-halfkana-key '("<Shift>F8")
	       '(wnn-keys4 mode-transition)
	       '(key)
	       (N_ "[Wnn] halfwidth katakana mode")
	       (N_ "long description will be here"))

(define-custom 'wnn-halfwidth-alnum-key '("<Shift>F10")
	       '(wnn-keys4 mode-transition)
	       '(key)
	       (N_ "[Wnn] halfwidth alphanumeric mode")
	       (N_ "long description will be here"))

(define-custom 'wnn-fullwidth-alnum-key '("<Shift>F9")
	       '(wnn-keys4 mode-transition)
	       '(key)
	       (N_ "[Wnn] fullwidth alphanumeric mode")
	       (N_ "long description will be here"))

(define-custom 'wnn-kana-toggle-key '()
               '(wnn-keys4 advanced)
	       '(key)
	       (N_ "[Wnn] toggle hiragana/katakana mode")
	       (N_ "long description will be here"))

(define-custom 'wnn-alkana-toggle-key '()
	       '(wnn-keys4 advanced)
	       '(key)
	       (N_ "[Wnn] toggle kana/alphanumeric mode")
	       (N_ "long description will be here"))

(define-custom 'wnn-next-prediction-key '("tab" "down" "<IgnoreCase><Control>n" "<IgnoreCase><Control>i")
               '(wnn-keys4 wnn-prediction)
               '(key)
               (N_ "[Wnn] Next prediction candidate")
               (N_ "long description will be here"))

(define-custom 'wnn-prev-prediction-key '(generic-prev-candidate-key)
               '(wnn-keys4 wnn-prediction)
               '(key)
               (N_ "[Wnn] Previous prediction candidate")
               (N_ "long description will be here"))
