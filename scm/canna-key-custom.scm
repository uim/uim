;;; canna-key-custom.scm: Customization variables for canna.scm
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


(define-custom-group 'canna-keys1
		     (N_ "Canna key bindings 1")
		     (N_ "long description will be here."))

(define-custom-group 'canna-keys2
		     (N_ "Canna key bindings 2")
		     (N_ "long description will be here."))

(define-custom-group 'canna-keys3
		     (N_ "Canna key bindings 3")
		     (N_ "long description will be here."))

(define-custom-group 'canna-keys4
		     (N_ "Canna key bindings 4")
		     (N_ "long description will be here."))

(define-custom 'canna-next-segment-key '(generic-go-right-key)
               '(canna-keys1)
	       '(key)
	       (N_ "[Canna] next segment")
	       (N_ "long description will be here"))

(define-custom 'canna-prev-segment-key '(generic-go-left-key)
               '(canna-keys1)
	       '(key)
	       (N_ "[Canna] previous segment")
	       (N_ "long description will be here"))

(define-custom 'canna-extend-segment-key '("<IgnoreCase><Control>o" "<Shift>right")
               '(canna-keys1)
	       '(key)
	       (N_ "[Canna] extend segment")
	       (N_ "long description will be here"))

(define-custom 'canna-shrink-segment-key '("<IgnoreCase><Control>i" "<Shift>left")
               '(canna-keys1)
	       '(key)
	       (N_ "[Canna] shrink segment")
	       (N_ "long description will be here"))

(define-custom 'canna-transpose-as-hiragana-key '("F6" "Muhenkan")
               '(canna-keys1)
	       '(key)
	       (N_ "[Canna] convert to hiragana")
	       (N_ "long description will be here"))

(define-custom 'canna-transpose-as-katakana-key '("F7" "Muhenkan")
               '(canna-keys1)
	       '(key)
	       (N_ "[Canna] convert to katakana")
	       (N_ "long description will be here"))

(define-custom 'canna-transpose-as-halfkana-key '("F8" "Muhenkan")
               '(canna-keys1)
	       '(key)
	       (N_ "[Canna] convert to halfwidth katakana")
	       (N_ "long description will be here"))

(define-custom 'canna-transpose-as-halfwidth-alnum-key '("F10")
	       '(canna-keys1)
	       '(key)
	       (N_ "[Canna] convert to halfwidth alphanumeric")
	       (N_ "long description will be here"))

(define-custom 'canna-transpose-as-fullwidth-alnum-key '("F9")
	       '(canna-keys1)
	       '(key)
	       (N_ "[Canna] convert to fullwidth alphanumeric")
	       (N_ "long description will be here"))

(define-custom 'canna-commit-as-opposite-kana-key '("<IgnoreCase><Shift>q")  ;; "Q"
               '(canna-keys1)
	       '(key)
	       (N_ "[Canna] commit as transposed kana")
	       (N_ "long description will be here"))

;;
;; overriding generic keys
;;
(define-custom 'canna-on-key '("<Control>\\" generic-on-key)
               '(canna-keys2)
	       '(key)
	       (N_ "[Canna] on")
	       (N_ "long description will be here"))

;;(define-custom 'canna-off-key '("l" generic-on-key)
(define-custom 'canna-off-key '("<Control>\\" generic-off-key)
               '(canna-keys2)
	       '(key)
	       (N_ "[Canna] off")
	       (N_ "long description will be here"))

(define-custom 'canna-begin-conv-key '(generic-begin-conv-key)
               '(canna-keys2)
	       '(key)
	       (N_ "[Canna] begin conversion")
	       (N_ "long description will be here"))

(define-custom 'canna-commit-key '(generic-commit-key)
               '(canna-keys2)
	       '(key)
	       (N_ "[Canna] commit")
	       (N_ "long description will be here"))

(define-custom 'canna-cancel-key '(generic-cancel-key)
               '(canna-keys2)
	       '(key)
	       (N_ "[Canna] cancel")
	       (N_ "long description will be here"))

(define-custom 'canna-next-candidate-key '(generic-next-candidate-key)
               '(canna-keys2)
	       '(key)
	       (N_ "[Canna] next candidate")
	       (N_ "long description will be here"))

(define-custom 'canna-prev-candidate-key '(generic-prev-candidate-key)
               '(canna-keys2)
	       '(key)
	       (N_ "[Canna] previous candidate")
	       (N_ "long description will be here"))

(define-custom 'canna-next-page-key '(generic-next-page-key)
             '(canna-keys2)
	       '(key)
	       (N_ "[Canna] next page of candidate window")
	       (N_ "long description will be here"))

(define-custom 'canna-prev-page-key '(generic-prev-page-key)
               '(canna-keys2)
	       '(key)
	       (N_ "[Canna] previous page of candidate window")
	       (N_ "long description will be here"))

;;
;; overriding generic keys (advanced)
;;
(define-custom 'canna-beginning-of-preedit-key '(generic-beginning-of-preedit-key)
               '(canna-keys3)
	       '(key)
	       (N_ "[Canna] beginning of preedit")
	       (N_ "long description will be here"))

(define-custom 'canna-end-of-preedit-key '(generic-end-of-preedit-key)
               '(canna-keys3)
	       '(key)
	       (N_ "[Canna] end of preedit")
	       (N_ "long description will be here"))

(define-custom 'canna-kill-key '(generic-kill-key)
               '(canna-keys3)
	       '(key)
	       (N_ "[Canna] erase after cursor")
	       (N_ "long description will be here"))

(define-custom 'canna-kill-backward-key '(generic-kill-backward-key)
               '(canna-keys3)
	       '(key)
	       (N_ "[Canna] erase before cursor")
	       (N_ "long description will be here"))

(define-custom 'canna-backspace-key '(generic-backspace-key)
               '(canna-keys3)
	       '(key)
	       (N_ "[Canna] backspace")
	       (N_ "long description will be here"))

(define-custom 'canna-delete-key '(generic-delete-key)
               '(canna-keys3)
	       '(key)
	       (N_ "[Canna] delete")
	       (N_ "long description will be here"))

(define-custom 'canna-go-left-key '(generic-go-left-key)
               '(canna-keys3)
	       '(key)
	       (N_ "[Canna] go left")
	       (N_ "long description will be here"))

(define-custom 'canna-go-right-key '(generic-go-right-key)
               '(canna-keys3)
	       '(key)
	       (N_ "[Canna] go right")
	       (N_ "long description will be here"))

(define-custom 'canna-vi-escape-key '("escape" "<Control>[")
               '(canna-keys3)
	       '(key)
	       (N_ "[Canna] ESC keys on vi-cooperative mode")
	       (N_ "long description will be here"))

;;
;; ja advanced
;;

(define-custom 'canna-hiragana-key '("<Shift>F6")
	       '(canna-keys4 mode-transition)
	       '(key)
	       (N_ "[Canna] hiragana mode")
	       (N_ "long description will be here"))

(define-custom 'canna-katakana-key '("<Shift>F7")
	       '(canna-keys4 mode-transition)
	       '(key)
	       (N_ "[Canna] katakana mode")
	       (N_ "long description will be here"))

(define-custom 'canna-halfkana-key '("<Shift>F8")
	       '(canna-keys4 mode-transition)
	       '(key)
	       (N_ "[Canna] halfwidth katakana mode")
	       (N_ "long description will be here"))

(define-custom 'canna-halfwidth-alnum-key '("<Shift>F10")
	       '(canna-keys4 mode-transition)
	       '(key)
	       (N_ "[Canna] halfwidth alphanumeric mode")
	       (N_ "long description will be here"))

(define-custom 'canna-fullwidth-alnum-key '("<Shift>F9")
	       '(canna-keys4 mode-transition)
	       '(key)
	       (N_ "[Canna] fullwidth alphanumeric mode")
	       (N_ "long description will be here"))

(define-custom 'canna-kana-toggle-key '()
               '(canna-keys4 advanced)
	       '(key)
	       (N_ "[Canna] toggle hiragana/katakana mode")
	       (N_ "long description will be here"))

(define-custom 'canna-alkana-toggle-key '()
	       '(canna-keys4 advanced)
	       '(key)
	       (N_ "[Canna] toggle kana/alphanumeric mode")
	       (N_ "long description will be here"))

(define-custom 'canna-next-prediction-key '("tab" "down" "<IgnoreCase><Control>n" "<IgnoreCase><Control>i")
               '(canna-keys4 canna-prediction)
               '(key)
               (N_ "[Canna] Next prediction candidate")
               (N_ "long description will be here"))

(define-custom 'canna-prev-prediction-key '(generic-prev-candidate-key)
               '(canna-keys4 canna-prediction)
               '(key)
               (N_ "[Canna] Previous prediction candidate")
               (N_ "long description will be here"))
