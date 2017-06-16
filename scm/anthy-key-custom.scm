;;; anthy-key-custom.scm: Customization variables for anthy.scm
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


(define-custom-group 'anthy-keys1
		     (N_ "Anthy key bindings 1")
		     (N_ "long description will be here."))

(define-custom-group 'anthy-keys2
		     (N_ "Anthy key bindings 2")
		     (N_ "long description will be here."))

(define-custom-group 'anthy-keys3
		     (N_ "Anthy key bindings 3")
		     (N_ "long description will be here."))

(define-custom-group 'anthy-keys4
		     (N_ "Anthy key bindings 4")
		     (N_ "long description will be here."))

(define-custom 'anthy-next-segment-key '(generic-go-right-key)
               '(anthy-keys1)
	       '(key)
	       (N_ "[Anthy] next segment")
	       (N_ "long description will be here"))

(define-custom 'anthy-prev-segment-key '(generic-go-left-key)
               '(anthy-keys1)
	       '(key)
	       (N_ "[Anthy] previous segment")
	       (N_ "long description will be here"))

(define-custom 'anthy-extend-segment-key '("<IgnoreCase><Control>o" "<Shift>right")
               '(anthy-keys1)
	       '(key)
	       (N_ "[Anthy] extend segment")
	       (N_ "long description will be here"))

(define-custom 'anthy-shrink-segment-key '("<IgnoreCase><Control>i" "<Shift>left")
               '(anthy-keys1)
	       '(key)
	       (N_ "[Anthy] shrink segment")
	       (N_ "long description will be here"))

(define-custom 'anthy-transpose-as-hiragana-key '("F6" "Muhenkan")
               '(anthy-keys1)
	       '(key)
	       (N_ "[Anthy] convert to hiragana")
	       (N_ "long description will be here"))

(define-custom 'anthy-transpose-as-katakana-key '("F7" "Muhenkan")
               '(anthy-keys1)
	       '(key)
	       (N_ "[Anthy] convert to katakana")
	       (N_ "long description will be here"))

(define-custom 'anthy-transpose-as-halfkana-key '("F8" "Muhenkan")
               '(anthy-keys1)
	       '(key)
	       (N_ "[Anthy] convert to halfwidth katakana")
	       (N_ "long description will be here"))

(define-custom 'anthy-transpose-as-halfwidth-alnum-key '("F10")
               '(anthy-keys1)
	       '(key)
	       (N_ "[Anthy] convert to halfwidth alphanumeric")
	       (N_ "long description will be here"))

(define-custom 'anthy-transpose-as-fullwidth-alnum-key '("F9")
               '(anthy-keys1)
	       '(key)
	       (N_ "[Anthy] convert to fullwidth alphanumeric")
	       (N_ "long description will be here"))

(define-custom 'anthy-commit-as-opposite-kana-key '()
               '(anthy-keys1)
	       '(key)
	       (N_ "[Anthy] commit as transposed kana")
	       (N_ "long description will be here"))

;;
;; overriding generic keys
;;

(define-custom 'anthy-on-key '(generic-on-key)
               '(anthy-keys2)
	       '(key)
	       (N_ "[Anthy] on")
	       (N_ "long description will be here"))

(define-custom 'anthy-off-key '(generic-off-key)
               '(anthy-keys2)
	       '(key)
	       (N_ "[Anthy] off")
	       (N_ "long description will be here"))

(define-custom 'anthy-begin-conv-key '(generic-begin-conv-key)
               '(anthy-keys2)
	       '(key)
	       (N_ "[Anthy] begin conversion")
	       (N_ "long description will be here"))

(define-custom 'anthy-commit-key '(generic-commit-key)
               '(anthy-keys2)
	       '(key)
	       (N_ "[Anthy] commit")
	       (N_ "long description will be here"))

(define-custom 'anthy-cancel-key '(generic-cancel-key)
               '(anthy-keys2)
	       '(key)
	       (N_ "[Anthy] cancel")
	       (N_ "long description will be here"))

(define-custom 'anthy-next-candidate-key '(generic-next-candidate-key)
               '(anthy-keys2)
	       '(key)
	       (N_ "[Anthy] next candidate")
	       (N_ "long description will be here"))

(define-custom 'anthy-prev-candidate-key '(generic-prev-candidate-key)
               '(anthy-keys2)
	       '(key)
	       (N_ "[Anthy] previous candidate")
	       (N_ "long description will be here"))

(define-custom 'anthy-next-page-key '(generic-next-page-key)
               '(anthy-keys2)
	       '(key)
	       (N_ "[Anthy] next page of candidate window")
	       (N_ "long description will be here"))

(define-custom 'anthy-prev-page-key '(generic-prev-page-key)
               '(anthy-keys2)
	       '(key)
	       (N_ "[Anthy] previous page of candidate window")
	       (N_ "long description will be here"))

;;
;; overriding generic keys (advanced)
;;

(define-custom 'anthy-beginning-of-preedit-key '(generic-beginning-of-preedit-key)
               '(anthy-keys3)
	       '(key)
	       (N_ "[Anthy] beginning of preedit")
	       (N_ "long description will be here"))

(define-custom 'anthy-end-of-preedit-key '(generic-end-of-preedit-key)
               '(anthy-keys3)
	       '(key)
	       (N_ "[Anthy] end of preedit")
	       (N_ "long description will be here"))

(define-custom 'anthy-kill-key '(generic-kill-key)
               '(anthy-keys3)
	       '(key)
	       (N_ "[Anthy] erase after cursor")
	       (N_ "long description will be here"))

(define-custom 'anthy-kill-backward-key '(generic-kill-backward-key)
               '(anthy-keys3)
	       '(key)
	       (N_ "[Anthy] erase before cursor")
	       (N_ "long description will be here"))

(define-custom 'anthy-backspace-key '(generic-backspace-key)
               '(anthy-keys3)
	       '(key)
	       (N_ "[Anthy] backspace")
	       (N_ "long description will be here"))

(define-custom 'anthy-delete-key '(generic-delete-key)
               '(anthy-keys3)
	       '(key)
	       (N_ "[Anthy] delete")
	       (N_ "long description will be here"))

(define-custom 'anthy-go-left-key '(generic-go-left-key)
               '(anthy-keys3)
	       '(key)
	       (N_ "[Anthy] go left")
	       (N_ "long description will be here"))

(define-custom 'anthy-go-right-key '(generic-go-right-key)
               '(anthy-keys3)
	       '(key)
	       (N_ "[Anthy] go right")
	       (N_ "long description will be here"))

(define-custom 'anthy-vi-escape-key '("escape" "<Control>[")
               '(anthy-keys3)
	       '(key)
	       (N_ "[Anthy] ESC keys on vi-cooperative mode")
	       (N_ "long description will be here"))

;;
;; ja advanced
;;

(define-custom 'anthy-hiragana-key '("<Shift>F6")
               '(anthy-keys4 mode-transition)
	       '(key)
	       (N_ "[Anthy] hiragana mode")
	       (N_ "long description will be here"))

(define-custom 'anthy-katakana-key '("<Shift>F7")
               '(anthy-keys4 mode-transition)
	       '(key)
	       (N_ "[Anthy] katakana mode")
	       (N_ "long description will be here"))

(define-custom 'anthy-halfkana-key '("<Shift>F8")
               '(anthy-keys4 mode-transition)
	       '(key)
	       (N_ "[Anthy] halfwidth katakana mode")
	       (N_ "long description will be here"))

(define-custom 'anthy-halfwidth-alnum-key '("<Shift>F10")
               '(anthy-keys4 mode-transition)
	       '(key)
	       (N_ "[Anthy] halfwidth alphanumeric mode")
	       (N_ "long description will be here"))

(define-custom 'anthy-fullwidth-alnum-key '("<Shift>F9")
               '(anthy-keys4 mode-transition)
	       '(key)
	       (N_ "[Anthy] fullwidth alphanumeric mode")
	       (N_ "long description will be here"))

(define-custom 'anthy-kana-toggle-key '()
               '(anthy-keys4 advanced)
	       '(key)
	       (N_ "[Anthy] toggle hiragana/katakana mode")
	       (N_ "long description will be here"))

(define-custom 'anthy-alkana-toggle-key '()
               '(anthy-keys4 advanced)
	       '(key)
	       (N_ "[Anthy] toggle kana/alphanumeric mode")
	       (N_ "long description will be here"))

(define-custom 'anthy-next-prediction-key '("tab" "down" "<IgnoreCase><Control>n" "<IgnoreCase><Control>i")
               '(anthy-keys4 anthy-prediction)
	       '(key)
	       (N_ "[Anthy] Next prediction candidate")
	       (N_ "long description will be here"))

(define-custom 'anthy-prev-prediction-key '(generic-prev-candidate-key)
               '(anthy-keys4 anthy-prediction)
	       '(key)
	       (N_ "[Anthy] Previous prediction candidate")
	       (N_ "long description will be here"))
