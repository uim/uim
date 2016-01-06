;;; mana-key-custom.scm: Customization variables for mana.scm
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


(define-custom-group 'mana-keys1
		     (N_ "Mana key bindings 1")
		     (N_ "long description will be here."))

(define-custom-group 'mana-keys2
		     (N_ "Mana key bindings 2")
		     (N_ "long description will be here."))

(define-custom-group 'mana-keys3
		     (N_ "Mana key bindings 3")
		     (N_ "long description will be here."))

(define-custom-group 'mana-keys4
		     (N_ "Mana key bindings 4")
		     (N_ "long description will be here."))

(define-custom 'mana-next-segment-key '(generic-go-right-key)
               '(mana-keys1)
	       '(key)
	       (N_ "[Mana] next segment")
	       (N_ "long description will be here"))

(define-custom 'mana-prev-segment-key '(generic-go-left-key)
               '(mana-keys1)
	       '(key)
	       (N_ "[Mana] previous segment")
	       (N_ "long description will be here"))

(define-custom 'mana-extend-segment-key '("<IgnoreCase><Control>o" "<Shift>right")
               '(mana-keys1)
	       '(key)
	       (N_ "[Mana] extend segment")
	       (N_ "long description will be here"))

(define-custom 'mana-shrink-segment-key '("<IgnoreCase><Control>i" "<Shift>left")
               '(mana-keys1)
	       '(key)
	       (N_ "[Mana] shrink segment")
	       (N_ "long description will be here"))

(define-custom 'mana-transpose-as-hiragana-key '("F6" "Muhenkan")
               '(mana-keys1)
	       '(key)
	       (N_ "[Mana] convert to hiragana")
	       (N_ "long description will be here"))

(define-custom 'mana-transpose-as-katakana-key '("F7" "Muhenkan")
               '(mana-keys1)
	       '(key)
	       (N_ "[Mana] convert to katakana")
	       (N_ "long description will be here"))

(define-custom 'mana-transpose-as-halfkana-key '("F8" "Muhenkan")
               '(mana-keys1)
	       '(key)
	       (N_ "[Mana] convert to halfwidth katakana")
	       (N_ "long description will be here"))

(define-custom 'mana-transpose-as-halfwidth-alnum-key '("F10")
               '(mana-keys1)
	       '(key)
	       (N_ "[Mana] convert to halfwidth alphanumeric")
	       (N_ "long description will be here"))

(define-custom 'mana-transpose-as-fullwidth-alnum-key '("F9")
               '(mana-keys1)
	       '(key)
	       (N_ "[Mana] convert to fullwidth alphanumeric")
	       (N_ "long description will be here"))

(define-custom 'mana-commit-as-opposite-kana-key '()
               '(mana-keys1)
	       '(key)
	       (N_ "[Mana] commit as transposed kana")
	       (N_ "long description will be here"))

;;
;; overriding generic keys
;;

(define-custom 'mana-on-key '(generic-on-key)
               '(mana-keys2)
	       '(key)
	       (N_ "[Mana] on")
	       (N_ "long description will be here"))

(define-custom 'mana-off-key '(generic-off-key)
               '(mana-keys2)
	       '(key)
	       (N_ "[Mana] off")
	       (N_ "long description will be here"))

(define-custom 'mana-begin-conv-key '(generic-begin-conv-key)
               '(mana-keys2)
	       '(key)
	       (N_ "[Mana] begin conversion")
	       (N_ "long description will be here"))

(define-custom 'mana-commit-key '(generic-commit-key)
               '(mana-keys2)
	       '(key)
	       (N_ "[Mana] commit")
	       (N_ "long description will be here"))

(define-custom 'mana-cancel-key '(generic-cancel-key)
               '(mana-keys2)
	       '(key)
	       (N_ "[Mana] cancel")
	       (N_ "long description will be here"))

(define-custom 'mana-next-candidate-key '(generic-next-candidate-key)
               '(mana-keys2)
	       '(key)
	       (N_ "[Mana] next candidate")
	       (N_ "long description will be here"))

(define-custom 'mana-prev-candidate-key '(generic-prev-candidate-key)
               '(mana-keys2)
	       '(key)
	       (N_ "[Mana] previous candidate")
	       (N_ "long description will be here"))

(define-custom 'mana-next-page-key '(generic-next-page-key)
               '(mana-keys2)
	       '(key)
	       (N_ "[Mana] next page of candidate window")
	       (N_ "long description will be here"))

(define-custom 'mana-prev-page-key '(generic-prev-page-key)
               '(mana-keys2)
	       '(key)
	       (N_ "[Mana] previous page of candidate window")
	       (N_ "long description will be here"))

;;
;; overriding generic keys (advanced)
;;

(define-custom 'mana-beginning-of-preedit-key '(generic-beginning-of-preedit-key)
               '(mana-keys3)
	       '(key)
	       (N_ "[Mana] beginning of preedit")
	       (N_ "long description will be here"))

(define-custom 'mana-end-of-preedit-key '(generic-end-of-preedit-key)
               '(mana-keys3)
	       '(key)
	       (N_ "[Mana] end of preedit")
	       (N_ "long description will be here"))

(define-custom 'mana-kill-key '(generic-kill-key)
               '(mana-keys3)
	       '(key)
	       (N_ "[Mana] erase after cursor")
	       (N_ "long description will be here"))

(define-custom 'mana-kill-backward-key '(generic-kill-backward-key)
               '(mana-keys3)
	       '(key)
	       (N_ "[Mana] erase before cursor")
	       (N_ "long description will be here"))

(define-custom 'mana-backspace-key '(generic-backspace-key)
               '(mana-keys3)
	       '(key)
	       (N_ "[Mana] backspace")
	       (N_ "long description will be here"))

(define-custom 'mana-delete-key '(generic-delete-key)
               '(mana-keys3)
	       '(key)
	       (N_ "[Mana] delete")
	       (N_ "long description will be here"))

(define-custom 'mana-go-left-key '(generic-go-left-key)
               '(mana-keys3)
	       '(key)
	       (N_ "[Mana] go left")
	       (N_ "long description will be here"))

(define-custom 'mana-go-right-key '(generic-go-right-key)
               '(mana-keys3)
	       '(key)
	       (N_ "[Mana] go right")
	       (N_ "long description will be here"))

(define-custom 'mana-vi-escape-key '("escape" "<Control>[")
               '(mana-keys3)
	       '(key)
	       (N_ "[Mana] ESC keys on vi-cooperative mode")
	       (N_ "long description will be here"))

;; ja advanced

(define-custom 'mana-hiragana-key '("<Shift>F6")
	       '(mana-keys4 mode-transition)
	       '(key)
	       (N_ "[Mana] hiragana mode")
	       (N_ "long description will be here"))

(define-custom 'mana-katakana-key '("<Shift>F7")
	       '(mana-keys4 mode-transition)
	       '(key)
	       (N_ "[Mana] katakana mode")
	       (N_ "long description will be here"))

(define-custom 'mana-halfkana-key '("<Shift>F8")
	       '(mana-keys4 mode-transition)
	       '(key)
	       (N_ "[Mana] halfwidth katakana mode")
	       (N_ "long description will be here"))

(define-custom 'mana-halfwidth-alnum-key '("<Shift>F10")
	       '(mana-keys4 mode-transition)
	       '(key)
	       (N_ "[Mana] halfwidth alphanumeric mode")
	       (N_ "long description will be here"))

(define-custom 'mana-fullwidth-alnum-key '("<Shift>F9")
	       '(mana-keys4 mode-transition)
	       '(key)
	       (N_ "[Mana] fullwidth alphanumeric mode")
	       (N_ "long description will be here"))

(define-custom 'mana-kana-toggle-key '()
	       '(mana-keys4 advanced)
	       '(key)
	       (N_ "[Mana] toggle hiragana/katakana mode")
	       (N_ "long description will be here"))

(define-custom 'mana-alkana-toggle-key '()
	       '(mana-keys4 advanced)
	       '(key)
	       (N_ "[Mana] toggle kana/alphanumeric mode")
	       (N_ "long description will be here"))
