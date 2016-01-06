;;; skk-key-custom.scm: Customization variables for SKK key bindings
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

;; original key definitions

;;(define-key skk-latin-key? '("l" generic-off-key?))
;;(define-key skk-wide-latin-key? "L")
;;(define-key skk-begin-conv-key? 'generic-begin-conv-key?)
;;(define-key skk-begin-completion-key? '("tab" "<Control>i" "<Control>I"))
;;(define-key skk-next-completion-key? '("." skk-begin-completion-key?))
;;(define-key skk-prev-completion-key? ",")
;;(define-key skk-on-key? '("<Control>j" "<Control>J" generic-on-key?))
;;(define-key skk-hankaku-kana-key? '("<Control>q" "<Control>Q"))
;;(define-key skk-return-key? 'generic-return-key?)
;;(define-key skk-commit-key? '("<Control>j" "<Control>J"))
;;(define-key skk-next-candidate-key? 'generic-next-candidate-key?)
;;(define-key skk-prev-candidate-key? '("x" generic-prev-candidate-key?))
;;(define-key skk-next-page-key? 'generic-next-page-key?)
;;(define-key skk-prev-page-key? 'generic-prev-page-key?)
;;(define-key skk-kana-toggle-key? "q")
;;(define-key skk-cancel-key? 'generic-cancel-key?)
;;(define-key skk-backspace-key? 'generic-backspace-key?)
;;(define-key skk-go-left-key? 'generic-go-left-key?)
;;(define-key skk-go-right-key? 'generic-go-right-key?)
;;(define-key skk-latin-conv-key? "/")
;;(define-key skk-kanji-mode-key? "Q")
;;(define-key skk-special-midashi-key? '(">" "<" "?"))
;;(define-key skk-conv-wide-latin-key? '("<Control>q" "<Control>Q"))
;;(define-key skk-plain-space-key? " ")  ;; should not be changed
;;(define-key skk-vi-escape-key? '("escape" "<Control>["))
;;(define-key skk-state-direct-no-preedit-nop-key? '("<Control>j" "<Control>J"))

(define-custom-group 'skk-keys1
		     (N_ "SKK key bindings 1")
		     (N_ "long description will be here."))

(define-custom-group 'skk-keys2
		     (N_ "SKK key bindings 2")
		     (N_ "long description will be here."))

(define-custom-group 'skk-keys3
		     (N_ "SKK key bindings 3")
		     (N_ "long description will be here."))

;; subgroup
(define-custom-group 'skk-keys-completion
		     (N_ "Completion")
		     (N_ "long description will be here."))

(define-custom-group 'skk-keys-dcomp
		     (N_ "Dynamic completion")
		     (N_ "long description will be here."))

(define-custom-group 'skk-keys-latin-conv
		     (N_ "Latin conversion")
		     (N_ "long description will be here."))


(define-custom 'skk-on-key '("<IgnoreCase><Control>j" generic-on-key)
               '(skk-keys1 mode-transition)
	       '(key)
	       (N_ "[SKK] on")
	       (N_ "long description will be here"))

(define-custom 'skk-latin-key '("<IgnoreCase>l" generic-off-key)  ;; "l"
               '(skk-keys1 mode-transition)
	       '(key)
	       (N_ "[SKK] latin mode")
	       (N_ "long description will be here"))

(define-custom 'skk-wide-latin-key '("<IgnoreCase><Shift>l")  ;; "L"
               '(skk-keys1 mode-transition)
	       '(key)
	       (N_ "[SKK] wide-latin mode")
	       (N_ "long description will be here"))

(define-custom 'skk-kcode-input-key '("yen")
               '(skk-keys1 mode-transition)
	       '(key)
	       (N_ "[SKK] kanji code input mode")
	       (N_ "long description will be here"))

(define-custom 'skk-kanji-mode-key '("<IgnoreCase><Shift>q")  ;; "Q"
               '(skk-keys1 mode-transition)
	       '(key)
	       (N_ "[SKK] kanji mode")
	       (N_ "long description will be here"))

(define-custom 'skk-hankaku-kana-key '("<IgnoreCase><Control>q")
               '(skk-keys1 mode-transition)
	       '(key)
	       (N_ "[SKK] halfwidth katakana mode")
	       (N_ "long description will be here"))

(define-custom 'skk-kana-toggle-key '("<IgnoreCase>q")  ;; "q"
               '(skk-keys1 mode-transition)
	       '(key)
	       (N_ "[SKK] toggle hiragana/katakana mode")
	       (N_ "long description will be here"))

(define-custom 'skk-begin-conv-key '(generic-begin-conv-key)
               '(skk-keys1)
	       '(key)
	       (N_ "[SKK] begin conversion")
	       (N_ "long description will be here"))

(define-custom 'skk-commit-key '("<IgnoreCase><Control>j")
               '(skk-keys1)
	       '(key)
	       (N_ "[SKK] commit")
	       (N_ "long description will be here"))

(define-custom 'skk-cancel-key '(generic-cancel-key)
               '(skk-keys1)
	       '(key)
	       (N_ "[SKK] cancel")
	       (N_ "long description will be here"))

(define-custom 'skk-sticky-key '()
               '(skk-keys1)
	       '(key)
	       (N_ "[SKK] sticky")
	       (N_ "long description will be here"))

;;
;; advanced 1
;;

(define-custom 'skk-return-key '(generic-return-key)
               '(skk-keys2)
	       '(key)
	       (N_ "[SKK] return")
	       (N_ "long description will be here"))

(define-custom 'skk-latin-conv-key '("/")
               '(skk-keys2 skk-keys-latin-conv)
	       '(key)
	       (N_ "[SKK] begin latin conversion")
	       (N_ "long description will be here"))

(define-custom 'skk-conv-wide-latin-key '("<IgnoreCase><Control>q")
               '(skk-keys2 skk-keys-latin-conv)
	       '(key)
	       (N_ "[SKK] commit as fullwidth alphanumeric")
	       (N_ "long description will be here"))

(define-custom 'skk-conv-opposite-case-key '("<IgnoreCase><Control>u")
               '(skk-keys2 skk-keys-latin-conv)
	       '(key)
	       (N_ "[SKK] commit as opposite case in latin conversion")
	       (N_ "long description will be here"))

(define-custom 'skk-begin-completion-key '("tab" "<IgnoreCase><Control>i" skk-new-completion-from-current-comp-key)
               '(skk-keys2 skk-keys-completion)
	       '(key)
	       (N_ "[SKK] begin completion")
	       (N_ "long description will be here"))

(define-custom 'skk-next-completion-key '("." "tab" "<IgnoreCase><Control>i")
               '(skk-keys2 skk-keys-completion)
	       '(key)
	       (N_ "[SKK] next completion candidate")
	       (N_ "long description will be here"))

(define-custom 'skk-prev-completion-key '(",")
               '(skk-keys2 skk-keys-completion)
	       '(key)
	       (N_ "[SKK] previous completion candidate")
	       (N_ "long description will be here"))

(define-custom 'skk-new-completion-from-current-comp-key '("<Alt>tab" "<IgnoreCase><Control><Alt>i")
	       '(skk-keys2 skk-keys-completion)
	       '(key)
	       (N_ "[SKK] new completion using current completion")
	       (N_ "long description will be here"))

(define-custom 'skk-begin-conv-with-completion-key '("<Alt> ")
	       '(skk-keys3 skk-keys-dcomp)
	       '(key)
	       (N_ "[SKK] begin conversion with completion")
	       (N_ "long description will be here"))

(define-custom 'skk-commit-with-conv-completion-key '("<IgnoreCase><Control><Alt>j")
	       '(skk-keys3 skk-keys-dcomp)
	       '(key)
	       (N_ "[SKK] commit the first candidate with completion")
	       (N_ "long description will be here"))

(define-custom 'skk-special-midashi-key '("<IgnoreShift>>" "<IgnoreShift><" "<IgnoreShift>?")
               '(skk-keys2)
	       '(key)
	       (N_ "[SKK] join prefix or suffix")
	       (N_ "long description will be here"))

(define-custom 'skk-vi-escape-key '("escape" "<Control>[")
               '(skk-keys2)
	       '(key)
	       (N_ "[SKK] ESC keys on vi-cooperative mode")
	       (N_ "long description will be here"))

(define-custom 'skk-state-direct-no-preedit-nop-key '("<IgnoreCase><Control>j")
               '(skk-keys2)
	       '(key)
	       (N_ "[SKK] no-operation on no-preedit state")
	       (N_ "See [Anthy-dev 1616] and related messages for further information"))

(define-custom 'skk-purge-candidate-key '("<IgnoreCase><Shift>x")  ;; "X"
	       '(skk-keys2)
	       '(key)
	       (N_ "[SKK] purge the entry from dictionary")
	       (N_ "long description will be here"))

;; should not be changed
(define-key skk-plain-space-key? '(" "))
;;(define-custom 'skk-plain-space-key '(" ")
;;               '(skk-keys2)
;;	       '(key)
;;	       (N_ "[SKK] plain space")
;;	       (N_ "long description will be here"))

;;
;; advanced 2
;;

(define-custom 'skk-next-candidate-key '(generic-next-candidate-key)
               '(skk-keys3)
	       '(key)
	       (N_ "[SKK] next candidate")
	       (N_ "long description will be here"))

(define-custom 'skk-prev-candidate-key '("<IgnoreCase>x" generic-prev-candidate-key) ;; "x"
               '(skk-keys3)
	       '(key)
	       (N_ "[SKK] previous candidate")
	       (N_ "long description will be here"))

(define-custom 'skk-next-page-key '(generic-next-page-key)
               '(skk-keys3)
	       '(key)
	       (N_ "[SKK] next page of candidate window")
	       (N_ "long description will be here"))

(define-custom 'skk-prev-page-key '(generic-prev-page-key)
               '(skk-keys3)
	       '(key)
	       (N_ "[SKK] previous page of candidate window")
	       (N_ "long description will be here"))

(define-custom 'skk-backspace-key '(generic-backspace-key)
               '(skk-keys3)
	       '(key)
	       (N_ "[SKK] backspace")
	       (N_ "long description will be here"))

(define-custom 'skk-go-left-key '(generic-go-left-key)
               '(skk-keys3)
	       '(key)
	       (N_ "[SKK] go left")
	       (N_ "long description will be here"))

(define-custom 'skk-go-right-key '(generic-go-right-key)
               '(skk-keys3)
	       '(key)
	       (N_ "[SKK] go right")
	       (N_ "long description will be here"))
