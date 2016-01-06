;;; sj3-custom.scm: Customization variables for sj3.scm
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

(define-custom-group 'sj3-keys1
		     (N_ "SJ3 key bindings 1")
		     (N_ "long description will be here."))

(define-custom-group 'sj3-keys2
		     (N_ "SJ3 key bindings 2")
		     (N_ "long description will be here."))

(define-custom-group 'sj3-keys3
		     (N_ "SJ3 key bindings 3")
		     (N_ "long description will be here."))

(define-custom-group 'sj3-keys4
		     (N_ "SJ3 key bindings 4")
		     (N_ "long description will be here."))

(define-custom 'sj3-next-segment-key '(generic-go-right-key)
               '(sj3-keys1)
	       '(key)
	       (N_ "[SJ3] next segment")
	       (N_ "long description will be here"))

(define-custom 'sj3-prev-segment-key '(generic-go-left-key)
               '(sj3-keys1)
	       '(key)
	       (N_ "[SJ3] previous segment")
	       (N_ "long description will be here"))

(define-custom 'sj3-extend-segment-key '("<IgnoreCase><Control>o" "<Shift>right")
               '(sj3-keys1)
	       '(key)
	       (N_ "[SJ3] extend segment")
	       (N_ "long description will be here"))

(define-custom 'sj3-shrink-segment-key '("<IgnoreCase><Control>i" "<Shift>left")
               '(sj3-keys1)
	       '(key)
	       (N_ "[SJ3] shrink segment")
	       (N_ "long description will be here"))

(define-custom 'sj3-transpose-as-hiragana-key '("F6" "Muhenkan")
               '(sj3-keys1)
	       '(key)
	       (N_ "[SJ3] convert to hiragana")
	       (N_ "long description will be here"))

(define-custom 'sj3-transpose-as-katakana-key '("F7" "Muhenkan")
               '(sj3-keys1)
	       '(key)
	       (N_ "[SJ3] convert to katakana")
	       (N_ "long description will be here"))

(define-custom 'sj3-transpose-as-halfkana-key '("F8" "Muhenkan")
               '(sj3-keys1)
	       '(key)
	       (N_ "[SJ3] convert to halfwidth katakana")
	       (N_ "long description will be here"))

(define-custom 'sj3-transpose-as-halfwidth-alnum-key '("F10")
	       '(sj3-keys1)
	       '(key)
	       (N_ "[SJ3] convert to halfwidth alphanumeric")
	       (N_ "long description will be here"))

(define-custom 'sj3-transpose-as-fullwidth-alnum-key '("F9")
	       '(sj3-keys1)
	       '(key)
	       (N_ "[SJ3] convert to fullwidth alphanumeric")
	       (N_ "long description will be here"))

(define-custom 'sj3-commit-as-opposite-kana-key '("<IgnoreCase><Shift>q")  ;; "Q"
               '(sj3-keys1)
	       '(key)
	       (N_ "[SJ3] commit as transposed kana")
	       (N_ "long description will be here"))

;;
;; overriding generic keys
;;
(define-custom 'sj3-on-key '("<Control>\\" generic-on-key)
               '(sj3-keys2)
	       '(key)
	       (N_ "[SJ3] on")
	       (N_ "long description will be here"))

;;(define-custom 'sj3-off-key '("l" generic-on-key)
(define-custom 'sj3-off-key '("<Control>\\" generic-off-key)
               '(sj3-keys2)
	       '(key)
	       (N_ "[SJ3] off")
	       (N_ "long description will be here"))

(define-custom 'sj3-begin-conv-key '(generic-begin-conv-key)
               '(sj3-keys2)
	       '(key)
	       (N_ "[SJ3] begin conversion")
	       (N_ "long description will be here"))

(define-custom 'sj3-commit-key '(generic-commit-key)
               '(sj3-keys2)
	       '(key)
	       (N_ "[SJ3] commit")
	       (N_ "long description will be here"))

(define-custom 'sj3-cancel-key '(generic-cancel-key)
               '(sj3-keys2)
	       '(key)
	       (N_ "[SJ3] cancel")
	       (N_ "long description will be here"))

(define-custom 'sj3-next-candidate-key '(generic-next-candidate-key)
               '(sj3-keys2)
	       '(key)
	       (N_ "[SJ3] next candidate")
	       (N_ "long description will be here"))

(define-custom 'sj3-prev-candidate-key '(generic-prev-candidate-key)
               '(sj3-keys2)
	       '(key)
	       (N_ "[SJ3] previous candidate")
	       (N_ "long description will be here"))

(define-custom 'sj3-next-page-key '(generic-next-page-key)
             '(sj3-keys2)
	       '(key)
	       (N_ "[SJ3] next page of candidate window")
	       (N_ "long description will be here"))

(define-custom 'sj3-prev-page-key '(generic-prev-page-key)
               '(sj3-keys2)
	       '(key)
	       (N_ "[SJ3] previous page of candidate window")
	       (N_ "long description will be here"))

;;
;; overriding generic keys (advanced)
;;
(define-custom 'sj3-beginning-of-preedit-key '(generic-beginning-of-preedit-key)
               '(sj3-keys3)
	       '(key)
	       (N_ "[SJ3] beginning of preedit")
	       (N_ "long description will be here"))

(define-custom 'sj3-end-of-preedit-key '(generic-end-of-preedit-key)
               '(sj3-keys3)
	       '(key)
	       (N_ "[SJ3] end of preedit")
	       (N_ "long description will be here"))

(define-custom 'sj3-kill-key '(generic-kill-key)
               '(sj3-keys3)
	       '(key)
	       (N_ "[SJ3] erase after cursor")
	       (N_ "long description will be here"))

(define-custom 'sj3-kill-backward-key '(generic-kill-backward-key)
               '(sj3-keys3)
	       '(key)
	       (N_ "[SJ3] erase before cursor")
	       (N_ "long description will be here"))

(define-custom 'sj3-backspace-key '(generic-backspace-key)
               '(sj3-keys3)
	       '(key)
	       (N_ "[SJ3] backspace")
	       (N_ "long description will be here"))

(define-custom 'sj3-delete-key '(generic-delete-key)
               '(sj3-keys3)
	       '(key)
	       (N_ "[SJ3] delete")
	       (N_ "long description will be here"))

(define-custom 'sj3-go-left-key '(generic-go-left-key)
               '(sj3-keys3)
	       '(key)
	       (N_ "[SJ3] go left")
	       (N_ "long description will be here"))

(define-custom 'sj3-go-right-key '(generic-go-right-key)
               '(sj3-keys3)
	       '(key)
	       (N_ "[SJ3] go right")
	       (N_ "long description will be here"))

(define-custom 'sj3-vi-escape-key '("escape" "<Control>[")
               '(sj3-keys3)
	       '(key)
	       (N_ "[SJ3] ESC keys on vi-cooperative mode")
	       (N_ "long description will be here"))

;;
;; ja advanced
;;

(define-custom 'sj3-hiragana-key '("<Shift>F6")
	       '(sj3-keys4 mode-transition)
	       '(key)
	       (N_ "[SJ3] hiragana mode")
	       (N_ "long description will be here"))

(define-custom 'sj3-katakana-key '("<Shift>F7")
	       '(sj3-keys4 mode-transition)
	       '(key)
	       (N_ "[SJ3] katakana mode")
	       (N_ "long description will be here"))

(define-custom 'sj3-halfkana-key '("<Shift>F8")
	       '(sj3-keys4 mode-transition)
	       '(key)
	       (N_ "[SJ3] halfwidth katakana mode")
	       (N_ "long description will be here"))

(define-custom 'sj3-halfwidth-alnum-key '("<Shift>F10")
	       '(sj3-keys4 mode-transition)
	       '(key)
	       (N_ "[SJ3] halfwidth alphanumeric mode")
	       (N_ "long description will be here"))

(define-custom 'sj3-fullwidth-alnum-key '("<Shift>F9")
	       '(sj3-keys4 mode-transition)
	       '(key)
	       (N_ "[SJ3] fullwidth alphanumeric mode")
	       (N_ "long description will be here"))

(define-custom 'sj3-kana-toggle-key '()
               '(sj3-keys4 advanced)
	       '(key)
	       (N_ "[SJ3] toggle hiragana/katakana mode")
	       (N_ "long description will be here"))

(define-custom 'sj3-alkana-toggle-key '()
	       '(sj3-keys4 advanced)
	       '(key)
	       (N_ "[SJ3] toggle kana/alphanumeric mode")
	       (N_ "long description will be here"))

(define-custom 'sj3-next-prediction-key '("tab" "down" "<IgnoreCase><Control>n" "<IgnoreCase><Control>i")
               '(sj3-keys4 sj3-prediction)
               '(key)
               (N_ "[SJ3] Next prediction candidate")
               (N_ "long description will be here"))

(define-custom 'sj3-prev-prediction-key '(generic-prev-candidate-key)
               '(sj3-keys4 sj3-prediction)
               '(key)
               (N_ "[SJ3] Previous prediction candidate")
               (N_ "long description will be here"))
