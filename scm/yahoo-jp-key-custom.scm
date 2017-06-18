;;; yahoo-jp-custom.scm: Customization variables for yahoo-jp.scm
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

(define-custom-group 'yahoo-jp-keys1
		     (N_ "Yahoo-Jp key bindings 1")
		     (N_ "long description will be here."))

(define-custom-group 'yahoo-jp-keys2
		     (N_ "Yahoo-Jp key bindings 2")
		     (N_ "long description will be here."))

(define-custom-group 'yahoo-jp-keys3
		     (N_ "Yahoo-Jp key bindings 3")
		     (N_ "long description will be here."))

(define-custom-group 'yahoo-jp-keys4
		     (N_ "Yahoo-Jp key bindings 4")
		     (N_ "long description will be here."))

(define-custom 'yahoo-jp-next-segment-key '(generic-go-right-key)
               '(yahoo-jp-keys1)
	       '(key)
	       (N_ "[Yahoo-Jp] next segment")
	       (N_ "long description will be here"))

(define-custom 'yahoo-jp-prev-segment-key '(generic-go-left-key)
               '(yahoo-jp-keys1)
	       '(key)
	       (N_ "[Yahoo-Jp] previous segment")
	       (N_ "long description will be here"))

(define-custom 'yahoo-jp-extend-segment-key '("<IgnoreCase><Control>o" "<Shift>right")
               '(yahoo-jp-keys1)
	       '(key)
	       (N_ "[Yahoo-Jp] extend segment")
	       (N_ "long description will be here"))

(define-custom 'yahoo-jp-shrink-segment-key '("<IgnoreCase><Control>i" "<Shift>left")
               '(yahoo-jp-keys1)
	       '(key)
	       (N_ "[Yahoo-Jp] shrink segment")
	       (N_ "long description will be here"))

(define-custom 'yahoo-jp-transpose-as-hiragana-key '("F6" "Muhenkan")
               '(yahoo-jp-keys1)
	       '(key)
	       (N_ "[Yahoo-Jp] convert to hiragana")
	       (N_ "long description will be here"))

(define-custom 'yahoo-jp-transpose-as-katakana-key '("F7" "Muhenkan")
               '(yahoo-jp-keys1)
	       '(key)
	       (N_ "[Yahoo-Jp] convert to katakana")
	       (N_ "long description will be here"))

(define-custom 'yahoo-jp-transpose-as-halfkana-key '("F8" "Muhenkan")
               '(yahoo-jp-keys1)
	       '(key)
	       (N_ "[Yahoo-Jp] convert to halfwidth katakana")
	       (N_ "long description will be here"))

(define-custom 'yahoo-jp-transpose-as-halfwidth-alnum-key '("F10")
	       '(yahoo-jp-keys1)
	       '(key)
	       (N_ "[Yahoo-Jp] convert to halfwidth alphanumeric")
	       (N_ "long description will be here"))

(define-custom 'yahoo-jp-transpose-as-fullwidth-alnum-key '("F9")
	       '(yahoo-jp-keys1)
	       '(key)
	       (N_ "[Yahoo-Jp] convert to fullwidth alphanumeric")
	       (N_ "long description will be here"))

(define-custom 'yahoo-jp-commit-as-opposite-kana-key '("<IgnoreCase><Shift>q")  ;; "Q"
               '(yahoo-jp-keys1)
	       '(key)
	       (N_ "[Yahoo-Jp] commit as transposed kana")
	       (N_ "long description will be here"))

;;
;; overriding generic keys
;;
(define-custom 'yahoo-jp-on-key '("<Control>\\" generic-on-key)
               '(yahoo-jp-keys2)
	       '(key)
	       (N_ "[Yahoo-Jp] on")
	       (N_ "long description will be here"))

;;(define-custom 'yahoo-jp-off-key '("l" generic-on-key)
(define-custom 'yahoo-jp-off-key '("<Control>\\" generic-off-key)
               '(yahoo-jp-keys2)
	       '(key)
	       (N_ "[Yahoo-Jp] off")
	       (N_ "long description will be here"))

(define-custom 'yahoo-jp-begin-conv-key '(generic-begin-conv-key)
               '(yahoo-jp-keys2)
	       '(key)
	       (N_ "[Yahoo-Jp] begin conversion")
	       (N_ "long description will be here"))

(define-custom 'yahoo-jp-commit-key '(generic-commit-key)
               '(yahoo-jp-keys2)
	       '(key)
	       (N_ "[Yahoo-Jp] commit")
	       (N_ "long description will be here"))

(define-custom 'yahoo-jp-cancel-key '(generic-cancel-key)
               '(yahoo-jp-keys2)
	       '(key)
	       (N_ "[Yahoo-Jp] cancel")
	       (N_ "long description will be here"))

(define-custom 'yahoo-jp-next-candidate-key '(generic-next-candidate-key)
               '(yahoo-jp-keys2)
	       '(key)
	       (N_ "[Yahoo-Jp] next candidate")
	       (N_ "long description will be here"))

(define-custom 'yahoo-jp-prev-candidate-key '(generic-prev-candidate-key)
               '(yahoo-jp-keys2)
	       '(key)
	       (N_ "[Yahoo-Jp] previous candidate")
	       (N_ "long description will be here"))

(define-custom 'yahoo-jp-next-page-key '(generic-next-page-key)
             '(yahoo-jp-keys2)
	       '(key)
	       (N_ "[Yahoo-Jp] next page of candidate window")
	       (N_ "long description will be here"))

(define-custom 'yahoo-jp-prev-page-key '(generic-prev-page-key)
               '(yahoo-jp-keys2)
	       '(key)
	       (N_ "[Yahoo-Jp] previous page of candidate window")
	       (N_ "long description will be here"))

;;
;; overriding generic keys (advanced)
;;
(define-custom 'yahoo-jp-beginning-of-preedit-key '(generic-beginning-of-preedit-key)
               '(yahoo-jp-keys3)
	       '(key)
	       (N_ "[Yahoo-Jp] beginning of preedit")
	       (N_ "long description will be here"))

(define-custom 'yahoo-jp-end-of-preedit-key '(generic-end-of-preedit-key)
               '(yahoo-jp-keys3)
	       '(key)
	       (N_ "[Yahoo-Jp] end of preedit")
	       (N_ "long description will be here"))

(define-custom 'yahoo-jp-kill-key '(generic-kill-key)
               '(yahoo-jp-keys3)
	       '(key)
	       (N_ "[Yahoo-Jp] erase after cursor")
	       (N_ "long description will be here"))

(define-custom 'yahoo-jp-kill-backward-key '(generic-kill-backward-key)
               '(yahoo-jp-keys3)
	       '(key)
	       (N_ "[Yahoo-Jp] erase before cursor")
	       (N_ "long description will be here"))

(define-custom 'yahoo-jp-backspace-key '(generic-backspace-key)
               '(yahoo-jp-keys3)
	       '(key)
	       (N_ "[Yahoo-Jp] backspace")
	       (N_ "long description will be here"))

(define-custom 'yahoo-jp-delete-key '(generic-delete-key)
               '(yahoo-jp-keys3)
	       '(key)
	       (N_ "[Yahoo-Jp] delete")
	       (N_ "long description will be here"))

(define-custom 'yahoo-jp-go-left-key '(generic-go-left-key)
               '(yahoo-jp-keys3)
	       '(key)
	       (N_ "[Yahoo-Jp] go left")
	       (N_ "long description will be here"))

(define-custom 'yahoo-jp-go-right-key '(generic-go-right-key)
               '(yahoo-jp-keys3)
	       '(key)
	       (N_ "[Yahoo-Jp] go right")
	       (N_ "long description will be here"))

(define-custom 'yahoo-jp-vi-escape-key '("escape" "<Control>[")
               '(yahoo-jp-keys3)
	       '(key)
	       (N_ "[Yahoo-Jp] ESC keys on vi-cooperative mode")
	       (N_ "long description will be here"))

;;
;; ja advanced
;;

(define-custom 'yahoo-jp-hiragana-key '("<Shift>F6")
	       '(yahoo-jp-keys4 mode-transition)
	       '(key)
	       (N_ "[Yahoo-Jp] hiragana mode")
	       (N_ "long description will be here"))

(define-custom 'yahoo-jp-katakana-key '("<Shift>F7")
	       '(yahoo-jp-keys4 mode-transition)
	       '(key)
	       (N_ "[Yahoo-Jp] katakana mode")
	       (N_ "long description will be here"))

(define-custom 'yahoo-jp-halfkana-key '("<Shift>F8")
	       '(yahoo-jp-keys4 mode-transition)
	       '(key)
	       (N_ "[Yahoo-Jp] halfwidth katakana mode")
	       (N_ "long description will be here"))

(define-custom 'yahoo-jp-halfwidth-alnum-key '("<Shift>F10")
	       '(yahoo-jp-keys4 mode-transition)
	       '(key)
	       (N_ "[Yahoo-Jp] halfwidth alphanumeric mode")
	       (N_ "long description will be here"))

(define-custom 'yahoo-jp-fullwidth-alnum-key '("<Shift>F9")
	       '(yahoo-jp-keys4 mode-transition)
	       '(key)
	       (N_ "[Yahoo-Jp] fullwidth alphanumeric mode")
	       (N_ "long description will be here"))

(define-custom 'yahoo-jp-kana-toggle-key '()
               '(yahoo-jp-keys4 advanced)
	       '(key)
	       (N_ "[Yahoo-Jp] toggle hiragana/katakana mode")
	       (N_ "long description will be here"))

(define-custom 'yahoo-jp-alkana-toggle-key '()
	       '(yahoo-jp-keys4 advanced)
	       '(key)
	       (N_ "[Yahoo-Jp] toggle kana/alphanumeric mode")
	       (N_ "long description will be here"))

(define-custom 'yahoo-jp-next-prediction-key '("tab" "down" "<IgnoreCase><Control>n" "<IgnoreCase><Control>i")
               '(yahoo-jp-keys4 yahoo-jp-prediction)
               '(key)
               (N_ "[Yahoo-Jp] Next prediction candidate")
               (N_ "long description will be here"))

(define-custom 'yahoo-jp-prev-prediction-key '(generic-prev-candidate-key)
               '(yahoo-jp-keys4 yahoo-jp-prediction)
               '(key)
               (N_ "[Yahoo-Jp] Previous prediction candidate")
               (N_ "long description will be here"))
