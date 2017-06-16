;;; baidu-olime-jp-key-custom.scm: Customization variables for baidu-olime-jp.scm
;;;
;;; Copyright (c) 2012- uim Project https://github.com/uim/uim
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

(define-custom-group 'baidu-olime-jp-keys1
		     (N_ "Baidu-OnlineIME-Jp key bindings 1")
		     (N_ "long description will be here."))

(define-custom-group 'baidu-olime-jp-keys2
		     (N_ "Baidu-OnlineIME-Jp key bindings 2")
		     (N_ "long description will be here."))

(define-custom-group 'baidu-olime-jp-keys3
		     (N_ "Baidu-OnlineIME-Jp key bindings 3")
		     (N_ "long description will be here."))

(define-custom-group 'baidu-olime-jp-keys4
		     (N_ "Baidu-OnlineIME-Jp key bindings 4")
		     (N_ "long description will be here."))

(define-custom 'baidu-olime-jp-next-segment-key '(generic-go-right-key)
               '(baidu-olime-jp-keys1)
	       '(key)
	       (N_ "[Baidu-OnlineIME-Jp] next segment")
	       (N_ "long description will be here"))

(define-custom 'baidu-olime-jp-prev-segment-key '(generic-go-left-key)
               '(baidu-olime-jp-keys1)
	       '(key)
	       (N_ "[Baidu-OnlineIME-Jp] previous segment")
	       (N_ "long description will be here"))

(define-custom 'baidu-olime-jp-extend-segment-key '("<IgnoreCase><Control>o" "<Shift>right")
               '(baidu-olime-jp-keys1)
	       '(key)
	       (N_ "[Baidu-OnlineIME-Jp] extend segment")
	       (N_ "long description will be here"))

(define-custom 'baidu-olime-jp-shrink-segment-key '("<IgnoreCase><Control>i" "<Shift>left")
               '(baidu-olime-jp-keys1)
	       '(key)
	       (N_ "[Baidu-OnlineIME-Jp] shrink segment")
	       (N_ "long description will be here"))

(define-custom 'baidu-olime-jp-transpose-as-hiragana-key '("F6" "Muhenkan")
               '(baidu-olime-jp-keys1)
	       '(key)
	       (N_ "[Baidu-OnlineIME-Jp] convert to hiragana")
	       (N_ "long description will be here"))

(define-custom 'baidu-olime-jp-transpose-as-katakana-key '("F7" "Muhenkan")
               '(baidu-olime-jp-keys1)
	       '(key)
	       (N_ "[Baidu-OnlineIME-Jp] convert to katakana")
	       (N_ "long description will be here"))

(define-custom 'baidu-olime-jp-transpose-as-halfkana-key '("F8" "Muhenkan")
               '(baidu-olime-jp-keys1)
	       '(key)
	       (N_ "[Baidu-OnlineIME-Jp] convert to halfwidth katakana")
	       (N_ "long description will be here"))

(define-custom 'baidu-olime-jp-transpose-as-halfwidth-alnum-key '("F10")
	       '(baidu-olime-jp-keys1)
	       '(key)
	       (N_ "[Baidu-OnlineIME-Jp] convert to halfwidth alphanumeric")
	       (N_ "long description will be here"))

(define-custom 'baidu-olime-jp-transpose-as-fullwidth-alnum-key '("F9")
	       '(baidu-olime-jp-keys1)
	       '(key)
	       (N_ "[Baidu-OnlineIME-Jp] convert to fullwidth alphanumeric")
	       (N_ "long description will be here"))

(define-custom 'baidu-olime-jp-commit-as-opposite-kana-key '("<IgnoreCase><Shift>q")  ;; "Q"
               '(baidu-olime-jp-keys1)
	       '(key)
	       (N_ "[Baidu-OnlineIME-Jp] commit as transposed kana")
	       (N_ "long description will be here"))

;;
;; overriding generic keys
;;
(define-custom 'baidu-olime-jp-on-key '("<Control>\\" generic-on-key)
               '(baidu-olime-jp-keys2)
	       '(key)
	       (N_ "[Baidu-OnlineIME-Jp] on")
	       (N_ "long description will be here"))

;;(define-custom 'baidu-olime-jp-off-key '("l" generic-on-key)
(define-custom 'baidu-olime-jp-off-key '("<Control>\\" generic-off-key)
               '(baidu-olime-jp-keys2)
	       '(key)
	       (N_ "[Baidu-OnlineIME-Jp] off")
	       (N_ "long description will be here"))

(define-custom 'baidu-olime-jp-begin-conv-key '(generic-begin-conv-key)
               '(baidu-olime-jp-keys2)
	       '(key)
	       (N_ "[Baidu-OnlineIME-Jp] begin conversion")
	       (N_ "long description will be here"))

(define-custom 'baidu-olime-jp-commit-key '(generic-commit-key)
               '(baidu-olime-jp-keys2)
	       '(key)
	       (N_ "[Baidu-OnlineIME-Jp] commit")
	       (N_ "long description will be here"))

(define-custom 'baidu-olime-jp-cancel-key '(generic-cancel-key)
               '(baidu-olime-jp-keys2)
	       '(key)
	       (N_ "[Baidu-OnlineIME-Jp] cancel")
	       (N_ "long description will be here"))

(define-custom 'baidu-olime-jp-next-candidate-key '(generic-next-candidate-key)
               '(baidu-olime-jp-keys2)
	       '(key)
	       (N_ "[Baidu-OnlineIME-Jp] next candidate")
	       (N_ "long description will be here"))

(define-custom 'baidu-olime-jp-prev-candidate-key '(generic-prev-candidate-key)
               '(baidu-olime-jp-keys2)
	       '(key)
	       (N_ "[Baidu-OnlineIME-Jp] previous candidate")
	       (N_ "long description will be here"))

(define-custom 'baidu-olime-jp-next-page-key '(generic-next-page-key)
             '(baidu-olime-jp-keys2)
	       '(key)
	       (N_ "[Baidu-OnlineIME-Jp] next page of candidate window")
	       (N_ "long description will be here"))

(define-custom 'baidu-olime-jp-prev-page-key '(generic-prev-page-key)
               '(baidu-olime-jp-keys2)
	       '(key)
	       (N_ "[Baidu-OnlineIME-Jp] previous page of candidate window")
	       (N_ "long description will be here"))

;;
;; overriding generic keys (advanced)
;;
(define-custom 'baidu-olime-jp-beginning-of-preedit-key '(generic-beginning-of-preedit-key)
               '(baidu-olime-jp-keys3)
	       '(key)
	       (N_ "[Baidu-OnlineIME-Jp] beginning of preedit")
	       (N_ "long description will be here"))

(define-custom 'baidu-olime-jp-end-of-preedit-key '(generic-end-of-preedit-key)
               '(baidu-olime-jp-keys3)
	       '(key)
	       (N_ "[Baidu-OnlineIME-Jp] end of preedit")
	       (N_ "long description will be here"))

(define-custom 'baidu-olime-jp-kill-key '(generic-kill-key)
               '(baidu-olime-jp-keys3)
	       '(key)
	       (N_ "[Baidu-OnlineIME-Jp] erase after cursor")
	       (N_ "long description will be here"))

(define-custom 'baidu-olime-jp-kill-backward-key '(generic-kill-backward-key)
               '(baidu-olime-jp-keys3)
	       '(key)
	       (N_ "[Baidu-OnlineIME-Jp] erase before cursor")
	       (N_ "long description will be here"))

(define-custom 'baidu-olime-jp-backspace-key '(generic-backspace-key)
               '(baidu-olime-jp-keys3)
	       '(key)
	       (N_ "[Baidu-OnlineIME-Jp] backspace")
	       (N_ "long description will be here"))

(define-custom 'baidu-olime-jp-delete-key '(generic-delete-key)
               '(baidu-olime-jp-keys3)
	       '(key)
	       (N_ "[Baidu-OnlineIME-Jp] delete")
	       (N_ "long description will be here"))

(define-custom 'baidu-olime-jp-go-left-key '(generic-go-left-key)
               '(baidu-olime-jp-keys3)
	       '(key)
	       (N_ "[Baidu-OnlineIME-Jp] go left")
	       (N_ "long description will be here"))

(define-custom 'baidu-olime-jp-go-right-key '(generic-go-right-key)
               '(baidu-olime-jp-keys3)
	       '(key)
	       (N_ "[Baidu-OnlineIME-Jp] go right")
	       (N_ "long description will be here"))

(define-custom 'baidu-olime-jp-vi-escape-key '("escape" "<Control>[")
               '(baidu-olime-jp-keys3)
	       '(key)
	       (N_ "[Baidu-OnlineIME-Jp] ESC keys on vi-cooperative mode")
	       (N_ "long description will be here"))

;;
;; ja advanced
;;

(define-custom 'baidu-olime-jp-hiragana-key '("<Shift>F6")
	       '(baidu-olime-jp-keys4 mode-transition)
	       '(key)
	       (N_ "[Baidu-OnlineIME-Jp] hiragana mode")
	       (N_ "long description will be here"))

(define-custom 'baidu-olime-jp-katakana-key '("<Shift>F7")
	       '(baidu-olime-jp-keys4 mode-transition)
	       '(key)
	       (N_ "[Baidu-OnlineIME-Jp] katakana mode")
	       (N_ "long description will be here"))

(define-custom 'baidu-olime-jp-halfkana-key '("<Shift>F8")
	       '(baidu-olime-jp-keys4 mode-transition)
	       '(key)
	       (N_ "[Baidu-OnlineIME-Jp] halfwidth katakana mode")
	       (N_ "long description will be here"))

(define-custom 'baidu-olime-jp-halfwidth-alnum-key '("<Shift>F10")
	       '(baidu-olime-jp-keys4 mode-transition)
	       '(key)
	       (N_ "[Baidu-OnlineIME-Jp] halfwidth alphanumeric mode")
	       (N_ "long description will be here"))

(define-custom 'baidu-olime-jp-fullwidth-alnum-key '("<Shift>F9")
	       '(baidu-olime-jp-keys4 mode-transition)
	       '(key)
	       (N_ "[Baidu-OnlineIME-Jp] fullwidth alphanumeric mode")
	       (N_ "long description will be here"))

(define-custom 'baidu-olime-jp-kana-toggle-key '()
               '(baidu-olime-jp-keys4 advanced)
	       '(key)
	       (N_ "[Baidu-OnlineIME-Jp] toggle hiragana/katakana mode")
	       (N_ "long description will be here"))

(define-custom 'baidu-olime-jp-alkana-toggle-key '()
	       '(baidu-olime-jp-keys4 advanced)
	       '(key)
	       (N_ "[Baidu-OnlineIME-Jp] toggle kana/alphanumeric mode")
	       (N_ "long description will be here"))

(define-custom 'baidu-olime-jp-next-prediction-key '("tab" "down" "<IgnoreCase><Control>n" "<IgnoreCase><Control>i")
               '(baidu-olime-jp-keys4 baidu-olime-jp-prediction)
               '(key)
               (N_ "[Baidu-OnlineIME-Jp] Next prediction candidate")
               (N_ "long description will be here"))

(define-custom 'baidu-olime-jp-prev-prediction-key '(generic-prev-candidate-key)
               '(baidu-olime-jp-keys4 baidu-olime-jp-prediction)
               '(key)
               (N_ "[Baidu-OnlineIME-Jp] Previous prediction candidate")
               (N_ "long description will be here"))
