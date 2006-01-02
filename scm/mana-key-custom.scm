;;; mana-custom.scm: Customization variables for mana.scm
;;;
;;; Copyright (c) 2003-2005 uim Project http://uim.freedesktop.org/
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
		     (_ "Mana key bindings 1")
		     (_ "long description will be here."))

(define-custom-group 'mana-keys2
		     (_ "Mana key bindings 2")
		     (_ "long description will be here."))

(define-custom-group 'mana-keys3
		     (_ "Mana key bindings 3")
		     (_ "long description will be here."))


(define-custom 'mana-next-segment-key '(generic-go-right-key)
               '(mana-keys1)
	       '(key)
	       (_ "[Mana] next segment")
	       (_ "long description will be here"))

(define-custom 'mana-prev-segment-key '(generic-go-left-key)
               '(mana-keys1)
	       '(key)
	       (_ "[Mana] previous segment")
	       (_ "long description will be here"))

(define-custom 'mana-extend-segment-key '("<IgnoreCase><Control>o" "<Shift>right")
               '(mana-keys1)
	       '(key)
	       (_ "[Mana] extend segment")
	       (_ "long description will be here"))

(define-custom 'mana-shrink-segment-key '("<IgnoreCase><Control>i" "<Shift>left")
               '(mana-keys1)
	       '(key)
	       (_ "[Mana] shrink segment")
	       (_ "long description will be here"))

(define-custom 'mana-transpose-as-latin-key '("F10")
               '(mana-keys1)
	       '(key)
	       (_ "[Mana] convert to halfwidth alphanumeric")
	       (_ "long description will be here"))

(define-custom 'mana-transpose-as-wide-latin-key '("F9")
               '(mana-keys1)
	       '(key)
	       (_ "[Mana] convert to fullwidth alphanumeric")
	       (_ "long description will be here"))

(define-custom 'mana-transpose-as-hiragana-key '("F6")
               '(mana-keys1)
	       '(key)
	       (_ "[Mana] convert to hiragana")
	       (_ "long description will be here"))

(define-custom 'mana-transpose-as-katakana-key '("F7")
               '(mana-keys1)
	       '(key)
	       (_ "[Mana] convert to katakana")
	       (_ "long description will be here"))

(define-custom 'mana-transpose-as-hankana-key '("F8")
               '(mana-keys1)
	       '(key)
	       (_ "[Mana] convert to halfwidth katakana")
	       (_ "long description will be here"))

(define-custom 'mana-commit-as-opposite-kana-key '()
               '(mana-keys1)
	       '(key)
	       (_ "[Mana] commit as transposed kana")
	       (_ "long description will be here"))

(define-custom 'mana-wide-latin-key '()
               '(mana-keys1)
	       '(key)
	       (_ "[Mana] fullwidth alphanumeric mode")
	       (_ "long description will be here"))

;(define-custom 'mana-hankaku-kana-key '("<IgnoreCase><Control>q")
(define-custom 'mana-hankaku-kana-key '()
               '(mana-keys1)
	       '(key)
	       (_ "[Mana] halfwidth katakana mode")
	       (_ "long description will be here"))

(define-custom 'mana-kana-toggle-key '()
               '(mana-keys1)
	       '(key)
	       (_ "[Mana] toggle hiragana/katakana mode")
	       (_ "long description will be here"))


;;
;; overriding generic keys
;;

(define-custom 'mana-on-key '("<IgnoreCase><Control>j" generic-on-key)
               '(mana-keys2)
	       '(key)
	       (_ "[Mana] on")
	       (_ "long description will be here"))

(define-custom 'mana-latin-key '("<IgnoreCase><Control>j" generic-off-key)
               '(mana-keys2)
	       '(key)
	       (_ "[Mana] off")
	       (_ "long description will be here"))

(define-custom 'mana-begin-conv-key '(generic-begin-conv-key)
               '(mana-keys2)
	       '(key)
	       (_ "[Mana] begin conversion")
	       (_ "long description will be here"))

(define-custom 'mana-commit-key '(generic-commit-key)
               '(mana-keys2)
	       '(key)
	       (_ "[Mana] commit")
	       (_ "long description will be here"))

(define-custom 'mana-cancel-key '(generic-cancel-key)
               '(mana-keys2)
	       '(key)
	       (_ "[Mana] cancel")
	       (_ "long description will be here"))

(define-custom 'mana-next-candidate-key '(generic-next-candidate-key)
               '(mana-keys2)
	       '(key)
	       (_ "[Mana] next candidate")
	       (_ "long description will be here"))

(define-custom 'mana-prev-candidate-key '(generic-prev-candidate-key)
               '(mana-keys2)
	       '(key)
	       (_ "[Mana] previous candidate")
	       (_ "long description will be here"))

(define-custom 'mana-next-page-key '(generic-next-page-key)
               '(mana-keys2)
	       '(key)
	       (_ "[Mana] next page of candidate window")
	       (_ "long description will be here"))

(define-custom 'mana-prev-page-key '(generic-prev-page-key)
               '(mana-keys2)
	       '(key)
	       (_ "[Mana] previous page of candidate window")
	       (_ "long description will be here"))

;;
;; overriding generic keys (advanced)
;;

(define-custom 'mana-beginning-of-preedit-key '(generic-beginning-of-preedit-key)
               '(mana-keys3)
	       '(key)
	       (_ "[Mana] beginning of preedit")
	       (_ "long description will be here"))

(define-custom 'mana-end-of-preedit-key '(generic-end-of-preedit-key)
               '(mana-keys3)
	       '(key)
	       (_ "[Mana] end of preedit")
	       (_ "long description will be here"))

(define-custom 'mana-kill-key '(generic-kill-key)
               '(mana-keys3)
	       '(key)
	       (_ "[Mana] erase after cursor")
	       (_ "long description will be here"))

(define-custom 'mana-kill-backward-key '(generic-kill-backward-key)
               '(mana-keys3)
	       '(key)
	       (_ "[Mana] erase before cursor")
	       (_ "long description will be here"))

(define-custom 'mana-backspace-key '(generic-backspace-key)
               '(mana-keys3)
	       '(key)
	       (_ "[Mana] backspace")
	       (_ "long description will be here"))

(define-custom 'mana-delete-key '(generic-delete-key)
               '(mana-keys3)
	       '(key)
	       (_ "[Mana] delete")
	       (_ "long description will be here"))

(define-custom 'mana-go-left-key '(generic-go-left-key)
               '(mana-keys3)
	       '(key)
	       (_ "[Mana] go left")
	       (_ "long description will be here"))

(define-custom 'mana-go-right-key '(generic-go-right-key)
               '(mana-keys3)
	       '(key)
	       (_ "[Mana] go right")
	       (_ "long description will be here"))

(define-custom 'mana-vi-escape-key '("escape" "<Control>[")
               '(mana-keys3)
	       '(key)
	       (_ "[Mana] ESC keys on vi-cooperative mode")
	       (_ "long description will be here"))
