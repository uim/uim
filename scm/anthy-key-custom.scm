;;; anthy-custom.scm: Customization variables for anthy.scm
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
;;; THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
;;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
;;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
;;; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;;; SUCH DAMAGE.
;;;;

(require "i18n.scm")


(define-custom-group 'anthy-keys
		     (_ "Anthy key bindings")
		     (_ "long description will be here."))

(define-custom-group 'anthy-keys-advanced1
		     (_ "Anthy advanced key bindings (1)")
		     (_ "long description will be here."))

(define-custom-group 'anthy-keys-advanced2
		     (_ "Anthy advanced key bindings (2)")
		     (_ "long description will be here."))


(define-custom 'anthy-next-segment-key '(generic-go-right-key)
               '(anthy-keys)
	       '(key)
	       (_ "[Anthy] next segment")
	       (_ "long description will be here"))

(define-custom 'anthy-prev-segment-key '(generic-go-left-key)
               '(anthy-keys)
	       '(key)
	       (_ "[Anthy] previous segment")
	       (_ "long description will be here"))

(define-custom 'anthy-extend-segment-key '("<IgnoreCase><Control>o" "<Shift>right")
               '(anthy-keys)
	       '(key)
	       (_ "[Anthy] extend segment")
	       (_ "long description will be here"))

(define-custom 'anthy-shrink-segment-key '("<IgnoreCase><Control>i" "<Shift>left")
               '(anthy-keys)
	       '(key)
	       (_ "[Anthy] shrink segment")
	       (_ "long description will be here"))

(define-custom 'anthy-commit-as-latin-key '("F10")
               '(anthy-keys)
	       '(key)
	       (_ "[Anthy] commit preedit string as halfwidth alphanumeric")
	       (_ "long description will be here"))

(define-custom 'anthy-commit-as-wide-latin-key '("F9")
               '(anthy-keys)
	       '(key)
	       (_ "[Anthy] commit preedit string as fullwidth alphanumeric")
	       (_ "long description will be here"))

(define-custom 'anthy-commit-as-katakana-key '("F7")
               '(anthy-keys)
	       '(key)
	       (_ "[Anthy] commit preedit string as katakana")
	       (_ "long description will be here"))

(define-custom 'anthy-commit-as-hankana-key '("F8")
               '(anthy-keys)
	       '(key)
	       (_ "[Anthy] commit preedit string as halfwidth katakana")
	       (_ "long description will be here"))

(define-custom 'anthy-commit-as-opposite-kana-key '()
               '(anthy-keys)
	       '(key)
	       (_ "[Anthy] commit preedit string as transposed kana")
	       (_ "long description will be here"))

(define-custom 'anthy-wide-latin-key '()
               '(anthy-keys)
	       '(key)
	       (_ "[Anthy] fullwidth alphanumeric mode")
	       (_ "long description will be here"))

;(define-custom 'anthy-hankaku-kana-key '("<IgnoreCase><Control>q")
(define-custom 'anthy-hankaku-kana-key '()
               '(anthy-keys)
	       '(key)
	       (_ "[Anthy] halfwidth katakana mode")
	       (_ "long description will be here"))

(define-custom 'anthy-kana-toggle-key '()
               '(anthy-keys)
	       '(key)
	       (_ "[Anthy] toggle hiragana/katakana mode")
	       (_ "long description will be here"))


;;
;; overriding generic keys
;;

(define-custom 'anthy-on-key '("<IgnoreCase><Control>j" generic-on-key)
               '(anthy-keys-advanced1)
	       '(key)
	       (_ "[Anthy] on")
	       (_ "long description will be here"))

(define-custom 'anthy-latin-key '("<IgnoreCase><Control>j" generic-off-key)
               '(anthy-keys-advanced1)
	       '(key)
	       (_ "[Anthy] off")
	       (_ "long description will be here"))

(define-custom 'anthy-begin-conv-key '(generic-begin-conv-key)
               '(anthy-keys-advanced1)
	       '(key)
	       (_ "[Anthy] begin conversion")
	       (_ "long description will be here"))

(define-custom 'anthy-commit-key '(generic-commit-key)
               '(anthy-keys-advanced1)
	       '(key)
	       (_ "[Anthy] commit")
	       (_ "long description will be here"))

(define-custom 'anthy-cancel-key '(generic-cancel-key)
               '(anthy-keys-advanced1)
	       '(key)
	       (_ "[Anthy] cancel")
	       (_ "long description will be here"))

(define-custom 'anthy-next-candidate-key '(generic-next-candidate-key)
               '(anthy-keys-advanced1)
	       '(key)
	       (_ "[Anthy] next candidate")
	       (_ "long description will be here"))

(define-custom 'anthy-prev-candidate-key '(generic-prev-candidate-key)
               '(anthy-keys-advanced1)
	       '(key)
	       (_ "[Anthy] previous candidate")
	       (_ "long description will be here"))

(define-custom 'anthy-next-page-key '(generic-next-page-key)
               '(anthy-keys-advanced1)
	       '(key)
	       (_ "[Anthy] next page of candidate window")
	       (_ "long description will be here"))

(define-custom 'anthy-prev-page-key '(generic-prev-page-key)
               '(anthy-keys-advanced1)
	       '(key)
	       (_ "[Anthy] previous page of candidate window")
	       (_ "long description will be here"))

;;
;; overriding generic keys (advanced)
;;

(define-custom 'anthy-beginning-of-preedit-key '(generic-beginning-of-preedit-key)
               '(anthy-keys-advanced2)
	       '(key)
	       (_ "[Anthy] beginning of preedit")
	       (_ "long description will be here"))

(define-custom 'anthy-end-of-preedit-key '(generic-end-of-preedit-key)
               '(anthy-keys-advanced2)
	       '(key)
	       (_ "[Anthy] end of preedit")
	       (_ "long description will be here"))

(define-custom 'anthy-kill-key '(generic-kill-key)
               '(anthy-keys-advanced2)
	       '(key)
	       (_ "[Anthy] erase after cursor")
	       (_ "long description will be here"))

(define-custom 'anthy-kill-backward-key '(generic-kill-backward-key)
               '(anthy-keys-advanced2)
	       '(key)
	       (_ "[Anthy] erase before cursor")
	       (_ "long description will be here"))

(define-custom 'anthy-backspace-key '(generic-backspace-key)
               '(anthy-keys-advanced2)
	       '(key)
	       (_ "[Anthy] backspace")
	       (_ "long description will be here"))

(define-custom 'anthy-delete-key '(generic-delete-key)
               '(anthy-keys-advanced2)
	       '(key)
	       (_ "[Anthy] delete")
	       (_ "long description will be here"))

(define-custom 'anthy-go-left-key '(generic-go-left-key)
               '(anthy-keys-advanced2)
	       '(key)
	       (_ "[Anthy] go left")
	       (_ "long description will be here"))

(define-custom 'anthy-go-right-key '(generic-go-right-key)
               '(anthy-keys-advanced2)
	       '(key)
	       (_ "[Anthy] go right")
	       (_ "long description will be here"))
