;;; canna-custom.scm: Customization variables for canna.scm
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


(define-custom-group 'canna-keys
		     (_ "Canna key bindings")
		     (_ "long description will be here."))

(define-custom-group 'canna-keys-advanced1
		     (_ "Canna advanced key bindings (1)")
		     (_ "long description will be here."))

(define-custom-group 'canna-keys-advanced2
		     (_ "Canna advanced key bindings (2)")
		     (_ "long description will be here."))


(define-custom 'canna-next-segment-key '(generic-go-right-key)
               '(canna-keys)
	       '(key)
	       (_ "[Canna] next segment")
	       (_ "long description will be here"))

(define-custom 'canna-prev-segment-key '(generic-go-left-key)
               '(canna-keys)
	       '(key)
	       (_ "[Canna] previous segment")
	       (_ "long description will be here"))

(define-custom 'canna-extend-segment-key '("<IgnoreCase><Control>o" "<Shift>right")
               '(canna-keys)
	       '(key)
	       (_ "[Canna] extend segment")
	       (_ "long description will be here"))

(define-custom 'canna-shrink-segment-key '("<IgnoreCase><Control>i" "<Shift>left")
               '(canna-keys)
	       '(key)
	       (_ "[Canna] shrink segment")
	       (_ "long description will be here"))

(define-custom 'canna-commit-as-opposite-kana-key '("<IgnoreCase><Shift>q")  ;; "Q"
               '(canna-keys)
	       '(key)
	       (_ "[Canna] commit preedit string as transposed kana")
	       (_ "long description will be here"))

(define-custom 'canna-wide-latin-key '("<IgnoreCase><Shift>l")  ;; "L"
               '(canna-keys)
	       '(key)
	       (_ "[Canna] fullwidth alphanumeric mode")
	       (_ "long description will be here"))

(define-custom 'canna-hankaku-kana-key '("<IgnoreCase><Control>q")
               '(canna-keys)
	       '(key)
	       (_ "[Canna] halfwidth katakana mode")
	       (_ "long description will be here"))

(define-custom 'canna-kana-toggle-key '("<IgnoreCase>q")  ;; "q"
               '(canna-keys)
	       '(key)
	       (_ "[Canna] toggle hiragana/katakana mode")
	       (_ "long description will be here"))

;;
;; overriding generic keys
;;
(define-custom 'canna-on-key '("<Control>\\" generic-on-key)
               '(canna-keys-advanced1)
	       '(key)
	       (_ "[Canna] on")
	       (_ "long description will be here"))

;;(define-custom 'canna-latin-key '("l" generic-on-key)
(define-custom 'canna-latin-key '("<Control>\\" generic-off-key)
               '(canna-keys-advanced1)
	       '(key)
	       (_ "[Canna] off")
	       (_ "long description will be here"))

(define-custom 'canna-begin-conv-key '(generic-begin-conv-key generic-on-key)
               '(canna-keys-advanced1)
	       '(key)
	       (_ "[Canna] begin conversion")
	       (_ "long description will be here"))

(define-custom 'canna-commit-key '(generic-commit-key)
               '(canna-keys-advanced1)
	       '(key)
	       (_ "[Canna] commit")
	       (_ "long description will be here"))

(define-custom 'canna-cancel-key '(generic-cancel-key)
               '(canna-keys-advanced1)
	       '(key)
	       (_ "[Canna] cancel")
	       (_ "long description will be here"))

(define-custom 'canna-next-candidate-key '(generic-next-candidate-key)
               '(canna-keys-advanced1)
	       '(key)
	       (_ "[Canna] next candidate")
	       (_ "long description will be here"))

(define-custom 'canna-prev-candidate-key '(generic-prev-candidate-key)
               '(canna-keys-advanced1)
	       '(key)
	       (_ "[Canna] previous candidate")
	       (_ "long description will be here"))

;;(define-custom 'canna-next-page-key '(generic-next-page-key)
;;             '(canna-keys-advanced1)
;;	       '(key)
;;	       (_ "[Canna] next page of candidate window")
;;	       (_ "long description will be here"))

;;(define-custom 'canna-prev-page-key '(generic-prev-page-key)
;;               '(canna-keys-advanced1)
;;	       '(key)
;;	       (_ "[Canna] previous page of candidate window")
;;	       (_ "long description will be here"))

;;
;; overriding generic keys (advanced)
;;
(define-custom 'canna-beginning-of-preedit-key '(generic-beginning-of-preedit-key)
               '(canna-keys-advanced2)
	       '(key)
	       (_ "[Canna] beginning of preedit")
	       (_ "long description will be here"))

(define-custom 'canna-end-of-preedit-key '(generic-end-of-preedit-key)
               '(canna-keys-advanced2)
	       '(key)
	       (_ "[Canna] end of preedit")
	       (_ "long description will be here"))

;;(define-custom 'canna-kill-key '(generic-kill-key)
;;               '(canna-keys-advanced2)
;;	       '(key)
;;	       (_ "[Canna] erase after cursor")
;;	       (_ "long description will be here"))

;;(define-custom 'canna-kill-backward-key '(generic-kill-backward-key)
;;               '(canna-keys-advanced2)
;;	       '(key)
;;	       (_ "[Canna] erase before cursor")
;;	       (_ "long description will be here"))

(define-custom 'canna-backspace-key '(generic-backspace-key)
               '(canna-keys-advanced2)
	       '(key)
	       (_ "[Canna] backspace")
	       (_ "long description will be here"))

(define-custom 'canna-delete-key '(generic-delete-key)
               '(canna-keys-advanced2)
	       '(key)
	       (_ "[Canna] delete")
	       (_ "long description will be here"))

(define-custom 'canna-go-left-key '(generic-go-left-key)
               '(canna-keys-advanced2)
	       '(key)
	       (_ "[Canna] go left")
	       (_ "long description will be here"))

(define-custom 'canna-go-right-key '(generic-go-right-key)
               '(canna-keys-advanced2)
	       '(key)
	       (_ "[Canna] go right")
	       (_ "long description will be here"))
