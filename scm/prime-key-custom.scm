;;; prime-key-custom.scm: Customization variables for PRIME key bindings
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

;; original key definitions

;;(define-key prime-latin-key?          '("<Control>l" generic-off-key?))
;;(define-key prime-wide-latin-key?     "<Control>L")
;;(define-key prime-begin-conv-key?     'generic-begin-conv-key?)
;;(define-key prime-on-key?         '("<Control>j" "<Control>J" generic-on-key?))
;;(define-key prime-commit-key?         'generic-commit-key?)
;;(define-key prime-next-candidate-key? 'generic-next-candidate-key?)
;;(define-key prime-prev-candidate-key? 'generic-prev-candidate-key?)
;;(define-key prime-next-page-key?      'generic-next-page-key?)
;;(define-key prime-prev-page-key?      'generic-prev-page-key?)
;;(define-key prime-cancel-key?         'generic-cancel-key?)
;;(define-key prime-backspace-key?      'generic-backspace-key?)
;;(define-key prime-delete-key?         'generic-delete-key?)
;;(define-key prime-go-left-key?        'generic-go-left-key?)
;;(define-key prime-go-right-key?       'generic-go-right-key?)
;;(define-key prime-go-left-edge-key?   '("<Control>a" "<Control>left"))
;;(define-key prime-go-right-edge-key?  '("<Control>e" "<Control>right"))
;;(define-key prime-register-key?       '("<Control>w"))
;;(define-key prime-typing-mode-hiragana-key?   "F6")
;;(define-key prime-typing-mode-katakana-key?   "F7")
;;(define-key prime-typing-mode-hankana-key?    "F8")
;;(define-key prime-typing-mode-wideascii-key?  "F9")
;;(define-key prime-typing-mode-ascii-key?      "F10")

(define-custom-group 'prime-keys1
		     (_ "PRIME key bindings 1")
		     (_ "long description will be here."))

(define-custom-group 'prime-keys2
		     (_ "PRIME key bindings 2)")
		     (_ "long description will be here."))

(define-custom-group 'prime-keys3
		     (_ "PRIME key bindings 3")
		     (_ "long description will be here."))


(define-custom 'prime-register-key '("<Control>w")
               '(prime-keys1)
	       '(key)
	       (_ "[PRIME] prime-register-key")
	       (_ "long description will be here"))

(define-custom 'prime-typing-mode-hiragana-key '("F6")
               '(prime-keys1 mode-transition)
	       '(key)
	       (_ "[PRIME] prime-typing-mode-hiragana-key")
	       (_ "long description will be here"))

(define-custom 'prime-typing-mode-katakana-key '("F7")
               '(prime-keys1 mode-transition)
	       '(key)
	       (_ "[PRIME] prime-typing-mode-katakana-key")
	       (_ "long description will be here"))

(define-custom 'prime-typing-mode-hankana-key '("F8")
               '(prime-keys1 mode-transition)
	       '(key)
	       (_ "[PRIME] prime-typing-mode-hankana-key")
	       (_ "long description will be here"))

(define-custom 'prime-typing-mode-wideascii-key '("F9")
               '(prime-keys1 mode-transition)
	       '(key)
	       (_ "[PRIME] prime-typing-mode-wideascii-key")
	       (_ "long description will be here"))

(define-custom 'prime-typing-mode-ascii-key '("F10")
               '(prime-keys1 mode-transition)
	       '(key)
	       (_ "[PRIME] prime-typing-mode-ascii-key")
	       (_ "long description will be here"))

;;
;; advanced 1
;;
(define-custom 'prime-on-key '("<IgnoreCase><Control>j" generic-on-key)
               '(prime-keys2 mode-transition)
	       '(key)
	       (_ "[PRIME] on")
	       (_ "long description will be here"))

(define-custom 'prime-latin-key '("<Control>l" generic-off-key)
               '(prime-keys2 mode-transition)
	       '(key)
	       (_ "[PRIME] off")
	       (_ "long description will be here"))

(define-custom 'prime-wide-latin-key '("<Control>L")
               '(prime-keys2 mode-transition)
	       '(key)
	       (_ "[PRIME] fullwidth alphanumeric mode")
	       (_ "long description will be here"))

(define-custom 'prime-begin-conv-key '(generic-begin-conv-key)
               '(prime-keys2)
	       '(key)
	       (_ "[PRIME] begin conversion")
	       (_ "long description will be here"))

(define-custom 'prime-commit-key '(generic-commit-key)
               '(prime-keys2)
	       '(key)
	       (_ "[PRIME] commit")
	       (_ "long description will be here"))

(define-custom 'prime-cancel-key '(generic-cancel-key)
               '(prime-keys2)
	       '(key)
	       (_ "[PRIME] cancel")
	       (_ "long description will be here"))

(define-custom 'prime-next-candidate-key '(generic-next-candidate-key)
               '(prime-keys2)
	       '(key)
	       (_ "[PRIME] next candidate")
	       (_ "long description will be here"))

(define-custom 'prime-prev-candidate-key '(generic-prev-candidate-key)
               '(prime-keys2)
	       '(key)
	       (_ "[PRIME] previous candidate")
	       (_ "long description will be here"))

(define-custom 'prime-next-page-key '(generic-next-page-key)
               '(prime-keys2)
	       '(key)
	       (_ "[PRIME] next page of candidate window")
	       (_ "long description will be here"))

(define-custom 'prime-prev-page-key '(generic-prev-page-key)
               '(prime-keys2)
	       '(key)
	       (_ "[PRIME] previous page of candidate window")
	       (_ "long description will be here"))

;;
;; advanced 2
;;
(define-custom 'prime-go-left-edge-key '("<IgnoreCase><Control>a" "<Control>left")
               '(prime-keys3)
	       '(key)
	       (_ "[PRIME] beginning of preedit")
	       (_ "long description will be here"))

(define-custom 'prime-go-right-edge-key '("<IgnoreCase><Control>e" "<Control>right")
               '(prime-keys3)
	       '(key)
	       (_ "[PRIME] end of preedit")
	       (_ "long description will be here"))

(define-custom 'prime-backspace-key '(generic-backspace-key)
               '(prime-keys3)
	       '(key)
	       (_ "[PRIME] backspace")
	       (_ "long description will be here"))

(define-custom 'prime-delete-key '(generic-delete-key)
               '(prime-keys3)
	       '(key)
	       (_ "[PRIME] delete")
	       (_ "long description will be here"))

(define-custom 'prime-go-left-key '(generic-go-left-key)
               '(prime-keys3)
	       '(key)
	       (_ "[PRIME] go left")
	       (_ "long description will be here"))

(define-custom 'prime-go-right-key '(generic-go-right-key)
               '(prime-keys3)
	       '(key)
	       (_ "[PRIME] go right")
	       (_ "long description will be here"))
