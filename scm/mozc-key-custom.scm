;;;
;;; Copyright (c) 2010-2012 uim Project http://code.google.com/p/uim/
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

;;; This file originates from the MacUIM project by Etsushi Kato
;;; (https://github.com/e-kato/macuim, Mozc/scm/mozc-key-custom.scm)
;;; and carries its BSD-3-Clause license above.

(require "i18n.scm")

(define-custom-group 'mozc-keys
		     (_ "Mozc key bindings")
		     (_ "long description will be here."))

;;
;; overriding generic keys
;;
(define-custom 'mozc-on-key '(generic-on-key)
               '(mozc-keys)
	       '(key)
	       (_ "[Mozc] on")
	       (_ "long description will be here"))

(define-custom 'mozc-off-key '(generic-off-key)
               '(mozc-keys)
	       '(key)
	       (_ "[Mozc] off")
	       (_ "long description will be here"))

(define-custom 'mozc-kana-toggle-key '()
               '(mozc-keys)
	       '(key)
	       (_ "[Mozc] toggle hiragana/katakana mode")
	       (_ "long description will be here"))

;;(define-custom 'mozc-cancel-key '(generic-cancel-key)
;;               '(mozc-keys)
;;	       '(key)
;;	       (_ "[Mozc] cancel")
;;	       (_ "long description will be here"))
;;
;;(define-custom 'mozc-prev-segment-key '(generic-go-left-key)
;;               '(mozc-keys)
;;	       '(key)
;;	       (_ "[Mozc] previous segment")
;;	       (_ "long description will be here"))

(define-custom 'mozc-vi-escape-key '("escape" "<Control>[")
               '(mozc-keys)
	       '(key)
	       (_ "[Mozc] ESC keys on vi-cooperative mode")
	       (_ "long description will be here"))
