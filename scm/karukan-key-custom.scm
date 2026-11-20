;;;
;;; Copyright (c) 2026 uim Project https://github.com/uim/uim
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

(define-custom-group 'karukan-keys
                     (N_ "Karukan key bindings")
                     (N_ "long description will be here."))

(define-custom 'karukan-on-key '(generic-on-key)
               '(karukan-keys)
               '(key)
               (N_ "[Karukan] on")
               (N_ "long description will be here"))

(define-custom 'karukan-off-key '(generic-off-key)
               '(karukan-keys)
               '(key)
               (N_ "[Karukan] off")
               (N_ "long description will be here"))

;; These add keys to the ones of Karukan: a key is sent to Karukan as the
;; key of Karukan that has the same meaning. The keys of Karukan such as
;; "backspace" keep working.

;; generic-commit-key has <Control>j, which is another key of Karukan.
(define-custom 'karukan-commit-key '(generic-return-key)
               '(karukan-keys)
               '(key)
               (N_ "[Karukan] commit")
               (N_ "long description will be here"))

(define-custom 'karukan-cancel-key '(generic-cancel-key)
               '(karukan-keys)
               '(key)
               (N_ "[Karukan] cancel")
               (N_ "long description will be here"))

(define-custom 'karukan-backspace-key '(generic-backspace-key)
               '(karukan-keys)
               '(key)
               (N_ "[Karukan] backspace")
               (N_ "long description will be here"))

(define-custom 'karukan-delete-key '(generic-delete-key)
               '(karukan-keys)
               '(key)
               (N_ "[Karukan] delete")
               (N_ "long description will be here"))

;; generic-next-candidate-key has " ", which has other meanings in
;; Karukan such as inserting a space in the alphanumeric mode.
(define-custom 'karukan-next-candidate-key '("down" "<IgnoreCase><Control>n")
               '(karukan-keys)
               '(key)
               (N_ "[Karukan] next candidate")
               (N_ "long description will be here"))

(define-custom 'karukan-prev-candidate-key '(generic-prev-candidate-key)
               '(karukan-keys)
               '(key)
               (N_ "[Karukan] previous candidate")
               (N_ "long description will be here"))

(define-custom 'karukan-next-page-key '(generic-next-page-key)
               '(karukan-keys)
               '(key)
               (N_ "[Karukan] next page of candidate window")
               (N_ "long description will be here"))

(define-custom 'karukan-prev-page-key '(generic-prev-page-key)
               '(karukan-keys)
               '(key)
               (N_ "[Karukan] previous page of candidate window")
               (N_ "long description will be here"))

(define-custom 'karukan-beginning-of-preedit-key
               '(generic-beginning-of-preedit-key)
               '(karukan-keys)
               '(key)
               (N_ "[Karukan] beginning of preedit")
               (N_ "long description will be here"))

(define-custom 'karukan-end-of-preedit-key '(generic-end-of-preedit-key)
               '(karukan-keys)
               '(key)
               (N_ "[Karukan] end of preedit")
               (N_ "long description will be here"))

(define-custom 'karukan-go-left-key '(generic-go-left-key)
               '(karukan-keys)
               '(key)
               (N_ "[Karukan] go left")
               (N_ "long description will be here"))

(define-custom 'karukan-go-right-key '(generic-go-right-key)
               '(karukan-keys)
               '(key)
               (N_ "[Karukan] go right")
               (N_ "long description will be here"))
