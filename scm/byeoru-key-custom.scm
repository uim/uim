;;; byeoru-key-custom.scm: Key customization variables for byeoru.scm
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


(define-custom-group 'byeoru-keys1
  (N_ "Byeoru key bindings 1")
  (N_ "long description will be here."))

(define-custom-group 'byeoru-keys2
  (N_ "Byeoru key bindings 2")
  (N_ "long description will be here."))


(define-custom 'byeoru-on-key '("<Shift> ")
  '(byeoru-keys1)
  '(key)
  (N_ "[Byeoru] on")
  (N_ "long description will be here"))

(define-custom 'byeoru-latin-key '("<Shift> ")
  '(byeoru-keys1)
  '(key)
  (N_ "[Byeoru] off")
  (N_ "long description will be here"))

(define-custom 'byeoru-conversion-key '("F9")
  '(byeoru-keys1)
  '(key)
  (N_ "[Byeoru] convert Hangul to Chinese characters")
  (N_ "long description will be here"))

(define-custom 'byeoru-commit-key '(generic-commit-key)
  '(byeoru-keys1)
  '(key)
  (N_ "[Byeoru] confirm conversion")
  (N_ "long description will be here"))

(define-custom 'byeoru-cancel-key '(generic-cancel-key)
  '(byeoru-keys1)
  '(key)
  (N_ "[Byeoru] cancel conversion")
  (N_ "long description will be here"))

(define-custom 'byeoru-next-candidate-key '(generic-next-candidate-key)
  '(byeoru-keys2)
  '(key)
  (N_ "[Byeoru] next candidate")
  (N_ "long description will be here"))

(define-custom 'byeoru-prev-candidate-key '(generic-prev-candidate-key)
  '(byeoru-keys2)
  '(key)
  (N_ "[Byeoru] previous candidate")
  (N_ "long description will be here"))

(define-custom 'byeoru-next-page-key '(generic-next-page-key)
  '(byeoru-keys2)
  '(key)
  (N_ "[Byeoru] next page of candidate window")
  (N_ "long description will be here"))

(define-custom 'byeoru-prev-page-key '(generic-prev-page-key)
  '(byeoru-keys2)
  '(key)
  (N_ "[Byeoru] previous page of candidate window")
  (N_ "long description will be here"))

(define-custom 'byeoru-backspace-key '(generic-backspace-key)
  '(byeoru-keys2)
  '(key)
  (N_ "[Byeoru] backspace")
  (N_ "long description will be here"))

(define-custom 'byeoru-delete-key '(generic-delete-key)
  '(byeoru-keys2)
  '(key)
  (N_ "[Byeoru] delete")
  (N_ "long description will be here"))

(define-custom 'byeoru-go-left-key '(generic-go-left-key)
  '(byeoru-keys2)
  '(key)
  (N_ "[Byeoru] go left")
  (N_ "long description will be here"))

(define-custom 'byeoru-go-right-key '(generic-go-right-key)
  '(byeoru-keys2)
  '(key)
  (N_ "[Byeoru] go right")
  (N_ "long description will be here"))

(define-custom 'byeoru-beginning-of-preedit-key '(generic-beginning-of-preedit-key)
  '(byeoru-keys2)
  '(key)
  (N_ "[Byeoru] beginning of word")
  (N_ "long description will be here"))

(define-custom 'byeoru-end-of-preedit-key '(generic-end-of-preedit-key)
  '(byeoru-keys2)
  '(key)
  (N_ "[Byeoru] end of word")
  (N_ "long description will be here"))

;; Local Variables:
;; mode: scheme
;; coding: utf-8
;; End:
