;;;
;;; Copyright (c) 2003-2026 uim Project https://github.com/uim/uim
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

(require "japanese.scm")

;; Deprecated: The definitions with the "-utf8" suffix are for
;; compatibility. They're the same as the ones without it because all
;; strings are UTF-8 now.

(define string-to-list-utf8 string-to-list)
(define ja-rk-rule-basic-utf8 ja-rk-rule-basic)
(define ja-rk-rule-additional-utf8 ja-rk-rule-additional)
(define ja-rk-rule-utf8 ja-rk-rule)
(define ja-wide-rule-utf8 ja-wide-rule)
(define ja-space-utf8 ja-space)
(define ja-alnum-space-utf8 ja-alnum-space)
(define ja-wide-utf8 ja-wide)
(define ja-string-list-to-wide-alphabet-utf8 ja-string-list-to-wide-alphabet)
(define ja-join-vu-utf8 ja-join-vu)
(define ja-find-kana-list-from-rule-utf8 ja-find-kana-list-from-rule)
(define ja-make-kana-str-list-utf8 ja-make-kana-str-list)
(define japanese-auto-start-henkan-keyword-list-utf8 japanese-auto-start-henkan-keyword-list)
(define ja-rk-rule-update-utf8 ja-rk-rule-update)
(define ja-kanji-code-input-jis-utf8 ja-kanji-code-input-jis)
(define ja-kanji-code-input-kuten-utf8 ja-kanji-code-input-kuten)
(define ja-kanji-code-input-ucs-utf8 ja-kanji-code-input-ucs)
(define ja-kanji-code-input-utf8 ja-kanji-code-input)

;; ja-rk-rule-basic and ja-rk-rule are updated by ja-rk-rule-update
;; through this.
(define ja-rk-rule-keep-consonant-update
  (let ((update ja-rk-rule-keep-consonant-update))
    (lambda ()
      (update)
      (set! ja-rk-rule-basic-utf8 ja-rk-rule-basic)
      (set! ja-rk-rule-utf8 ja-rk-rule))))
(define ja-rk-rule-keep-consonant-update-utf8 ja-rk-rule-keep-consonant-update)
