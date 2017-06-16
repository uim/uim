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

(require "generic.scm")
;;
(define py-init-handler
  (lambda (id im arg)
    (require "py.scm")
    (generic-context-new id im py-rule #f)))

(generic-register-im
 'py
 "zh_CN"
 "UTF-8"
 (N_ "New Pinyin (Simplified)")
 (N_ "Pinyin input method (Simplified Chinese version)")
 py-init-handler)

(define pyunihan-init-handler
  (lambda (id im arg)
    (require "pyunihan.scm")
    (generic-context-new id im pyunihan-rule #f)))

(generic-register-im
 'pyunihan
 "zh"
 "UTF-8"
 (N_ "Pinyin (Unicode)")
 (N_ "Pinyin input method (Unicode version)")
 pyunihan-init-handler)

(define pinyin-big5-init-handler
  (lambda (id im arg)
    (require "pinyin-big5.scm")
    (generic-context-new id im pinyin-big5-rule #f)))

(generic-register-im
 'pinyin-big5
 "zh_TW:zh_HK"
 "UTF-8"
 (N_ "Pinyin (Traditional)")
 (N_ "Pinyin input method (Traditional Chinese version)")
 pinyin-big5-init-handler)
