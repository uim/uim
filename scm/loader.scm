;;;
;;; Copyright (c) 2003,2004 uim Project http://uim.freedesktop.org/
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

;; the loading order of input methods affecs which IM is preferred at
;; the default IM selection process for each locale. i.e. list
;; preferable IM first for each language

;; Chinese input methods
(require "pyload.scm")

;; Janapese input methods
(require "anthy.scm")
(require "canna.scm")
(require "prime.scm")
(require "skk.scm")
(require "tcode.scm")
(require "tutcode.scm")

;; Korean input methods
(require "hangul.scm")

;; Vietnamese input methods
(require "viqr.scm")

;; other input methods
(require "ipa.scm")
;(require "spellcheck.scm")

;; latin input method
(require "latin.scm")

(load-plugin "libuim-m17nlib.so")
(if (symbol-bound? 'm17nlib-lib-init)
    (require "m17nlib.scm"))

;(load-plugin "libuim-scim.so")
;(if (symbol-bound? 'scim-lib-init)
;    (require "scim.scm"))
