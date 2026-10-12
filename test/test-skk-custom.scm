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
;;;

(require-extension (unittest))

;; im-custom.scm requires stub-im-list defined in loader.scm but
;; loader.scm isn't loaded with LIBUIM_VANILLA=1.
(define stub-im-list '())
(require "custom.scm")
(require "skk-custom.scm")

(test-begin "skkserv encoding default and activity")
(test-eq    'euc-jp skk-skkserv-encoding)
(test-true  (custom-valid? 'skk-skkserv-encoding 'euc-jp))
(test-true  (custom-valid? 'skk-skkserv-encoding 'utf-8))
(test-false (custom-active? 'skk-skkserv-encoding))
(test-true  (custom-set-value! 'skk-use-skkserv? #t))
(test-true  (custom-active? 'skk-skkserv-encoding))
(test-true  (custom-set-value! 'skk-skkserv-encoding 'utf-8))
(test-eq    'utf-8 skk-skkserv-encoding)
(test-true  (custom-set-value! 'skk-skkserv-encoding 'euc-jp))
(test-eq    'euc-jp skk-skkserv-encoding)
(test-end)

(test-begin "dictionary encoding defaults and choices")
(test-eq    'euc-jp skk-dic-file-encoding)
(test-eq    'euc-jp skk-personal-dic-encoding)
(test-eq    'euc-jp skk-uim-personal-dic-encoding)
(test-true  (custom-valid? 'skk-dic-file-encoding 'euc-jp))
(test-true  (custom-valid? 'skk-dic-file-encoding 'utf-8))
(test-true  (custom-valid? 'skk-personal-dic-encoding 'euc-jp))
(test-true  (custom-valid? 'skk-personal-dic-encoding 'utf-8))
(test-true  (custom-valid? 'skk-uim-personal-dic-encoding 'euc-jp))
(test-true  (custom-valid? 'skk-uim-personal-dic-encoding 'utf-8))
(test-true  (custom-set-value! 'skk-use-skkserv? #f))
(test-true  (custom-active? 'skk-dic-file-encoding))
(test-true  (custom-set-value! 'skk-use-skkserv? #t))
(test-false (custom-active? 'skk-dic-file-encoding))
(test-end)

(test-report-result)
