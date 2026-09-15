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

(use test.unit)

(require "test/uim-test-utils")

(define-uim-test-case "testcase skk skkserv encoding"
  (setup
   (lambda ()
     (uim '(load-enabled-modules))
     (uim '(require "custom.scm"))
     (uim '(require "skk-custom.scm"))))

  ("test skkserv encoding default and activity"
   (assert-equal 'euc-jp (uim 'skk-skkserv-encoding))
   (assert-true  (uim-bool '(custom-valid? 'skk-skkserv-encoding 'euc-jp)))
   (assert-true  (uim-bool '(custom-valid? 'skk-skkserv-encoding 'utf-8)))
   (assert-false (uim-bool '(custom-active? 'skk-skkserv-encoding)))
   (assert-true  (uim-bool '(custom-set-value! 'skk-use-skkserv? #t)))
   (assert-true  (uim-bool '(custom-active? 'skk-skkserv-encoding)))
   (assert-true  (uim-bool '(custom-set-value! 'skk-skkserv-encoding 'utf-8)))
   (assert-equal 'utf-8 (uim 'skk-skkserv-encoding))
   (assert-true  (uim-bool '(custom-set-value! 'skk-skkserv-encoding 'euc-jp)))
   (assert-equal 'euc-jp (uim 'skk-skkserv-encoding))))

(define-uim-test-case "testcase skk dictionary encoding"
  (setup
   (lambda ()
     (uim '(load-enabled-modules))
     (uim '(require "custom.scm"))
     (uim '(require "skk-custom.scm"))))

  ("test dictionary encoding defaults and choices"
   (assert-equal 'euc-jp (uim 'skk-dic-file-encoding))
   (assert-equal 'euc-jp (uim 'skk-personal-dic-encoding))
   (assert-equal 'euc-jp (uim 'skk-uim-personal-dic-encoding))
   (assert-true (uim-bool '(custom-valid? 'skk-dic-file-encoding 'euc-jp)))
   (assert-true (uim-bool '(custom-valid? 'skk-dic-file-encoding 'utf-8)))
   (assert-true (uim-bool '(custom-valid? 'skk-personal-dic-encoding 'euc-jp)))
   (assert-true (uim-bool '(custom-valid? 'skk-personal-dic-encoding 'utf-8)))
   (assert-true (uim-bool '(custom-valid? 'skk-uim-personal-dic-encoding 'euc-jp)))
   (assert-true (uim-bool '(custom-valid? 'skk-uim-personal-dic-encoding 'utf-8)))
   (assert-true (uim-bool '(custom-set-value! 'skk-use-skkserv? #f)))
   (assert-true (uim-bool '(custom-active? 'skk-dic-file-encoding)))
   (assert-true (uim-bool '(custom-set-value! 'skk-use-skkserv? #t)))
   (assert-false (uim-bool '(custom-active? 'skk-dic-file-encoding)))))
