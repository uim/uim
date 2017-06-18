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
;;;

;; These tests are passed at revision 6605 (new repository)

(define-module test.util.test-multi-segment
  (use test.unit.test-case)
  (use test.uim-test))
(select-module test.util.test-multi-segment)

(define (setup)
  (uim-test-setup))

(define (teardown)
  (uim-test-teardown))

;;;FIXME: multi-segment functions are withdrawn -- YamaKen 2007-01-07

(define (no-test-make-index-list)
  (uim '(define old-lst '(0 1 2 3 4)))
  (assert-uim-false '(multi-segment-make-index-list -1 old-lst))
  (assert-uim-equal '()
                    '(multi-segment-make-index-list 0 old-lst))
  (assert-uim-equal '(0)
                    '(multi-segment-make-index-list 1 old-lst))
  (assert-uim-equal '(0 1)
                    '(multi-segment-make-index-list 2 old-lst))
  (assert-uim-equal '(0 1 2 3 4)
                    '(multi-segment-make-index-list 5 old-lst))
  (assert-uim-equal '(0 1 2 3 4 0)
                    '(multi-segment-make-index-list 6 old-lst))
  (assert-uim-equal '(0 1 2 3 4 0 0)
                    '(multi-segment-make-index-list 7 old-lst))
  #f)

(define (no-test-opposite-kana)
  (assert-uim-equal (uim 'multi-segment-type-katakana)
                    '(multi-segment-opposite-kana
                      multi-segment-type-hiragana))
  (assert-uim-equal (uim 'multi-segment-type-hiragana)
                    '(multi-segment-opposite-kana
                      multi-segment-type-katakana))
  (assert-uim-equal (uim 'multi-segment-type-hiragana)
                    '(multi-segment-opposite-kana
                      multi-segment-type-hankana))
  #f)

(provide "test/util/test-multi-segment")
