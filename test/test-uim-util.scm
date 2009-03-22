#!/usr/bin/env gosh

;;; Copyright (c) 2003-2009 uim Project http://code.google.com/p/uim/
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

;; These tests are passed at revision 5329 (new repository)

(use test.unit)

(require "test/uim-test-utils")

;; this test assumes that string encoding of Gauche is configured as
;; UTF-8
(define-uim-test-case "testcase uim-util rk"
  (setup
   (lambda ()
     (uim '(define test-rk-rule '(((("a"). ())("あ" "ア" "ｱ"))
				  ((("i"). ())("い" "イ" "ｲ"))
				  ((("u"). ())("う" "ウ" "ｳ"))
				  ((("e"). ())("え" "エ" "ｴ"))
				  ((("o"). ())("お" "オ" "ｵ"))
				  
				  ((("k" "a"). ())("か" "カ" "ｶ"))
				  ((("k" "i"). ())("き" "キ" "ｷ"))
				  ((("k" "u"). ())("く" "ク" "ｸ"))
				  ((("k" "e"). ())("け" "ケ" "ｹ"))
				  ((("k" "o"). ())("こ" "コ" "ｺ"))
				  ((("k" "y" "a"). ())("きゃ" "キャ" "ｷｬ"))
				  ((("k" "y" "i"). ())("きぃ" "キィ" "ｷｨ"))
				  ((("k" "y" "u"). ())("きゅ" "キュ" "ｷｭ"))
				  ((("k" "y" "e"). ())("きぇ" "キェ" "ｷｪ"))
				  ((("k" "y" "o"). ())("きょ" "キョ" "ｷｮ"))
				  
				  ((("s" "s"). ("s"))("っ" "ッ" "ｯ"))
				  ((("s" "a"). ())("さ" "サ" "ｻ"))
				  ((("s" "i"). ())("し" "シ" "ｼ"))
				  ((("s" "u"). ())("す" "ス" "ｽ"))
				  ((("s" "e"). ())("せ" "セ" "ｾ"))
				  ((("s" "o"). ())("そ" "ソ" "ｿ"))
				  
				  ((("p" "p"). ("p"))("っ" "ッ" "ｯ")))))))

  ("test rk-lib-find-seq"
   ;; non existence seq does not match
   (assert-false (uim-bool '(rk-lib-find-seq () test-rk-rule)))
   (assert-false (uim-bool '(rk-lib-find-seq '("") test-rk-rule)))

   ;; test first rule
   (assert-equal '((("a"). ())("あ" "ア" "ｱ"))
		 (uim '(rk-lib-find-seq '("a") test-rk-rule)))

   (assert-equal '((("i"). ())("い" "イ" "ｲ"))
		 (uim '(rk-lib-find-seq '("i") test-rk-rule)))
   (assert-equal '((("o"). ())("お" "オ" "ｵ"))
		 (uim '(rk-lib-find-seq '("o") test-rk-rule)))
   ;; non existence seq does not match
   (assert-false (uim-bool '(rk-lib-find-seq '("z") test-rk-rule)))

   (assert-equal '((("k" "y" "a"). ())("きゃ" "キャ" "ｷｬ"))
		 (uim '(rk-lib-find-seq '("k" "y" "a") test-rk-rule)))
   (assert-equal '((("k" "y" "i"). ())("きぃ" "キィ" "ｷｨ"))
		 (uim '(rk-lib-find-seq '("k" "y" "i") test-rk-rule)))
   (assert-equal '((("k" "y" "o"). ())("きょ" "キョ" "ｷｮ"))
		 (uim '(rk-lib-find-seq '("k" "y" "o") test-rk-rule)))
   ;; partial seq does not match
   (assert-false (uim-bool '(rk-lib-find-seq '("k" "y") test-rk-rule)))
   (assert-false (uim-bool '(rk-lib-find-seq '("k" "y" "y") test-rk-rule)))

   (assert-equal '((("s" "s"). ("s"))("っ" "ッ" "ｯ"))
		 (uim '(rk-lib-find-seq '("s" "s") test-rk-rule)))
   ;; partial seq does not match
   (assert-false (uim-bool '(rk-lib-find-seq '("s") test-rk-rule)))
   (assert-false (uim-bool '(rk-lib-find-seq '("s" "s" "s") test-rk-rule)))

   ;; test last rule
   (assert-equal '((("p" "p"). ("p"))("っ" "ッ" "ｯ"))
		 (uim '(rk-lib-find-seq '("p" "p") test-rk-rule)))
   (assert-false (uim-bool '(rk-lib-find-seq '("p") test-rk-rule)))
   (assert-false (uim-bool '(rk-lib-find-seq '("p" "p" "p") test-rk-rule))))

  ("test rk-lib-find-partial-seq"
   ;; null sequence matches first rule
   (assert-equal '((("a"). ())("あ" "ア" "ｱ"))
		 (uim '(rk-lib-find-partial-seq () test-rk-rule)))
   ;; non existence seq does not match
   (assert-false (uim-bool '(rk-lib-find-partial-seq '("") test-rk-rule)))

   ;; test first rule: exact key does not match
   (assert-false (uim-bool '(rk-lib-find-partial-seq '("a") test-rk-rule)))
   (assert-false (uim-bool '(rk-lib-find-partial-seq '("i") test-rk-rule)))
   (assert-false (uim-bool '(rk-lib-find-partial-seq '("o") test-rk-rule)))
   ;; non existence seq does not match
   (assert-false (uim-bool '(rk-lib-find-partial-seq '("z") test-rk-rule)))

   ;; exact key does not match
   (assert-false (uim-bool '(rk-lib-find-partial-seq '("k" "y" "a")
						     test-rk-rule)))
   (assert-false (uim-bool '(rk-lib-find-partial-seq '("k" "y" "i")
						     test-rk-rule)))
   (assert-false (uim-bool '(rk-lib-find-partial-seq '("k" "y" "o")
						     test-rk-rule)))
   ;; partial seq matches first entry
   (assert-equal '((("k" "a"). ())("か" "カ" "ｶ"))
		 (uim '(rk-lib-find-partial-seq '("k") test-rk-rule)))
   (assert-equal '((("k" "y" "a"). ())("きゃ" "キャ" "ｷｬ"))
		 (uim '(rk-lib-find-partial-seq '("k" "y") test-rk-rule)))
   ;; non existence seq does not match
   (assert-false (uim-bool '(rk-lib-find-partial-seq '("k" "y" "y") test-rk-rule)))

   ;; exact key does not match
   (assert-false (uim-bool '(rk-lib-find-partial-seq '("s" "s") test-rk-rule)))
   ;; partial match
   (assert-equal '((("s" "s"). ("s"))("っ" "ッ" "ｯ"))
		 (uim '(rk-lib-find-partial-seq '("s") test-rk-rule)))
   ;; non existence seq does not match
   (assert-false (uim-bool '(rk-lib-find-partial-seq '("s" "s" "s")
						     test-rk-rule)))

   ;; test last rule
   (assert-false (uim-bool '(rk-lib-find-partial-seq '("p" "p") test-rk-rule)))
   (assert-equal '((("p" "p"). ("p"))("っ" "ッ" "ｯ"))
		 (uim '(rk-lib-find-partial-seq '("p") test-rk-rule)))
   (assert-false (uim-bool '(rk-lib-find-partial-seq '("p" "p" "p")
						     test-rk-rule))))

  ("test rk-lib-expect-seq"
   (assert-equal '("p" "s" "s" "s" "s" "s" "s"
		   "k" "k" "k" "k" "k" "k" "k" "k" "k" "k"
		   "o" "e" "u" "i" "a")
		 (uim '(rk-lib-expect-seq () test-rk-rule)))
   (assert-equal '("y" "y" "y" "y" "y" "o" "e" "u" "i" "a")
		 (uim '(rk-lib-expect-seq '("k") test-rk-rule)))
   (assert-equal '("o" "e" "u" "i" "a")
		 (uim '(rk-lib-expect-seq '("k" "y") test-rk-rule)))
   (assert-equal '("o" "e" "u" "i" "a")
		 (uim '(rk-lib-expect-seq '("k" "y") test-rk-rule)))
   ;; rk-lib-expect-seq returns null list on exact match
   (assert-equal ()
		 (uim '(rk-lib-expect-seq '("k" "y" "a") test-rk-rule)))
   ;; rk-lib-expect-seq returns null list on fail
   (assert-equal ()
		 (uim '(rk-lib-expect-seq '("k" "y" "a" "a") test-rk-rule)))
   (assert-equal '("o" "e" "u" "i" "a")
		 (uim '(rk-lib-expect-seq '("k" "y") test-rk-rule)))

   (assert-equal '("o" "e" "u" "i" "a" "s")
		 (uim '(rk-lib-expect-seq '("s") test-rk-rule)))
   (assert-equal ()
		 (uim '(rk-lib-expect-seq '("s" "s") test-rk-rule)))
   (assert-equal '("p")
		 (uim '(rk-lib-expect-seq '("p") test-rk-rule)))
   (assert-equal ()
		 (uim '(rk-lib-expect-seq '("p" "p") test-rk-rule)))))
