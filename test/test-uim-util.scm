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

(define-uim-test-case "testcase uim-util"
  (setup
   (lambda ()
     (uim '(require "util.scm"))
     (uim '(define lst '(1 2 3 4)))))

  ;; See "Specification changes of utility procedures" of doc/COMPATIBILITY
;;  ("test string-split (uim 1.4)"
;;   ;; ordinary split
;;   (assert-equal '("h" "geh" "ge")
;;		 (uim '(string-split "hogehoge" "o")))
;;   ;; case sensitive
;;   (assert-equal '("hogehoge")
;;		 (uim '(string-split "hogehoge" "O")))
;;   ;; split by sequence
;;   (assert-equal '("h" "eh" "e")
;;		 (uim '(string-split "hogehoge" "og")))
;;   ;; split by first character
;;   (assert-equal '("oge" "oge")
;;		 (uim '(string-split "hogehoge" "h")))
;;   ;; split by first sequence
;;   (assert-equal '("ge" "ge")
;;		 (uim '(string-split "hogehoge" "ho")))
;;   ;; split by last character
;;   (assert-equal '("hog" "hog")
;;		 (uim '(string-split "hogehoge" "e")))
;;   ;; split by last sequence
;;   (assert-equal '("ho" "ho")
;;		 (uim '(string-split "hogehoge" "ge")))
;;   ;; split by whole string
;;   (assert-equal ()
;;		 (uim '(string-split "hogehoge" "hogehoge")))
;;   ;; repeated splitter
;;   (assert-equal ()
;;		 (uim '(string-split "hhh" "h")))
;;   ;; split by space
;;   (assert-equal '("h" "o" "g" "e" "hoge")
;;		 (uim '(string-split " h o g e hoge" " ")))
;;   ;; split by symbolic character
;;   (assert-equal '("h" "o" "g" "e" "hoge")
;;		 (uim '(string-split "|h|o|g|e|hoge" "|")))
;;   ;; split by non existent character
;;   (assert-equal '("hogehoge")
;;		 (uim '(string-split "hogehoge" "|"))))

  ;; See "Specification changes of utility procedures" of doc/COMPATIBILITY
  ("test string-split (uim 1.5)"
   ;; ordinary split
   (assert-equal '("h" "geh" "ge")
		 (uim '(string-split "hogehoge" "o")))
   ;; case sensitive
   (assert-equal '("hogehoge")
		 (uim '(string-split "hogehoge" "O")))
   ;; split by sequence
   (assert-equal '("h" "eh" "e")
		 (uim '(string-split "hogehoge" "og")))
   ;; split by first character
   (assert-equal '("" "oge" "oge")
		 (uim '(string-split "hogehoge" "h")))
   ;; split by first sequence
   (assert-equal '("" "ge" "ge")
		 (uim '(string-split "hogehoge" "ho")))
   ;; split by last character
   (assert-equal '("hog" "hog" "")
		 (uim '(string-split "hogehoge" "e")))
   ;; split by last sequence
   (assert-equal '("ho" "ho" "")
		 (uim '(string-split "hogehoge" "ge")))
   ;; split by whole string
   (assert-equal '("" "")
		 (uim '(string-split "hogehoge" "hogehoge")))
   ;; repeated splitter
   (assert-equal '("" "" "" "")
		 (uim '(string-split "hhh" "h")))
   ;; split by space
   (assert-equal '("" "h" "o" "g" "e" "hoge")
		 (uim '(string-split " h o g e hoge" " ")))
   ;; split by symbolic character
   (assert-equal '("" "h" "o" "g" "e" "hoge")
		 (uim '(string-split "|h|o|g|e|hoge" "|")))
   ;; split by non existent character
   (assert-equal '("hogehoge")
		 (uim '(string-split "hogehoge" "|"))))

  ;; split EUC-JP string into reversed character list
  ("test string-to-list"
   (assert-equal '()
		 (uim '(string-to-list "")))
   (assert-equal '("s")
		 (uim '(string-to-list "s")))
   (assert-equal '("t" "s")
		 (uim '(string-to-list "st")))
   (assert-equal '("g" "n" "i" "r" "t" "s")
		 (uim '(string-to-list "string")))
   ;; TODO: activate following EUC-JP string test
;   (assert-equal '("あ")
;		 (uim '(string-to-list "あ")))
;   (assert-equal '("あ" "a")
;		 (uim '(string-to-list "aあ")))
;   (assert-equal '("a" "あ")
;		 (uim '(string-to-list "あa")))
;   (assert-equal '("語" "本" "日")
;		 (uim '(string-to-list "日本語")))
;   (assert-equal '("c" "語" "本" "b" "日" "a")
;		 (uim '(string-to-list "a日b本語c")))
   )

  ("test string-contains"
   (assert-equal 0 (uim '(string-contains ""         "" 0)))
   (assert-false   (uim '(string-contains ""         "f" 0)))
   (assert-equal 0 (uim '(string-contains "foo"      "" 0)))
   (assert-equal 0 (uim '(string-contains "foo"      "f" 0)))
   (assert-equal 1 (uim '(string-contains "foo"      "o" 0)))
   (assert-equal 1 (uim '(string-contains "foo"      "oo" 0)))
   (assert-false   (uim '(string-contains "foo"      "oof" 0)))
   (assert-equal 1 (uim '(string-contains "foo"      "o" 1)))
   (assert-equal 2 (uim '(string-contains "foo"      "o" 2))))

  ("test string-prefix?"
   (assert-true  (uim-bool '(string-prefix? ""         "foo_bar")))
   (assert-true  (uim-bool '(string-prefix? "f"        "foo_bar")))
   (assert-true  (uim-bool '(string-prefix? "fo"       "foo_bar")))
   (assert-true  (uim-bool '(string-prefix? "foo"      "foo_bar")))
   (assert-true  (uim-bool '(string-prefix? "foo_"     "foo_bar")))
   (assert-true  (uim-bool '(string-prefix? "foo_b"    "foo_bar")))
   (assert-true  (uim-bool '(string-prefix? "foo_ba"   "foo_bar")))
   (assert-true  (uim-bool '(string-prefix? "foo_bar"  "foo_bar")))
   (assert-false (uim-bool '(string-prefix? "foo_bar_" "foo_bar")))
   (assert-error (lambda () (uim-bool '(string-prefix? #f         "foo_bar"))))
   (assert-error (lambda () (uim-bool '(string-prefix? "foo_bar"  #f))))
   (assert-false (uim-bool '(string-prefix? "Foo"      "foo_bar")))
   (assert-false (uim-bool '(string-prefix? "oo_"      "foo_bar")))
   (assert-false (uim-bool '(string-prefix? "bar"      "foo_bar")))

   (assert-true  (uim-bool '(string-prefix? ""    "")))
   (assert-false (uim-bool '(string-prefix? "foo" "")))
   (assert-error (lambda () (uim-bool '(string-prefix? #f    ""))))
   (assert-error (lambda () (uim-bool '(string-prefix? ""    #f)))))

  ("test string-prefix-ci?"
   (assert-true  (uim-bool '(string-prefix-ci? ""         "foo_bar")))
   (assert-true  (uim-bool '(string-prefix-ci? "f"        "foo_bar")))
   (assert-true  (uim-bool '(string-prefix-ci? "fo"       "foo_bar")))
   (assert-true  (uim-bool '(string-prefix-ci? "foo"      "foo_bar")))
   (assert-true  (uim-bool '(string-prefix-ci? "foo_"     "foo_bar")))
   (assert-true  (uim-bool '(string-prefix-ci? "foo_b"    "foo_bar")))
   (assert-true  (uim-bool '(string-prefix-ci? "foo_ba"   "foo_bar")))
   (assert-true  (uim-bool '(string-prefix-ci? "foo_bar"  "foo_bar")))
   (assert-false (uim-bool '(string-prefix-ci? "foo_bar_" "foo_bar")))
   (assert-error (lambda () (uim-bool '(string-prefix-ci? #f         "foo_bar"))))
   (assert-error (lambda () (uim-bool '(string-prefix-ci? "foo_bar"  #f))))
   (assert-true  (uim-bool '(string-prefix-ci? "Foo"      "foo_bar")))
   (assert-true  (uim-bool '(string-prefix-ci? "fOo"      "foo_bar")))
   (assert-true  (uim-bool '(string-prefix-ci? "fOO"      "foo_bar")))
   (assert-true  (uim-bool '(string-prefix-ci? "FOO"      "foo_bar")))
   (assert-true  (uim-bool '(string-prefix-ci? "FOO_bar"  "foo_bar")))
   (assert-false (uim-bool '(string-prefix-ci? "oo_"      "foo_bar")))
   (assert-false (uim-bool '(string-prefix-ci? "bar"      "foo_bar")))

   (assert-true  (uim-bool '(string-prefix-ci? ""    "")))
   (assert-false (uim-bool '(string-prefix-ci? "foo" "")))
   (assert-error (lambda () (uim-bool '(string-prefix-ci? #f    ""))))
   (assert-error (lambda () (uim-bool '(string-prefix-ci? ""    #f)))))

  ("test string=?"
   (assert-true  (uim-bool '(string=? "foo1" "foo1")))
   (assert-true  (uim-bool '(string=? "Foo1" "Foo1")))
   (assert-true  (uim-bool '(string=? "FOO1" "FOO1")))
   (assert-true  (uim-bool '(string=? "1foo" "1foo")))
   (assert-true  (uim-bool '(string=? "1Foo" "1Foo")))
   (assert-true  (uim-bool '(string=? "1FOO" "1FOO")))
   (assert-true  (uim-bool '(string=? "" "")))
   (assert-false (uim-bool '(string=? "foo1" "")))
   (assert-false (uim-bool '(string=? "" "foo1")))
   (assert-false (uim-bool '(string=? "foo1" "Foo1")))
   (assert-false (uim-bool '(string=? "Foo1" "foo1"))))

  ("test nthcdr"
   (assert-equal '(1 2 3 4)
		 (uim '(nthcdr 0 lst)))
   (assert-equal '(2 3 4)
		 (uim '(nthcdr 1 lst)))
   (assert-equal '(3 4)
		 (uim '(nthcdr 2 lst)))
   (assert-equal '(4)
		 (uim '(nthcdr 3 lst)))
   (assert-equal ()
		 (uim '(nthcdr 4 lst)))
   (assert-equal #f
		 (uim '(nthcdr 5 lst))))

  ("test charcode->string"
   (assert-equal ""   (uim '(charcode->string 'return)))
   (assert-equal ""   (uim '(charcode->string 0)))
   (assert-equal "\n" (uim '(charcode->string 10)))
   (assert-equal "\r" (uim '(charcode->string 13)))
   (assert-equal " "  (uim '(charcode->string 32)))
   (assert-equal "!"  (uim '(charcode->string 33)))
   (assert-equal "/"  (uim '(charcode->string 47)))
   (assert-equal "0"  (uim '(charcode->string 48)))
   (assert-equal "9"  (uim '(charcode->string 57)))
   (assert-equal ":"  (uim '(charcode->string 58)))
   (assert-equal "@"  (uim '(charcode->string 64)))
   (assert-equal "A"  (uim '(charcode->string 65)))
   (assert-equal "Z"  (uim '(charcode->string 90)))
   (assert-equal "["  (uim '(charcode->string 91)))
   (assert-equal "\\" (uim '(charcode->string 92)))
   (assert-equal "`"  (uim '(charcode->string 96)))
   (assert-equal "a"  (uim '(charcode->string 97)))
   (assert-equal "z"  (uim '(charcode->string 122)))
   (assert-equal "{"  (uim '(charcode->string 123)))
   (assert-equal "~"  (uim '(charcode->string 126))))

  ("test string->charcode"
   (assert-equal 0   (uim '(string->charcode "")))
   (assert-equal 10  (uim '(string->charcode "\n")))
   (assert-equal 13  (uim '(string->charcode "\r")))
   (assert-equal 32  (uim '(string->charcode " ")))
   (assert-equal 33  (uim '(string->charcode "!")))
   (assert-equal 47  (uim '(string->charcode "/")))
   (assert-equal 48  (uim '(string->charcode "0")))
   (assert-equal 57  (uim '(string->charcode "9")))
   (assert-equal 58  (uim '(string->charcode ":")))
   (assert-equal 64  (uim '(string->charcode "@")))
   (assert-equal 65  (uim '(string->charcode "A")))
   (assert-equal 90  (uim '(string->charcode "Z")))
   (assert-equal 91  (uim '(string->charcode "[")))
   (assert-equal 92  (uim '(string->charcode "\\")))
   (assert-equal 96  (uim '(string->charcode "`")))
   (assert-equal 97  (uim '(string->charcode "a")))
   (assert-equal 122 (uim '(string->charcode "z")))
   (assert-equal 123 (uim '(string->charcode "{")))
   (assert-equal 126 (uim '(string->charcode "~"))))

  ("test digit->string"
   ;; for storage-fatty@32-bit
   ;;(assert-equal "-2147483648" (uim '(digit->string -2147483648)))
   ;; for storage-compact@32-bit
   (assert-equal "-134217728" (uim '(digit->string -134217728)))
   (assert-equal "-10"  (uim '(digit->string -10)))
   (assert-equal "-2"   (uim '(digit->string -2)))
   (assert-equal "-1"   (uim '(digit->string -1)))
   (assert-equal "0"    (uim '(digit->string 0)))
   (assert-equal "1"    (uim '(digit->string 1)))
   (assert-equal "2"    (uim '(digit->string 2)))
   (assert-equal "3"    (uim '(digit->string 3)))
   (assert-equal "4"    (uim '(digit->string 4)))
   (assert-equal "5"    (uim '(digit->string 5)))
   (assert-equal "6"    (uim '(digit->string 6)))
   (assert-equal "7"    (uim '(digit->string 7)))
   (assert-equal "8"    (uim '(digit->string 8)))
   (assert-equal "9"    (uim '(digit->string 9)))
   (assert-equal "10"   (uim '(digit->string 10)))
   (assert-equal "11"   (uim '(digit->string 11)))
   (assert-equal "12"   (uim '(digit->string 12)))
   (assert-equal "13"   (uim '(digit->string 13)))
   (assert-equal "14"   (uim '(digit->string 14)))
   (assert-equal "15"   (uim '(digit->string 15)))
   (assert-equal "16"   (uim '(digit->string 16)))
   (assert-equal "17"   (uim '(digit->string 17)))
   (assert-equal "18"   (uim '(digit->string 18)))
   (assert-equal "19"   (uim '(digit->string 19)))
   (assert-equal "100"  (uim '(digit->string 100)))
   (assert-equal "1000" (uim '(digit->string 1000)))
   ;; for storage-fatty@32-bit
   ;;(assert-equal "2147483647" (uim '(digit->string 2147483647)))
   ;; for storage-compact@32-bit
   (assert-equal "134217727" (uim '(digit->string 134217727)))
   )

  ;; compare string sequence
  ("test str-seq-equal?"
   (assert-true  (uim-bool '(str-seq-equal? () ())))
   (assert-true  (uim-bool '(str-seq-equal? '("") '(""))))
   (assert-false (uim-bool '(str-seq-equal? () '(""))))
   (assert-false (uim-bool '(str-seq-equal? '("") ())))
   (assert-true  (uim-bool '(str-seq-equal? '("a") '("a"))))
   (assert-false (uim-bool '(str-seq-equal? '("a") '("A"))))
   (assert-false (uim-bool '(str-seq-equal? '("a") '("b"))))
   (assert-true  (uim-bool '(str-seq-equal? '("a" "b" "c")
					    '("a" "b" "c"))))
   (assert-false (uim-bool '(str-seq-equal? '("a" "b" "c")
					    '("a" "b" "c" "d"))))
   (assert-false (uim-bool '(str-seq-equal? '("a" "b" "c")
					    '("z" "a" "b" "c"))))
   (assert-false (uim-bool '(str-seq-equal? '("a" "b" "c" "d")
					    '("a" "b" "c")))))

  ;; Partial -> first string of remaining sequence
  ;;  eg. ("a" "b") ("a" "b" "c") -> "c"
  ;; Not partial -> #f
  ("test str-seq-partial?"
   (assert-false (uim-bool '(str-seq-partial? () ())))
   (assert-false (uim-bool '(str-seq-partial? '("") '(""))))
   (assert-equal ""
		 (uim '(str-seq-partial? () '(""))))
   (assert-false (uim-bool '(str-seq-partial? '("") ())))
   (assert-false (uim-bool '(str-seq-partial? '("a") '("a"))))
   (assert-false (uim-bool '(str-seq-partial? '("a") '("A"))))
   (assert-false (uim-bool '(str-seq-partial? '("a") '("b"))))
   (assert-equal "b"
		 (uim '(str-seq-partial? '("a")
					 '("a" "b"))))
   (assert-false (uim-bool '(str-seq-partial? '("a" "b" "c")
					      '("a" "b" "c"))))
   (assert-equal "d"
		 (uim '(str-seq-partial? '("a" "b" "c")
					 '("a" "b" "c" "d"))))
   (assert-equal "d"
		 (uim '(str-seq-partial? '("a" "b" "c")
					 '("a" "b" "c" "d" "e"))))
   (assert-false (uim-bool '(str-seq-partial? '("a" "b" "c" "d")
					      '("a" "b" "c"))))))


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
