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

(define-module test.util.test-string
  (use test.unit.test-case)
  (use test.uim-test))
(select-module test.util.test-string)

(define (setup)
  (uim-test-setup))

(define (teardown)
  (uim-test-teardown))

(define (test-string-list-concat)
  (assert-uim-equal ""
                    '(string-list-concat ()))
  (assert-uim-equal ""
                    '(string-list-concat '("")))
  (assert-uim-equal "foo"
                    '(string-list-concat '("foo")))
  (assert-uim-equal "barfoo"
                    '(string-list-concat '("foo" "bar")))
  (assert-uim-equal "bazbarfoo"
                    '(string-list-concat '("foo" "bar" "baz")))
  #f)

(define (test-string-find)
  (assert-uim-false '(string-find () ""))
  (assert-uim-false '(string-find () "quux"))
  (assert-uim-false '(string-find '("foo") ""))
  (assert-uim-equal '("foo")
                    '(string-find '("foo") "foo"))
  (assert-uim-false '(string-find '("foo") "quux"))
  (assert-uim-false '(string-find '("foo" "bar") ""))
  (assert-uim-equal '("foo" "bar")
                    '(string-find '("foo" "bar") "foo"))
  (assert-uim-equal '("bar")
                    '(string-find '("foo" "bar") "bar"))
  (assert-uim-false '(string-find '("foo" "bar") "quux"))
  (assert-uim-false '(string-find '("foo" "bar" "baz") ""))
  (assert-uim-equal '("foo" "bar" "baz")
                    '(string-find '("foo" "bar" "baz") "foo"))
  (assert-uim-equal '("bar" "baz")
                    '(string-find '("foo" "bar" "baz") "bar"))
  (assert-uim-equal '("baz")
                    '(string-find '("foo" "bar" "baz") "baz"))
  (assert-uim-false '(string-find '("foo" "bar" "baz") "quux"))
  #f)

;; See "Specification changes of utility procedures" of doc/COMPATIBILITY
(define (no-test-string-split-uim-1.4)
  ;; ordinary split
  (assert-uim-equal '("h" "geh" "ge")
                    '(string-split "hogehoge" "o"))
  ;; case sensitive
  (assert-uim-equal '("hogehoge")
                    '(string-split "hogehoge" "O"))
  ;; split by sequence
  (assert-uim-equal '("h" "eh" "e")
                    '(string-split "hogehoge" "og"))
  ;; split by first character
  (assert-uim-equal '("oge" "oge")
                    '(string-split "hogehoge" "h"))
  ;; split by first sequence
  (assert-uim-equal '("ge" "ge")
                    '(string-split "hogehoge" "ho"))
  ;; split by last character
  (assert-uim-equal '("hog" "hog")
                    '(string-split "hogehoge" "e"))
  ;; split by last sequence
  (assert-uim-equal '("ho" "ho")
                    '(string-split "hogehoge" "ge"))
  ;; split by whole string
  (assert-uim-equal ()
                    '(string-split "hogehoge" "hogehoge"))
  ;; repeated splitter
  (assert-uim-equal ()
                    '(string-split "hhh" "h"))
  ;; split by space
  (assert-uim-equal '("h" "o" "g" "e" "hoge")
                    '(string-split " h o g e hoge" " "))
  ;; split by symbolic character
  (assert-uim-equal '("h" "o" "g" "e" "hoge")
                    '(string-split "|h|o|g|e|hoge" "|"))
  ;; split by non existent character
  (assert-uim-equal '("hogehoge")
                    '(string-split "hogehoge" "|"))
  #f)

;; See "Specification changes of utility procedures" of doc/COMPATIBILITY
(define (test-string-split-uim-1.5)
  ;; ordinary split
  (assert-uim-equal '("h" "geh" "ge")
                    '(string-split "hogehoge" "o"))
  ;; case sensitive
  (assert-uim-equal '("hogehoge")
                    '(string-split "hogehoge" "O"))
  ;; split by sequence
  (assert-uim-equal '("h" "eh" "e")
                    '(string-split "hogehoge" "og"))
  ;; split by first character
  (assert-uim-equal '("" "oge" "oge")
                    '(string-split "hogehoge" "h"))
  ;; split by first sequence
  (assert-uim-equal '("" "ge" "ge")
                    '(string-split "hogehoge" "ho"))
  ;; split by last character
  (assert-uim-equal '("hog" "hog" "")
                    '(string-split "hogehoge" "e"))
  ;; split by last sequence
  (assert-uim-equal '("ho" "ho" "")
                    '(string-split "hogehoge" "ge"))
  ;; split by whole string
  (assert-uim-equal '("" "")
                    '(string-split "hogehoge" "hogehoge"))
  ;; repeated splitter
  (assert-uim-equal '("" "" "" "")
                    '(string-split "hhh" "h"))
  ;; split by space
  (assert-uim-equal '("" "h" "o" "g" "e" "hoge")
                    '(string-split " h o g e hoge" " "))
  ;; split by symbolic character
  (assert-uim-equal '("" "h" "o" "g" "e" "hoge")
                    '(string-split "|h|o|g|e|hoge" "|"))
  ;; split by non existent character
  (assert-uim-equal '("hogehoge")
                    '(string-split "hogehoge" "|"))
  #f)

;; split EUC-JP string into reversed character list
(define (test-string-to-list)
  (assert-uim-equal '()
                    '(string-to-list ""))
  (assert-uim-equal '("s")
                    '(string-to-list "s"))
  (assert-uim-equal '("t" "s")
                    '(string-to-list "st"))
  (assert-uim-equal '("g" "n" "i" "r" "t" "s")
                    '(string-to-list "string"))

  (assert-uim-equal-euc-jp '("あ")
                           '(string-to-list "あ"))
  (assert-uim-equal-euc-jp '("あ" "a")
                           '(string-to-list "aあ"))
  (assert-uim-equal-euc-jp '("a" "あ")
                           '(string-to-list "あa"))
  (assert-uim-equal-euc-jp '("語" "本" "日")
                           '(string-to-list "日本語"))
  (assert-uim-equal-euc-jp '("c" "語" "本" "b" "日" "a")
                           '(string-to-list "a日b本語c"))
  #f)

(define (test-string-contains)
  (assert-uim-equal 0 '(string-contains ""         "" 0))
  (assert-uim-false   '(string-contains ""         "f" 0))
  (assert-uim-equal 0 '(string-contains "foo"      "" 0))
  (assert-uim-equal 0 '(string-contains "foo"      "f" 0))
  (assert-uim-equal 1 '(string-contains "foo"      "o" 0))
  (assert-uim-equal 1 '(string-contains "foo"      "oo" 0))
  (assert-uim-false   '(string-contains "foo"      "oof" 0))
  (assert-uim-equal 1 '(string-contains "foo"      "o" 1))
  (assert-uim-equal 2 '(string-contains "foo"      "o" 2))
  #f)

(define (test-string-prefix?)
  (assert-uim-true  '(string-prefix? ""         "foo_bar"))
  (assert-uim-true  '(string-prefix? "f"        "foo_bar"))
  (assert-uim-true  '(string-prefix? "fo"       "foo_bar"))
  (assert-uim-true  '(string-prefix? "foo"      "foo_bar"))
  (assert-uim-true  '(string-prefix? "foo_"     "foo_bar"))
  (assert-uim-true  '(string-prefix? "foo_b"    "foo_bar"))
  (assert-uim-true  '(string-prefix? "foo_ba"   "foo_bar"))
  (assert-uim-true  '(string-prefix? "foo_bar"  "foo_bar"))
  (assert-uim-false '(string-prefix? "foo_bar_" "foo_bar"))
  (assert-uim-error '(string-prefix? #f         "foo_bar"))
  (assert-uim-error '(string-prefix? "foo_bar"  #f))
  (assert-uim-false '(string-prefix? "Foo"      "foo_bar"))
  (assert-uim-false '(string-prefix? "oo_"      "foo_bar"))
  (assert-uim-false '(string-prefix? "bar"      "foo_bar"))

  (assert-uim-true  '(string-prefix? ""    ""))
  (assert-uim-false '(string-prefix? "foo" ""))
  (assert-uim-error '(string-prefix? #f    ""))
  (assert-uim-error '(string-prefix? ""    #f))
  #f)

(define (test-string-prefix-ci?)
  (assert-uim-true  '(string-prefix-ci? ""         "foo_bar"))
  (assert-uim-true  '(string-prefix-ci? "f"        "foo_bar"))
  (assert-uim-true  '(string-prefix-ci? "fo"       "foo_bar"))
  (assert-uim-true  '(string-prefix-ci? "foo"      "foo_bar"))
  (assert-uim-true  '(string-prefix-ci? "foo_"     "foo_bar"))
  (assert-uim-true  '(string-prefix-ci? "foo_b"    "foo_bar"))
  (assert-uim-true  '(string-prefix-ci? "foo_ba"   "foo_bar"))
  (assert-uim-true  '(string-prefix-ci? "foo_bar"  "foo_bar"))
  (assert-uim-false '(string-prefix-ci? "foo_bar_" "foo_bar"))
  (assert-uim-error '(string-prefix-ci? #f         "foo_bar"))
  (assert-uim-error '(string-prefix-ci? "foo_bar"  #f))
  (assert-uim-true  '(string-prefix-ci? "Foo"      "foo_bar"))
  (assert-uim-true  '(string-prefix-ci? "fOo"      "foo_bar"))
  (assert-uim-true  '(string-prefix-ci? "fOO"      "foo_bar"))
  (assert-uim-true  '(string-prefix-ci? "FOO"      "foo_bar"))
  (assert-uim-true  '(string-prefix-ci? "FOO_bar"  "foo_bar"))
  (assert-uim-false '(string-prefix-ci? "oo_"      "foo_bar"))
  (assert-uim-false '(string-prefix-ci? "bar"      "foo_bar"))

  (assert-uim-true  '(string-prefix-ci? ""    ""))
  (assert-uim-false '(string-prefix-ci? "foo" ""))
  (assert-uim-error '(string-prefix-ci? #f    ""))
  (assert-uim-error '(string-prefix-ci? ""    #f))
  #f)

(define (test-string=?)
  (assert-uim-true  '(string=? "foo1" "foo1"))
  (assert-uim-true  '(string=? "Foo1" "Foo1"))
  (assert-uim-true  '(string=? "FOO1" "FOO1"))
  (assert-uim-true  '(string=? "1foo" "1foo"))
  (assert-uim-true  '(string=? "1Foo" "1Foo"))
  (assert-uim-true  '(string=? "1FOO" "1FOO"))
  (assert-uim-true  '(string=? "" ""))
  (assert-uim-false '(string=? "foo1" ""))
  (assert-uim-false '(string=? "" "foo1"))
  (assert-uim-false '(string=? "foo1" "Foo1"))
  (assert-uim-false '(string=? "Foo1" "foo1"))
  #f)

(define (test-charcode->string)
  (assert-uim-equal ""   '(charcode->string 'return))
  (assert-uim-equal ""   '(charcode->string 0))
  (assert-uim-equal "\n" '(charcode->string 10))
  (assert-uim-equal "\r" '(charcode->string 13))
  (assert-uim-equal " "  '(charcode->string 32))
  (assert-uim-equal "!"  '(charcode->string 33))
  (assert-uim-equal "/"  '(charcode->string 47))
  (assert-uim-equal "0"  '(charcode->string 48))
  (assert-uim-equal "9"  '(charcode->string 57))
  (assert-uim-equal ":"  '(charcode->string 58))
  (assert-uim-equal "@"  '(charcode->string 64))
  (assert-uim-equal "A"  '(charcode->string 65))
  (assert-uim-equal "Z"  '(charcode->string 90))
  (assert-uim-equal "["  '(charcode->string 91))
  (assert-uim-equal "\\" '(charcode->string 92))
  (assert-uim-equal "`"  '(charcode->string 96))
  (assert-uim-equal "a"  '(charcode->string 97))
  (assert-uim-equal "z"  '(charcode->string 122))
  (assert-uim-equal "{"  '(charcode->string 123))
  (assert-uim-equal "~"  '(charcode->string 126))
  #f)

(define (test-string->charcode)
  (assert-uim-equal 0   '(string->charcode ""))
  (assert-uim-equal 10  '(string->charcode "\n"))
  (assert-uim-equal 13  '(string->charcode "\r"))
  (assert-uim-equal 32  '(string->charcode " "))
  (assert-uim-equal 33  '(string->charcode "!"))
  (assert-uim-equal 47  '(string->charcode "/"))
  (assert-uim-equal 48  '(string->charcode "0"))
  (assert-uim-equal 57  '(string->charcode "9"))
  (assert-uim-equal 58  '(string->charcode ":"))
  (assert-uim-equal 64  '(string->charcode "@"))
  (assert-uim-equal 65  '(string->charcode "A"))
  (assert-uim-equal 90  '(string->charcode "Z"))
  (assert-uim-equal 91  '(string->charcode "["))
  (assert-uim-equal 92  '(string->charcode "\\"))
  (assert-uim-equal 96  '(string->charcode "`"))
  (assert-uim-equal 97  '(string->charcode "a"))
  (assert-uim-equal 122 '(string->charcode "z"))
  (assert-uim-equal 123 '(string->charcode "{"))
  (assert-uim-equal 126 '(string->charcode "~"))
  #f)

(define (test-digit->string)
  ;; for storage-fatty@32-bit
  ;;(assert-uim-equal "-2147483648" '(digit->string -2147483648))
  ;; for storage-compact@32-bit
  (assert-uim-equal "-134217728" '(digit->string -134217728))
  (assert-uim-equal "-10"  '(digit->string -10))
  (assert-uim-equal "-2"   '(digit->string -2))
  (assert-uim-equal "-1"   '(digit->string -1))
  (assert-uim-equal "0"    '(digit->string 0))
  (assert-uim-equal "1"    '(digit->string 1))
  (assert-uim-equal "2"    '(digit->string 2))
  (assert-uim-equal "3"    '(digit->string 3))
  (assert-uim-equal "4"    '(digit->string 4))
  (assert-uim-equal "5"    '(digit->string 5))
  (assert-uim-equal "6"    '(digit->string 6))
  (assert-uim-equal "7"    '(digit->string 7))
  (assert-uim-equal "8"    '(digit->string 8))
  (assert-uim-equal "9"    '(digit->string 9))
  (assert-uim-equal "10"   '(digit->string 10))
  (assert-uim-equal "11"   '(digit->string 11))
  (assert-uim-equal "12"   '(digit->string 12))
  (assert-uim-equal "13"   '(digit->string 13))
  (assert-uim-equal "14"   '(digit->string 14))
  (assert-uim-equal "15"   '(digit->string 15))
  (assert-uim-equal "16"   '(digit->string 16))
  (assert-uim-equal "17"   '(digit->string 17))
  (assert-uim-equal "18"   '(digit->string 18))
  (assert-uim-equal "19"   '(digit->string 19))
  (assert-uim-equal "100"  '(digit->string 100))
  (assert-uim-equal "1000" '(digit->string 1000))
  ;; for storage-fatty@32-bit
  ;;(assert-uim-equal "2147483647" '(digit->string 2147483647))
  ;; for storage-compact@32-bit
  (assert-uim-equal "134217727" '(digit->string 134217727))
  #f)

;; compare string sequence
(define (test-str-seq-equal?)
  (assert-uim-true  '(str-seq-equal? () ()))
  (assert-uim-true  '(str-seq-equal? '("") '("")))
  (assert-uim-false '(str-seq-equal? () '("")))
  (assert-uim-false '(str-seq-equal? '("") ()))
  (assert-uim-true  '(str-seq-equal? '("a") '("a")))
  (assert-uim-false '(str-seq-equal? '("a") '("A")))
  (assert-uim-false '(str-seq-equal? '("a") '("b")))
  (assert-uim-true  '(str-seq-equal? '("a" "b" "c")
                                     '("a" "b" "c")))
  (assert-uim-false '(str-seq-equal? '("a" "b" "c")
                                     '("a" "b" "c" "d")))
  (assert-uim-false '(str-seq-equal? '("a" "b" "c")
                                     '("z" "a" "b" "c")))
  (assert-uim-false '(str-seq-equal? '("a" "b" "c" "d")
                                     '("a" "b" "c")))
  #f)

;; Partial -> first string of remaining sequence
;;  eg. ("a" "b") ("a" "b" "c") -> "c"
;; Not partial -> #f
(define (test-str-seq-partial?)
  (assert-uim-false '(str-seq-partial? () ()))
  (assert-uim-false '(str-seq-partial? '("") '("")))
  (assert-uim-equal ""
                    '(str-seq-partial? () '("")))
  (assert-uim-false '(str-seq-partial? '("") ()))
  (assert-uim-false '(str-seq-partial? '("a") '("a")))
  (assert-uim-false '(str-seq-partial? '("a") '("A")))
  (assert-uim-false '(str-seq-partial? '("a") '("b")))
  (assert-uim-equal "b"
                    '(str-seq-partial? '("a")
                                       '("a" "b")))
  (assert-uim-false '(str-seq-partial? '("a" "b" "c")
                                       '("a" "b" "c")))
  (assert-uim-equal "d"
                    '(str-seq-partial? '("a" "b" "c")
                                       '("a" "b" "c" "d")))
  (assert-uim-equal "d"
                    '(str-seq-partial? '("a" "b" "c")
                                       '("a" "b" "c" "d" "e")))
  (assert-uim-false '(str-seq-partial? '("a" "b" "c" "d")
                                       '("a" "b" "c")))
  #f)


(define (test-string-join)
  (assert-uim-equal ""
                    '(string-join () ()))
  (assert-uim-error '(string-join '(()) ()))
  (assert-uim-error '(string-join '(1) ()))
  (assert-uim-error '(string-join '(() ()) ()))
  (assert-uim-error '(string-join '(1 2) ()))
  (assert-uim-error '(string-join '(1 2 3) ()))
  (assert-uim-error '(string-join '(one two three) ()))
  (assert-uim-error '(string-join '("1" "2" "3") ()))
  (assert-uim-error '(string-join '(() () ()) ()))

  (assert-uim-equal ""
                    '(string-join () "/"))
  (assert-uim-equal ""
                    '(string-join '("") "/"))
  (assert-uim-equal "1"
                    '(string-join '("1") "/"))
  (assert-uim-equal "1/2"
                    '(string-join '("1" "2") "/"))
  (assert-uim-equal "1/2/3"
                    '(string-join '("1" "2" "3") "/"))

  (assert-uim-equal ""
                    '(string-join () "-sep-"))
  (assert-uim-equal ""
                    '(string-join '("") "-sep-"))
  (assert-uim-equal "1"
                    '(string-join '("1") "-sep-"))
  (assert-uim-equal "1-sep-2"
                    '(string-join '("1" "2") "-sep-"))
  (assert-uim-equal "1-sep-2-sep-3"
                    '(string-join '("1" "2" "3") "-sep-"))
  #f)

(define (test-string-append-map)
  (assert-uim-equal ""
                    '(string-append-map car ()))
  (assert-uim-equal "c"
                    '(string-append-map car '(("c" "C"))))
  (assert-uim-equal "ca"
                    '(string-append-map car '(("c" "C") ("a" "A"))))
  (assert-uim-equal "car"
                    '(string-append-map car '(("c" "C") ("a" "A") ("r" "R"))))
  #f)

(provide "test/util/test-string")
