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

(require-extension (unittest))

(test-begin "string-list-concat")
(test-equal ""
            (string-list-concat ()))
(test-equal ""
            (string-list-concat '("")))
(test-equal "foo"
            (string-list-concat '("foo")))
(test-equal "barfoo"
            (string-list-concat '("foo" "bar")))
(test-equal "bazbarfoo"
            (string-list-concat '("foo" "bar" "baz")))
(test-end)

(test-begin "string-find")
(test-false (string-find () ""))
(test-false (string-find () "quux"))
(test-false (string-find '("foo") ""))
(test-equal '("foo")
            (string-find '("foo") "foo"))
(test-false (string-find '("foo") "quux"))
(test-false (string-find '("foo" "bar") ""))
(test-equal '("foo" "bar")
            (string-find '("foo" "bar") "foo"))
(test-equal '("bar")
            (string-find '("foo" "bar") "bar"))
(test-false (string-find '("foo" "bar") "quux"))
(test-false (string-find '("foo" "bar" "baz") ""))
(test-equal '("foo" "bar" "baz")
            (string-find '("foo" "bar" "baz") "foo"))
(test-equal '("bar" "baz")
            (string-find '("foo" "bar" "baz") "bar"))
(test-equal '("baz")
            (string-find '("foo" "bar" "baz") "baz"))
(test-false (string-find '("foo" "bar" "baz") "quux"))
(test-end)

;; See "Specification changes of utility procedures" of doc/COMPATIBILITY
(test-begin "string-split-uim-1.5")
;; ordinary split
(test-equal '("h" "geh" "ge")
            (string-split "hogehoge" "o"))
;; case sensitive
(test-equal '("hogehoge")
            (string-split "hogehoge" "O"))
;; split by sequence
(test-equal '("h" "eh" "e")
            (string-split "hogehoge" "og"))
;; split by first character
(test-equal '("" "oge" "oge")
            (string-split "hogehoge" "h"))
;; split by first sequence
(test-equal '("" "ge" "ge")
            (string-split "hogehoge" "ho"))
;; split by last character
(test-equal '("hog" "hog" "")
            (string-split "hogehoge" "e"))
;; split by last sequence
(test-equal '("ho" "ho" "")
            (string-split "hogehoge" "ge"))
;; split by whole string
(test-equal '("" "")
            (string-split "hogehoge" "hogehoge"))
;; repeated splitter
(test-equal '("" "" "" "")
            (string-split "hhh" "h"))
;; split by space
(test-equal '("" "h" "o" "g" "e" "hoge")
            (string-split " h o g e hoge" " "))
;; split by symbolic character
(test-equal '("" "h" "o" "g" "e" "hoge")
            (string-split "|h|o|g|e|hoge" "|"))
;; split by non existent character
(test-equal '("hogehoge")
            (string-split "hogehoge" "|"))
(test-end)

;; split EUC-JP string into reversed character list
(test-begin "string-to-list")
(test-equal '()
            (string-to-list ""))
(test-equal '("s")
            (string-to-list "s"))
(test-equal '("t" "s")
            (string-to-list "st"))
(test-equal '("g" "n" "i" "r" "t" "s")
            (string-to-list "string"))
;; EUC-JP: "\xa4;\xa2;" = "あ", "\xc6;\xfc;" = "日", "\xcb;\xdc;" = "本",
;; "\xb8;\xec;" = "語"
(test-equal '("\xa4;\xa2;")
            (string-to-list "\xa4;\xa2;"))
(test-equal '("\xa4;\xa2;" "a")
            (string-to-list "a\xa4;\xa2;"))
(test-equal '("a" "\xa4;\xa2;")
            (string-to-list "\xa4;\xa2;a"))
(test-equal '("\xb8;\xec;" "\xcb;\xdc;" "\xc6;\xfc;")
            (string-to-list "\xc6;\xfc;\xcb;\xdc;\xb8;\xec;"))
(test-equal '("c" "\xb8;\xec;" "\xcb;\xdc;" "b" "\xc6;\xfc;" "a")
            (string-to-list "a\xc6;\xfc;b\xcb;\xdc;\xb8;\xec;c"))
(test-end)

(test-begin "string-to-list-utf8")
(test-equal '()
            (begin
              (require "japanese-utf8.scm")
              (string-to-list-utf8 "")))
(test-equal '("語" "本" "日")
            (begin
              (require "japanese-utf8.scm")
              (string-to-list-utf8 "日本語")))
(test-equal '("c" "語" "本" "b" "日" "a")
            (begin
              (require "japanese-utf8.scm")
              (string-to-list-utf8 "a日b本語c")))
(test-end)

(test-begin "string-contains")
(test-equal 0 (string-contains ""         "" 0))
(test-false   (string-contains ""         "f" 0))
(test-equal 0 (string-contains "foo"      "" 0))
(test-equal 0 (string-contains "foo"      "f" 0))
(test-equal 1 (string-contains "foo"      "o" 0))
(test-equal 1 (string-contains "foo"      "oo" 0))
(test-false   (string-contains "foo"      "oof" 0))
(test-equal 1 (string-contains "foo"      "o" 1))
(test-equal 2 (string-contains "foo"      "o" 2))
(test-end)

(test-begin "string-prefix?")
(test-eq #t (string-prefix? ""         "foo_bar"))
(test-eq #t (string-prefix? "f"        "foo_bar"))
(test-eq #t (string-prefix? "fo"       "foo_bar"))
(test-eq #t (string-prefix? "foo"      "foo_bar"))
(test-eq #t (string-prefix? "foo_"     "foo_bar"))
(test-eq #t (string-prefix? "foo_b"    "foo_bar"))
(test-eq #t (string-prefix? "foo_ba"   "foo_bar"))
(test-eq #t (string-prefix? "foo_bar"  "foo_bar"))
(test-false (string-prefix? "foo_bar_" "foo_bar"))
(test-error (string-prefix? #f         "foo_bar"))
(test-error (string-prefix? "foo_bar"  #f))
(test-false (string-prefix? "Foo"      "foo_bar"))
(test-false (string-prefix? "oo_"      "foo_bar"))
(test-false (string-prefix? "bar"      "foo_bar"))
(test-eq #t (string-prefix? ""    ""))
(test-false (string-prefix? "foo" ""))
(test-error (string-prefix? #f    ""))
(test-error (string-prefix? ""    #f))
(test-end)

(test-begin "string-prefix-ci?")
(test-eq #t (string-prefix-ci? ""         "foo_bar"))
(test-eq #t (string-prefix-ci? "f"        "foo_bar"))
(test-eq #t (string-prefix-ci? "fo"       "foo_bar"))
(test-eq #t (string-prefix-ci? "foo"      "foo_bar"))
(test-eq #t (string-prefix-ci? "foo_"     "foo_bar"))
(test-eq #t (string-prefix-ci? "foo_b"    "foo_bar"))
(test-eq #t (string-prefix-ci? "foo_ba"   "foo_bar"))
(test-eq #t (string-prefix-ci? "foo_bar"  "foo_bar"))
(test-false (string-prefix-ci? "foo_bar_" "foo_bar"))
(test-error (string-prefix-ci? #f         "foo_bar"))
(test-error (string-prefix-ci? "foo_bar"  #f))
(test-eq #t (string-prefix-ci? "Foo"      "foo_bar"))
(test-eq #t (string-prefix-ci? "fOo"      "foo_bar"))
(test-eq #t (string-prefix-ci? "fOO"      "foo_bar"))
(test-eq #t (string-prefix-ci? "FOO"      "foo_bar"))
(test-eq #t (string-prefix-ci? "FOO_bar"  "foo_bar"))
(test-false (string-prefix-ci? "oo_"      "foo_bar"))
(test-false (string-prefix-ci? "bar"      "foo_bar"))
(test-eq #t (string-prefix-ci? ""    ""))
(test-false (string-prefix-ci? "foo" ""))
(test-error (string-prefix-ci? #f    ""))
(test-error (string-prefix-ci? ""    #f))
(test-end)

(test-begin "string=?")
(test-eq #t (string=? "foo1" "foo1"))
(test-eq #t (string=? "Foo1" "Foo1"))
(test-eq #t (string=? "FOO1" "FOO1"))
(test-eq #t (string=? "1foo" "1foo"))
(test-eq #t (string=? "1Foo" "1Foo"))
(test-eq #t (string=? "1FOO" "1FOO"))
(test-eq #t (string=? "" ""))
(test-false (string=? "foo1" ""))
(test-false (string=? "" "foo1"))
(test-false (string=? "foo1" "Foo1"))
(test-false (string=? "Foo1" "foo1"))
(test-end)

(test-begin "charcode->string")
(test-equal ""   (charcode->string 'return))
(test-equal ""   (charcode->string 0))
(test-equal "\n" (charcode->string 10))
(test-equal "\r" (charcode->string 13))
(test-equal " "  (charcode->string 32))
(test-equal "!"  (charcode->string 33))
(test-equal "/"  (charcode->string 47))
(test-equal "0"  (charcode->string 48))
(test-equal "9"  (charcode->string 57))
(test-equal ":"  (charcode->string 58))
(test-equal "@"  (charcode->string 64))
(test-equal "A"  (charcode->string 65))
(test-equal "Z"  (charcode->string 90))
(test-equal "["  (charcode->string 91))
(test-equal "\\" (charcode->string 92))
(test-equal "`"  (charcode->string 96))
(test-equal "a"  (charcode->string 97))
(test-equal "z"  (charcode->string 122))
(test-equal "{"  (charcode->string 123))
(test-equal "~"  (charcode->string 126))
(test-end)

(test-begin "string->charcode")
(test-equal 0   (string->charcode ""))
(test-equal 10  (string->charcode "\n"))
(test-equal 13  (string->charcode "\r"))
(test-equal 32  (string->charcode " "))
(test-equal 33  (string->charcode "!"))
(test-equal 47  (string->charcode "/"))
(test-equal 48  (string->charcode "0"))
(test-equal 57  (string->charcode "9"))
(test-equal 58  (string->charcode ":"))
(test-equal 64  (string->charcode "@"))
(test-equal 65  (string->charcode "A"))
(test-equal 90  (string->charcode "Z"))
(test-equal 91  (string->charcode "["))
(test-equal 92  (string->charcode "\\"))
(test-equal 96  (string->charcode "`"))
(test-equal 97  (string->charcode "a"))
(test-equal 122 (string->charcode "z"))
(test-equal 123 (string->charcode "{"))
(test-equal 126 (string->charcode "~"))
(test-end)

(test-begin "digit->string")
(test-equal "-134217728" (digit->string -134217728))
(test-equal "-10"  (digit->string -10))
(test-equal "-2"   (digit->string -2))
(test-equal "-1"   (digit->string -1))
(test-equal "0"    (digit->string 0))
(test-equal "1"    (digit->string 1))
(test-equal "2"    (digit->string 2))
(test-equal "3"    (digit->string 3))
(test-equal "4"    (digit->string 4))
(test-equal "5"    (digit->string 5))
(test-equal "6"    (digit->string 6))
(test-equal "7"    (digit->string 7))
(test-equal "8"    (digit->string 8))
(test-equal "9"    (digit->string 9))
(test-equal "10"   (digit->string 10))
(test-equal "11"   (digit->string 11))
(test-equal "12"   (digit->string 12))
(test-equal "13"   (digit->string 13))
(test-equal "14"   (digit->string 14))
(test-equal "15"   (digit->string 15))
(test-equal "16"   (digit->string 16))
(test-equal "17"   (digit->string 17))
(test-equal "18"   (digit->string 18))
(test-equal "19"   (digit->string 19))
(test-equal "100"  (digit->string 100))
(test-equal "1000" (digit->string 1000))
(test-equal "134217727" (digit->string 134217727))
(test-end)

;; compare string sequence
(test-begin "str-seq-equal?")
(test-eq #t (str-seq-equal? () ()))
(test-eq #t (str-seq-equal? '("") '("")))
(test-false (str-seq-equal? () '("")))
(test-false (str-seq-equal? '("") ()))
(test-eq #t (str-seq-equal? '("a") '("a")))
(test-false (str-seq-equal? '("a") '("A")))
(test-false (str-seq-equal? '("a") '("b")))
(test-eq #t (str-seq-equal? '("a" "b" "c")
                            '("a" "b" "c")))
(test-false (str-seq-equal? '("a" "b" "c")
                            '("a" "b" "c" "d")))
(test-false (str-seq-equal? '("a" "b" "c")
                            '("z" "a" "b" "c")))
(test-false (str-seq-equal? '("a" "b" "c" "d")
                            '("a" "b" "c")))
(test-end)

;; Partial -> first string of remaining sequence
;;  eg. ("a" "b") ("a" "b" "c") -> "c"
;; Not partial -> #f
(test-begin "str-seq-partial?")
(test-false (str-seq-partial? () ()))
(test-false (str-seq-partial? '("") '("")))
(test-equal ""
            (str-seq-partial? () '("")))
(test-false (str-seq-partial? '("") ()))
(test-false (str-seq-partial? '("a") '("a")))
(test-false (str-seq-partial? '("a") '("A")))
(test-false (str-seq-partial? '("a") '("b")))
(test-equal "b"
            (str-seq-partial? '("a")
                              '("a" "b")))
(test-false (str-seq-partial? '("a" "b" "c")
                              '("a" "b" "c")))
(test-equal "d"
            (str-seq-partial? '("a" "b" "c")
                              '("a" "b" "c" "d")))
(test-equal "d"
            (str-seq-partial? '("a" "b" "c")
                              '("a" "b" "c" "d" "e")))
(test-false (str-seq-partial? '("a" "b" "c" "d")
                              '("a" "b" "c")))
(test-end)


(test-begin "string-join")
(test-equal ""
            (string-join () ()))
(test-error (string-join '(()) ()))
(test-error (string-join '(1) ()))
(test-error (string-join '(() ()) ()))
(test-error (string-join '(1 2) ()))
(test-error (string-join '(1 2 3) ()))
(test-error (string-join '(one two three) ()))
(test-error (string-join '("1" "2" "3") ()))
(test-error (string-join '(() () ()) ()))
(test-equal ""
            (string-join () "/"))
(test-equal ""
            (string-join '("") "/"))
(test-equal "1"
            (string-join '("1") "/"))
(test-equal "1/2"
            (string-join '("1" "2") "/"))
(test-equal "1/2/3"
            (string-join '("1" "2" "3") "/"))
(test-equal ""
            (string-join () "-sep-"))
(test-equal ""
            (string-join '("") "-sep-"))
(test-equal "1"
            (string-join '("1") "-sep-"))
(test-equal "1-sep-2"
            (string-join '("1" "2") "-sep-"))
(test-equal "1-sep-2-sep-3"
            (string-join '("1" "2" "3") "-sep-"))
(test-end)

(test-begin "string-append-map")
(test-equal ""
            (string-append-map car ()))
(test-equal "c"
            (string-append-map car '(("c" "C"))))
(test-equal "ca"
            (string-append-map car '(("c" "C") ("a" "A"))))
(test-equal "car"
            (string-append-map car '(("c" "C") ("a" "A") ("r" "R"))))
(test-end)

(test-report-result)
