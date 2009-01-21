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

(define-uim-test-case "testcase i18n locale"
  ("test locale-new"
   ;; full format
   (assert-equal '("ja" "JP" "EUC-JP")
		 (uim '(locale-new "ja_JP.EUC-JP")))
   ;; codeset can be omitted
   (assert-equal '("ja" "JP" "")
		 (uim '(locale-new "ja_JP")))
   ;; territory can also be omitted
   (assert-equal '("ja" "" "")
		 (uim '(locale-new "ja")))
   ;; codeset without territory is a valid format
   (assert-equal '("ja" "" "EUC-JP")
		 (uim '(locale-new "ja.EUC-JP")))
   ;; #f, "C" and "POSIX" is interpreted as "en" locale
   (assert-equal '("en" "" "")
		 (uim '(locale-new #f)))
   (assert-equal '("en" "" "")
		 (uim '(locale-new "C")))
   (assert-equal '("en" "" "")
		 (uim '(locale-new "POSIX")))
   ;; invalid locale strings are rejected
   (assert-equal '("" "" "")
		 (uim '(locale-new "d_DE")))
   (assert-equal '("" "" "")
		 (uim '(locale-new "deu_DE")))
   (assert-equal '("" "" "")
		 (uim '(locale-new "de_de_DE")))
   (assert-equal '("" "" "")
		 (uim '(locale-new "de_D")))
   (assert-equal '("" "" "")
		 (uim '(locale-new "de_DEU")))
   (assert-equal '("" "" "")
		 (uim '(locale-new "de_de_DEU")))
   (assert-equal '("" "" "")
		 (uim '(locale-new "_DE")))
   (assert-equal '("" "" "")
		 (uim '(locale-new "_DE.")))
   (assert-equal '("" "" "")
		 (uim '(locale-new "_DE.UTF-8")))
   (assert-equal '("" "" "")
		 (uim '(locale-new "de_.UTF-8")))
   (assert-equal '("" "" "")
		 (uim '(locale-new "_.UTF-8")))
   (assert-equal '("" "" "")
		 (uim '(locale-new ".UTF-8"))))

  ;; empty locale is instructs to use the locale of native environment
  ("test locale-new native environment"
   ;; it fallbacks to "en" if neither LC_ALL nor LANG defined
   (uim '(unsetenv  "LC_ALL"))
   (uim '(unsetenv  "LANG"))
   (assert-equal '("en" "" "")
		 (uim '(locale-new "")))
   ;; it looks both LC_ALL and LANG
   (uim '(setenv  "LC_ALL" "ja_JP.EUC-JP" #t))
   (uim '(unsetenv  "LANG"))
   (assert-equal '("ja" "JP" "EUC-JP")
		 (uim '(locale-new "")))
   (uim '(unsetenv  "LC_ALL"))
   (uim '(setenv  "LANG" "ja_JP.EUC-JP" #t))
   (assert-equal '("ja" "JP" "EUC-JP")
		 (uim '(locale-new "")))
   ;; LC_ALL precedes LANG
   (uim '(setenv  "LC_ALL" "de_DE.UTF-8" #t))
   (uim '(setenv  "LANG" "ja_JP.EUC-JP" #t))
   (assert-equal '("de" "DE" "UTF-8")
		 (uim '(locale-new "")))
   ;; special locale name from the variables
   (uim '(setenv  "LC_ALL" "C" #t))
   (uim '(unsetenv  "LANG"))
   (assert-equal '("en" "" "")
		 (uim '(locale-new "")))
   (uim '(setenv  "LC_ALL" "POSIX" #t))
   (uim '(unsetenv  "LANG"))
   (assert-equal '("en" "" "")
		 (uim '(locale-new "")))
   (uim '(unsetenv  "LC_ALL"))
   (uim '(setenv  "LANG" "C" #t))
   (assert-equal '("en" "" "")
		 (uim '(locale-new "")))
   (uim '(unsetenv  "LC_ALL"))
   (uim '(setenv  "LANG" "POSIX" #t))
   (assert-equal '("en" "" "")
		 (uim '(locale-new ""))))

  ("test locale-set-lang!"
   (assert-equal ""
		 (uim '(locale-set-lang! (locale-new "en") "")))
   ;; valid langs
   (assert-equal "ja"
		 (uim '(locale-set-lang! (locale-new "") "ja")))
   (assert-equal "zh"
		 (uim '(locale-set-lang! (locale-new "") "zh")))
   (assert-equal "de"
		 (uim '(locale-set-lang! (locale-new "") "de")))
   ;; locale-set-lang! only accepts two-letter language codes
   (assert-equal ""
		 (uim '(locale-set-lang! (locale-new "") "jpn")))
   (assert-equal ""
		 (uim '(locale-set-lang! (locale-new "") "zh_CN")))
   (assert-equal ""
		 (uim '(locale-set-lang! (locale-new "") "d"))))

  ("test locale-set-territory!"
   (assert-equal ""
		 (uim '(locale-set-territory! (locale-new "en") "")))
   ;; valid territories
   (assert-equal "JP"
		 (uim '(locale-set-territory! (locale-new "") "JP")))
   (assert-equal "CN"
		 (uim '(locale-set-territory! (locale-new "") "CN")))
   (assert-equal "DE"
		 (uim '(locale-set-territory! (locale-new "") "DE")))
   ;; locale-set-territory! only accepts two-letter country codes
   (assert-equal ""
		 (uim '(locale-set-territory! (locale-new "") "Japan")))
   (assert-equal ""
		 (uim '(locale-set-territory! (locale-new "") "zh_CN")))
   (assert-equal ""
		 (uim '(locale-set-territory! (locale-new "") "ger"))))

  ("test locale-lang-territory-str"
   (assert-equal ""
		 (uim '(locale-lang-territory-str (locale-new "invalid_IN."))))
   (assert-equal "ja_JP"
		 (uim '(locale-lang-territory-str (locale-new "ja_JP.EUC-JP"))))
   (assert-equal "ja_JP"
		 (uim '(locale-lang-territory-str (locale-new "ja_JP"))))
   (assert-equal "ja"
		 (uim '(locale-lang-territory-str (locale-new "ja"))))
   (assert-equal "ja"
		 (uim '(locale-lang-territory-str (locale-new "ja.EUC-JP"))))
   (assert-equal "zh_CN"
		 (uim '(locale-lang-territory-str (locale-new "zh_CN.UTF-8"))))
   (assert-equal "zh_CN"
		 (uim '(locale-lang-territory-str (locale-new "zh_CN"))))
   (assert-equal "zh"
		 (uim '(locale-lang-territory-str (locale-new "zh"))))
   (assert-equal "zh"
		 (uim '(locale-lang-territory-str (locale-new "zh.UTF-8")))))

  ("test locale-str"
   (assert-equal ""
		 (uim '(locale-str (locale-new "invalid_IN."))))
   (assert-equal "ja_JP.EUC-JP"
		 (uim '(locale-str (locale-new "ja_JP.EUC-JP"))))
   (assert-equal "ja_JP"
		 (uim '(locale-str (locale-new "ja_JP"))))
   (assert-equal "ja"
		 (uim '(locale-str (locale-new "ja"))))
   (assert-equal "ja.EUC-JP"
		 (uim '(locale-str (locale-new "ja.EUC-JP"))))
   (assert-equal "zh_CN.UTF-8"
		 (uim '(locale-str (locale-new "zh_CN.UTF-8"))))
   (assert-equal "zh_CN"
		 (uim '(locale-str (locale-new "zh_CN"))))
   (assert-equal "zh"
		 (uim '(locale-str (locale-new "zh"))))
   (assert-equal "zh.UTF-8"
		 (uim '(locale-str (locale-new "zh.UTF-8")))))

  ("test locale-zh-awared-lang"
   (assert-equal ""
		 (uim '(locale-zh-awared-lang (locale-new "invalid_IN."))))
   (assert-equal "ja"
		 (uim '(locale-zh-awared-lang (locale-new "ja_JP.EUC-JP"))))
   (assert-equal "ja"
		 (uim '(locale-zh-awared-lang (locale-new "ja_JP"))))
   (assert-equal "ja"
		 (uim '(locale-zh-awared-lang (locale-new "ja"))))
   (assert-equal "ja"
		 (uim '(locale-zh-awared-lang (locale-new "ja.EUC-JP"))))
   (assert-equal "en"
		 (uim '(locale-zh-awared-lang (locale-new "en_US.UTF-8"))))
   (assert-equal "en"
		 (uim '(locale-zh-awared-lang (locale-new "en_US"))))
   (assert-equal "en"
		 (uim '(locale-zh-awared-lang (locale-new "en"))))
   (assert-equal "en"
		 (uim '(locale-zh-awared-lang (locale-new "en.UTF-8"))))
   ;; returns "zh_XX" form if lang part is "zh"
   (assert-equal "zh_CN"
		 (uim '(locale-zh-awared-lang (locale-new "zh_CN.UTF-8"))))
   (assert-equal "zh_CN"
		 (uim '(locale-zh-awared-lang (locale-new "zh_CN"))))
   (assert-equal "zh"
		 (uim '(locale-zh-awared-lang (locale-new "zh"))))
   (assert-equal "zh"
		 (uim '(locale-zh-awared-lang (locale-new "zh.UTF-8"))))
   (assert-equal "zh_TW"
		 (uim '(locale-zh-awared-lang (locale-new "zh_TW.UTF-8"))))
   (assert-equal "zh_TW"
		 (uim '(locale-zh-awared-lang (locale-new "zh_TW"))))
   (assert-equal "zh"
		 (uim '(locale-zh-awared-lang (locale-new "zh"))))
   (assert-equal "zh"
		 (uim '(locale-zh-awared-lang (locale-new "zh.UTF-8"))))
   (assert-equal "zh_HK"
		 (uim '(locale-zh-awared-lang (locale-new "zh_HK.UTF-8"))))
   (assert-equal "zh_HK"
		 (uim '(locale-zh-awared-lang (locale-new "zh_HK"))))
   (assert-equal "zh"
		 (uim '(locale-zh-awared-lang (locale-new "zh"))))
   (assert-equal "zh"
		 (uim '(locale-zh-awared-lang (locale-new "zh.UTF-8")))))

  ("test langgroup-covers?"
   ;; exact match
   (assert-true  (uim-bool '(langgroup-covers? "ja" "ja")))
   (assert-true  (uim-bool '(langgroup-covers? "en" "en")))
   (assert-true  (uim-bool '(langgroup-covers? "de" "de")))
   (assert-true  (uim-bool '(langgroup-covers? "fr" "fr")))
   (assert-true  (uim-bool '(langgroup-covers? "zh" "zh")))
   (assert-true  (uim-bool '(langgroup-covers? "zh_CN" "zh_CN")))
   (assert-true  (uim-bool '(langgroup-covers? "zh_TW" "zh_TW")))
   (assert-true  (uim-bool '(langgroup-covers? "zh_HK" "zh_HK")))
   (assert-false (uim-bool '(langgroup-covers? "de" "ja")))
   (assert-false (uim-bool '(langgroup-covers? "de" "en")))
   (assert-false (uim-bool '(langgroup-covers? "de" "fr")))
   (assert-false (uim-bool '(langgroup-covers? "de" "zh")))
   (assert-false (uim-bool '(langgroup-covers? "de" "zh_CN")))
   (assert-false (uim-bool '(langgroup-covers? "de" "zh_TW")))
   (assert-false (uim-bool '(langgroup-covers? "de" "zh_HK")))
   ;; group match
   (assert-false (uim-bool '(langgroup-covers? "de:en:fr" "ja")))
   (assert-true  (uim-bool '(langgroup-covers? "de:en:fr" "en")))
   (assert-true  (uim-bool '(langgroup-covers? "de:en:fr" "de")))
   (assert-true  (uim-bool '(langgroup-covers? "de:en:fr" "fr")))
   (assert-false (uim-bool '(langgroup-covers? "de:en:fr" "zh")))
   (assert-false (uim-bool '(langgroup-covers? "de:en:fr" "zh_CN")))
   (assert-false (uim-bool '(langgroup-covers? "de:en:fr" "zh_TW")))
   (assert-false (uim-bool '(langgroup-covers? "de:en:fr" "zh_HK")))
   ;; group expression is only allowed for first arg
   (assert-false (uim-bool '(langgroup-covers? "de:en:fr" "de:en")))
   (assert-false (uim-bool '(langgroup-covers? "de:en:fr" "de:en:fr")))
   ;; wildcard
   (assert-true  (uim-bool '(langgroup-covers? "*" "ja")))
   (assert-true  (uim-bool '(langgroup-covers? "*" "en")))
   (assert-true  (uim-bool '(langgroup-covers? "*" "de")))
   (assert-true  (uim-bool '(langgroup-covers? "*" "fr")))
   (assert-true  (uim-bool '(langgroup-covers? "*" "zh")))
   (assert-true  (uim-bool '(langgroup-covers? "*" "zh_CN")))
   (assert-true  (uim-bool '(langgroup-covers? "*" "zh_TW")))
   (assert-true  (uim-bool '(langgroup-covers? "*" "zh_HK")))
   ;; wildcard is only allowed for first arg
   (assert-false (uim-bool '(langgroup-covers? "en" "*")))
   ;; 'nothing'
   (assert-false (uim-bool '(langgroup-covers? "" "ja")))
   (assert-false (uim-bool '(langgroup-covers? "" "en")))
   (assert-false (uim-bool '(langgroup-covers? "" "de")))
   (assert-false (uim-bool '(langgroup-covers? "" "fr")))
   (assert-false (uim-bool '(langgroup-covers? "" "zh")))
   (assert-false (uim-bool '(langgroup-covers? "" "zh_CN")))
   (assert-false (uim-bool '(langgroup-covers? "" "zh_TW")))
   (assert-false (uim-bool '(langgroup-covers? "" "zh_HK")))
   (assert-false (uim-bool '(langgroup-covers? "" "*")))
   (assert-false (uim-bool '(langgroup-covers? "" "")))
   ;; no special handling for Chinese
   (assert-false (uim-bool '(langgroup-covers? "zh" "zh_CN")))
   (assert-false (uim-bool '(langgroup-covers? "zh" "zh_TW")))
   (assert-false (uim-bool '(langgroup-covers? "zh" "zh_HK")))
   (assert-false (uim-bool '(langgroup-covers? "zh_CN" "zh")))
   (assert-false (uim-bool '(langgroup-covers? "zh_TW" "zh")))
   (assert-false (uim-bool '(langgroup-covers? "zh_HK" "zh")))
   (assert-false (uim-bool '(langgroup-covers? "zh_CN" "zh_TW")))
   (assert-false (uim-bool '(langgroup-covers? "zh_CN" "zh_HK")))
   (assert-false (uim-bool '(langgroup-covers? "zh_HK" "zh_CN")))
   (assert-false (uim-bool '(langgroup-covers? "zh_HK" "zh_TW")))
   (assert-true  (uim-bool '(langgroup-covers? "zh_TW:zh_HK" "zh_TW")))
   (assert-true  (uim-bool '(langgroup-covers? "zh_TW:zh_HK" "zh_HK")))
   (assert-false (uim-bool '(langgroup-covers? "zh_TW:zh_HK" "zh_CN")))
   (assert-false (uim-bool '(langgroup-covers? "zh_TW:zh_HK" "zh")))
   (assert-false (uim-bool '(langgroup-covers? "zh_CN:zh_TW:zh_HK" "zh")))))

(define-uim-test-case "testcase i18n ISO 639-1 language code #1"
  (setup
   (lambda ()
     (sys-putenv "LC_ALL" "C")))

  ("test lang-code->lang-name in en locale"
   (assert-equal "Japanese"
		 (uim '(lang-code->lang-name "ja")))
   (assert-equal "English"
		 (uim '(lang-code->lang-name "en")))
   (assert-equal "Chinese"
		 (uim '(lang-code->lang-name "zh")))))

;(define-uim-test-case "testcase i18n ISO 639-1 language code #2"
;  (setup
;   (lambda ()
;     (sys-putenv "LC_ALL" "ja_JP")))
;
;  ("test lang-code->lang-name in ja_JP locale"
;   (uim '(bind_textdomain_codeset "uim" "UTF-8"))
;   (assert-equal "日本語"
;		 (uim '(lang-code->lang-name "ja")))
;   (assert-equal "英語"
;		 (uim '(lang-code->lang-name "en")))
;   (assert-equal "中国語"
;		 (uim '(lang-code->lang-name "zh")))))
