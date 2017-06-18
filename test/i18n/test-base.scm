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

;; These tests are passed at revision 6605 (new repository)

(define-module test.i18n.test-base
  (use test.unit.test-case)
  (use test.uim-test))
(select-module test.i18n.test-base)

(define (setup)
  (uim-test-setup))

(define (teardown)
  (uim-test-teardown))

(define (test-locale-new)
  ;; full format
  (assert-uim-equal '("ja" "JP" "EUC-JP")
                    '(locale-new "ja_JP.EUC-JP"))
  ;; codeset can be omitted
  (assert-uim-equal '("ja" "JP" "")
                    '(locale-new "ja_JP"))
  ;; territory can also be omitted
  (assert-uim-equal '("ja" "" "")
                    '(locale-new "ja"))
  ;; codeset without territory is a valid format
  (assert-uim-equal '("ja" "" "EUC-JP")
                    '(locale-new "ja.EUC-JP"))
  ;; #f, "C" and "POSIX" is interpreted as "en" locale
  (assert-uim-equal '("en" "" "")
                    '(locale-new #f))
  (assert-uim-equal '("en" "" "")
                    '(locale-new "C"))
  (assert-uim-equal '("en" "" "")
                    '(locale-new "POSIX"))
  ;; invalid locale strings are rejected
  (assert-uim-equal '("" "" "")
                    '(locale-new "d_DE"))
  (assert-uim-equal '("" "" "")
                    '(locale-new "deu_DE"))
  (assert-uim-equal '("" "" "")
                    '(locale-new "de_de_DE"))
  (assert-uim-equal '("" "" "")
                    '(locale-new "de_D"))
  (assert-uim-equal '("" "" "")
                    '(locale-new "de_DEU"))
  (assert-uim-equal '("" "" "")
                    '(locale-new "de_de_DEU"))
  (assert-uim-equal '("" "" "")
                    '(locale-new "_DE"))
  (assert-uim-equal '("" "" "")
                    '(locale-new "_DE."))
  (assert-uim-equal '("" "" "")
                    '(locale-new "_DE.UTF-8"))
  (assert-uim-equal '("" "" "")
                    '(locale-new "de_.UTF-8"))
  (assert-uim-equal '("" "" "")
                    '(locale-new "_.UTF-8"))
  (assert-uim-equal '("" "" "")
                    '(locale-new ".UTF-8"))
  #f)

;; empty locale is instructs to use the locale of native environment
(define (test-locale-new-native-environment)
  ;; it fallbacks to "en" if neither LC_ALL nor LANG defined
  (uim-eval '(unsetenv  "LC_ALL"))
  (uim-eval '(unsetenv  "LANG"))
  (assert-uim-equal '("en" "" "")
                    '(locale-new ""))
  ;; it looks both LC_ALL and LANG
  (uim-eval '(setenv  "LC_ALL" "ja_JP.EUC-JP" #t))
  (uim-eval '(unsetenv  "LANG"))
  (assert-uim-equal '("ja" "JP" "EUC-JP")
                    '(locale-new ""))
  (uim-eval '(unsetenv  "LC_ALL"))
  (uim-eval '(setenv  "LANG" "ja_JP.EUC-JP" #t))
  (assert-uim-equal '("ja" "JP" "EUC-JP")
                    '(locale-new ""))
  ;; LC_ALL precedes LANG
  (uim-eval '(setenv  "LC_ALL" "de_DE.UTF-8" #t))
  (uim-eval '(setenv  "LANG" "ja_JP.EUC-JP" #t))
  (assert-uim-equal '("de" "DE" "UTF-8")
                    '(locale-new ""))
  ;; special locale name from the variables
  (uim-eval '(setenv  "LC_ALL" "C" #t))
  (uim-eval '(unsetenv  "LANG"))
  (assert-uim-equal '("en" "" "")
                    '(locale-new ""))
  (uim-eval '(setenv  "LC_ALL" "POSIX" #t))
  (uim-eval '(unsetenv  "LANG"))
  (assert-uim-equal '("en" "" "")
                    '(locale-new ""))
  (uim-eval '(unsetenv  "LC_ALL"))
  (uim-eval '(setenv  "LANG" "C" #t))
  (assert-uim-equal '("en" "" "")
                    '(locale-new ""))
  (uim-eval '(unsetenv  "LC_ALL"))
  (uim-eval '(setenv  "LANG" "POSIX" #t))
  (assert-uim-equal '("en" "" "")
                    '(locale-new ""))
  #f)

(define (test-locale-set-lang!)
  (assert-uim-equal ""
                    '(locale-set-lang! (locale-new "en") ""))
  ;; valid langs
  (assert-uim-equal "ja"
                    '(locale-set-lang! (locale-new "") "ja"))
  (assert-uim-equal "zh"
                    '(locale-set-lang! (locale-new "") "zh"))
  (assert-uim-equal "de"
                    '(locale-set-lang! (locale-new "") "de"))
  ;; locale-set-lang! only accepts two-letter language codes
  (assert-uim-equal ""
                    '(locale-set-lang! (locale-new "") "jpn"))
  (assert-uim-equal ""
                    '(locale-set-lang! (locale-new "") "zh_CN"))
  (assert-uim-equal ""
                    '(locale-set-lang! (locale-new "") "d"))
  #f)

(define (test-locale-set-territory!)
  (assert-uim-equal ""
                    '(locale-set-territory! (locale-new "en") ""))
  ;; valid territories
  (assert-uim-equal "JP"
                    '(locale-set-territory! (locale-new "") "JP"))
  (assert-uim-equal "CN"
                    '(locale-set-territory! (locale-new "") "CN"))
  (assert-uim-equal "DE"
                    '(locale-set-territory! (locale-new "") "DE"))
  ;; locale-set-territory! only accepts two-letter country codes
  (assert-uim-equal ""
                    '(locale-set-territory! (locale-new "") "Japan"))
  (assert-uim-equal ""
                    '(locale-set-territory! (locale-new "") "zh_CN"))
  (assert-uim-equal ""
                    '(locale-set-territory! (locale-new "") "ger"))
  #f)

(define (test-locale-lang-territory-str)
  (assert-uim-equal ""
                    '(locale-lang-territory-str (locale-new "invalid_IN.")))
  (assert-uim-equal "ja_JP"
                    '(locale-lang-territory-str (locale-new "ja_JP.EUC-JP")))
  (assert-uim-equal "ja_JP"
                    '(locale-lang-territory-str (locale-new "ja_JP")))
  (assert-uim-equal "ja"
                    '(locale-lang-territory-str (locale-new "ja")))
  (assert-uim-equal "ja"
                    '(locale-lang-territory-str (locale-new "ja.EUC-JP")))
  (assert-uim-equal "zh_CN"
                    '(locale-lang-territory-str (locale-new "zh_CN.UTF-8")))
  (assert-uim-equal "zh_CN"
                    '(locale-lang-territory-str (locale-new "zh_CN")))
  (assert-uim-equal "zh"
                    '(locale-lang-territory-str (locale-new "zh")))
  (assert-uim-equal "zh"
                    '(locale-lang-territory-str (locale-new "zh.UTF-8")))
  #f)

(define (test-locale-str)
  (assert-uim-equal ""
                    '(locale-str (locale-new "invalid_IN.")))
  (assert-uim-equal "ja_JP.EUC-JP"
                    '(locale-str (locale-new "ja_JP.EUC-JP")))
  (assert-uim-equal "ja_JP"
                    '(locale-str (locale-new "ja_JP")))
  (assert-uim-equal "ja"
                    '(locale-str (locale-new "ja")))
  (assert-uim-equal "ja.EUC-JP"
                    '(locale-str (locale-new "ja.EUC-JP")))
  (assert-uim-equal "zh_CN.UTF-8"
                    '(locale-str (locale-new "zh_CN.UTF-8")))
  (assert-uim-equal "zh_CN"
                    '(locale-str (locale-new "zh_CN")))
  (assert-uim-equal "zh"
                    '(locale-str (locale-new "zh")))
  (assert-uim-equal "zh.UTF-8"
                    '(locale-str (locale-new "zh.UTF-8")))
  #f)

(define (test-locale-zh-awared-lang)
  (assert-uim-equal ""
                    '(locale-zh-awared-lang (locale-new "invalid_IN.")))
  (assert-uim-equal "ja"
                    '(locale-zh-awared-lang (locale-new "ja_JP.EUC-JP")))
  (assert-uim-equal "ja"
                    '(locale-zh-awared-lang (locale-new "ja_JP")))
  (assert-uim-equal "ja"
                    '(locale-zh-awared-lang (locale-new "ja")))
  (assert-uim-equal "ja"
                    '(locale-zh-awared-lang (locale-new "ja.EUC-JP")))
  (assert-uim-equal "en"
                    '(locale-zh-awared-lang (locale-new "en_US.UTF-8")))
  (assert-uim-equal "en"
                    '(locale-zh-awared-lang (locale-new "en_US")))
  (assert-uim-equal "en"
                    '(locale-zh-awared-lang (locale-new "en")))
  (assert-uim-equal "en"
                    '(locale-zh-awared-lang (locale-new "en.UTF-8")))
  ;; returns "zh_XX" form if lang part is "zh"
  (assert-uim-equal "zh_CN"
                    '(locale-zh-awared-lang (locale-new "zh_CN.UTF-8")))
  (assert-uim-equal "zh_CN"
                    '(locale-zh-awared-lang (locale-new "zh_CN")))
  (assert-uim-equal "zh"
                    '(locale-zh-awared-lang (locale-new "zh")))
  (assert-uim-equal "zh"
                    '(locale-zh-awared-lang (locale-new "zh.UTF-8")))
  (assert-uim-equal "zh_TW"
                    '(locale-zh-awared-lang (locale-new "zh_TW.UTF-8")))
  (assert-uim-equal "zh_TW"
                    '(locale-zh-awared-lang (locale-new "zh_TW")))
  (assert-uim-equal "zh"
                    '(locale-zh-awared-lang (locale-new "zh")))
  (assert-uim-equal "zh"
                    '(locale-zh-awared-lang (locale-new "zh.UTF-8")))
  (assert-uim-equal "zh_HK"
                    '(locale-zh-awared-lang (locale-new "zh_HK.UTF-8")))
  (assert-uim-equal "zh_HK"
                    '(locale-zh-awared-lang (locale-new "zh_HK")))
  (assert-uim-equal "zh"
                    '(locale-zh-awared-lang (locale-new "zh")))
  (assert-uim-equal "zh"
                    '(locale-zh-awared-lang (locale-new "zh.UTF-8")))
  #f)

(define (test-langgroup-covers?)
  ;; exact match
  (assert-uim-equal '("ja")
                    '(langgroup-covers? "ja" "ja"))
  (assert-uim-equal '("en")
                    '(langgroup-covers? "en" "en"))
  (assert-uim-equal '("de")
                    '(langgroup-covers? "de" "de"))
  (assert-uim-equal '("fr")
                    '(langgroup-covers? "fr" "fr"))
  (assert-uim-equal '("zh")
                    '(langgroup-covers? "zh" "zh"))
  (assert-uim-equal '("zh_CN")
                    '(langgroup-covers? "zh_CN" "zh_CN"))
  (assert-uim-equal '("zh_TW")
                    '(langgroup-covers? "zh_TW" "zh_TW"))
  (assert-uim-equal '("zh_HK")
                    '(langgroup-covers? "zh_HK" "zh_HK"))
  (assert-uim-false '(langgroup-covers? "de" "ja"))
  (assert-uim-false '(langgroup-covers? "de" "en"))
  (assert-uim-false '(langgroup-covers? "de" "fr"))
  (assert-uim-false '(langgroup-covers? "de" "zh"))
  (assert-uim-false '(langgroup-covers? "de" "zh_CN"))
  (assert-uim-false '(langgroup-covers? "de" "zh_TW"))
  (assert-uim-false '(langgroup-covers? "de" "zh_HK"))
  ;; group match
  (assert-uim-false '(langgroup-covers? "de:en:fr" "ja"))
  (assert-uim-equal '("en" "fr")
                    '(langgroup-covers? "de:en:fr" "en"))
  (assert-uim-equal '("de" "en" "fr")
                    '(langgroup-covers? "de:en:fr" "de"))
  (assert-uim-equal '("fr")
                    '(langgroup-covers? "de:en:fr" "fr"))
  (assert-uim-false '(langgroup-covers? "de:en:fr" "zh"))
  (assert-uim-false '(langgroup-covers? "de:en:fr" "zh_CN"))
  (assert-uim-false '(langgroup-covers? "de:en:fr" "zh_TW"))
  (assert-uim-false '(langgroup-covers? "de:en:fr" "zh_HK"))
  ;; group expression is only allowed for first arg
  (assert-uim-false '(langgroup-covers? "de:en:fr" "de:en"))
  (assert-uim-false '(langgroup-covers? "de:en:fr" "de:en:fr"))
  ;; wildcard
  (assert-uim-true  '(langgroup-covers? "*" "ja"))
  (assert-uim-true  '(langgroup-covers? "*" "en"))
  (assert-uim-true  '(langgroup-covers? "*" "de"))
  (assert-uim-true  '(langgroup-covers? "*" "fr"))
  (assert-uim-true  '(langgroup-covers? "*" "zh"))
  (assert-uim-true  '(langgroup-covers? "*" "zh_CN"))
  (assert-uim-true  '(langgroup-covers? "*" "zh_TW"))
  (assert-uim-true  '(langgroup-covers? "*" "zh_HK"))
  ;; wildcard is only allowed for first arg
  (assert-uim-false '(langgroup-covers? "en" "*"))
  ;; 'nothing'
  (assert-uim-false '(langgroup-covers? "" "ja"))
  (assert-uim-false '(langgroup-covers? "" "en"))
  (assert-uim-false '(langgroup-covers? "" "de"))
  (assert-uim-false '(langgroup-covers? "" "fr"))
  (assert-uim-false '(langgroup-covers? "" "zh"))
  (assert-uim-false '(langgroup-covers? "" "zh_CN"))
  (assert-uim-false '(langgroup-covers? "" "zh_TW"))
  (assert-uim-false '(langgroup-covers? "" "zh_HK"))
  (assert-uim-false '(langgroup-covers? "" "*"))
  (assert-uim-false '(langgroup-covers? "" ""))
  ;; no special handling for Chinese
  (assert-uim-false '(langgroup-covers? "zh" "zh_CN"))
  (assert-uim-false '(langgroup-covers? "zh" "zh_TW"))
  (assert-uim-false '(langgroup-covers? "zh" "zh_HK"))
  (assert-uim-false '(langgroup-covers? "zh_CN" "zh"))
  (assert-uim-false '(langgroup-covers? "zh_TW" "zh"))
  (assert-uim-false '(langgroup-covers? "zh_HK" "zh"))
  (assert-uim-false '(langgroup-covers? "zh_CN" "zh_TW"))
  (assert-uim-false '(langgroup-covers? "zh_CN" "zh_HK"))
  (assert-uim-false '(langgroup-covers? "zh_HK" "zh_CN"))
  (assert-uim-false '(langgroup-covers? "zh_HK" "zh_TW"))
  (assert-uim-equal '("zh_TW" "zh_HK")
                    '(langgroup-covers? "zh_TW:zh_HK" "zh_TW"))
  (assert-uim-equal '("zh_HK")
                    '(langgroup-covers? "zh_TW:zh_HK" "zh_HK"))
  (assert-uim-false '(langgroup-covers? "zh_TW:zh_HK" "zh_CN"))
  (assert-uim-false '(langgroup-covers? "zh_TW:zh_HK" "zh"))
  (assert-uim-false '(langgroup-covers? "zh_CN:zh_TW:zh_HK" "zh"))
  #f)

(provide "test/i18n/test-base")
