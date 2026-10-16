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
;;;;

(require-extension (unittest))

(test-begin "locale-new")
;; full format
(test-equal '("ja" "JP" "EUC-JP")
            (locale-new "ja_JP.EUC-JP"))
;; codeset can be omitted
(test-equal '("ja" "JP" "")
            (locale-new "ja_JP"))
;; territory can also be omitted
(test-equal '("ja" "" "")
            (locale-new "ja"))
;; codeset without territory is a valid format
(test-equal '("ja" "" "EUC-JP")
            (locale-new "ja.EUC-JP"))
;; #f, "C" and "POSIX" is interpreted as "en" locale
(test-equal '("en" "" "")
            (locale-new #f))
(test-equal '("en" "" "")
            (locale-new "C"))
(test-equal '("en" "" "")
            (locale-new "POSIX"))
;; invalid locale strings are rejected
(test-equal '("" "" "")
            (locale-new "d_DE"))
(test-equal '("" "" "")
            (locale-new "deu_DE"))
(test-equal '("" "" "")
            (locale-new "de_de_DE"))
(test-equal '("" "" "")
            (locale-new "de_D"))
(test-equal '("" "" "")
            (locale-new "de_DEU"))
(test-equal '("" "" "")
            (locale-new "de_de_DEU"))
(test-equal '("" "" "")
            (locale-new "_DE"))
(test-equal '("" "" "")
            (locale-new "_DE."))
(test-equal '("" "" "")
            (locale-new "_DE.UTF-8"))
(test-equal '("" "" "")
            (locale-new "de_.UTF-8"))
(test-equal '("" "" "")
            (locale-new "_.UTF-8"))
(test-equal '("" "" "")
            (locale-new ".UTF-8"))
(test-end)

;; empty locale is instructs to use the locale of native environment
(test-begin "locale-new-native-environment")
;; it fallbacks to "en" if neither LC_ALL nor LANG defined
(unsetenv  "LC_ALL")
(unsetenv  "LANG")
(test-equal '("en" "" "")
            (locale-new ""))
;; it looks both LC_ALL and LANG
(setenv  "LC_ALL" "ja_JP.EUC-JP" #t)
(unsetenv  "LANG")
(test-equal '("ja" "JP" "EUC-JP")
            (locale-new ""))
(unsetenv  "LC_ALL")
(setenv  "LANG" "ja_JP.EUC-JP" #t)
(test-equal '("ja" "JP" "EUC-JP")
            (locale-new ""))
;; LC_ALL precedes LANG
(setenv  "LC_ALL" "de_DE.UTF-8" #t)
(setenv  "LANG" "ja_JP.EUC-JP" #t)
(test-equal '("de" "DE" "UTF-8")
            (locale-new ""))
;; special locale name from the variables
(setenv  "LC_ALL" "C" #t)
(unsetenv  "LANG")
(test-equal '("en" "" "")
            (locale-new ""))
(setenv  "LC_ALL" "POSIX" #t)
(unsetenv  "LANG")
(test-equal '("en" "" "")
            (locale-new ""))
(unsetenv  "LC_ALL")
(setenv  "LANG" "C" #t)
(test-equal '("en" "" "")
            (locale-new ""))
(unsetenv  "LC_ALL")
(setenv  "LANG" "POSIX" #t)
(test-equal '("en" "" "")
            (locale-new ""))
(test-end)

;; The setters return unspecified values. Check the set values instead.
(define (test-set-lang! locale lang)
  (locale-set-lang! locale lang)
  (locale-lang locale))

(define (test-set-territory! locale territory)
  (locale-set-territory! locale territory)
  (locale-territory locale))

(test-begin "locale-set-lang!")
(test-equal ""
            (test-set-lang! (locale-new "en") ""))
;; valid langs
(test-equal "ja"
            (test-set-lang! (locale-new "") "ja"))
(test-equal "zh"
            (test-set-lang! (locale-new "") "zh"))
(test-equal "de"
            (test-set-lang! (locale-new "") "de"))
;; locale-set-lang! only accepts two-letter language codes
(test-equal ""
            (test-set-lang! (locale-new "") "jpn"))
(test-equal ""
            (test-set-lang! (locale-new "") "zh_CN"))
(test-equal ""
            (test-set-lang! (locale-new "") "d"))
(test-end)

(test-begin "locale-set-territory!")
(test-equal ""
            (test-set-territory! (locale-new "en") ""))
;; valid territories
(test-equal "JP"
            (test-set-territory! (locale-new "") "JP"))
(test-equal "CN"
            (test-set-territory! (locale-new "") "CN"))
(test-equal "DE"
            (test-set-territory! (locale-new "") "DE"))
;; locale-set-territory! only accepts two-letter country codes
(test-equal ""
            (test-set-territory! (locale-new "") "Japan"))
(test-equal ""
            (test-set-territory! (locale-new "") "zh_CN"))
(test-equal ""
            (test-set-territory! (locale-new "") "ger"))
(test-end)

(test-begin "locale-lang-territory-str")
(test-equal ""
            (locale-lang-territory-str (locale-new "invalid_IN.")))
(test-equal "ja_JP"
            (locale-lang-territory-str (locale-new "ja_JP.EUC-JP")))
(test-equal "ja_JP"
            (locale-lang-territory-str (locale-new "ja_JP")))
(test-equal "ja"
            (locale-lang-territory-str (locale-new "ja")))
(test-equal "ja"
            (locale-lang-territory-str (locale-new "ja.EUC-JP")))
(test-equal "zh_CN"
            (locale-lang-territory-str (locale-new "zh_CN.UTF-8")))
(test-equal "zh_CN"
            (locale-lang-territory-str (locale-new "zh_CN")))
(test-equal "zh"
            (locale-lang-territory-str (locale-new "zh")))
(test-equal "zh"
            (locale-lang-territory-str (locale-new "zh.UTF-8")))
(test-end)

(test-begin "locale-str")
(test-equal ""
            (locale-str (locale-new "invalid_IN.")))
(test-equal "ja_JP.EUC-JP"
            (locale-str (locale-new "ja_JP.EUC-JP")))
(test-equal "ja_JP"
            (locale-str (locale-new "ja_JP")))
(test-equal "ja"
            (locale-str (locale-new "ja")))
(test-equal "ja.EUC-JP"
            (locale-str (locale-new "ja.EUC-JP")))
(test-equal "zh_CN.UTF-8"
            (locale-str (locale-new "zh_CN.UTF-8")))
(test-equal "zh_CN"
            (locale-str (locale-new "zh_CN")))
(test-equal "zh"
            (locale-str (locale-new "zh")))
(test-equal "zh.UTF-8"
            (locale-str (locale-new "zh.UTF-8")))
(test-end)

(test-begin "locale-zh-awared-lang")
(test-equal ""
            (locale-zh-awared-lang (locale-new "invalid_IN.")))
(test-equal "ja"
            (locale-zh-awared-lang (locale-new "ja_JP.EUC-JP")))
(test-equal "ja"
            (locale-zh-awared-lang (locale-new "ja_JP")))
(test-equal "ja"
            (locale-zh-awared-lang (locale-new "ja")))
(test-equal "ja"
            (locale-zh-awared-lang (locale-new "ja.EUC-JP")))
(test-equal "en"
            (locale-zh-awared-lang (locale-new "en_US.UTF-8")))
(test-equal "en"
            (locale-zh-awared-lang (locale-new "en_US")))
(test-equal "en"
            (locale-zh-awared-lang (locale-new "en")))
(test-equal "en"
            (locale-zh-awared-lang (locale-new "en.UTF-8")))
;; returns "zh_XX" form if lang part is "zh"
(test-equal "zh_CN"
            (locale-zh-awared-lang (locale-new "zh_CN.UTF-8")))
(test-equal "zh_CN"
            (locale-zh-awared-lang (locale-new "zh_CN")))
(test-equal "zh"
            (locale-zh-awared-lang (locale-new "zh")))
(test-equal "zh"
            (locale-zh-awared-lang (locale-new "zh.UTF-8")))
(test-equal "zh_TW"
            (locale-zh-awared-lang (locale-new "zh_TW.UTF-8")))
(test-equal "zh_TW"
            (locale-zh-awared-lang (locale-new "zh_TW")))
(test-equal "zh"
            (locale-zh-awared-lang (locale-new "zh")))
(test-equal "zh"
            (locale-zh-awared-lang (locale-new "zh.UTF-8")))
(test-equal "zh_HK"
            (locale-zh-awared-lang (locale-new "zh_HK.UTF-8")))
(test-equal "zh_HK"
            (locale-zh-awared-lang (locale-new "zh_HK")))
(test-equal "zh"
            (locale-zh-awared-lang (locale-new "zh")))
(test-equal "zh"
            (locale-zh-awared-lang (locale-new "zh.UTF-8")))
(test-end)

(test-begin "langgroup-covers?")
;; exact match
(test-equal '("ja")
            (langgroup-covers? "ja" "ja"))
(test-equal '("en")
            (langgroup-covers? "en" "en"))
(test-equal '("de")
            (langgroup-covers? "de" "de"))
(test-equal '("fr")
            (langgroup-covers? "fr" "fr"))
(test-equal '("zh")
            (langgroup-covers? "zh" "zh"))
(test-equal '("zh_CN")
            (langgroup-covers? "zh_CN" "zh_CN"))
(test-equal '("zh_TW")
            (langgroup-covers? "zh_TW" "zh_TW"))
(test-equal '("zh_HK")
            (langgroup-covers? "zh_HK" "zh_HK"))
(test-false (langgroup-covers? "de" "ja"))
(test-false (langgroup-covers? "de" "en"))
(test-false (langgroup-covers? "de" "fr"))
(test-false (langgroup-covers? "de" "zh"))
(test-false (langgroup-covers? "de" "zh_CN"))
(test-false (langgroup-covers? "de" "zh_TW"))
(test-false (langgroup-covers? "de" "zh_HK"))
;; group match
(test-false (langgroup-covers? "de:en:fr" "ja"))
(test-equal '("en" "fr")
            (langgroup-covers? "de:en:fr" "en"))
(test-equal '("de" "en" "fr")
            (langgroup-covers? "de:en:fr" "de"))
(test-equal '("fr")
            (langgroup-covers? "de:en:fr" "fr"))
(test-false (langgroup-covers? "de:en:fr" "zh"))
(test-false (langgroup-covers? "de:en:fr" "zh_CN"))
(test-false (langgroup-covers? "de:en:fr" "zh_TW"))
(test-false (langgroup-covers? "de:en:fr" "zh_HK"))
;; group expression is only allowed for first arg
(test-false (langgroup-covers? "de:en:fr" "de:en"))
(test-false (langgroup-covers? "de:en:fr" "de:en:fr"))
;; wildcard
(test-eq #t (langgroup-covers? "*" "ja"))
(test-eq #t (langgroup-covers? "*" "en"))
(test-eq #t (langgroup-covers? "*" "de"))
(test-eq #t (langgroup-covers? "*" "fr"))
(test-eq #t (langgroup-covers? "*" "zh"))
(test-eq #t (langgroup-covers? "*" "zh_CN"))
(test-eq #t (langgroup-covers? "*" "zh_TW"))
(test-eq #t (langgroup-covers? "*" "zh_HK"))
;; wildcard is only allowed for first arg
(test-false (langgroup-covers? "en" "*"))
;; 'nothing'
(test-false (langgroup-covers? "" "ja"))
(test-false (langgroup-covers? "" "en"))
(test-false (langgroup-covers? "" "de"))
(test-false (langgroup-covers? "" "fr"))
(test-false (langgroup-covers? "" "zh"))
(test-false (langgroup-covers? "" "zh_CN"))
(test-false (langgroup-covers? "" "zh_TW"))
(test-false (langgroup-covers? "" "zh_HK"))
(test-false (langgroup-covers? "" "*"))
(test-false (langgroup-covers? "" ""))
;; no special handling for Chinese
(test-false (langgroup-covers? "zh" "zh_CN"))
(test-false (langgroup-covers? "zh" "zh_TW"))
(test-false (langgroup-covers? "zh" "zh_HK"))
(test-false (langgroup-covers? "zh_CN" "zh"))
(test-false (langgroup-covers? "zh_TW" "zh"))
(test-false (langgroup-covers? "zh_HK" "zh"))
(test-false (langgroup-covers? "zh_CN" "zh_TW"))
(test-false (langgroup-covers? "zh_CN" "zh_HK"))
(test-false (langgroup-covers? "zh_HK" "zh_CN"))
(test-false (langgroup-covers? "zh_HK" "zh_TW"))
(test-equal '("zh_TW" "zh_HK")
            (langgroup-covers? "zh_TW:zh_HK" "zh_TW"))
(test-equal '("zh_HK")
            (langgroup-covers? "zh_TW:zh_HK" "zh_HK"))
(test-false (langgroup-covers? "zh_TW:zh_HK" "zh_CN"))
(test-false (langgroup-covers? "zh_TW:zh_HK" "zh"))
(test-false (langgroup-covers? "zh_CN:zh_TW:zh_HK" "zh"))
(test-end)

(test-report-result)
