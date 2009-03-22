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

(define-module test.test-intl
  (use test.unit.test-case)
  (use test.uim-test)
  (use file.util)
  (use gauche.process))
(select-module test.test-intl)

(define current-lang (or (sys-getenv "LANG")
                         (sys-getenv "LC_ALL")))
;; At least on glibc 2.6.1-1ubuntu9 on Ubuntu 7.10, gettext(3)
;; does not read the translation for "en_US" and "C". So I
;; specify "ja_JP" as an arbitrary locale for these tests.
;;   -- YamaKen 2008-03-23
;;(define lang "en_US")  ;; doesn't work
;;(define lang "C")      ;; doesn't work
;;(define lang "ja_JP")
;;
;; "ja_JP" doesn't work on "ja_JP" isn't generated environment.
;; For example, "ja_JP" isn't generated but "ja_JP.UTF-8" is
;; generated on my environment. So, I guess available locales
;; from "locale -a" and use the current locale as fallback locale.
;;   -- kou 2009-03-22 -- Wow! just (- a-year 1) ago!
(define lang
  (let ((locale-process (run-process '("locale" "-a")
                                     :output :pipe
                                     :error :pipe
                                     :wait #t)))
    (or (and (= 0 (process-exit-status locale-process))
             (find #/\./ (port->string-list (process-output locale-process))))
        current-lang)))

(define locale-dir (uim-test-build-path "test" "locale"))
(define LC_MESSAGES-dir (build-path locale-dir lang "LC_MESSAGES"))
(define domain "uim")
(define msgid "hello")
(define msgstr "Hello")


(define (setup)
  (make-directory* LC_MESSAGES-dir)
  (with-output-to-file (build-path LC_MESSAGES-dir #`",|domain|.po")
    (lambda ()
      (display
       (string-join
        `("msgid \"\""
          "msgstr \"\""
          "\"MIME-Version: 1.0\\n\""
          "\"Content-Type: text/plain; charset=UTF-8\\n\""
          "\"Content-Transfer-Encoding: 8bit\\n\""
          ""
          ,#`"msgid \",|msgid|\""
          ,#`"msgstr \",|msgstr|\"")
        "\n"))))
  (run-process "msgfmt" "-o"
               (build-path LC_MESSAGES-dir #`",|domain|.mo")
               (build-path LC_MESSAGES-dir #`",|domain|.po")
               :wait #t)

  (when lang
    (sys-putenv "LANG" lang)
    (sys-putenv "LC_ALL" lang))
  (uim-test-setup)
  (when lang
    (sys-putenv "LANG" current-lang)
    (sys-putenv "LC_ALL" current-lang)))

(define (teardown)
  (uim-test-teardown)

  (remove-directory* locale-dir))

(define (test-gettext)
  (assert-equal msgid (uim `(gettext ,msgid)))
  (assert-equal locale-dir (uim `(bindtextdomain ,domain ,locale-dir)))
  (assert-equal locale-dir (uim `(bindtextdomain ,domain #f)))
  (assert-equal msgstr (uim `(dgettext ,domain ,msgid)))
  (assert-equal domain (uim `(textdomain ,domain)))
  (assert-equal domain (uim `(textdomain #f)))
  (assert-equal msgstr (uim `(gettext ,msgid)))
  #f)

(provide "test/test-intl")
