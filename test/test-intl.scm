#!/usr/bin/env gosh

;;; Copyright (c) 2003-2008 uim Project http://code.google.com/p/uim/
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

;; These tests are passed at revision 4403 (new repository)

(use test.unit)
(use file.util)

(require "test/uim-test-utils")

(let* ((current-lang #f)
       (lang "en_US")
       (locale-dir (build-path "test" "locale"))
       (LC_MESSAGES-dir (build-path locale-dir "en_US" "LC_MESSAGES"))
       (domain "uim")
       (msgid "hello")
       (msgstr "Hello"))
  (define-uim-test-case "test intl"
    (setup
     (lambda ()
       (set! current-lang (or (sys-getenv "LANG")
			      (sys-getenv "LC_ALL")
			      "C"))
       (sys-putenv "LANG" lang)
       (sys-putenv "LC_ALL" lang)
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
       (*uim-sh-setup-proc*)))
    (teardown
     (lambda ()
       (sys-putenv "LANG" current-lang)
       (sys-putenv "LC_ALL" current-lang)
       (remove-directory* locale-dir)))
    ("test gettext"
     (assert-equal msgid (uim `(gettext ,msgid)))
     (uim `(bindtextdomain ,domain ,locale-dir))
     (assert-equal msgstr (uim `(dgettext ,domain ,msgid)))
     (uim `(textdomain ,domain))
     (assert-equal msgstr (uim `(gettext ,msgid))))))
