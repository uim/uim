;;; Copyright (c) 2005-2010 uim Project http://code.google.com/p/uim/
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

(define-module test.test-foo
  (use test.unit.test-case)
  (use test.uim-test))
(select-module test.test-foo)

(define (setup)
  (uim-test-setup)
  ;; Cancels LIBUIM_VANILLA=1. See init.scm for further details.
  (uim-eval '(load-enabled-modules))
  (uim-define-siod-compatible-require)
  (uim-eval '(require "lazy-load.scm")))

(define (teardown)
  (uim-test-teardown))

(define (test-stub-im-generate-init-handler)
  (uim-eval
   '(begin
      (set! im-list ())
      (undefine *hangul.scm-loaded*)))
  (assert-uim-false '(symbol-bound? '*hangul.scm-loaded*))

  (uim-eval
   '(define init-handler
      (stub-im-generate-init-handler 'hangul2 "hangul")))
  (assert-uim-true  '(procedure? init-handler))
  (assert-uim-false '(retrieve-im 'hangul2))
  (uim-eval
   '(define test-context
      (init-handler 0 #f #f)))
  (assert-uim-equal 'hangul2
                    '(im-name (retrieve-im 'hangul2)))
  (assert-uim-equal "hangul"
                    '(im-module-name (retrieve-im 'hangul2)))
  (assert-uim-equal 'hangul2
                    '(im-name (context-im test-context)))
  (assert-uim-equal "hangul"
                    '(im-module-name (context-im test-context)))
  #f)

(define (test-register-stub-im)
  (uim-eval
   '(begin
      (set! im-list ())
      (undefine *hangul.scm-loaded*)))
  (assert-uim-false '(symbol-bound? '*hangul.scm-loaded*))

  (uim-eval
   '(begin
      (register-stub-im
       'hangul2
       "ko"
       "UTF-8"
       "Hangul (2-bul)"
       "2-bul style hangul input method"
       "hangul")
      (define init-handler (im-init-handler (retrieve-im 'hangul2)))
      (im-set-init-handler! (retrieve-im 'hangul2) 'init)))
  (assert-uim-equal '(hangul2
                      "ko"
                      "UTF-8"
                      "Hangul (2-bul)"
                      "2-bul style hangul input method"
                      ;; replace () with #f for R5RS compliant interpreter
                      #f ;; arg
                      init
                      #f ;; release-handler
                      #f ;; mode-handler
                      #f ;; press-key-handler
                      #f ;; release-key-handler
                      #f ;; reset-handler
                      #f ;; get-candidate-handler
                      #f ;; set-candidate-index-handler
                      #f ;; prop-activate-handler
                      #f ;; input-string-handler
                      #f ;; focus-in-handler
                      #f ;; focus-out-handler
                      #f ;; place-handler
                      #f ;; displace-handler
                      "hangul")
                    '(retrieve-im 'hangul2))
  (uim-eval '(im-set-init-handler! (retrieve-im 'hangul2) init-handler))

  (assert-uim-true  '(procedure? (im-init-handler
                                  (retrieve-im 'hangul2))))
  ;; to prevent SEGV on create-context
  (uim-eval
   '(begin
      (define im-set-encoding (lambda arg #f))
      (define im-clear-preedit (lambda arg #f))
      (define im-pushback-preedit (lambda arg #f))
      (define im-update-preedit (lambda arg #f))
      (define im-update-prop-list (lambda arg #f))
      (define im-clear-mode-list (lambda arg #f))
      (define im-pushback-mode-list (lambda arg #f))
      (define im-update-mode-list (lambda arg #f))
      (define im-update-mode (lambda arg #f))
      (create-context 0 #f 'hangul2)

      (define test-context (assv 0 context-list))))

  (assert-uim-equal 'hangul2
                    '(im-name (context-im test-context)))
  (assert-uim-equal "hangul"
                    '(im-module-name (context-im test-context)))
  (uim-eval '(define test-hangul2 (retrieve-im 'hangul2)))
  (assert-uim-equal 'hangul2
                    '(im-name test-hangul2))
  (assert-uim-equal "hangul"
                    '(im-module-name test-hangul2))
  (assert-uim-true  '(procedure? (im-init-handler test-hangul2)))
  (assert-uim-false '(procedure? (im-release-handler test-hangul2)))
  (assert-uim-true  '(procedure? (im-mode-handler test-hangul2)))
  (assert-uim-true  '(procedure? (im-key-press-handler test-hangul2)))
  (assert-uim-true  '(procedure? (im-key-release-handler test-hangul2)))
  (assert-uim-true  '(procedure? (im-reset-handler test-hangul2)))
  (assert-uim-true  '(procedure? (im-get-candidate-handler test-hangul2)))
  (assert-uim-true  '(procedure? (im-set-candidate-index-handler test-hangul2)))
  (assert-uim-true  '(procedure? (im-prop-activate-handler test-hangul2)))
  #f)

(define (test-stub-im-generate-stub-im-list)
  (uim-eval
   '(begin
      (set! im-list ())
      (undefine *tcode.scm-loaded*)
      (undefine *hangul.scm-loaded*)))
  (assert-uim-false '(symbol-bound? '*tcode.scm-loaded*))
  (assert-uim-false '(symbol-bound? '*hangul.scm-loaded*))

  (assert-uim-false '(retrieve-im 'tcode))
  (assert-uim-false '(retrieve-im 'hangul2))
  (assert-uim-false '(retrieve-im 'hangul3))

  (assert-uim-equal ()
                    '(stub-im-generate-stub-im-list ()))
  (assert-uim-equal (list
                     (string-append
                      "    (hangul2\n"
                      "     \"ko\"\n"
                      "     \"UTF-8\"\n"
                      "     \"Hangul (2-beol)\"\n"
                      "     \"2-beol style hangul input method\"\n"
                      "     \"hangul\")\n"))
                    '(stub-im-generate-stub-im-list '(hangul2)))
  (assert-uim-equal (list
                     (string-append
                      "    (hangul3\n"
                      "     \"ko\"\n"
                      "     \"UTF-8\"\n"
                      "     \"Hangul (3-beol)\"\n"
                      "     \"3-beol style hangul input method\"\n"
                      "     \"hangul\")\n"))
                    '(stub-im-generate-stub-im-list '(hangul3)))
  (assert-uim-equal (list
                     (string-append
                      "    (tcode\n"
                      "     \"ja\"\n"
                      "     \"EUC-JP\"\n"
                      "     \"T-Code\"\n"
                      "     \"A kanji direct input method\"\n"
                      "     \"tcode\")\n"))
                    '(stub-im-generate-stub-im-list '(tcode)))

  (assert-uim-equal (list
                     (string-append
                      "    (hangul2\n"
                      "     \"ko\"\n"
                      "     \"UTF-8\"\n"
                      "     \"Hangul (2-beol)\"\n"
                      "     \"2-beol style hangul input method\"\n"
                      "     \"hangul\")\n")
                     (string-append
                      "    (tcode\n"
                      "     \"ja\"\n"
                      "     \"EUC-JP\"\n"
                      "     \"T-Code\"\n"
                      "     \"A kanji direct input method\"\n"
                      "     \"tcode\")\n")
                     (string-append
                      "    (hangul3\n"
                      "     \"ko\"\n"
                      "     \"UTF-8\"\n"
                      "     \"Hangul (3-beol)\"\n"
                      "     \"3-beol style hangul input method\"\n"
                      "     \"hangul\")\n"))
                    '(stub-im-generate-stub-im-list '(hangul2 tcode hangul3)))
  #f)

(define (test-stub-im-generate-all-stub-im-list)
  (uim-eval
   '(begin
      (set! im-list ())
      (undefine *tcode.scm-loaded*)
      (undefine *hangul.scm-loaded*)
      (set! installed-im-module-list '("tcode" "hangul"))))
  (assert-uim-equal (list
                     (string-append
                      "    (tcode\n"
                      "     \"ja\"\n"
                      "     \"EUC-JP\"\n"
                      "     \"T-Code\"\n"
                      "     \"A kanji direct input method\"\n"
                      "     \"tcode\")\n")
                     (string-append
                      "    (hangul2\n"
                      "     \"ko\"\n"
                      "     \"UTF-8\"\n"
                      "     \"Hangul (2-beol)\"\n"
                      "     \"2-beol style hangul input method\"\n"
                      "     \"hangul\")\n")
                     (string-append
                      "    (hangul3\n"
                      "     \"ko\"\n"
                      "     \"UTF-8\"\n"
                      "     \"Hangul (3-beol)\"\n"
                      "     \"3-beol style hangul input method\"\n"
                      "     \"hangul\")\n")
                     (string-append
                      "    (romaja\n"
                      "     \"ko\"\n"
                      "     \"UTF-8\"\n"
                      "     \"Hangul (Romaja)\"\n"
                      "     \"Romaja input style hangul input method\"\n"
                      "     \"hangul\")\n"))
                    '(stub-im-generate-all-stub-im-list))

  (uim-eval
   '(begin
      (set! im-list ())
      (undefine *tcode.scm-loaded*)
      (undefine *hangul.scm-loaded*)
      (set! installed-im-module-list '())))
  (assert-uim-equal ()
                    '(stub-im-generate-all-stub-im-list))
  #f)

(provide "test/test-lazy-load")
