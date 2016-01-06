;;; Copyright (c) 2005-2013 uim Project https://github.com/uim/uim
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

(define-module test.test-foo
  (use test.unit.test-case)
  (use test.uim-test))
(select-module test.test-foo)

(define (setup)
  (uim-test-setup)
  ;; Cancels LIBUIM_VANILLA=1. See init.scm for further details.
  (uim-eval '(set! enable-lazy-loading? #t))
  (uim-eval '(load-enabled-modules))
  (uim-define-siod-compatible-require)
  (uim-eval '(require "lazy-load.scm")))

(define (teardown)
  (uim-test-teardown))

(define (test-stub-im-generate-init-handler)
  (uim-eval
   '(begin
      (set! im-list ())
      (undefine *pyload.scm-loaded*)))
  (assert-uim-false '(symbol-bound? '*pyload.scm-loaded*))

  (uim-eval
   '(define init-handler
      (stub-im-generate-init-handler 'py "pyload")))
  (assert-uim-true  '(procedure? init-handler))
  (assert-uim-false '(retrieve-im 'py))
  (uim-eval
   '(define test-context
      (init-handler 0 #f #f)))
  (assert-uim-equal 'py
                    '(im-name (retrieve-im 'py)))
  (assert-uim-equal "pyload"
                    '(im-module-name (retrieve-im 'py)))
  (assert-uim-equal 'py
                    '(im-name (context-im test-context)))
  (assert-uim-equal "pyload"
                    '(im-module-name (context-im test-context)))
  #f)

(define (test-register-stub-im)
  (uim-eval
   '(begin
      (set! im-list ())
      (undefine *pyload.scm-loaded*)))
  (assert-uim-false '(symbol-bound? '*pyload.scm-loaded*))

  (uim-eval
   '(begin
      (register-stub-im
       'py
       "zh_CN"
       "UTF-8"
       "New Pinyin (Simplified)"
       "Pinyin input method (Simplified Chinese version)"
       "pyload")
      (define init-handler (im-init-handler (retrieve-im 'py)))
      (im-set-init-handler! (retrieve-im 'py) 'init)))
  (assert-uim-equal '(py
                      "zh_CN"
                      "UTF-8"
                      "New Pinyin (Simplified)"
                      "Pinyin input method (Simplified Chinese version)"
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
                      "pyload"
                      #f) ;; delay-activating-handler
                    '(retrieve-im 'py))
  (uim-eval '(im-set-init-handler! (retrieve-im 'py) init-handler))

  (assert-uim-true  '(procedure? (im-init-handler
                                  (retrieve-im 'py))))
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
      (create-context 0 #f 'py)

      (define test-context (assv 0 context-list))))

  (assert-uim-equal 'py
                    '(im-name (context-im test-context)))
  (assert-uim-equal "pyload"
                    '(im-module-name (context-im test-context)))
  (uim-eval '(define test-py (retrieve-im 'py)))
  (assert-uim-equal 'py
                    '(im-name test-py))
  (assert-uim-equal "pyload"
                    '(im-module-name test-py))
  (assert-uim-true  '(procedure? (im-init-handler test-py)))
  (assert-uim-false '(procedure? (im-release-handler test-py)))
  (assert-uim-true  '(procedure? (im-mode-handler test-py)))
  (assert-uim-true  '(procedure? (im-key-press-handler test-py)))
  (assert-uim-true  '(procedure? (im-key-release-handler test-py)))
  (assert-uim-true  '(procedure? (im-reset-handler test-py)))
  (assert-uim-true  '(procedure? (im-get-candidate-handler test-py)))
  (assert-uim-true  '(procedure? (im-set-candidate-index-handler test-py)))
  (assert-uim-true  '(procedure? (im-prop-activate-handler test-py)))
  #f)

(define (test-stub-im-generate-stub-im-list)
  (uim-eval
   '(begin
      (set! im-list ())
      (undefine *latin.scm-loaded*)
      (undefine *pyload.scm-loaded*)
      (set! installed-im-module-list '("latin" "pyload"))))
  (assert-uim-false '(symbol-bound? '*latin.scm-loaded*))
  (assert-uim-false '(symbol-bound? '*pyload.scm-loaded*))

  (assert-uim-false '(retrieve-im 'latin))
  (assert-uim-false '(retrieve-im 'py))
  (assert-uim-false '(retrieve-im 'pyunihan))

  (assert-uim-equal ()
                    '(stub-im-generate-stub-im-list ()))
  (assert-uim-equal (list
                     (string-append
                      "    (py\n"
                      "     \"zh_CN\"\n"
                      "     \"UTF-8\"\n"
                      "     \"New Pinyin (Simplified)\"\n"
                      "     \"Pinyin input method (Simplified Chinese version)\"\n"
                      "     \"pyload\")\n"))
                    '(stub-im-generate-stub-im-list '(py)))
  (assert-uim-equal (list
                     (string-append
                      "    (pyunihan\n"
                      "     \"zh\"\n"
                      "     \"UTF-8\"\n"
                      "     \"Pinyin (Unicode)\"\n"
                      "     \"Pinyin input method (Unicode version)\"\n"
                      "     \"pyload\")\n"))
                    '(stub-im-generate-stub-im-list '(pyunihan)))
  (assert-uim-equal (list
                     (string-append
                      "    (latin\n"
                      "     \"\"\n"
                      "     \"UTF-8\"\n"
                      "     \"Latin characters\"\n"
                      "     \"Latin characters mainly used for Latin and Germanic languages\"\n"
                      "     \"latin\")\n"))
                    '(stub-im-generate-stub-im-list '(latin)))

  (assert-uim-equal (list
                     (string-append
                      "    (py\n"
                      "     \"zh_CN\"\n"
                      "     \"UTF-8\"\n"
                      "     \"New Pinyin (Simplified)\"\n"
                      "     \"Pinyin input method (Simplified Chinese version)\"\n"
                      "     \"pyload\")\n")
                     (string-append
                      "    (latin\n"
                      "     \"\"\n"
                      "     \"UTF-8\"\n"
                      "     \"Latin characters\"\n"
                      "     \"Latin characters mainly used for Latin and Germanic languages\"\n"
                      "     \"latin\")\n")
                     (string-append
                      "    (pyunihan\n"
                      "     \"zh\"\n"
                      "     \"UTF-8\"\n"
                      "     \"Pinyin (Unicode)\"\n"
                      "     \"Pinyin input method (Unicode version)\"\n"
                      "     \"pyload\")\n"))
                    '(stub-im-generate-stub-im-list '(py latin pyunihan)))
  #f)

(define (test-stub-im-generate-all-stub-im-list)
  (uim-eval
   '(begin
      (set! im-list ())
      (undefine *latin.scm-loaded*)
      (undefine *pyload.scm-loaded*)
      (set! installed-im-module-list '("latin" "pyload"))))
  (assert-uim-equal (list
                     (string-append
                      "    (latin\n"
                      "     \"\"\n"
                      "     \"UTF-8\"\n"
                      "     \"Latin characters\"\n"
                      "     \"Latin characters mainly used for Latin and Germanic languages\"\n"
                      "     \"latin\")\n")
                     (string-append
                      "    (py\n"
                      "     \"zh_CN\"\n"
                      "     \"UTF-8\"\n"
                      "     \"New Pinyin (Simplified)\"\n"
                      "     \"Pinyin input method (Simplified Chinese version)\"\n"
                      "     \"pyload\")\n")
                     (string-append
                      "    (pyunihan\n"
                      "     \"zh\"\n"
                      "     \"UTF-8\"\n"
                      "     \"Pinyin (Unicode)\"\n"
                      "     \"Pinyin input method (Unicode version)\"\n"
                      "     \"pyload\")\n")
                     (string-append
                      "    (pinyin-big5\n"
                      "     \"zh_TW:zh_HK\"\n"
                      "     \"UTF-8\"\n"
                      "     \"Pinyin (Traditional)\"\n"
                      "     \"Pinyin input method (Traditional Chinese version)\"\n"
                      "     \"pyload\")\n"))
                    '(stub-im-generate-all-stub-im-list))

  (uim-eval
   '(begin
      (set! im-list ())
      (undefine *latin.scm-loaded*)
      (undefine *pyload.scm-loaded*)
      (set! installed-im-module-list '())))
  (assert-uim-equal ()
                    '(stub-im-generate-all-stub-im-list))
  #f)

(provide "test/test-lazy-load")
