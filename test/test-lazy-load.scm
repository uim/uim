;;; Copyright (c) 2005-2026 uim Project https://github.com/uim/uim
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

(require "lazy-load.scm")

;; Only the IMs in enabled-im-list are registered. It's defined in
;; loader.scm but loader.scm isn't loaded with LIBUIM_VANILLA=1.
(set! enabled-im-list '(direct latin py pyunihan pinyin-big5))

(define setup
  (lambda ()
    (set! im-list ())))

;; require-module doesn't load a module that is already loaded. So the
;; tests for stub-im-generate-stub-im-list and
;; stub-im-generate-all-stub-im-list must be run before the other tests
;; load the modules.
(test-begin "stub-im-generate-stub-im-list")
(setup)
(set! installed-im-module-list '("latin" "pyload"))

(test-false (retrieve-im 'latin))
(test-false (retrieve-im 'py))
(test-false (retrieve-im 'pyunihan))

(test-equal '()
	    (stub-im-generate-stub-im-list '()))
(test-equal (list
	     (string-append
	      "    (py\n"
	      "     \"zh_CN\"\n"
	      "     \"UTF-8\"\n"
	      "     \"New Pinyin (Simplified)\"\n"
	      "     \"Pinyin input method (Simplified Chinese version)\"\n"
	      "     \"pyload\")\n"))
	    (stub-im-generate-stub-im-list '(py)))
(test-equal (list
	     (string-append
	      "    (pyunihan\n"
	      "     \"zh\"\n"
	      "     \"UTF-8\"\n"
	      "     \"Pinyin (Unicode)\"\n"
	      "     \"Pinyin input method (Unicode version)\"\n"
	      "     \"pyload\")\n"))
	    (stub-im-generate-stub-im-list '(pyunihan)))
(test-equal (list
	     (string-append
	      "    (latin\n"
	      "     \"\"\n"
	      "     \"UTF-8\"\n"
	      "     \"Latin characters\"\n"
	      "     \"Latin characters mainly used for Latin and Germanic languages\"\n"
	      "     \"latin\")\n"))
	    (stub-im-generate-stub-im-list '(latin)))

(test-equal (list
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
	    (stub-im-generate-stub-im-list '(py latin pyunihan)))
(test-end)

(test-begin "stub-im-generate-all-stub-im-list")
;; Use the IMs registered by the previous test because the modules are
;; already loaded.
(set! installed-im-module-list '("latin" "pyload"))
(test-equal (list
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
	    (stub-im-generate-all-stub-im-list))

(setup)
(set! installed-im-module-list '())
(test-equal '()
	    (stub-im-generate-all-stub-im-list))
(test-end)

(test-begin "stub-im-generate-init-handler")
(setup)
(define init-handler
  (stub-im-generate-init-handler 'py "pyload"))
(test-true  (procedure? init-handler))
(test-false (retrieve-im 'py))
(define test-context
  (init-handler 0 #f #f))
(test-eq    'py
	    (im-name (retrieve-im 'py)))
(test-equal "pyload"
	    (im-module-name (retrieve-im 'py)))
(test-eq    'py
	    (im-name (context-im test-context)))
(test-equal "pyload"
	    (im-module-name (context-im test-context)))
(test-end)

(test-begin "register-stub-im")
(setup)
(register-stub-im
 'py
 "zh_CN"
 "UTF-8"
 "New Pinyin (Simplified)"
 "Pinyin input method (Simplified Chinese version)"
 "pyload")
(define init-handler (im-init-handler (retrieve-im 'py)))
(im-set-init-handler! (retrieve-im 'py) 'init)
(test-equal '(py
	      "zh_CN"
	      "UTF-8"
	      "New Pinyin (Simplified)"
	      "Pinyin input method (Simplified Chinese version)"
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
	    (retrieve-im 'py))
(im-set-init-handler! (retrieve-im 'py) init-handler)

(test-true  (procedure? (im-init-handler
			 (retrieve-im 'py))))
;; to prevent SEGV on create-context
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
(define test-context (assv 0 context-list))

(test-eq    'py
	    (im-name (context-im test-context)))
(test-equal "pyload"
	    (im-module-name (context-im test-context)))
(define test-py (retrieve-im 'py))
(test-eq    'py
	    (im-name test-py))
(test-equal "pyload"
	    (im-module-name test-py))
(test-true  (procedure? (im-init-handler test-py)))
(test-false (procedure? (im-release-handler test-py)))
(test-true  (procedure? (im-mode-handler test-py)))
(test-true  (procedure? (im-key-press-handler test-py)))
(test-true  (procedure? (im-key-release-handler test-py)))
(test-true  (procedure? (im-reset-handler test-py)))
(test-true  (procedure? (im-get-candidate-handler test-py)))
(test-true  (procedure? (im-set-candidate-index-handler test-py)))
(test-true  (procedure? (im-prop-activate-handler test-py)))
(test-end)

(test-report-result)
