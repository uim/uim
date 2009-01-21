#!/usr/bin/env gosh

;;; Copyright (c) 2005-2009 uim Project http://code.google.com/p/uim/
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

(define-uim-test-case "testcase stub-im"
  (setup
   (lambda ()
     ;; Cancels LIBUIM_VANILLA=1. See init.scm for further details.
     (uim '(load-enabled-modules))

     (uim-define-siod-compatible-require)
     (uim '(require "lazy-load.scm"))))

  ("test stub-im-generate-init-handler"
   (uim '(set! im-list ()))
   (uim '(undefine *hangul.scm-loaded*))
   (assert-false (uim-bool '(symbol-bound? '*hangul.scm-loaded*)))

   (uim '(define init-handler (stub-im-generate-init-handler 'hangul2
							     "hangul")))
   (assert-true  (uim-bool '(procedure? init-handler)))
   (assert-false (uim-bool '(retrieve-im 'hangul2)))
   (uim '(define test-context (init-handler 0 #f #f)))
   (assert-equal 'hangul2
		 (uim '(im-name (retrieve-im 'hangul2))))
   (assert-equal "hangul"
		 (uim '(im-module-name (retrieve-im 'hangul2))))
   (assert-equal 'hangul2
		 (uim '(im-name (context-im test-context))))
   (assert-equal "hangul"
		 (uim '(im-module-name (context-im test-context)))))

  ("test register-stub-im"
   (uim '(set! im-list ()))
   (uim '(undefine *hangul.scm-loaded*))
   (assert-false (uim-bool '(symbol-bound? '*hangul.scm-loaded*)))

   (uim '(register-stub-im
	  'hangul2
	  "ko"
	  "UTF-8"
	  "Hangul (2-bul)"
	  "2-bul style hangul input method"
	  "hangul"))
   (uim '(define init-handler (im-init-handler (retrieve-im 'hangul2))))
   (uim '(im-set-init-handler! (retrieve-im 'hangul2) 'init))
   (assert-equal '(hangul2
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
		 (uim '(retrieve-im 'hangul2)))
   (uim '(im-set-init-handler! (retrieve-im 'hangul2) init-handler))

   (assert-true  (uim-bool '(procedure? (im-init-handler
					 (retrieve-im 'hangul2)))))
   ;; to prevent SEGV on create-context
   (uim '(define im-set-encoding (lambda arg #f)))
   (uim '(define im-clear-preedit (lambda arg #f)))
   (uim '(define im-pushback-preedit (lambda arg #f)))
   (uim '(define im-update-preedit (lambda arg #f)))
   (uim '(define im-update-prop-list (lambda arg #f)))
   (uim '(define im-clear-mode-list (lambda arg #f)))
   (uim '(define im-pushback-mode-list (lambda arg #f)))
   (uim '(define im-update-mode-list (lambda arg #f)))
   (uim '(define im-update-mode (lambda arg #f)))

   (uim '(begin
	   (create-context 0 #f 'hangul2)
	   #f))
   (uim '(begin
	   (define test-context (assv 0 context-list))
	   #f))

   (assert-equal 'hangul2
		 (uim '(im-name (context-im test-context))))
   (assert-equal "hangul"
		 (uim '(im-module-name (context-im test-context))))
   (uim '(define test-hangul2 (retrieve-im 'hangul2)))
   (assert-equal 'hangul2
		 (uim '(im-name test-hangul2)))
   (assert-equal "hangul"
		 (uim '(im-module-name test-hangul2)))
   (assert-true  (uim-bool '(procedure? (im-init-handler test-hangul2))))
   (assert-false (uim-bool '(procedure? (im-release-handler test-hangul2))))
   (assert-true  (uim-bool '(procedure? (im-mode-handler test-hangul2))))
   (assert-true  (uim-bool '(procedure? (im-key-press-handler test-hangul2))))
   (assert-true  (uim-bool '(procedure? (im-key-release-handler test-hangul2))))
   (assert-true  (uim-bool '(procedure? (im-reset-handler test-hangul2))))
   (assert-true  (uim-bool '(procedure? (im-get-candidate-handler test-hangul2))))
   (assert-true  (uim-bool '(procedure? (im-set-candidate-index-handler test-hangul2))))
   (assert-true  (uim-bool '(procedure? (im-prop-activate-handler test-hangul2)))))

  ("test stub-im-generate-stub-im-list"
   (uim '(set! im-list ()))
   (uim '(undefine *tcode.scm-loaded*))
   (uim '(undefine *hangul.scm-loaded*))
   (assert-false (uim-bool '(symbol-bound? '*tcode.scm-loaded*)))
   (assert-false (uim-bool '(symbol-bound? '*hangul.scm-loaded*)))

   (assert-false (uim-bool '(retrieve-im 'tcode)))
   (assert-false (uim-bool '(retrieve-im 'hangul2)))
   (assert-false (uim-bool '(retrieve-im 'hangul3)))

   (assert-equal ()
		 (uim '(stub-im-generate-stub-im-list ())))
   (assert-equal (list
		  (string-append
		   "    (hangul2\n"
		   "     \"ko\"\n"
		   "     \"UTF-8\"\n"
		   "     \"Hangul (2-beol)\"\n"
		   "     \"2-beol style hangul input method\"\n"
		   "     \"hangul\")\n"))
		 (uim '(stub-im-generate-stub-im-list '(hangul2))))
   (assert-equal (list
		  (string-append
		   "    (hangul3\n"
		   "     \"ko\"\n"
		   "     \"UTF-8\"\n"
		   "     \"Hangul (3-beol)\"\n"
		   "     \"3-beol style hangul input method\"\n"
		   "     \"hangul\")\n"))
		 (uim '(stub-im-generate-stub-im-list '(hangul3))))
   (assert-equal (list
		  (string-append
		   "    (tcode\n"
		   "     \"ja\"\n"
		   "     \"EUC-JP\"\n"
		   "     \"T-Code\"\n"
		   "     \"A kanji direct input method\"\n"
		   "     \"tcode\")\n"))
		 (uim '(stub-im-generate-stub-im-list '(tcode))))

   (assert-equal (list
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
		 (uim '(stub-im-generate-stub-im-list '(hangul2 tcode hangul3)))))

  ("test stub-im-generate-all-stub-im-list"
   (uim '(set! im-list ()))
   (uim '(undefine *tcode.scm-loaded*))
   (uim '(undefine *hangul.scm-loaded*))
   (uim '(set! installed-im-module-list '("tcode" "hangul")))
   (assert-equal (list
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
		 (uim '(stub-im-generate-all-stub-im-list)))

   (uim '(set! im-list ()))
   (uim '(undefine *tcode.scm-loaded*))
   (uim '(undefine *hangul.scm-loaded*))
   (uim '(set! installed-im-module-list '()))
   (assert-equal ()
		 (uim '(stub-im-generate-all-stub-im-list)))))
