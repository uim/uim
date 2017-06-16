#!/usr/bin/env gosh

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

(use test.unit)

(require "test/uim-test-utils")

(define-uim-test-case "test im misc definitions"
  ("test preedit-attr?"
   (assert-true  (uim-bool '(preedit-attr? preedit-none)))
   (assert-true  (uim-bool '(preedit-attr? preedit-underline)))
   (assert-true  (uim-bool '(preedit-attr? preedit-reverse)))
   (assert-true  (uim-bool '(preedit-attr? preedit-cursor)))
   (assert-true  (uim-bool '(preedit-attr? preedit-separator)))
   (assert-false (uim-bool '(preedit-attr? #f)))
   (assert-false (uim-bool '(preedit-attr? 2398)))))  ;; arbitrary integer

;; N.B. This test requires m17nlib enabled
(define-uim-test-case "testcase im im-management"
  (setup
   (lambda ()
     (uim '(begin
	     (require-extension (srfi 1))
	     (require-module "anthy")
	     (require-module "canna")
	     (require-module "skk")
	     (require-module "latin")
	     ;; Disable IMs that affect the default IM selection.
	     (define test-im-disabled-im-list '(look
						m17n-unicode
						m17n-rfc1345
						m17n-en-ispell
						m17n-fr-azerty
						m17n-latn-pre
						m17n-latn-post
						m17n-bopo-kbd
						m17n-zh-quick
						m17n-zh-cangjie
						m17n-zh-tonepy
						m17n-zh-tonepy-b5
						m17n-zh-tonepy-gb
						m17n-zh-py
						m17n-zh-py-b5
						m17n-zh-py-gb
						m17n-syrc-phonetic
						m17n-vi-nomtelex
                                                zm
                                                wb86
						))
	     (set! enabled-im-list
		   (append (lset-difference eq? enabled-im-list
					        test-im-disabled-im-list)
			   '(test-im test-im2)))
	     (for-each require-module installed-im-module-list)
	     (define prev-im #f)
	     (define prev-nr-ims (length im-list))
	     (define test-im-init-args #f)
	     (define test-im-alt-init-args #f)
	     ;; temporary workaround to cheat on revised register-im
	     ;; TODO: rewrite tests in accordance with enabled-im-list
	     (define custom-full-featured? #t)
	     (set! test-im-init-args (list 'test-im
					   "ja"
					   "UTF-8"
					   "a label"
					   "a short description"
					   #f
					   direct-init-handler
					   direct-release-handler
					   context-mode-handler
					   direct-key-press-handler
					   direct-key-release-handler
					   direct-reset-handler
					   direct-get-candidate-handler
					   direct-set-candidate-index-handler
					   context-prop-activate-handler
					   #f
					   #f
					   #f
					   #f
					   #f))
	     (set! test-im-alt-init-args (list 'test-im
					       "en"
					       "en_US.UTF-8"
					       "an alternative label"
					       "an alternative short desc"
					       'alt-arg
					       'alt-init-handler
					       'alt-release-handler
					       'alt-mode-handler
					       'alt-key-press-handler
					       'alt-key-release-handler
					       'alt-reset-handler
					       'alt-get-candidate-handler
					       'alt-set-candidate-index-handler
					       'alt-prop-activate-handler
					       #f
					       #f
					       #f
					       #f
					       #f))
	     #t))))

  ("test normalize-im-list"
   (uim '(set! im-list (remove (lambda (im)
				 (not (eq? (im-name im)
					   'direct)))
			       im-list)))
   (assert-equal '(direct)
		 (uim '(map im-name im-list)))
   ;; direct IM always remains at head
   (assert-true  (uim-bool '(apply register-im test-im-init-args)))
   (assert-equal '(direct test-im)
		 (uim '(map im-name im-list)))
   ;; other IMs are cons'ed at right of direct
   (assert-true  (uim-bool '(apply register-im (cons 'test-im2
						     (cdr test-im-init-args)))))
   (assert-equal '(direct test-im2 test-im)
		 (uim '(map im-name im-list)))
   ;; direct IM can be registered to null list
   (uim '(set! im-list ()))
   ;; second time register-im for 'direct returns #t if im-list does not
   ;; contain 'direct
   (assert-true (uim-bool '(apply register-im (cons 'direct
                                                    (cdr test-im-init-args)))))
   (assert-equal '(direct)
		 (uim '(map im-name im-list)))
   ;; ordinary IM can be registered to null list
   (uim '(set! im-list ()))
   ;; second time register-im for 'test-im returns #t if im-list does not
   ;; contain 'test-im
   (assert-true  (uim-bool '(apply register-im test-im-init-args)))
   (assert-equal '(test-im)
		 (uim '(map im-name im-list))))

  ("test register-im"
   (assert-true  (uim-bool '(apply register-im test-im-init-args)))
   (assert-equal (+ (uim 'prev-nr-ims) 1)
		 (uim '(length im-list)))
   (assert-equal 'test-im
		 (uim '(im-name (retrieve-im 'test-im))))
   (assert-false (uim-bool '(im-module-name (retrieve-im 'test-im))))
   (assert-equal 22
		 (uim '(length (retrieve-im 'test-im))))
   (uim '(im-set-module-name! (retrieve-im 'test-im) "foo"))
   (assert-equal "foo"
		 (uim '(im-module-name (retrieve-im 'test-im))))

   ;; duplicate register overwrites preexisting one
   (assert-false (uim-bool '(apply register-im test-im-alt-init-args)))
   (assert-equal (+ (uim 'prev-nr-ims) 1)
		 (uim '(length im-list)))
   (assert-equal '(test-im
		   "en"
		   "en_US.UTF-8"
		   "an alternative label"
		   "an alternative short desc"
		   alt-arg
		   alt-init-handler
		   alt-release-handler
		   alt-mode-handler
		   alt-key-press-handler
		   alt-key-release-handler
		   alt-reset-handler
		   alt-get-candidate-handler
		   alt-set-candidate-index-handler
		   alt-prop-activate-handler
		   ;; replace with #f for R5RS compliant interpreter
		   #f
		   #f
		   #f
		   #f
		   #f
		   #f
		   #f)
		 (uim '(retrieve-im 'test-im)))
   ;; subsequent registration that has different im-name will be
   ;; registered as another IM
   (assert-true  (uim-bool '(apply register-im
				   (cons 'test-im2
					 (cdr test-im-alt-init-args)))))
   (assert-equal (+ (uim 'prev-nr-ims) 2)
		 (uim '(length im-list)))
   (assert-equal '(test-im2
		   "en"
		   "en_US.UTF-8"
		   "an alternative label"
		   "an alternative short desc"
		   alt-arg
		   alt-init-handler
		   alt-release-handler
		   alt-mode-handler
		   alt-key-press-handler
		   alt-key-release-handler
		   alt-reset-handler
		   alt-get-candidate-handler
		   alt-set-candidate-index-handler
		   alt-prop-activate-handler
		   ;; replace with #f for R5RS compliant interpreter
		   #f
		   #f
		   #f
		   #f
		   #f
		   #f
		   #f)
		 (uim '(retrieve-im 'test-im2))))

  ("test register-im (module-name)"
   (assert-true  (uim-bool '(apply register-im test-im-init-args)))
   (assert-false (uim-bool '(im-module-name (retrieve-im 'test-im))))

   (uim '(set! currently-loading-module-name "foo"))
   (assert-true  (uim-bool '(apply register-im (cons 'test-im2
						     (cdr test-im-init-args)))))
   (assert-equal "foo"
		 (uim '(im-module-name (retrieve-im 'test-im2)))))

  ("test retrieve-im"
   (assert-false (uim-bool '(retrieve-im 'nonexistent)))
   (assert-equal 'direct
		 (uim '(im-name (retrieve-im 'direct))))
   (assert-equal 'anthy
		 (uim '(im-name (retrieve-im 'anthy))))
   (assert-equal 'ipa-x-sampa
		 (uim '(im-name (retrieve-im 'ipa-x-sampa))))
   (assert-false (uim-bool '(retrieve-im 'test-im)))
   (uim '(begin
	   (apply register-im test-im-init-args)
	   #t))
   (assert-equal 'test-im
		 (uim '(im-name (retrieve-im 'test-im)))))

  ("test default-im-for-debug"
   ;; this test requires --enable-debug
   (assert-true  (uim-bool '(feature? 'debug)))
   (uim '(unsetenv "UIM_IM_ENGINE"))
   (assert-false (uim-bool '(default-im-for-debug)))
   (uim '(setenv "UIM_IM_ENGINE" "nonexistent" #t))
   (assert-false (uim-bool '(default-im-for-debug)))
   (uim '(setenv "UIM_IM_ENGINE" "test-im" #t))
   (assert-false (uim-bool '(default-im-for-debug)))
   (uim '(begin
	   (apply register-im test-im-init-args)
	   #t))
   (assert-equal 'test-im
		 (uim '(im-name (default-im-for-debug))))
   (uim '(setenv "UIM_IM_ENGINE" "anthy" #t))
   (assert-equal 'anthy
		 (uim '(im-name (default-im-for-debug))))
   ;; default-im-name does not affect default-im-for-debug
   (uim '(set! default-im-name 'ipa-x-sampa))
   (assert-equal 'anthy
		 (uim '(im-name (default-im-for-debug)))))

  ("test find-im-for-locale"
   ;; IM existence test
   (assert-false (uim-bool '(memq 'nonexistent (map car im-list))))
   (assert-true  (uim-bool '(memq 'anthy (map car im-list))))
   (assert-true  (uim-bool '(memq 'skk (map car im-list))))
   (assert-true  (uim-bool '(memq 'latin(map car im-list))))
   (assert-true  (uim-bool '(memq 'tutcode (map car im-list))))
   (assert-true  (uim-bool '(memq 'py (map car im-list))))
   (assert-true  (uim-bool '(memq 'pyunihan (map car im-list))))
   (assert-true  (uim-bool '(memq 'pinyin-big5 (map car im-list))))
   (assert-true  (uim-bool '(memq 'viqr (map car im-list))))
   (assert-true  (uim-bool '(memq 'ipa-x-sampa (map car im-list))))
   (assert-true  (uim-bool '(memq 'direct (map car im-list))))
   ;; unsupported or direct-input languages
   (assert-equal 'direct
		 (uim '(im-name (find-im-for-locale "C"))))
   (assert-equal 'direct
		 (uim '(im-name (find-im-for-locale "POSIX"))))
   (assert-equal 'direct
		 (uim '(im-name (find-im-for-locale "en"))))
   (assert-equal 'direct
		 (uim '(im-name (find-im-for-locale "en_US"))))
   (assert-equal 'direct
		 (uim '(im-name (find-im-for-locale "en_US.US-ASCII"))))
   (assert-equal 'direct
		 (uim '(im-name (find-im-for-locale "en_US.UTF-8"))))
   (assert-equal 'direct
		 (uim '(im-name (find-im-for-locale "de"))))
   (assert-equal 'direct
		 (uim '(im-name (find-im-for-locale "fr"))))
   (assert-equal 'direct
		 (uim '(im-name (find-im-for-locale "de"))))
   (assert-equal 'direct
		 (uim '(im-name (find-im-for-locale "pt"))))
   ;; non-existent language code
   (assert-equal 'direct
		 (uim '(im-name (find-im-for-locale "xx"))))
   ;; default-im-name does not affect find-im-for-locale
   (uim '(set! default-im-name 'ipa-x-sampa))
   (assert-equal 'direct
		 (uim '(im-name (find-im-for-locale "pt"))))
   ;; default-im-for-debug does not affect find-im-for-locale
   (uim '(setenv "UIM_IM_ENGINE" "ipa-x-sampa" #t))
   (assert-equal 'direct
		 (uim '(im-name (find-im-for-locale "pt"))))
   (uim '(unsetenv "UIM_IM_ENGINE"))
   ;; Chinese
   (assert-equal 'm17n-zh-pinyin
		 (uim '(im-name (find-im-for-locale "zh"))))
   (assert-equal 'm17n-zh-pinyin
		 (uim '(im-name (find-im-for-locale "zh.GB18030"))))
   (assert-equal 'm17n-zh-pinyin
		 (uim '(im-name (find-im-for-locale "zh.Big5"))))
   (assert-equal 'm17n-zh-pinyin
		 (uim '(im-name (find-im-for-locale "zh.UTF-8"))))
   (assert-equal 'direct
		 (uim '(im-name (find-im-for-locale "zh_US"))))
   (assert-equal 'py
		 (uim '(im-name (find-im-for-locale "zh_CN"))))
   (assert-equal 'py
		 (uim '(im-name (find-im-for-locale "zh_CN.GB18030"))))
   (assert-equal 'py
		 (uim '(im-name (find-im-for-locale "zh_CN.UTF-8"))))
   (assert-equal 'pinyin-big5
		 (uim '(im-name (find-im-for-locale "zh_TW"))))
   (assert-equal 'pinyin-big5
		 (uim '(im-name (find-im-for-locale "zh_TW.Big5"))))
   (assert-equal 'pinyin-big5
		 (uim '(im-name (find-im-for-locale "zh_TW.UTF-8"))))
   (assert-equal 'pinyin-big5
		 (uim '(im-name (find-im-for-locale "zh_HK"))))
   (assert-equal 'pinyin-big5
		 (uim '(im-name (find-im-for-locale "zh_HK.Big5"))))
   (assert-equal 'pinyin-big5
		 (uim '(im-name (find-im-for-locale "zh_HK.UTF-8"))))
   ;; Japanese
   (assert-equal 'anthy
		 (uim '(im-name (find-im-for-locale "ja"))))
   (assert-equal 'anthy
		 (uim '(im-name (find-im-for-locale "ja_JP"))))
   (assert-equal 'anthy
		 (uim '(im-name (find-im-for-locale "ja_JP.EUC-JP"))))
   (assert-equal 'anthy
		 (uim '(im-name (find-im-for-locale "ja_JP.UTF-8"))))
   (assert-equal 'anthy
		 (uim '(im-name (find-im-for-locale "ja.UTF-8"))))
   ;; Korean
   (assert-equal 'byeoru
		 (uim '(im-name (find-im-for-locale "ko"))))
   (assert-equal 'byeoru
		 (uim '(im-name (find-im-for-locale "ko_KR"))))
   (assert-equal 'byeoru
		 (uim '(im-name (find-im-for-locale "ko_KR.EUC-KR"))))
   (assert-equal 'byeoru
		 (uim '(im-name (find-im-for-locale "ko_KR.UTF-8"))))
   (assert-equal 'byeoru
		 (uim '(im-name (find-im-for-locale "ko.UTF-8"))))
   ;; Vietnamese
   ;;(assert-equal 'm17n-vi-vni
   ;;		 (uim '(im-name (find-im-for-locale "vi"))))
   ;;(assert-equal 'm17n-vi-vni
   ;;		 (uim '(im-name (find-im-for-locale "vi_VN"))))
   ;;(assert-equal 'm17n-vi-vni
   ;;		 (uim '(im-name (find-im-for-locale "vi_VN.UTF-8"))))
   ;;(assert-equal 'm17n-vi-vni
   ;;		 (uim '(im-name (find-im-for-locale "vi.UTF-8"))))
   ;; native locale
   (uim '(unsetenv "LC_ALL"))
   (uim '(unsetenv "LANG"))
   (assert-equal 'direct
		 (uim '(im-name (find-im-for-locale ""))))
   (uim '(setenv "LC_ALL" "C" #t))
   (uim '(unsetenv "LANG"))
   (assert-equal 'direct
		 (uim '(im-name (find-im-for-locale ""))))
   (uim '(unsetenv "LC_ALL"))
   (uim '(setenv "LANG" "C" #t))
   (assert-equal 'direct
		 (uim '(im-name (find-im-for-locale ""))))
   (uim '(setenv "LC_ALL" "ja_JP.EUC-JP" #t))
   (uim '(unsetenv "LANG"))
   (assert-equal 'anthy
		 (uim '(im-name (find-im-for-locale ""))))
   ;; nonexistent native locale
   (uim '(setenv "LC_ALL" "xx" #t))
   (uim '(unsetenv "LANG"))
   (assert-equal 'direct
		 (uim '(im-name (find-im-for-locale "")))))

  ("test find-default-im"
   ;; find by find-im-for-locale
   (uim '(unsetenv "UIM_IM_ENGINE"))
   (uim '(set! default-im-name #f))
   (assert-equal 'direct
		 (uim '(im-name (find-default-im "C"))))
   (assert-equal 'direct
		 (uim '(im-name (find-default-im "en"))))
   (assert-equal 'direct
		 (uim '(im-name (find-default-im "pt"))))
   (assert-equal 'anthy
		 (uim '(im-name (find-default-im "ja_JP.EUC-JP"))))
   ;; empty locale string can be specified as native locale
   (uim '(unsetenv "UIM_IM_ENGINE"))
   (uim '(set! default-im-name #f))
   (uim '(unsetenv "LC_ALL"))
   (uim '(unsetenv "LANG"))
   (assert-equal 'direct
		 (uim '(im-name (find-default-im ""))))
   (uim '(setenv "LC_ALL" "C" #t))
   (assert-equal 'direct
		 (uim '(im-name (find-default-im ""))))
   (uim '(setenv "LC_ALL" "ja_JP.EUC-JP" #t))
   (assert-equal 'anthy
		 (uim '(im-name (find-default-im ""))))
   (uim '(unsetenv "LC_ALL"))
   ;; default-im-name precedes the locale specified by arg
   (uim '(unsetenv "UIM_IM_ENGINE"))
   (uim '(set! default-im-name 'ipa-x-sampa))
   (assert-equal 'ipa-x-sampa
		 (uim '(im-name (find-default-im "en"))))
   (assert-equal 'ipa-x-sampa
		 (uim '(im-name (find-default-im "pt"))))
   (assert-equal 'ipa-x-sampa
		 (uim '(im-name (find-default-im "ja_JP.EUC-JP"))))
   ;; default-im-for-debug precedes the locale specified by arg
   (uim '(setenv "UIM_IM_ENGINE" "py" #t))
   (uim '(set! default-im-name #f))
   (assert-equal 'py
		 (uim '(im-name (find-default-im "en"))))
   (assert-equal 'py
		 (uim '(im-name (find-default-im "pt"))))
   (assert-equal 'py
		 (uim '(im-name (find-default-im "ja_JP.EUC-JP"))))
   ;; default-im-for-debug precedes default-im-name
   (uim '(setenv "UIM_IM_ENGINE" "py" #t))
   (uim '(set! default-im-name 'ipa-x-sampa))
   (assert-equal 'py
		 (uim '(im-name (find-default-im "en"))))
   (assert-equal 'py
		 (uim '(im-name (find-default-im "pt"))))
   (assert-equal 'py
		 (uim '(im-name (find-default-im "ja_JP.EUC-JP")))))

  ("test find-im"
   (uim '(unsetenv "UIM_IM_ENGINE"))
   (uim '(unsetenv "LANG"))
   (uim '(unsetenv "LC_ALL"))
   ;; explicit IM specification
   (uim '(set! default-im-name #f))
   (assert-equal 'direct
		 (uim '(im-name (find-im 'direct #f))))
   (assert-equal 'direct
		 (uim '(im-name (find-im 'nonexistent #f))))
   (assert-equal 'anthy
		 (uim '(im-name (find-im 'anthy #f))))
   (assert-equal 'skk
		 (uim '(im-name (find-im 'skk #f))))
   (assert-equal 'latin
		 (uim '(im-name (find-im 'latin #f))))
   (assert-equal 'py
		 (uim '(im-name (find-im 'py #f))))
   (assert-equal 'pinyin-big5
		 (uim '(im-name (find-im 'pinyin-big5 #f))))
   (assert-equal 'pyunihan
		 (uim '(im-name (find-im 'pyunihan #f))))
   ;; implicit selection by locale information
   (assert-equal 'direct
		 (uim '(im-name (find-im #f #f))))
   (assert-equal 'direct
		 (uim '(im-name (find-im #f ""))))
   (assert-equal 'direct
		 (uim '(im-name (find-im #f "C"))))
   (assert-equal 'direct
		 (uim '(im-name (find-im #f "en"))))
   (assert-equal 'direct
		 (uim '(im-name (find-im #f "pt"))))
   (assert-equal 'anthy
		 (uim '(im-name (find-im #f "ja_JP.EUC-JP"))))
   (assert-equal 'direct
		 (uim '(im-name (find-im #f "xx"))))
   ;; im-name always precedes locale
   (assert-equal 'py
		 (uim '(im-name (find-im 'py #f))))
   (assert-equal 'py
		 (uim '(im-name (find-im 'py ""))))
   (assert-equal 'py
		 (uim '(im-name (find-im 'py "C"))))
   (assert-equal 'py
		 (uim '(im-name (find-im 'py "en"))))
   (assert-equal 'py
		 (uim '(im-name (find-im 'py "pt"))))
   (assert-equal 'py
		 (uim '(im-name (find-im 'py "ja_JP.EUC-JP"))))
   (assert-equal 'py
		 (uim '(im-name (find-im 'py "zh_TW.Big5"))))
   (assert-equal 'py
		 (uim '(im-name (find-im 'py "xx"))))
   (assert-equal 'anthy
		 (uim '(im-name (find-im 'nonexistent "ja_JP.EUC-JP"))))
   ;; explicit IM specification with default-im-name
   (uim '(unsetenv "UIM_IM_ENGINE"))
   (uim '(set! default-im-name 'pyunihan))
   (assert-equal 'pyunihan
		 (uim '(im-name (find-im #f #f))))
   (assert-equal 'pyunihan
		 (uim '(im-name (find-im #f "ja_JP.EUC-JP"))))
   (assert-equal 'direct
		 (uim '(im-name (find-im 'direct #f))))
   (assert-equal 'anthy
		 (uim '(im-name (find-im 'anthy #f))))
   (assert-equal 'skk
		 (uim '(im-name (find-im 'skk #f))))
   (assert-equal 'latin
		 (uim '(im-name (find-im 'latin #f))))
   (assert-equal 'py
		 (uim '(im-name (find-im 'py #f))))
   (assert-equal 'pinyin-big5
		 (uim '(im-name (find-im 'pinyin-big5 #f))))
   (assert-equal 'pyunihan
		 (uim '(im-name (find-im 'pyunihan #f))))
   (assert-equal 'pyunihan
		 (uim '(im-name (find-im 'nonexistent #f))))
   ;; implicit selection by locale information with default-im-name
   (uim '(unsetenv "UIM_IM_ENGINE"))
   (uim '(set! default-im-name 'pyunihan))
   (assert-equal 'pyunihan
		 (uim '(im-name (find-im #f #f))))
   (assert-equal 'pyunihan
		 (uim '(im-name (find-im #f ""))))
   (assert-equal 'pyunihan
		 (uim '(im-name (find-im #f "C"))))
   (assert-equal 'pyunihan
		 (uim '(im-name (find-im #f "en"))))
   (assert-equal 'pyunihan
		 (uim '(im-name (find-im #f "pt"))))
   (assert-equal 'pyunihan
		 (uim '(im-name (find-im #f "ja_JP.EUC-JP"))))
   (assert-equal 'pyunihan
		 (uim '(im-name (find-im #f "zh_TW.Big5"))))
   (assert-equal 'pyunihan
		 (uim '(im-name (find-im #f "xx"))))
   ;; explicit IM specification with UIM_IM_ENGINE
   (uim '(setenv "UIM_IM_ENGINE" "pyunihan" #t))
   (uim '(set! default-im-name #f))
   (assert-equal 'pyunihan
		 (uim '(im-name (find-im #f #f))))
   (assert-equal 'pyunihan
		 (uim '(im-name (find-im #f "ja_JP.EUC-JP"))))
   (assert-equal 'direct
		 (uim '(im-name (find-im 'direct #f))))
   (assert-equal 'anthy
		 (uim '(im-name (find-im 'anthy #f))))
   (assert-equal 'skk
		 (uim '(im-name (find-im 'skk #f))))
   (assert-equal 'latin
		 (uim '(im-name (find-im 'latin #f))))
   (assert-equal 'py
		 (uim '(im-name (find-im 'py #f))))
   (assert-equal 'pinyin-big5
		 (uim '(im-name (find-im 'pinyin-big5 #f))))
   (assert-equal 'pyunihan
		 (uim '(im-name (find-im 'pyunihan #f))))
   (assert-equal 'pyunihan
		 (uim '(im-name (find-im 'nonexistent #f))))
   ;; implicit selection by locale information with UIM_IM_ENGINE
   (uim '(setenv "UIM_IM_ENGINE" "pyunihan" #t))
   (uim '(set! default-im-name #f))
   (assert-equal 'pyunihan
		 (uim '(im-name (find-im #f #f))))
   (assert-equal 'pyunihan
		 (uim '(im-name (find-im #f ""))))
   (assert-equal 'pyunihan
		 (uim '(im-name (find-im #f "C"))))
   (assert-equal 'pyunihan
		 (uim '(im-name (find-im #f "en"))))
   (assert-equal 'pyunihan
		 (uim '(im-name (find-im #f "pt"))))
   (assert-equal 'pyunihan
		 (uim '(im-name (find-im #f "ja_JP.EUC-JP"))))
   (assert-equal 'pyunihan
		 (uim '(im-name (find-im #f "zh_TW.Big5"))))
   (assert-equal 'pyunihan
		 (uim '(im-name (find-im #f "xx"))))
   ;; UIM_IM_ENGINE precedes default-im-name (explicit im-name)
   (uim '(setenv "UIM_IM_ENGINE" "pyunihan" #t))
   (uim '(set! default-im-name 'py))
   (assert-equal 'pyunihan
		 (uim '(im-name (find-im #f #f))))
   (assert-equal 'pyunihan
		 (uim '(im-name (find-im #f "ja_JP.EUC-JP"))))
   (assert-equal 'direct
		 (uim '(im-name (find-im 'direct #f))))
   (assert-equal 'anthy
		 (uim '(im-name (find-im 'anthy #f))))
   (assert-equal 'skk
		 (uim '(im-name (find-im 'skk #f))))
   (assert-equal 'latin
		 (uim '(im-name (find-im 'latin #f))))
   (assert-equal 'py
		 (uim '(im-name (find-im 'py #f))))
   (assert-equal 'pinyin-big5
		 (uim '(im-name (find-im 'pinyin-big5 #f))))
   (assert-equal 'pyunihan
		 (uim '(im-name (find-im 'pyunihan #f))))
   (assert-equal 'pyunihan
		 (uim '(im-name (find-im 'nonexistent #f))))
   ;; UIM_IM_ENGINE precedes default-im-name (implicit selection)
   (uim '(setenv "UIM_IM_ENGINE" "pyunihan" #t))
   (uim '(set! default-im-name 'py))
   (assert-equal 'pyunihan
		 (uim '(im-name (find-im #f #f))))
   (assert-equal 'pyunihan
		 (uim '(im-name (find-im #f ""))))
   (assert-equal 'pyunihan
		 (uim '(im-name (find-im #f "C"))))
   (assert-equal 'pyunihan
		 (uim '(im-name (find-im #f "en"))))
   (assert-equal 'pyunihan
		 (uim '(im-name (find-im #f "pt"))))
   (assert-equal 'pyunihan
		 (uim '(im-name (find-im #f "ja_JP.EUC-JP"))))
   (assert-equal 'pyunihan
		 (uim '(im-name (find-im #f "zh_TW.Big5"))))
   (assert-equal 'pyunihan
		 (uim '(im-name (find-im #f "xx"))))))

(define-uim-test-case "testcase im im-switching"
  (setup
   (lambda ()
     (uim '(for-each require-module installed-im-module-list))
     (uim '(define test-im-anthy #f))
     (uim '(define test-im-skk #f))
     (uim '(define test-im-latin #f))
     (uim '(begin
	     (set! test-im-anthy (assq 'anthy im-list))
	     #t))
     (uim '(begin
	     (set! test-im-skk (assq 'skk im-list))
	     #t))
     (uim '(begin
	     (set! test-im-latin (assq 'latin im-list))
	     #t))
     (uim '(begin
	     (set! im-list (list test-im-anthy
				 test-im-skk
				 test-im-latin))
	     #t))
     (uim '(begin
	     (set! enabled-im-list '(anthy skk latin))
	     #t))))

  ("test next-im"
   (assert-equal 'skk
		 (uim '(next-im 'anthy)))
   (assert-equal 'latin
		 (uim '(next-im 'skk)))
   (assert-equal 'anthy
		 (uim '(next-im 'latin)))
   (assert-equal 'anthy
		 (uim '(next-im 'non-existent))))

  ;; TODO: enable this
  ("test switch-im"
;   (assert-equal ()
;		 (uim 'context-list))
;   ;; create-context from Scheme world fails because corresponding
;   ;; object in C world is missing
;   ;(uim '(create-context 0 #f 'anthy))
;   ;(uim '(create-context 1 #f 'skk))
;   ;(uim '(create-context 2 #f 'latin))
;   (assert-equal 'anthy
;		 (uim '(im-name current-im)))
;   ;; switch-im fails because create-context fails
;   ;(uim '(switch-im 1 'latin))
;   (assert-equal 'latin
;		 (uim '(im-name current-im)))
;   (assert-equal 'latin
;		 (uim '(im-name (context-im (find-context 1)))))
;   ;(uim '(switch-im 1 'skk))
;   (assert-equal 'skk
;		 (uim '(im-name current-im)))
;   (assert-equal 'skk
;		 (uim '(im-name (context-im (find-context 1)))))
   ))

(define-uim-test-case "testcase im context management"
  (setup
   (lambda ()
     (uim '(for-each require-module installed-im-module-list))
     ;; define as hand-made data to avoid that implementation of
     ;; register-context affect other tests
     (uim '(begin
	     (set! context-list (list (im-new 1 (retrieve-im 'latin))
				      (im-new 2 (retrieve-im 'direct))
				      (im-new 3 (retrieve-im 'skk))
				      (im-new 4 (retrieve-im 'anthy))))
	     #t))))
     
  ("test context-uc"
   (assert-equal 1
		 (uim '(context-uc (nth 0 context-list))))
   (assert-equal 2
		 (uim '(context-uc (nth 1 context-list))))
   (assert-equal 3
		 (uim '(context-uc (nth 2 context-list))))
   (assert-equal 4
		 (uim '(context-uc (nth 3 context-list)))))

  ("test context-im"
   (assert-equal 'latin
		 (uim '(im-name (context-im (nth 0 context-list)))))
   (assert-equal 'direct
		 (uim '(im-name (context-im (nth 1 context-list)))))
   (assert-equal 'skk
		 (uim '(im-name (context-im (nth 2 context-list)))))
   (assert-equal 'anthy
		 (uim '(im-name (context-im (nth 3 context-list))))))
   
  ("test remove-context"
   (assert-equal 4
		 (uim '(length context-list)))

   (uim '(begin (remove-context (assv 3 context-list)) #t))
   (assert-equal 3
		 (uim '(length context-list)))
   (assert-equal 'latin
		 (uim '(im-name (context-im (assv 1 context-list)))))
   (assert-equal 'direct
		 (uim '(im-name (context-im (assv 2 context-list)))))
   (assert-false (uim-bool '(assv 3 context-list)))
   (assert-equal 'anthy
		 (uim '(im-name (context-im (assv 4 context-list)))))

   (uim '(begin (remove-context (assv 1 context-list)) #t))
   (assert-equal 2
		 (uim '(length context-list)))
   (assert-false (uim-bool '(assv 1 context-list)))
   (assert-equal 'direct
		 (uim '(im-name (context-im (assv 2 context-list)))))
   (assert-false (uim-bool '(assv 3 context-list)))
   (assert-equal 'anthy
		 (uim '(im-name (context-im (assv 4 context-list)))))

   ;; test excessive removal
   (uim '(begin (remove-context (assv 1 context-list)) #t))
   (assert-equal 2
		 (uim '(length context-list)))
   (assert-false (uim-bool '(assv 1 context-list)))
   (assert-equal 'direct
		 (uim '(im-name (context-im (assv 2 context-list)))))
   (assert-false (uim-bool '(assv 3 context-list)))
   (assert-equal 'anthy
		 (uim '(im-name (context-im (assv 4 context-list)))))

   (uim '(begin (remove-context (assv 4 context-list)) #t))
   (assert-equal 1
		 (uim '(length context-list)))
   (assert-false (uim-bool '(assv 1 context-list)))
   (assert-equal 'direct
		 (uim '(im-name (context-im (assv 2 context-list)))))
   (assert-false (uim-bool '(assv 3 context-list)))
   (assert-false (uim-bool '(assv 4 context-list)))

   (uim '(begin (remove-context (assv 2 context-list)) #t))
   (assert-true  (uim-bool '(null? context-list)))
   (assert-false (uim-bool '(assv 1 context-list)))
   (assert-false (uim-bool '(assv 2 context-list)))
   (assert-false (uim-bool '(assv 3 context-list)))
   (assert-false (uim-bool '(assv 4 context-list)))

   ;; test exessive removal
   (uim '(begin (remove-context (assv 1 context-list))))
   (assert-true  (uim-bool '(null? context-list)))
   (assert-false (uim-bool '(assv 1 context-list)))
   (assert-false (uim-bool '(assv 2 context-list)))
   (assert-false (uim-bool '(assv 3 context-list)))
   (assert-false (uim-bool '(assv 4 context-list))))

  ("test register-context (add as new id)"
   (assert-equal 4
		 (uim '(length context-list)))
   (uim '(begin
	   (register-context (context-new 5 (find-im 'tutcode #f)))
	   #t))
   (assert-equal 5
		 (uim '(length context-list)))
   (assert-equal 'latin
		 (uim '(im-name (context-im (assv 1 context-list)))))
   (assert-equal 'tutcode
		 (uim '(im-name (context-im (assv 5 context-list)))))

   ;; sparse id must be accepted
   (uim '(begin
	   (register-context (context-new 10 (find-im 'py #f)))
	   #t))
   (assert-equal 6
		 (uim '(length context-list)))
   (assert-equal 'py
		 (uim '(im-name (context-im (assv 10 context-list)))))

   ;; additional sparse id
   (uim '(begin
	   (register-context (context-new 8 (find-im 'pyunihan #f)))
	   #t))
   (assert-equal 7
		 (uim '(length context-list)))
   (assert-equal 'pyunihan
		 (uim '(im-name (context-im (assv 8 context-list)))))

   ;; decrimented id
   (uim '(begin
	   (register-context (context-new 0 (find-im 'pinyin-big5 #f)))
	   #t))
   (assert-equal 8
		 (uim '(length context-list)))
   (assert-equal 'pinyin-big5
		 (uim '(im-name (context-im (assv 0 context-list))))))

  ("test register-context (duplicate id)"
   (assert-equal 4
		 (uim '(length context-list)))
   (uim '(begin
	   (register-context (context-new 1 (find-im 'tutcode #f)))
	   #t))
   ;; register-context doesn't check duplicate id, so caller have to
   ;; ensure no duplication. Result of duplicate context is undefined
   (assert-equal 5
		 (uim '(length context-list))))

  ("test create-context"
   ;; create-context from Scheme world fails because corresponding
   ;; object in C world is missing
   )

  ("test release-context"
   ;; release-context requires properly created context, so
   ;; create-context from Scheme world is required
   ))

;; TODO
(define-uim-test-case "test im handlers"
  ("test invoke-handler"
   )
  ("test key-press-handler"
   )
  ("test key-release-handler"
   )
  ("test reset-handler"
   )
  ("test mode-handler"
   )
  ("test prop-handler"
   )
  ("test get-candidate"
   )
  ("test set-candidate-index"
   )
  )

(define-uim-test-case "testcase im im-custom"
  (setup
   (lambda ()
     (uim '(begin
	     (require-module "anthy")
	     (require-module "canna")
	     (require-module "skk")
	     (require-module "latin")))))

  ("test custom-im-list-as-choice-rec"
   (assert-equal '((canna "Canna" "A multi-segment kana-kanji conversion engine")
		   (skk "SKK" "uim version of SKK input method")
		   (anthy "Anthy" "A multi-segment kana-kanji conversion engine"))
		 (uim '(custom-im-list-as-choice-rec
			(map retrieve-im '(canna skk anthy)))))
   (assert-equal '((latin
		    "Latin characters"
		    "Latin characters mainly used for Latin and Germanic languages"))
		 (uim '(custom-im-list-as-choice-rec
			(map retrieve-im '(latin)))))
   (assert-equal ()
		 (uim '(custom-im-list-as-choice-rec ())))))
