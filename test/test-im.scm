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

(require-extension (srfi 1))

(test-begin "preedit-attr?")
(test-true  (preedit-attr? preedit-none))
(test-true  (preedit-attr? preedit-underline))
(test-true  (preedit-attr? preedit-reverse))
(test-true  (preedit-attr? preedit-cursor))
(test-true  (preedit-attr? preedit-separator))
(test-false (preedit-attr? #f))
(test-false (preedit-attr? 2398))  ;; arbitrary integer
(test-end)

;; Use a fixed set of IM modules that work without their external
;; engines so that the results don't depend on the build. anthy must be
;; the first one for the language based IM selection.
(define test-im-module-list '("anthy" "canna" "skk" "latin" "tutcode"
			      "byeoru" "pyload" "viqr" "ipa-x-sampa"))
(define test-im-name-list '(direct anthy canna skk latin tutcode
			    byeoru py pyunihan pinyin-big5 viqr ipa-x-sampa))
(set! installed-im-module-list test-im-module-list)
(set! installed-im-list test-im-name-list)
(set! enabled-im-list test-im-name-list)
(for-each require-module test-im-module-list)
(define test-im-pristine-im-list (map list-copy im-list))

;; default-im-for-debug works only with --enable-debug.
(provide "debug")

(define (setup-im-list)
  (set! im-list (map list-copy test-im-pristine-im-list))
  (set! context-list ())
  (set! currently-loading-module-name #f)
  (set! default-im-name #f)
  (unsetenv "UIM_IM_ENGINE"))

(define prev-im #f)
(define prev-nr-ims #f)
(define test-im-init-args #f)
(define test-im-alt-init-args #f)

(define (setup)
  (setup-im-list)
  (set! enabled-im-list
	(append test-im-name-list
		'(test-im test-im2)))
  (set! prev-im #f)
  (set! prev-nr-ims (length im-list))
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
				    #f)))

(test-begin "normalize-im-list")
(setup)
(set! im-list (remove (lambda (im)
			(not (eq? (im-name im)
				  'direct)))
		      im-list))
(test-equal '(direct)
	    (map im-name im-list))
;; direct IM always remains at head
(test-true  (apply register-im test-im-init-args))
(test-equal '(direct test-im)
	    (map im-name im-list))
;; other IMs are cons'ed at right of direct
(test-true  (apply register-im (cons 'test-im2
				    (cdr test-im-init-args))))
(test-equal '(direct test-im2 test-im)
	    (map im-name im-list))
;; direct IM can be registered to null list
(set! im-list ())
;; second time register-im for 'direct returns #t if im-list does not
;; contain 'direct
(test-true  (apply register-im (cons 'direct
				    (cdr test-im-init-args))))
(test-equal '(direct)
	    (map im-name im-list))
;; ordinary IM can be registered to null list
(set! im-list ())
;; second time register-im for 'test-im returns #t if im-list does not
;; contain 'test-im
(test-true  (apply register-im test-im-init-args))
(test-equal '(test-im)
	    (map im-name im-list))
(test-end)

(test-begin "register-im")
(setup)
(test-true  (apply register-im test-im-init-args))
(test-equal (+ prev-nr-ims 1)
	    (length im-list))
(test-equal 'test-im
	    (im-name (retrieve-im 'test-im)))
(test-false (im-module-name (retrieve-im 'test-im)))
(test-equal 22
	    (length (retrieve-im 'test-im)))
(im-set-module-name! (retrieve-im 'test-im) "foo")
(test-equal "foo"
	    (im-module-name (retrieve-im 'test-im)))

;; duplicate register overwrites preexisting one
(test-false (apply register-im test-im-alt-init-args))
(test-equal (+ prev-nr-ims 1)
	    (length im-list))
(test-equal '(test-im
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
	    (retrieve-im 'test-im))
;; subsequent registration that has different im-name will be
;; registered as another IM
(test-true  (apply register-im
		   (cons 'test-im2
			 (cdr test-im-alt-init-args))))
(test-equal (+ prev-nr-ims 2)
	    (length im-list))
(test-equal '(test-im2
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
	    (retrieve-im 'test-im2))
(test-end)

(test-begin "register-im (module-name)")
(setup)
(test-true  (apply register-im test-im-init-args))
(test-false (im-module-name (retrieve-im 'test-im)))

(set! currently-loading-module-name "foo")
(test-true  (apply register-im (cons 'test-im2
				    (cdr test-im-init-args))))
(test-equal "foo"
	    (im-module-name (retrieve-im 'test-im2)))
(test-end)

(test-begin "retrieve-im")
(setup)
(test-false (retrieve-im 'nonexistent))
(test-equal 'direct
	    (im-name (retrieve-im 'direct)))
(test-equal 'anthy
	    (im-name (retrieve-im 'anthy)))
(test-equal 'ipa-x-sampa
	    (im-name (retrieve-im 'ipa-x-sampa)))
(test-false (retrieve-im 'test-im))
(apply register-im test-im-init-args)
(test-equal 'test-im
	    (im-name (retrieve-im 'test-im)))
(test-end)

(test-begin "default-im-for-debug")
(setup)
(test-true  (feature? 'debug))
(unsetenv "UIM_IM_ENGINE")
(test-false (default-im-for-debug))
(setenv "UIM_IM_ENGINE" "nonexistent" #t)
(test-false (default-im-for-debug))
(setenv "UIM_IM_ENGINE" "test-im" #t)
(test-false (default-im-for-debug))
(apply register-im test-im-init-args)
(test-equal 'test-im
	    (im-name (default-im-for-debug)))
(setenv "UIM_IM_ENGINE" "anthy" #t)
(test-equal 'anthy
	    (im-name (default-im-for-debug)))
;; default-im-name does not affect default-im-for-debug
(set! default-im-name 'ipa-x-sampa)
(test-equal 'anthy
	    (im-name (default-im-for-debug)))
(test-end)

(test-begin "find-im-for-locale")
(setup)
;; IM existence test
(test-false (memq 'nonexistent (map car im-list)))
(test-true  (memq 'anthy (map car im-list)))
(test-true  (memq 'skk (map car im-list)))
(test-true  (memq 'latin(map car im-list)))
(test-true  (memq 'tutcode (map car im-list)))
(test-true  (memq 'py (map car im-list)))
(test-true  (memq 'pyunihan (map car im-list)))
(test-true  (memq 'pinyin-big5 (map car im-list)))
(test-true  (memq 'viqr (map car im-list)))
(test-true  (memq 'ipa-x-sampa (map car im-list)))
(test-true  (memq 'direct (map car im-list)))
;; unsupported or direct-input languages
(test-equal 'direct
	    (im-name (find-im-for-locale "C")))
(test-equal 'direct
	    (im-name (find-im-for-locale "POSIX")))
(test-equal 'direct
	    (im-name (find-im-for-locale "en")))
(test-equal 'direct
	    (im-name (find-im-for-locale "en_US")))
(test-equal 'direct
	    (im-name (find-im-for-locale "en_US.US-ASCII")))
(test-equal 'direct
	    (im-name (find-im-for-locale "en_US.UTF-8")))
(test-equal 'direct
	    (im-name (find-im-for-locale "de")))
(test-equal 'direct
	    (im-name (find-im-for-locale "fr")))
(test-equal 'direct
	    (im-name (find-im-for-locale "de")))
(test-equal 'direct
	    (im-name (find-im-for-locale "pt")))
;; non-existent language code
(test-equal 'direct
	    (im-name (find-im-for-locale "xx")))
;; default-im-name does not affect find-im-for-locale
(set! default-im-name 'ipa-x-sampa)
(test-equal 'direct
	    (im-name (find-im-for-locale "pt")))
;; default-im-for-debug does not affect find-im-for-locale
(setenv "UIM_IM_ENGINE" "ipa-x-sampa" #t)
(test-equal 'direct
	    (im-name (find-im-for-locale "pt")))
(unsetenv "UIM_IM_ENGINE")
;; Chinese
;; pyunihan is chosen because the m17n-lib IMs aren't loaded.
(test-equal 'pyunihan
	    (im-name (find-im-for-locale "zh")))
(test-equal 'pyunihan
	    (im-name (find-im-for-locale "zh.GB18030")))
(test-equal 'pyunihan
	    (im-name (find-im-for-locale "zh.Big5")))
(test-equal 'pyunihan
	    (im-name (find-im-for-locale "zh.UTF-8")))
(test-equal 'direct
	    (im-name (find-im-for-locale "zh_US")))
(test-equal 'py
	    (im-name (find-im-for-locale "zh_CN")))
(test-equal 'py
	    (im-name (find-im-for-locale "zh_CN.GB18030")))
(test-equal 'py
	    (im-name (find-im-for-locale "zh_CN.UTF-8")))
(test-equal 'pinyin-big5
	    (im-name (find-im-for-locale "zh_TW")))
(test-equal 'pinyin-big5
	    (im-name (find-im-for-locale "zh_TW.Big5")))
(test-equal 'pinyin-big5
	    (im-name (find-im-for-locale "zh_TW.UTF-8")))
(test-equal 'pinyin-big5
	    (im-name (find-im-for-locale "zh_HK")))
(test-equal 'pinyin-big5
	    (im-name (find-im-for-locale "zh_HK.Big5")))
(test-equal 'pinyin-big5
	    (im-name (find-im-for-locale "zh_HK.UTF-8")))
;; Japanese
(test-equal 'anthy
	    (im-name (find-im-for-locale "ja")))
(test-equal 'anthy
	    (im-name (find-im-for-locale "ja_JP")))
(test-equal 'anthy
	    (im-name (find-im-for-locale "ja_JP.EUC-JP")))
(test-equal 'anthy
	    (im-name (find-im-for-locale "ja_JP.UTF-8")))
(test-equal 'anthy
	    (im-name (find-im-for-locale "ja.UTF-8")))
;; Korean
(test-equal 'byeoru
	    (im-name (find-im-for-locale "ko")))
(test-equal 'byeoru
	    (im-name (find-im-for-locale "ko_KR")))
(test-equal 'byeoru
	    (im-name (find-im-for-locale "ko_KR.EUC-KR")))
(test-equal 'byeoru
	    (im-name (find-im-for-locale "ko_KR.UTF-8")))
(test-equal 'byeoru
	    (im-name (find-im-for-locale "ko.UTF-8")))
;; native locale
(unsetenv "LC_ALL")
(unsetenv "LANG")
(test-equal 'direct
	    (im-name (find-im-for-locale "")))
(setenv "LC_ALL" "C" #t)
(unsetenv "LANG")
(test-equal 'direct
	    (im-name (find-im-for-locale "")))
(unsetenv "LC_ALL")
(setenv "LANG" "C" #t)
(test-equal 'direct
	    (im-name (find-im-for-locale "")))
(setenv "LC_ALL" "ja_JP.EUC-JP" #t)
(unsetenv "LANG")
(test-equal 'anthy
	    (im-name (find-im-for-locale "")))
;; nonexistent native locale
(setenv "LC_ALL" "xx" #t)
(unsetenv "LANG")
(test-equal 'direct
	    (im-name (find-im-for-locale "")))
(test-end)

(test-begin "find-default-im")
(setup)
;; find by find-im-for-locale
(unsetenv "UIM_IM_ENGINE")
(set! default-im-name #f)
(test-equal 'direct
	    (im-name (find-default-im "C")))
(test-equal 'direct
	    (im-name (find-default-im "en")))
(test-equal 'direct
	    (im-name (find-default-im "pt")))
(test-equal 'anthy
	    (im-name (find-default-im "ja_JP.EUC-JP")))
;; empty locale string can be specified as native locale
(unsetenv "UIM_IM_ENGINE")
(set! default-im-name #f)
(unsetenv "LC_ALL")
(unsetenv "LANG")
(test-equal 'direct
	    (im-name (find-default-im "")))
(setenv "LC_ALL" "C" #t)
(test-equal 'direct
	    (im-name (find-default-im "")))
(setenv "LC_ALL" "ja_JP.EUC-JP" #t)
(test-equal 'anthy
	    (im-name (find-default-im "")))
(unsetenv "LC_ALL")
;; default-im-name precedes the locale specified by arg
(unsetenv "UIM_IM_ENGINE")
(set! default-im-name 'ipa-x-sampa)
(test-equal 'ipa-x-sampa
	    (im-name (find-default-im "en")))
(test-equal 'ipa-x-sampa
	    (im-name (find-default-im "pt")))
(test-equal 'ipa-x-sampa
	    (im-name (find-default-im "ja_JP.EUC-JP")))
;; default-im-for-debug precedes the locale specified by arg
(setenv "UIM_IM_ENGINE" "py" #t)
(set! default-im-name #f)
(test-equal 'py
	    (im-name (find-default-im "en")))
(test-equal 'py
	    (im-name (find-default-im "pt")))
(test-equal 'py
	    (im-name (find-default-im "ja_JP.EUC-JP")))
;; default-im-for-debug precedes default-im-name
(setenv "UIM_IM_ENGINE" "py" #t)
(set! default-im-name 'ipa-x-sampa)
(test-equal 'py
	    (im-name (find-default-im "en")))
(test-equal 'py
	    (im-name (find-default-im "pt")))
(test-equal 'py
	    (im-name (find-default-im "ja_JP.EUC-JP")))
(test-end)

(test-begin "find-im")
(setup)
(unsetenv "UIM_IM_ENGINE")
(unsetenv "LANG")
(unsetenv "LC_ALL")
;; explicit IM specification
(set! default-im-name #f)
(test-equal 'direct
	    (im-name (find-im 'direct #f)))
(test-equal 'direct
	    (im-name (find-im 'nonexistent #f)))
(test-equal 'anthy
	    (im-name (find-im 'anthy #f)))
(test-equal 'skk
	    (im-name (find-im 'skk #f)))
(test-equal 'latin
	    (im-name (find-im 'latin #f)))
(test-equal 'py
	    (im-name (find-im 'py #f)))
(test-equal 'pinyin-big5
	    (im-name (find-im 'pinyin-big5 #f)))
(test-equal 'pyunihan
	    (im-name (find-im 'pyunihan #f)))
;; implicit selection by locale information
(test-equal 'direct
	    (im-name (find-im #f #f)))
(test-equal 'direct
	    (im-name (find-im #f "")))
(test-equal 'direct
	    (im-name (find-im #f "C")))
(test-equal 'direct
	    (im-name (find-im #f "en")))
(test-equal 'direct
	    (im-name (find-im #f "pt")))
(test-equal 'anthy
	    (im-name (find-im #f "ja_JP.EUC-JP")))
(test-equal 'direct
	    (im-name (find-im #f "xx")))
;; im-name always precedes locale
(test-equal 'py
	    (im-name (find-im 'py #f)))
(test-equal 'py
	    (im-name (find-im 'py "")))
(test-equal 'py
	    (im-name (find-im 'py "C")))
(test-equal 'py
	    (im-name (find-im 'py "en")))
(test-equal 'py
	    (im-name (find-im 'py "pt")))
(test-equal 'py
	    (im-name (find-im 'py "ja_JP.EUC-JP")))
(test-equal 'py
	    (im-name (find-im 'py "zh_TW.Big5")))
(test-equal 'py
	    (im-name (find-im 'py "xx")))
(test-equal 'anthy
	    (im-name (find-im 'nonexistent "ja_JP.EUC-JP")))
;; explicit IM specification with default-im-name
(unsetenv "UIM_IM_ENGINE")
(set! default-im-name 'pyunihan)
(test-equal 'pyunihan
	    (im-name (find-im #f #f)))
(test-equal 'pyunihan
	    (im-name (find-im #f "ja_JP.EUC-JP")))
(test-equal 'direct
	    (im-name (find-im 'direct #f)))
(test-equal 'anthy
	    (im-name (find-im 'anthy #f)))
(test-equal 'skk
	    (im-name (find-im 'skk #f)))
(test-equal 'latin
	    (im-name (find-im 'latin #f)))
(test-equal 'py
	    (im-name (find-im 'py #f)))
(test-equal 'pinyin-big5
	    (im-name (find-im 'pinyin-big5 #f)))
(test-equal 'pyunihan
	    (im-name (find-im 'pyunihan #f)))
(test-equal 'pyunihan
	    (im-name (find-im 'nonexistent #f)))
;; implicit selection by locale information with default-im-name
(unsetenv "UIM_IM_ENGINE")
(set! default-im-name 'pyunihan)
(test-equal 'pyunihan
	    (im-name (find-im #f #f)))
(test-equal 'pyunihan
	    (im-name (find-im #f "")))
(test-equal 'pyunihan
	    (im-name (find-im #f "C")))
(test-equal 'pyunihan
	    (im-name (find-im #f "en")))
(test-equal 'pyunihan
	    (im-name (find-im #f "pt")))
(test-equal 'pyunihan
	    (im-name (find-im #f "ja_JP.EUC-JP")))
(test-equal 'pyunihan
	    (im-name (find-im #f "zh_TW.Big5")))
(test-equal 'pyunihan
	    (im-name (find-im #f "xx")))
;; explicit IM specification with UIM_IM_ENGINE
(setenv "UIM_IM_ENGINE" "pyunihan" #t)
(set! default-im-name #f)
(test-equal 'pyunihan
	    (im-name (find-im #f #f)))
(test-equal 'pyunihan
	    (im-name (find-im #f "ja_JP.EUC-JP")))
(test-equal 'direct
	    (im-name (find-im 'direct #f)))
(test-equal 'anthy
	    (im-name (find-im 'anthy #f)))
(test-equal 'skk
	    (im-name (find-im 'skk #f)))
(test-equal 'latin
	    (im-name (find-im 'latin #f)))
(test-equal 'py
	    (im-name (find-im 'py #f)))
(test-equal 'pinyin-big5
	    (im-name (find-im 'pinyin-big5 #f)))
(test-equal 'pyunihan
	    (im-name (find-im 'pyunihan #f)))
(test-equal 'pyunihan
	    (im-name (find-im 'nonexistent #f)))
;; implicit selection by locale information with UIM_IM_ENGINE
(setenv "UIM_IM_ENGINE" "pyunihan" #t)
(set! default-im-name #f)
(test-equal 'pyunihan
	    (im-name (find-im #f #f)))
(test-equal 'pyunihan
	    (im-name (find-im #f "")))
(test-equal 'pyunihan
	    (im-name (find-im #f "C")))
(test-equal 'pyunihan
	    (im-name (find-im #f "en")))
(test-equal 'pyunihan
	    (im-name (find-im #f "pt")))
(test-equal 'pyunihan
	    (im-name (find-im #f "ja_JP.EUC-JP")))
(test-equal 'pyunihan
	    (im-name (find-im #f "zh_TW.Big5")))
(test-equal 'pyunihan
	    (im-name (find-im #f "xx")))
;; UIM_IM_ENGINE precedes default-im-name (explicit im-name)
(setenv "UIM_IM_ENGINE" "pyunihan" #t)
(set! default-im-name 'py)
(test-equal 'pyunihan
	    (im-name (find-im #f #f)))
(test-equal 'pyunihan
	    (im-name (find-im #f "ja_JP.EUC-JP")))
(test-equal 'direct
	    (im-name (find-im 'direct #f)))
(test-equal 'anthy
	    (im-name (find-im 'anthy #f)))
(test-equal 'skk
	    (im-name (find-im 'skk #f)))
(test-equal 'latin
	    (im-name (find-im 'latin #f)))
(test-equal 'py
	    (im-name (find-im 'py #f)))
(test-equal 'pinyin-big5
	    (im-name (find-im 'pinyin-big5 #f)))
(test-equal 'pyunihan
	    (im-name (find-im 'pyunihan #f)))
(test-equal 'pyunihan
	    (im-name (find-im 'nonexistent #f)))
;; UIM_IM_ENGINE precedes default-im-name (implicit selection)
(setenv "UIM_IM_ENGINE" "pyunihan" #t)
(set! default-im-name 'py)
(test-equal 'pyunihan
	    (im-name (find-im #f #f)))
(test-equal 'pyunihan
	    (im-name (find-im #f "")))
(test-equal 'pyunihan
	    (im-name (find-im #f "C")))
(test-equal 'pyunihan
	    (im-name (find-im #f "en")))
(test-equal 'pyunihan
	    (im-name (find-im #f "pt")))
(test-equal 'pyunihan
	    (im-name (find-im #f "ja_JP.EUC-JP")))
(test-equal 'pyunihan
	    (im-name (find-im #f "zh_TW.Big5")))
(test-equal 'pyunihan
	    (im-name (find-im #f "xx")))
(test-end)

(define (setup-im-switching)
  (setup-im-list)
  (set! im-list (list (retrieve-im 'anthy)
		      (retrieve-im 'skk)
		      (retrieve-im 'latin)))
  (set! enabled-im-list '(anthy skk latin)))

(test-begin "next-im")
(setup-im-switching)
(test-equal 'skk
	    (next-im 'anthy))
(test-equal 'latin
	    (next-im 'skk))
(test-equal 'anthy
	    (next-im 'latin))
(test-equal 'anthy
	    (next-im 'non-existent))
(test-end)

;; define as hand-made data to avoid that implementation of
;; register-context affect other tests
(define (setup-context-management)
  (setup-im-list)
  (set! context-list (list (im-new 1 (retrieve-im 'latin))
			   (im-new 2 (retrieve-im 'direct))
			   (im-new 3 (retrieve-im 'skk))
			   (im-new 4 (retrieve-im 'anthy)))))

(test-begin "context-uc")
(setup-context-management)
(test-equal 1
	    (context-uc (nth 0 context-list)))
(test-equal 2
	    (context-uc (nth 1 context-list)))
(test-equal 3
	    (context-uc (nth 2 context-list)))
(test-equal 4
	    (context-uc (nth 3 context-list)))
(test-end)

(test-begin "context-im")
(setup-context-management)
(test-equal 'latin
	    (im-name (context-im (nth 0 context-list))))
(test-equal 'direct
	    (im-name (context-im (nth 1 context-list))))
(test-equal 'skk
	    (im-name (context-im (nth 2 context-list))))
(test-equal 'anthy
	    (im-name (context-im (nth 3 context-list))))
(test-end)

(test-begin "remove-context")
(setup-context-management)
(test-equal 4
	    (length context-list))

(remove-context (assv 3 context-list))
(test-equal 3
	    (length context-list))
(test-equal 'latin
	    (im-name (context-im (assv 1 context-list))))
(test-equal 'direct
	    (im-name (context-im (assv 2 context-list))))
(test-false (assv 3 context-list))
(test-equal 'anthy
	    (im-name (context-im (assv 4 context-list))))

(remove-context (assv 1 context-list))
(test-equal 2
	    (length context-list))
(test-false (assv 1 context-list))
(test-equal 'direct
	    (im-name (context-im (assv 2 context-list))))
(test-false (assv 3 context-list))
(test-equal 'anthy
	    (im-name (context-im (assv 4 context-list))))

;; test excessive removal
(remove-context (assv 1 context-list))
(test-equal 2
	    (length context-list))
(test-false (assv 1 context-list))
(test-equal 'direct
	    (im-name (context-im (assv 2 context-list))))
(test-false (assv 3 context-list))
(test-equal 'anthy
	    (im-name (context-im (assv 4 context-list))))

(remove-context (assv 4 context-list))
(test-equal 1
	    (length context-list))
(test-false (assv 1 context-list))
(test-equal 'direct
	    (im-name (context-im (assv 2 context-list))))
(test-false (assv 3 context-list))
(test-false (assv 4 context-list))

(remove-context (assv 2 context-list))
(test-true  (null? context-list))
(test-false (assv 1 context-list))
(test-false (assv 2 context-list))
(test-false (assv 3 context-list))
(test-false (assv 4 context-list))

;; test exessive removal
(remove-context (assv 1 context-list))
(test-true  (null? context-list))
(test-false (assv 1 context-list))
(test-false (assv 2 context-list))
(test-false (assv 3 context-list))
(test-false (assv 4 context-list))
(test-end)

(test-begin "register-context (add as new id)")
(setup-context-management)
(test-equal 4
	    (length context-list))
(register-context (context-new 5 (find-im 'tutcode #f)))
(test-equal 5
	    (length context-list))
(test-equal 'latin
	    (im-name (context-im (assv 1 context-list))))
(test-equal 'tutcode
	    (im-name (context-im (assv 5 context-list))))

;; sparse id must be accepted
(register-context (context-new 10 (find-im 'py #f)))
(test-equal 6
	    (length context-list))
(test-equal 'py
	    (im-name (context-im (assv 10 context-list))))

;; additional sparse id
(register-context (context-new 8 (find-im 'pyunihan #f)))
(test-equal 7
	    (length context-list))
(test-equal 'pyunihan
	    (im-name (context-im (assv 8 context-list))))

;; decrimented id
(register-context (context-new 0 (find-im 'pinyin-big5 #f)))
(test-equal 8
	    (length context-list))
(test-equal 'pinyin-big5
	    (im-name (context-im (assv 0 context-list))))
(test-end)

(test-begin "register-context (duplicate id)")
(setup-context-management)
(test-equal 4
	    (length context-list))
(register-context (context-new 1 (find-im 'tutcode #f)))
;; register-context doesn't check duplicate id, so caller have to
;; ensure no duplication. Result of duplicate context is undefined
(test-equal 5
	    (length context-list))
(test-end)

(test-begin "custom-im-list-as-choice-rec")
(setup-im-list)
(test-equal '((canna "Canna" "A multi-segment kana-kanji conversion engine")
	      (skk "SKK" "uim version of SKK input method")
	      (anthy "Anthy" "A multi-segment kana-kanji conversion engine"))
	    (custom-im-list-as-choice-rec
	     (map retrieve-im '(canna skk anthy))))
(test-equal '((latin
	       "Latin characters"
	       "Latin characters mainly used for Latin and Germanic languages"))
	    (custom-im-list-as-choice-rec
	     (map retrieve-im '(latin))))
(test-equal '()
	    (custom-im-list-as-choice-rec '()))
(test-end)

(test-report-result)
