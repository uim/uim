;;; i18n.scm: Internationalization functions for uim
;;;
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

(require "util.scm")

;;
;; string translation
;;

;; WARNING: ugettext and _ should not be used for normal codes.
;; convenience shorthand of runtime translation
(define ugettext
  (if (provided? "nls")
      (lambda (str)
	(dgettext (gettext-package) str))
      (lambda (str)
	str)))

;; WARNING: ugettext and _ should not be used for normal codes.
;; shorthand version of gettext. it is also used as xgettext keyword
(define _ ugettext)

;; shorthand version of gettext_noop. it is used as xgettext keyword
(define N_
  (lambda (str)
    str))

;; All other gettext functions are defined in C. See uim/intl.c


;;
;; locale
;;
(define-record 'locale
  '((lang       "")
    (territory  "")
    (codeset    "")))
(define locale-new-internal locale-new)
(define locale-set-lang-internal! locale-set-lang!)
(define locale-set-territory-internal! locale-set-territory!)

;; Create a locale object from a localestr. localestr assumes
;; following format. modifier is not supported
;;
;;   language[_territory][.codeset]
;;
;; Some exception rules exist.
;;
;; * Accept #f, "C" and "POSIX" as "en" locale
;;
;; * Accept "" as native locale as like as performed in
;;   setlocale(). It attempt to retrieve the locale information from
;;   LC_ALL and LANG environment variable. If failed to retrieve the
;;   information from the two variable, it defaults to "en". It does
;;   not attempt to retrieve from particular locale category such as
;;   LC_CTYPE or LC_MESSAGES since appropriate category for input
;;   method does not exist
(define locale-new
  (lambda (localestr)
    (let* ((substituted-str (cond
			     ((not (string? localestr))	;; mainly for #f
			      "C")
			     ((string=? localestr "")
			      (or (getenv "LC_ALL")
				  (getenv "LANG")
				  "C"))
			     (else
			      localestr)))
	   (canonical-str (cond
			   ((or (string=? substituted-str "C")
				(string=? substituted-str "POSIX"))
			    "en")
			   (else
			    substituted-str)))
	   ;; detect delimiter with empty part such as "ja_", "_JP" or
	   ;; "ja."
	   (invalid-pair? (lambda (orig-str pair)
			    (case (length pair)
			      ((1) #f)
			      ((2)
			       (or (zero? (string-length (car pair)))
				   (zero? (string-length (cadr pair)))))
			      (else #t))))
	   (locale-split (lambda (locale delimiter)
			   (let* ((pair (string-split locale delimiter))
				  (invalid? (invalid-pair? locale pair))
				  (former (if (and (not invalid?)
						   (not (null? pair)))
					      (car pair)
					      ""))
				  (latter (if (and (not invalid?)
						   (not (null? pair))
						   (not (null? (cdr pair))))
					      (cadr pair)
					      "")))
			     (cons former latter))))
	   (lang-territory (car (locale-split canonical-str ".")))
	   (codeset (cdr (locale-split canonical-str ".")))
	   (lang (car (locale-split lang-territory "_")))
	   (territory (cdr (locale-split lang-territory "_")))
	   (localeobj (locale-new-internal)))
      ;; set attributes with validation
      (locale-set-lang! localeobj lang)
      (locale-set-territory! localeobj territory)
      (locale-set-codeset! localeobj codeset)
      ;; make whole part invalid if one of them is invalid
      (if (and (not (string=? (locale-lang localeobj)
			      ""))
	       (string=? (locale-territory localeobj)
			 territory)
	       (string=? (locale-codeset localeobj)
			 codeset))
	  localeobj
	  (locale-new-internal "")))))

(define locale-set-lang!
  (lambda (locale lang)
    (let ((validated-lang (or (and (string? lang)
				   (= (string-length lang)
				      2)
				   lang)
			      "")))
      (locale-set-lang-internal! locale validated-lang))))

(define locale-set-territory!
  (lambda (locale territory)
    (let ((validated-territory (or (and (string? territory)
					(= (string-length territory)
					   2)
					territory)
				   "")))
      (locale-set-territory-internal! locale validated-territory))))

(define locale-lang-territory-str
  (lambda (locale)
    (let ((lang (locale-lang locale))
	  (territory (locale-territory locale)))
      (if (and (= (string-length lang)
		  2)
	       (= (string-length territory)
		  2))
	  (string-append lang "_" territory)
	  lang))))

(define locale-str
  (lambda (locale)
    (let ((lang-territory (locale-lang-territory-str locale))
	  (codeset (locale-codeset locale)))
      (if (and (<= 2
		   (string-length lang-territory))
	       (< 0
		  (string-length codeset)))
	  (string-append lang-territory "." codeset)
	  lang-territory))))

(define locale-zh-awared-lang
  (lambda (locale)
    (if (string=? (locale-lang locale)
		  "zh")
	(locale-lang-territory-str locale)
	(locale-lang locale))))

;;
;; language handling
;;

;; requires 'N_' definition
(require "iso-639-1.scm")

;; This predicate supports following langgrp formats
;;
;; "ja"          matches with the language exactly
;; "en:fr:de"    matches with one of the colon-separated language
;; "zh_TW:zh_HK" matches with one of the colon-separated lang-territory string
;; "*"           matches with any languages
;; ""            matches with no languages
(define langgroup-covers?
  (lambda (langgrp lang)
    (cond
     ((or (not (string? langgrp))
	  (not (string? lang))
	  (string=? langgrp ""))
      #f)
     ((string=? langgrp "*")
      #t)
     (else
      (let ((langs (string-split langgrp ":")))
	(member lang langs))))))

(define lang-code->lang-name
  (lambda (langcode)
    (let* ((pair (assoc langcode iso-639-1-alist))
	   (langname (and pair
			  (cdr pair))))
      (or langname
	  "-"))))

(define lang-name->lang-code
  (lambda (langname)
    (or (find (lambda (pair)
                (and (string=? (cdr pair)
                               langname)
                     (car pair)))
              iso-639-1-alist)
        "-")))

;; returns "zh_TW" of "zh_TW:zh_HK"
(define langgroup-primary-lang-code
  (lambda (localestr)
    (let* ((primary-localestr (if (not (string=? localestr ""))
				  (car (string-split localestr ":"))
				  "invalid"))  ;; intentionally invalid
	   (locale (locale-new primary-localestr))
	   (langcode (locale-zh-awared-lang locale)))
      langcode)))
