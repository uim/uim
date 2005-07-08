;;; utext.scm: Flexible text representation
;;;
;;; Copyright (c) 2005 uim Project http://uim.freedesktop.org/
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

;; FIXME: write test

(require "util.scm")
(require "i18n.scm")


;; - The name 'utext' is inspired by Mtext of the m17n library
;; - 'uchar' represents a logical character. Logical character is
;;   arbitrary length string such as "a", "ae", "ｶﾞ" or "". All of
;;   these objects are treated as one character on editing
;; - To perform complex operation, use with ustr as (ustr-new utext)

;; utext       ::= () | (uchar . utext)
;;
;; uchar       ::= uchar-body | (uchar-body . utext-props)
;; uchar-body  ::= char | string
;; char        ::= #\a | #\あ | <any valid Scheme character>
;; string      ::= "" | "ae" | "abc" | "ガ" | "ｶﾞ" | "文字列"
;;                 | <any valid Scheme string>
;;
;; utext-props ::= (utext-prop) | (utext-prop . utext-props)
;; utext-prop  ::= (symbol . prop-value)
;; prop-value  ::= utext | <any valid Scheme object>


;;
;; uchar
;;

(define uchar-new
  (lambda (body props)
    (if (null? props)
	body
	(cons body props))))

(define uchar-copy
  (lambda (uchar)
    (if (pair? uchar)
	(cons (car uchar)
	      (cdr uchar))
	uchar)))

(define uchar-body
  (lambda (uchar)
    (if (pair? uchar)
	(car uchar)
	uchar)))

(define uchar-props
  (lambda (uchar)
    (if (pair? uchar)
	(cdr uchar)
	())))

(define uchar-prop
  (lambda (uchar prop-id)
    (assq prop-id (uchar-props uchar))))

(define uchar-add-prop
  (lambda (uchar prop)
    (uchar-new (uchar-body uchar)
	       (utext-props-add (uchar-props uchar) prop))))

(define uchar-add-props
  (lambda (uchar props)
    (uchar-new (uchar-body uchar)
	       (utext-props-merge (uchar-props uchar) props))))

(define uchar-body?
  (lambda (obj)
    (or (char? obj)
	(string? obj))))

(define uchar?
  (lambda (obj)
    (or (uchar-body? obj)
	(and (pair? obj)
	     (uchar-body? (car obj))
	     (utext-props? (cdr obj))))))

(define uchar=?
  (lambda (uchar other)
    (and (equal? (uchar-body uchar)
		 (uchar-body other))
	 (utext-props=? (uchar-props uchar)
			(uchar-props other)))))


;;
;; utext property
;;

(define utext-props-add
  (lambda (props new-prop)
    (alist-replace new-prop props)))
  
(define utext-props-merge
  (lambda (props new-props)
    (fold (lambda (p merged)
	    (utext-props-add merged p))
	  props
	  new-props)))

(define utext-prop?
  (lambda (obj)
    (symbol? obj)
    (and (pair? obj)
	 (symbol? (car obj)))))

(define utext-props? pair?)

;; FIXME: get lset=
;;(define utext-props=?
;;  (lambda (utext other)
;;    (lset= equal? utext other)))
(define utext-props=? equal?)


;;
;; utext
;;

(define utext-new
  (lambda (uchar-bodies props)
    (map (lambda (body)
	   (uchar-new body props))
	 uchar-bodies)))

(define utext-copy
  (lambda (utext)
    (map uchar-copy utext)))

(define utext?
  (lambda (utext)
    (every uchar? utext)))

(define utext-add-prop
  (lambda (utext prop)
    (map (lambda (uchar)
	   (uchar-add-prop uchar prop))
	 utext)))

(define utext-add-props
  (lambda (utext props)
    (map (lambda (uchar)
	   (uchar-add-props uchar props))
	 utext)))

;; logical length (!= physical character count)
(define utext-length length)

;; FIXME: replace nth with list-ref
(define utext-ref
  (lambda (utext idx)
    (nth idx utext)))

;; FIXME: nthcdr
(define utext-set!
  (lambda (utext idx uchar)
    (set-car! (list-tail utext idx)
	      uchar)))

(define utext=?
  (lambda (utext other)
    (every uchar=? utext other)))

;; .returns List of utext aggregated by uchar-props=?
(define utext-aggregate
  (lambda (utext)
    ))

;;(define utext-convert
;;  (lambda (utext new-locale)
;;    ))

(define eucjp-string->utext
  (lambda (str)
    (fold (lambda (c utext)
	    (cons (uchar-new c utext-props-eucjp-str)
		  utext))
	  ()
	  (string-to-list str))))

;; FIXME: support encoding conversion from other than EUC-JP
(define utext->eucjp-string
  (lambda (utext)
    (string-append-map uchar-body utext)))


;;
;; standard definitions
;;

(define utext-prop-default-locale (cons 'locale (locale-new #f)))
(define utext-prop-eucjp-locale (cons 'locale (locale-new "ja_JP.EUC-JP")))
(define utext-props-eucjp-str (list utext-prop-eucjp-locale))
(define uchar-std-cursor (cons "" (list utext-prop-default-locale
					'(cursor . #t))))
