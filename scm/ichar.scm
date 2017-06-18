;;; ichar.scm: Integer-based character processing (being obsoleted)
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

(require-extension (srfi 60))


;;
;; Converters
;;

;; TODO: write test
(define string->ichar
  (lambda (str)
    (and (= (string-length str)
	    1)
	 (string->charcode str))))

;; TODO: write test
(define string->printable-ichar
  (lambda (str)
    (let ((c (string->ichar str)))
      (and (ichar-printable? c)
	   c))))

(define string->alphabetic-ichar
  (lambda (str)
    (let ((c (string->printable-ichar str)))
      (and (ichar-alphabetic? c)
	   c))))

(define numeric-ichar->integer
  (lambda (c)
    (if (ichar-numeric? c)
	(- c 48)
	c)))

(define ucs->utf8-string
  (lambda (ucs)
    (with-char-codec "UTF-8"
      (lambda ()
	(let ((str (list->string (list (integer->char ucs)))))
	  (with-char-codec "ISO-8859-1"
	    (lambda ()
	      (%%string-reconstruct! str))))))))

;;
;; R5RS-like character procedures
;;

(define ichar-control?
  (lambda (c)
    (and (integer? c)
	 (or (<= c 31)
	     (= c 127)))))

(define ichar-upper-case?
  (lambda (c)
    (and (integer? c)
	 (>= c 65)
	 (<= c 90))))

(define ichar-lower-case?
  (lambda (c)
    (and (integer? c)
	 (>= c 97)
	 (<= c 122))))

(define ichar-alphabetic?
  (lambda (c)
    (or (ichar-upper-case? c)
	(ichar-lower-case? c))))

(define ichar-numeric?
  (lambda (c)
    (and (integer? c)
	 (>= c 48)
	 (<= c 57))))

(define ichar-printable?
  (lambda (c)
    (and (integer? c)
	 (<= c 127)
	 (not (ichar-control? c)))))

(define ichar-graphic?
  (lambda (c)
    (and (ichar-printable? c)
	 (not (= c 32)))))

;; TODO: write test
(define ichar-vowel?
  (let ((vowel-chars (map char->integer
			  '(#\a #\i #\u #\e #\o))))
    (lambda (c)
      (and (ichar-alphabetic? c)
	   (member (ichar-downcase c)
		   vowel-chars)))))

;; TODO: write test
(define ichar-consonant?
  (lambda (c)
    (and (ichar-alphabetic? c)
	 (not (ichar-vowel? c)))))

(define ichar-downcase
  (lambda (c)
    (if (ichar-upper-case? c)
	(+ c 32)
	c)))

(define ichar-upcase
  (lambda (c)
    (if (ichar-lower-case? c)
	(- c 32)
	c)))

;;
;; backward compatibility
;;

(define charcode->string
  (lambda (c)
    (if (and (integer? c)
	     (not (zero? c)))
	(list->string (list (integer->char (bitwise-and 255 c))))
	"")))

(define string->charcode
  (lambda (s)
    (let ((sl (with-char-codec "ISO-8859-1"
	        (lambda ()
		  (string->list s)))))
      (if (null? sl)
	  0
	  (char->integer (car sl))))))

;; FIXME: write test.
(define ucs-to-utf8-string ucs->utf8-string)

(define alist->icharlist
  (lambda (xs)
    (map
      (lambda (x)
        (cons (string->charcode (car x)) (string->charcode (cdr x))))
      xs)))
