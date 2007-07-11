;;; ichar.scm: Integer-based character processing (being obsoleted)
;;;
;;; Copyright (c) 2003-2007 uim Project http://code.google.com/p/uim/
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

(use srfi-60)


;; TODO: write test
(define string->char
  (lambda (str)
    (and (= (string-length str)
	    1)
	 (string->charcode str))))

;; TODO: write test
(define string->printable-char
  (lambda (str)
    (let ((c (string->char str)))
      (and (char-printable? c)
	   c))))

(define string->letter
  (lambda (str)
    (let ((c (string->printable-char str)))
      (and (char-alphabetic? c)
	   c))))

;;
;; R5RS-like character procedures
;;

(define char-control?
  (lambda (c)
    (and (integer? c)
	 (or (<= c 31)
	     (= c 127)))))

(define char-upper-case?
  (lambda (c)
    (and (integer? c)
	 (>= c 65)
	 (<= c 90))))

(define char-lower-case?
  (lambda (c)
    (and (integer? c)
	 (>= c 97)
	 (<= c 122))))

(define char-alphabetic?
  (lambda (c)
    (or (char-upper-case? c)
	(char-lower-case? c))))

(define char-numeric?
  (lambda (c)
    (and (integer? c)
	 (>= c 48)
	 (<= c 57))))

(define char-printable?
  (lambda (c)
    (and (integer? c)
	 (<= c 127)
	 (not (char-control? c)))))

(define char-graphic?
  (lambda (c)
    (and (char-printable? c)
	 (not (= c 32)))))

;; TODO: write test
(define char-vowel?
  (let ((vowel-chars (map char->integer
			  '(#\a #\i #\u #\e #\o))))
    (lambda (c)
      (and (char-alphabetic? c)
	   (member (char-downcase c)
		   vowel-chars)))))

;; TODO: write test
(define char-consonant?
  (lambda (c)
    (and (char-alphabetic? c)
	 (not (char-vowel? c)))))

(define numeral-char->number
  (lambda (c)
    (if (char-numeric? c)
	(- c 48)
	c)))

(define char-downcase
  (lambda (c)
    (if (char-upper-case? c)
	(+ c 32)
	c)))

(define char-upcase
  (lambda (c)
    (if (char-lower-case? c)
	(- c 32)
	c)))

;;
;; backward compatibility: should be obsoleted
;;

(define control-char? char-control?)
(define alphabet-char? char-alphabetic?)
(define numeral-char? char-numeric?)
(define usual-char? char-graphic?)
(define to-lower-char char-downcase)

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

(define ucs->utf8-string
  (lambda (ucs)
    (with-char-codec "UTF-8"
      (lambda ()
	(list->string (list (integer->char ucs)))))))

;; FIXME: write test.
(define ucs-to-utf8-string ucs->utf8-string)
