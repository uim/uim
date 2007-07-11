;;; util.scm: Deprecated utility functions for uim.
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

(use srfi-1)
(use srfi-34)


(define string-list-concat
  (lambda (lst)
    (apply string-append (reverse lst))))

(define string-find
  (lambda (lst str)
    (member str lst)))

;; should be obsoleted by 'take'
(define truncate-list
  (lambda (lst n)
    (guard (err
	    (else #f))
      (take lst n))))

;; should be obsoleted by 'take'
(define list-head take)

(define nconc
  (lambda (lst obj)
    (if (null? lst)
	obj
	(begin
	  (set-cdr! (last-pair lst) obj)
	  lst))))

;; split EUC-JP string into reversed character list
(define string-to-list
  (lambda (s)
    (with-char-codec "EUC-JP"
      (lambda ()
	(map! (lambda (c)
		(list->string (list c)))
	      (reverse! (string->list s)))))))

;; symbol-append is not yet defined at here.
;;(define symbolconc symbol-append)
(define symbolconc
  (lambda args
    (apply symbol-append args)))

;; should be obsoleted by list-ref
(define nth
  (lambda (k lst)
    (list-ref lst k)))

;; should be obsoleted by list-tail
(define nthcdr
  (lambda (k lst)
    (guard (err
	    (else #f))
      (list-tail lst k))))

;; should be obsoleted by list-copy of SRFI-1
(define copy-list
  (lambda (lst)
    (append lst '())))

(define digit->string
  (lambda (n)
    (and (number? n)
         (number->string n))))

;;
;; SIOD compatibility
;;
(define puts display)

;; TODO: Rename to more appropriate name such as 'inspect' (the name
;; came from debugging terms) or simply 'writeln'. But since I don't
;; know Scheme culture enough, I can't determine what is appropriate.
(define siod-print
  (lambda (obj)
    (write obj)
    (newline)))

(define print siod-print)

(define feature?
  (lambda (sym)
    (provided? (symbol->string sym))))

;; for backward compatibility
(define uim-symbol-value-str
  (lambda (sym)
    (let ((val (if (symbol-bound? sym)
		   (symbol-value sym)
		   "")))
      (if (symbol? val)
	  (symbol->string val)
	  val))))
