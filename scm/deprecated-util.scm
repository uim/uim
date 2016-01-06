;;; util.scm: Deprecated utility functions for uim.
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

;; To find deprecated procedure invocation, type as follows (or type
;; it into M-x grep). But replacement of the deprecated procedures are
;; not necessary for uim 1.5. Keeping in mind avoiding the procedures
;; on writing a new code is enough.  -- YamaKen 2007-07-11
;;
;; $ egrep '\((string-list-concat|string-find|truncate-list|list-head|nconc|string-to-list|symbolconc|nth|nthcdr|copy-list|digit->string|puts|siod-print|print|feature\?|uim-symbol-value-str|define-record)\b' *.scm

(require-extension (srfi 1 34))


;; TODO: rewrite list processing with 'string-append'
(define string-list-concat
  (lambda (lst)
    (apply string-append (reverse lst))))

;; TODO: replace with 'member'
(define string-find
  (lambda (lst str)
    (member str lst)))

;; TODO: replace with 'take'
(define truncate-list
  (lambda (lst n)
    (guard (err
	    (else #f))
      (take lst n))))

;; TODO: replace with 'take'
(define list-head take)

(define nconc
  (lambda (lst obj)
    (if (null? lst)
	obj
	(begin
	  (set-cdr! (last-pair lst) obj)
	  lst))))

;; TODO: rewrite list processing with 'string->list'
;; split EUC-JP string into reversed character list
(define string-to-list
  (lambda (s)
    (with-char-codec "EUC-JP"
      (lambda ()
	(map! (lambda (c)
		(let ((str (list->string (list c))))
		  (with-char-codec "ISO-8859-1"
		    (lambda ()
		      (%%string-reconstruct! str)))))
	      (reverse! (string->list s)))))))

;; TODO: replace with symbol-append
;;
;; Since symbol-append is not yet defined at here, enclose into closure.
;;(define symbolconc symbol-append)
(define symbolconc
  (lambda args
    (apply symbol-append args)))

;; TODO: replace with list-ref
(define nth
  (lambda (k lst)
    (list-ref lst k)))

;; TODO: replace with list-tail
(define nthcdr
  (lambda (k lst)
    (guard (err
	    (else #f))
      (list-tail lst k))))

;; TODO: replace with list-copy of SRFI-1
(define copy-list
  (lambda (lst)
    (append lst '())))

;; TODO: replace with number->string
(define digit->string
  (lambda (n)
    (and (number? n)
         (number->string n))))

;; TODO: Replace with define-vector-record or define-list-record
;;
;; See test/test-util.scm to know what define-record does. fld-specs
;; requires list of list rather than alist to keep extensibility
;; (e.g. (list-ref spec 2) and so on may be used)
(define define-record
  (lambda (rec-name fld-specs)
    (eval `(define-list-record ,rec-name ',fld-specs)
	  (interaction-environment))
    (let ((constructor-name (make-record-constructor-name rec-name))
	  (legacy-constructor-name (symbol-append rec-name %HYPHEN-SYM 'new)))
      (eval `(define ,legacy-constructor-name ,constructor-name)
	    (interaction-environment)))))

;;
;; SIOD compatibility
;;

;; TODO: replace with 'display'
(define puts display)

;; TODO: replace with 'writeln'
(define siod-print
  (lambda (obj)
    (write obj)
    (newline)))

;; TODO: replace with 'writeln'
(define print siod-print)

;; TODO: replace with 'provided?'
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
