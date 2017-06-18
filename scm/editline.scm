;;; editline.scm: libedit interface
;;;
;;; Copyright (c) 2007-2013 uim Project https://github.com/uim/uim
;;;
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;; 3. Neither the name of authors nor the names of its contributors
;;;    may be used to endorse or promote products derived from this software
;;;    without specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS
;;; IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
;;; THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;;; PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR
;;; CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;;; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(require-extension (srfi 0 6 23 34))
;;(require-extension (srfi 13))  ;; string-prefix?

(define *editline-prompt-beginning*  "> ")
(define *editline-prompt-succeeding* "")
(define %*editline-reading* #f)


(define editline-prompt
  (lambda ()
    (if %*editline-reading*
	*editline-prompt-succeeding*
	*editline-prompt-beginning*)))

(cond-expand
 (sigscheme
  (define %editline-eof-error?
    (lambda (err)
      (and (%%error-object? err)
	   (string-prefix? "in read: EOF " (cadr err)))))) ;; XXX
 (else
  (error "cannot detect EOF error")))

(define %editline-partial-read
  (lambda args
    (guard (err
	    ((%editline-eof-error? err) err))
      (apply read args))))

(define editline-read
  (let ((p (open-input-string ""))
	(buf ""))
    (lambda ()
      (let ((expr (%editline-partial-read p)))
	(if (or (eof-object? expr)
		(%editline-eof-error? expr))
	    (let ((line (editline-readline)))
	      (if (eof-object? line)
		  (if (%editline-eof-error? expr)
		      (raise expr)
		      line)
		  (begin
		    (set! buf (if (%editline-eof-error? expr)
				  (string-append buf line)
				  line))
		    (set! p (open-input-string buf))
		    (set! %*editline-reading* #t)
		    (editline-read))))
	    (begin
	      (set! buf "")
	      (set! %*editline-reading* #f)
	      expr))))))
