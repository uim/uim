;;; uim-sh.scm: uim interactive shell for debugging, batch processing
;;;             and serving as generic inferior process
;;;
;;; Copyright (c) 2003,2004 uim Project http://uim.freedesktop.org/
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
;;; THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
;;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
;;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
;;; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;;; SUCH DAMAGE.
;;;;

(define uim-sh-prompt "uim> ")
(define uim-sh-opt-batch #f)
(define uim-sh-opt-help #f)

(define uim-sh-loop
  (lambda ()
    (if (not uim-sh-opt-batch)
	(puts uim-sh-prompt))
    (let* ((expr (read))
	   (eof (eq? (eof-val) expr)))
      (if (not eof)
	  (begin
	    (print (eval expr))
	    (uim-sh-loop))
	  #f))))

(define uim-sh-parse-args
  (lambda (args)
    (set! uim-sh-opt-batch (or (string-find args "-b")
			       (string-find args "--batch")))
    (set! uim-sh-opt-help (or (string-find args "-h")
			      (string-find args "--help")))))

(define uim-sh-usage
  (lambda ()
    (puts "Usage: uim-sh [options]
  -b        batch mode. suppress shell prompts
  -h        show this help
")))

(define uim-sh
  (lambda (args)
    (uim-sh-parse-args args)
    (if uim-sh-opt-help
	(uim-sh-usage)
	(if (*catch
	     'all
	     (uim-sh-loop))
	    (uim-sh args)))))
