;;; uim-sh.scm: uim interactive shell for debugging, batch processing
;;;             and serving as generic inferior process
;;;
;;; Copyright (c) 2003-2006 uim Project http://uim.freedesktop.org/
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

(define uim-sh-prompt "uim> ")
(define uim-sh-opt-batch #f)
(define uim-sh-opt-strict-batch #f)
(define uim-sh-opt-help #f)

(define uim-editline-enabled #f)

(define uim-sh-loop
  (lambda ()
    (if (not uim-sh-opt-batch)
	(puts uim-sh-prompt))
    (let* ((expr (read))
	   (eof (eq? (eof-val) expr)))
      (if (not eof)
	  (begin
	    ((if  uim-sh-opt-strict-batch
		  (lambda () #f)
		  print)
	     (eval expr (interaction-environment)))
	    (uim-sh-loop))
	  #f))))

(define uim-sh-parse-args
  (lambda (args)
    (let ((batch? (or (member "-b" args)
		      (member "--batch" args)))
	  (strict-batch? (or (member "-B" args)
			     (member "--strict-batch" args))))
    (set! uim-sh-opt-batch (or batch?
			       strict-batch?))
    (set! uim-sh-opt-strict-batch strict-batch?)
    (set! uim-sh-opt-help (or (member "-h" args)
			      (member "--help" args)))
    (if (symbol-bound? 'uim-editline-readline)
	(set! uim-editline-enabled (or (and (member "-r" args)
					    (member "editline" args))
				       (member "--editline" args)))))))

(define uim-sh-usage
  (lambda ()
    (puts "Usage: uim-sh [options]
  -b        batch mode. suppress shell prompts
  -B        strict batch mode, implies -b. suppress shell prompts and
            evaluated results\n")
    (if (symbol-bound? 'uim-editline-readline)
	(puts "  -r [module] Load and import module.\n"))
    (puts "  -h        show this help\n")))

(define uim-sh
  (lambda (args)
    (uim-sh-parse-args args)
    (if uim-sh-opt-help
	(uim-sh-usage)
	(begin
	  (if (and uim-editline-enabled
		   (symbol-bound? 'uim-editline-readline))
	      (activate-editline))
	  (if (*catch
	       'all
	       (uim-sh-loop))
	      (uim-sh args))))))

(if (symbol-bound? 'uim-editline-readline)
    (begin
      (define uim-sh-loop-orig ())
      (define activate-editline
	(lambda ()
	  (set! uim-sh-loop-orig uim-sh-loop)
	  (set! uim-sh-loop
		(lambda ()
		  (if uim-sh-opt-batch
		      (uim-sh-loop-orig)
		      (let* ((line (uim-editline-readline))
			     (expr (read-from-string line))
			     (eof (eq? (eof-val) expr)))
			(if (not eof)
			    (begin
			      ((if uim-sh-opt-strict-batch
				   (lambda () #f)
				   print)
			       (eval expr (interaction-environment)))
			      (uim-sh-loop))
			    #f)))))
	#t))))
