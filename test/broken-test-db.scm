#! /usr/bin/env gosh

;;; Copyright (c) 2005-2013 uim Project https://github.com/uim/uim
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

;; uim-db.scm is not ported to and does not work on SigScheme-based uim.


; Tests for uim-db requires debugging information, so we have to let
; libuim load this file and give it a toplevel procedure.

(define test-db-find
  (lambda ()
    (if (feature? 'debug)
	(begin
	  (let ((check
		 (lambda (code)
		   (eq? (cdr code)
			(uim-db-find
			 (dbg-get-file code)
			 (+ 1 (dbg-get-line code)))))))
	    (let* ((q quote))
	      (let name ((code (q (place-holder
				   (target)))))
		(uim-db-set-break! (dbg-get-file check)
				   (dbg-get-line check))
		(check code)))))
	#t)))

; Certain functions in uim-db.scm are not allowed to call scheme
; functions in other files.  Otherwise setting a breakpoint to the
; function being used may cause an infinite recursion.
(define test-db-dep
  (lambda ()
    (letrec ((exclude
	      '(dbg-closures)) ; don't follow these symbols
	     (dependent?
	      (lambda (datum)
		(case (typeof datum)
		  ((tc_closure)
		   (or (if (or (null? (dbg-get-info datum))
			       (string=? (dbg-get-file datum)
					 (dbg-expand-file-name "uim-db.scm")))
			   #f
			    ; gosh doesn't recognize "#<CLOSURE arg...>"
			   (%%closure-code datum))
		       (dependent? (cddr (%%closure-code datum)))))
		  ((tc_symbol)
		   (and (symbol-bound? datum)
			(not (memq datum exclude))
			(begin
			  (set! exclude (cons datum exclude))
			  (dependent? (eval datum)))))
		  ((tc_cons)
		   (or (dependent? (car datum))
		       (dependent? (cdr datum))))
		  (else #f)))))
      (if (feature? 'debug)
	  (any dependent?
	       (cdr (srfi-assoc (dbg-expand-file-name "uim-db.scm")
				dbg-closures
				string=?)))
	  #f))))

; shadow this part from libuim
(if (and (not (symbol-bound? 'uim-sh))
         #f)  ;; disable this test until uim-db.scm is ported to SigScheme
    (begin
      (use test.unit)

      (require "test/uim-test-utils")

      (define-uim-test-case "testcase debugger"
	(setup
	 (lambda ()
	   (uim '(begin (load "../test/test-db.scm")
			(load "uim-db.scm")))))
	("test uim-db-find"
	 (assert-true (uim-bool '(test-db-find))))
	("test for external dependency"
	 (assert-false (uim-bool '(test-db-dep)))))))
