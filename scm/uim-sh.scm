;;; uim-sh.scm: uim interactive shell for debugging, batch processing
;;;             and serving as generic inferior process
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
;;;;

(require-extension (srfi 1 2 6 23 34))

(define uim-sh-prompt "uim> ")
(define uim-sh-opt-expression #f)
(define uim-sh-opt-arg-expression
  "(error \"no <expr> is passed as argument\")")

(define uim-sh-option-table
  `((("-b" "--batch")          . batch)
    (("-B" "--strict-batch")   . strict-batch)
    (("-r" "--require-module") . ,(lambda (args)
				    (and-let* ((name (safe-car args))
					       ((require-module name)))
				      (safe-cdr args))))
    (("--editline")            . ,(lambda (args)
				    (require-module "editline")
				    args))
    (("-e" "--expression")     . ,(lambda (args)
				    (set! uim-sh-opt-expression #t)
				    (and-let* ((expr (safe-car args)))
				      (set! uim-sh-opt-arg-expression expr)
				      (safe-cdr args))))
    (("-V" "--version")        . version)
    (("-h" "--help")           . help)))

(define uim-sh-usage
  (lambda ()
    (display "Usage: uim-sh [options] [file [arg ...]]
  -b
  --batch                 batch mode. suppress shell prompts
  -B
  --strict-batch          strict batch mode, implies -b. suppress shell prompts
                          and evaluated results
  -r <name>
  --require-module <name> require module
  --editline              require editline module for Emacs-like line editing
  -e <expr>
  --expression <expr>     evaluate <expr> (after loading the file, and disables
                          'main' procedure of it)
  -V
  --version               show software version
  -h
  --help                  show this help
  file                    absolute path or relative to system scm directory
  arg ...                 string argument(s) for 'main' procedure of the file
")))

(define uim-sh-display-version
  (lambda ()
    (display "uim-sh ")
    (display (uim-version))
    (newline)))

(define uim-sh-define-opt-vars
  (lambda ()
    (for-each (lambda (name)
		(eval `(define ,(symbol-append 'uim-sh-opt- name) #f)
		      (interaction-environment)))
	      (filter symbol? (map cdr uim-sh-option-table)))))

(define uim-sh-parse-args
  (lambda (args)
    (uim-sh-define-opt-vars)
    (let rec ((args args))
      (or (and-let* (((pair? args))
		     (opt (car args))
		     (rest (cdr args))
		     (ent (assoc opt uim-sh-option-table member))
		     (action (cdr ent)))
	    (cond
	     ((symbol? action)
	      (set-symbol-value! (symbol-append 'uim-sh-opt- action) #t))
	     ((procedure? action)
	      (set! rest (action rest)))
	     (else
	      (error "invalid action on option table")))
	    (rec rest))
	  args
	  '()))))  ;; an action possibly returns #f

(define uim-sh-loop
  (lambda (my-read)
    (if (and (not uim-sh-opt-batch)
	     (not uim-sh-opt-strict-batch)
	     (not (provided? "editline")))
	(display uim-sh-prompt))
    ;; Non-recoverable read error is turned into fatal errorr such as
    ;; non-ASCII char in token on a non-Unicode port.
    (let ((expr (guard (read-err
			(else (%%fatal-error read-err)))
		  (my-read))))
      (and (not (eof-object? expr))
	   (let ((res (eval expr (interaction-environment))))
	     (if (not uim-sh-opt-strict-batch)
		 (writeln res))
	     (uim-sh-loop my-read))))))

;; Loop even if error has been occurred. This is required to run
;; GaUnit-based unit test for uim.
(define uim-sh
  (lambda (args)
    (let* ((file.args (uim-sh-parse-args (cdr args))) ;; drop the command name
	   (script (safe-car file.args))
	   (my-read (if (provided? "editline")
			uim-editline-read
			read))
	   (EX_OK       0)
	   (EX_SOFTWARE 70))
      (if script
	  (require script))
      (cond
       (uim-sh-opt-help
	(uim-sh-usage)
	EX_OK)

       (uim-sh-opt-version
	(uim-sh-display-version)
	EX_OK)

       (uim-sh-opt-expression
	(let* ((expr (read (open-input-string uim-sh-opt-arg-expression)))
	       (result (eval expr (interaction-environment))))
	  (write result)
	  (newline)
	  EX_OK))

       ((symbol-bound? 'main)
	(let ((status (main file.args)))
	  (if (integer? status)
	      status
	      EX_SOFTWARE)))

       (else
	(let reloop ()
	  (and (guard (err (else
			    (%%inspect-error err)
			    #t))
		 (uim-sh-loop my-read))
	       (reloop)))
	EX_OK)))))

(define uim-editline-read
  (let ((p (open-input-string "")))
    (lambda ()
      (let ((expr (read p)))
	(if (eof-object? expr)
	    (let ((line (uim-editline-readline)))
	      (if (eof-object? line)
		  line
		  (begin
		    (set! p (open-input-string line))
		    (uim-editline-read))))
	    expr)))))

;; Verbose level must be greater than or equal to 1 to print anything.
(if (< (verbose) 1)
    (verbose 1))
