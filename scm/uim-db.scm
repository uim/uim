;;; uim-db.scm: uim interactive debugger
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

(define uim-db-prompt "uim-db> ")
(define uim-db-break-at-molecule #f)

(define uim-db-current-file "")
(define uim-db-next-id 1)
(define uim-db-next-display-id 1)
(define uim-db-breakpoint-alist '())
(define uim-db-display '())

(define uim-db-print
  (lambda (x)
    (puts "\n>>> ")
    (print (dbg-get-info x))
    (print x)))

; can we set breakpoints within x?
; FIXME: is there a better-known term for this?
(define uim-db-molecular?
  (lambda (x)
    (or (not (pair? x))
	(memq (car x) '(quote +internal-backquote)))))

; partial implementation
(define pair-fold
  (lambda (f e lis)
    (if (null? lis)
	e
	(let ((nextls (cdr lis)))	; be set-cdr! safe
	  (pair-fold f (f lis e) nextls)))))

(define uim-db-for-each
  (lambda (f l)
    (pair-fold (lambda (lis e) (f (car lis))) () l)))

(define uim-db-walk-tree
  (lambda (f x)
    (if (not (uim-db-molecular? x))
	(begin
	  (f x)
	  (uim-db-walk-tree f (car x))
	  (uim-db-walk-tree f (cdr x))))))

(define uim-db-add-display!
  (lambda (expr)
    (set! uim-db-display
	  (cons (cons uim-db-next-display-id
		      expr)
		uim-db-display))
    (set! uim-db-next-display-id
	  (+ 1 uim-db-next-display-id))))

(define uim-db-del-display!
  (lambda (id)
    (set! uim-db-display
	  (uim-db-alist-delete! id
				uim-db-display
				=))))

(define uim-db-find
  (lambda (file line)
    (let* ((closer
	    (lambda (l cand)
	      (if (and (<= (dbg-get-line cand)
			   (dbg-get-line l))
		       (<= (dbg-get-line l)
			   line)
		       (< (dbg-get-line cand)
			  line))
		  l
		  cand)))
	   (descend (lambda (x) (pair-fold closer '() x)))
	   (mod (srfi-assoc (dbg-expand-file-name file)
			    dbg-closures
			    string=?))
	   (proc (and mod
		      (pair-fold (lambda (l x) (closer (car l) x))
				 '()
				 (cdr mod)))))
      (if proc
	  (let probe ((prev (cddr (%%closure-code proc)))
		      (code (descend (cddr (%%closure-code proc)))))
	    (if (uim-db-molecular? (car code))
		(if uim-db-break-at-molecule
		    code
		    prev)
		(probe code (descend (car code)))))
	  #f))))

(define uim-db-insert-code!
  (lambda (pos c)
    (if (pair? pos)
	(let ((code (list 'begin c (car pos))))
	  (dbg-copy-info! (cdr code) '()) ; invalidate
	  (dbg-copy-info! (cddr code) pos)
	  (set-car! pos code))
	(print "Invalid argument to uim-db-insert-code!"))))

(define uim-db-restore-code!
  (lambda (pos)
    (set-car! pos (cadr (cdar pos)))))

(define uim-db-set-break!
  (lambda criteria
    (cond
     ((number? (car criteria))
      (uim-db-set-break! uim-db-current-file (car criteria)))
     ((and (string? (car criteria))
	   (number? (cadr criteria)))
      (let ((pos (uim-db-find (car criteria) (cadr criteria))))
	(if (pair? pos)
	    (let ((code (car pos)))
	      (uim-db-insert-code! pos (list 'uim-db-break
					     '(the-environment)
					     uim-db-next-id))
	      (uim-db-puts "Breakpoint "
			   uim-db-next-id
			   " set at "
			   (dbg-get-file pos)
			   ":"
			   (dbg-get-line pos)
			   ", on expression\n")
	      (print code)
	      (set! uim-db-breakpoint-alist
		    (cons (list uim-db-next-id pos code)
			  uim-db-breakpoint-alist))
	      (set! uim-db-next-id (+ uim-db-next-id 1))
	      (set! uim-db-current-file (car criteria)))
	    (puts "Error: specified code not found\n"))))
     (else
      (puts "Usage: (uim-db-set-break! file-name line-number)")))))

(define uim-db-del-break!
  (lambda (id)
    (let ((bp (srfi-assoc id uim-db-breakpoint-alist =)))
      (if bp
	  (begin
	    (set! uim-db-breakpoint-alist
		  (uim-db-alist-delete! id
					uim-db-breakpoint-alist
					=))
	    (uim-db-restore-code! (cadr bp))
	    (uim-db-puts "Deleted breakpoint "
			 id
			 " at "
			 (dbg-get-file (cadr bp))
			 ":"
			 (dbg-get-line (cadr bp))
			 "\n"))
	  (puts "Invalid breakpoint ID.\n")))))

(define uim-db-break
  (lambda (env id)
    (let ((bp (srfi-assoc id uim-db-breakpoint-alist =)))
      (uim-db-puts "Breakpoint "
		   (car bp)
		   " hit at "
		   (dbg-get-file (cadr bp))
		   ":"
		   (dbg-get-line (cadr bp))
		   "\n"
		   "Type @help if you "
		   "don't know what to do.\n")
      (uim-db-for-each
       (lambda (l)
	 (uim-db-puts "Display " (car l) ": ")
	 (print (cdr l))
	 (puts " ==> ")
	 (*catch 'all (print (eval (cdr l) env))))
       uim-db-display)
      (puts uim-db-prompt)
      (let interact ((expr (read)))
	(if (and (not (eq? (eof-val) expr))
		 (not (memq expr '(@c @cont @continue))))
	    (begin
	      (case expr
		((@break @b)
		 (let ((arg (read)))
		   (if (string? arg)
		       (begin
			 (set! uim-db-current-file arg)
			 (set! arg (read))))
		   (uim-db-set-break! uim-db-current-file arg)))
		((@del @d)
		 (uim-db-del-break! (read)))
		((@expression @expr @exp @e)
		 (uim-db-puts "This breakpoint is set on the expression:\n"
			      (car (cddr bp))
			      "Breakpoint "
			      (car bp)
			      " at "
			      (dbg-get-file (cadr bp))
			      ":"
			      (dbg-get-line (cadr bp))
			      "\n"))
		((@help)
		 (uim-db-puts "Basically this is uim-sh in the environment "
			      "surrounding the breakpoint.  You can inspect "
			      "and/or mutate global and local variables as "
			      "you wish.\n"
			      "In addition to that, a few special commands "
			      "beginning with `@' are available:\n"
			      "@continue/@cont/@c\n"
			      "\tContinues execution of the program.\n"
			      "@break/@b [<f>] <l>\n"
			      "\tA shorthand for (uim-db-set-break! <f> "
			      "<l>).\n"
			      "@del/@d <n>\n"
			      "\tSame as (uim-db-del-break! <n>)\n"
			      "@expression/@expr/@exp/@e\n"
			      "\tPrints the next expression to be "
			      "evaluated.\n"
			      "@display/@disp/@di <expr>\n"
			      "\tEquivalent to (uim-db-add-display! <expr>)\n"
			      "@undisplay/@undisp/@u <i>\n"
			      "\tShorthand for (uim-db-del-display! <i>)\n"))
		((@display @disp @di)
		 (uim-db-add-display! (read)))
		((@undisplay @undisp @u)
		 (uim-db-del-display! (read)))
		(else
		 (*catch 'all
			 (print (eval expr env)))))
	      (puts uim-db-prompt)
	      (interact (read)))
            ; @continue @cont @c
	    (puts "Continuing execution.\n"))))))

; You MUST NOT set breakpoints in the following functions,
; as uim-db-break calls them.
(define uim-db-puts
  (lambda args
    (uim-db-for-each
     (lambda (x)
       (case (typeof x)
	 ((tc_string tc_symbol) (puts x))
	 ((tc_intnum) (puts (integer->string x)))
	 (else (print x))))
     args)))

(define srfi-assoc
  (lambda args
    (let loop ((key (car args))
	       (alist (cadr args))
	       (key=? (if (pair? (cddr args))
			  (car (cddr args))
			  equal?)))
      (cond ((null? alist)
	     #f)
	    ((key=? (caar alist) key)
	     (car alist))
	    (else
	     (loop key (cdr alist) key=?))))))

(define uim-db-alist-delete!
  (lambda args
    (let ((key (car args))
	  (pred (if (pair? (cddr args))
		    (car (cddr args))
		    equal?))
	  (result (cons 0 (cadr args))))
      (pair-fold (lambda (lis ans)
		   (if (pred key (caar lis))
		       (begin
			 (set-cdr! ans (cdr lis))
			 ans)
		       (cdr ans)))
		 result
		 (cadr args))
      (cdr result))))



(define uim-db-help
  (lambda ()
    (puts
"(uim-db-set-break! file line) or
(uim-db-set-break! line)
Sets a breakpoint at the innermost list containing code at file:line.  The code must be enclosed in a toplevel closure.  If file is omitted, it's substituted with the one from the previous call.

(uim-db-del-break! id)
Deletes a breakpoint.

(uim-db-add-display! expr)
expr is evaluated and displayed every time a breakpoint is hit.

(uim-db-del-display! id)
Deletes a display.

uim-db-break-at-molecule (variable)
Set breakpoints at the code at file:line rather than around it.\n")))

