;;; uim-db.scm: uim interactive debugger
;;;
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

;; This code is not ported to and does not work on SigScheme-based uim.

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

(define uim-db-every
  (lambda (f l)
    (pair-fold (lambda (lis e) (and e (f (car lis)))) #t l)))

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

(define uim-db-do-display
  (lambda dummy
    (uim-db-for-each
     (lambda (l)
       (uim-db-puts "Display " (car l) ": ")
       (print (cdr l))
       (puts " ==> ")
       (*catch 'all (print (eval (cdr l) env))))
     uim-db-display)
    #t))

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
	(puts "Invalid argument to uim-db-insert-code!\n"))))

(define uim-db-restore-code!
  (lambda (pos)
    (set-car! pos (cadr (cdar pos)))))

; breakpoint descriptor
(define-record
  'uim-db-breakpoint
  (list
   (list 'id #f)
   (list 'pos ())		 ; the position that uim-db-find found
   (list 'expr ())		  ; the expression that this is set on
   (list 'next-hook-id 1)
   (list 'hook-alist (list
		      (cons -1 uim-db-do-display)))))

(define uim-db-add-hook!
  (lambda (break-id f)
    (if (procedure? f)
	(let ((bp (srfi-assoc break-id uim-db-breakpoint-alist =)))
	  (uim-db-breakpoint-set-hook-alist!
	   bp
	   (cons (cons (uim-db-breakpoint-next-hook-id bp) f)
		 (uim-db-breakpoint-hook-alist bp)))
	  (uim-db-puts "Set hook "
		       (uim-db-breakpoint-next-hook-id bp)
		       " on breakpoint "
		       break-id
		       "\n")
	  (uim-db-breakpoint-set-next-hook-id!
	   bp
	   (+ 1 (uim-db-breakpoint-next-hook-id bp))))
	(puts "Invalid argument to uim-db-add-hook!\n"))))

(define uim-db-del-hook!
  (lambda (break-id hook-id)
    (let ((bp (srfi-assoc break-id uim-db-breakpoint-alist =)))
      (if bp
	  (if (srfi-assoc hook-id
			  (uim-db-breakpoint-hook-alist bp)
			  =)
	      (begin
		(uim-db-breakpoint-set-hook-alist!
		 bp
		 (uim-db-alist-delete! hook-id
				       (uim-db-breakpoint-hook-alist bp)
				       =))
		(uim-db-puts "Deleted hook "
			     hook-id
			     " of breakpoint "
			     break-id
			     "\n"))
	      (puts "Invalid hook ID.\n"))
	  (puts "Invalid breakpoint ID.\n")))))

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
					     '(%%current-environment)
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
		    (cons (uim-db-breakpoint-new uim-db-next-id pos code)
			  uim-db-breakpoint-alist))
	      (set! uim-db-next-id (+ uim-db-next-id 1))
	      (set! uim-db-current-file (car criteria)))
	    (puts "Error: specified code not found\n"))))
     (else
      (puts "Usage: (uim-db-set-break! file-name line-number)\n")))))

(define uim-db-del-break!
  (lambda (id)
    (let ((bp (srfi-assoc id uim-db-breakpoint-alist =)))
      (if bp
	  (begin
	    (set! uim-db-breakpoint-alist
		  (uim-db-alist-delete! id
					uim-db-breakpoint-alist
					=))
	    (uim-db-restore-code! (uim-db-breakpoint-pos bp))
	    (uim-db-puts "Deleted breakpoint "
			 id
			 " at "
			 (dbg-get-file (uim-db-breakpoint-pos bp))
			 ":"
			 (dbg-get-line (uim-db-breakpoint-pos bp))
			 "\n"))
	  (print "Invalid breakpoint ID.\n")))))

(define uim-db-break
  (lambda (env id)
    (let ((bp (srfi-assoc id uim-db-breakpoint-alist =)))
      (if (uim-db-every
	   (lambda (x) ((cdr x) env bp))
	   (uim-db-breakpoint-hook-alist bp))
	   (begin
	     (uim-db-puts "Breakpoint "
			  (uim-db-breakpoint-id bp)
			  " hit at "
			  (dbg-get-file (uim-db-breakpoint-pos bp))
			  ":"
			  (dbg-get-line (uim-db-breakpoint-pos bp))
			  "\n"
			  "Type (uim-db-help 'shell) if you don't "
			  "know what to do.\n")
	     (uim-db-shell env bp)
	     (puts "Continuing execution.\n"))))))

(define uim-db-shell
  (lambda args
    (puts uim-db-prompt)
    (let ((env (if (>= (length args) 1) (car args) ()))
	  (bp (if (>= (length args) 2) (cadr args) #f))
	  (expr (*catch 'all (read))))
	(if (or (eq? (eof-val) expr)
		(memq expr '(@c @cont @continue)))
	    #f
	    (begin
	      (*catch
	       'all
	       (case expr
		 ((@break @b)
		  (let ((arg (eval (read))))
		    (if (string? arg)
			(begin
			  (set! uim-db-current-file arg)
			  (set! arg (eval (read)))))
		    (uim-db-set-break! uim-db-current-file arg)))
		 ((@del @d)
		  (uim-db-del-break! (eval (read))))
		 ((@expression @expr @exp @e)
		  (if (null? bp)
		      (puts "You can't do that in a manually-invoked shell.\n")
		      (uim-db-puts
		       "This breakpoint is set on the expression:\n"
		       (uim-db-breakpoint-expr bp)
		       "Breakpoint "
		       (uim-db-breakpoint-id bp)
		       " at "
		       (dbg-get-file (uim-db-breakpoint-pos bp))
		       ":"
		       (dbg-get-line (uim-db-breakpoint-pos bp))
		       "\n")))
		 ((@display @disp @di)
		  (uim-db-add-display! (read))) ; don't eval
		 ((@undisplay @undisp @u)
		  (uim-db-del-display! (eval (read))))
		 ((@hook)
		  (if (null? bp)
		      (puts "You can't do that in a manually-invoked shell.\n")
		      (uim-db-add-hook! (uim-db-breakpoint-id bp)
					(eval (read)))))
		 ((@unhook @delhook)
		  (if (null? bp)
		      (puts "You can't do that in a manually-invoked shell.\n")
		      (uim-db-del-hook! (uim-db-breakpoint-id bp)
					(eval (read)))))
		 (else
		  (print (eval expr env)))))
	      (uim-db-shell env bp))))))

; You MUST NOT set breakpoints in the following functions,
; as uim-db-break calls them.
(define uim-db-puts
  (lambda args
    (uim-db-for-each
     (lambda (x)
       (case (typeof x)
	 ((tc_string tc_symbol) (puts x))
	 ((tc_intnum) (puts (number->string x)))
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
  (lambda args
    (let help ((topics args)
	       (database uim-db-help-database))
      (cond
       ((not database)
	(puts "Sorry, that topic isn't available."))
       ((null? topics)
	(apply uim-db-puts (cadr database))
	(if (pair? (cddr database))
	    (begin
	      (puts "\nSubtopics:\n")
	      (uim-db-for-each
	       (lambda (db)
		 (print (car db)))
	       (cddr database)))))
       (else
	(help (cdr topics) (srfi-assoc (car topics)
				       (cdr database))))))))

(define uim-db-help-database
  ; <database> --> (<entry>+)
  ; <entry> --> (<topic> (<string>+) <entry>*)
  '(()
    ("(uim-db-help <topic> [<subtopic> <subsubtopic>...])\n"
     "This is the manual for uim-db. <topic>s should be passed as symbols.\n")
    (breakpoint
     ("(uim-db-set-break! file line) or (uim-db-set-break! line) sets "
      "a breakpoint. (uim-db-del-break! id) deletes it.\n")
     (uim-db-set-break!
      ("(uim-db-set-break! file line) or \n"
       "(uim-db-set-break! line)\n"
       "Sets a breakpoint at the innermost list containing code at "
       "file:line.  The code must be enclosed in a toplevel closure.  "
       "If file is omitted, it's substituted with the one from the "
       "previous call.\n"
       "If uim-db-break-at-molecule is set to true (#f by default), "
       "The breakpoint is set *on* a code at file:line rather than "
       "around it.\n"))
     (uim-db-del-break!
      ("(uim-db-del-break! id)\n"
       "Deletes a breakpoint.  The ID should have been shown when "
       "you set it.\n"))
     (descriptor
      ("A breakpoint is described by a list whose field can be retreived "
       "with uim-db-breakpoint-<field> and mutated with "
       "uim-db-breakpoint-set-<field>! where <field> is any of:\n"
       "id\n"
       "\tThe breakpoint's ID.\n"
       "pos\n"
       "\tThe position at which the breakpoint is set. Pass it to "
       "dbg-get-info to obtain the file and line.\n"
       "expr\n"
       "\tThe expression on which the breakpoint is set.\n\n"
       "See also: hook\n")))
    (display
     ("Displays are expressions evaluated and printed every time "
      "execution stops at a breakpoint.  (uim-db-add-display! expr) "
      "adds one, (uim-db-del-display! id) deletes one.  "
      "(uim-db-do-display) does the actual displaying.\n"
      "See also: hook\n"))
    (hook
     ("Each breakpoint can have hook functions assigned.  "
      "If any of the hooks returns #f, "
      "the breakpoint is skipped.  ALL SUBSEQUENT HOOKS AND DISPLAYS "
      "ARE SKIPPED as well.  If you want the display, explicitly call "
      "uim-db-do-display before returning #f.\n"
      "Hooks are executed in LIFO order, so you can entirely disable a "
      "breakpoint by adding (lambda dummy #f) to its hooks.\n"
      "See also: display\n")
     (uim-db-add-hook!
      ("(uim-db-add-hook! id hook)\n"
       "Adds the hook to the breakpoint specified by id.\n"))
     (uim-db-del-hook!
      ("(uim-db-del-hook! breakpoint-id hook-id)\n"
       "Deletes a hook.  Hook IDs are local to each breakpoint.\n"))
     (hook-spec
      ("A hook is a function "
       "that looks like (lambda (env bp . extra)...).  Env will be "
       "bound to the "
       "environment that was active right before hitting the breakpoint, "
       "bp will be the breakpoint's descriptor, and extra is reserved for "
       "future use.  Return #f if "
       "you want the breakpoint to be skipped.  All subsequent hooks "
       "and displays are skipped, too, so be careful.\n")))
    (shell
     ("When a breakpoint is hit, the debugger shell, identified by the "
      "prompt uim-db>, will be invoked.  This is identical to uim-sh "
      "except that the environment in effect will be the one that was "
      "active on encountering the breakpoint.  Hence you can examine "
      "and/or manipulate local variables.\n"
      "In addition, a few special commands (mainly shorthands) "
      "beginning with `@' are supplied.  You can also invoke the shell "
      "manually.\n")
     (uim-db-shell
      ("(uim-db-shell) or\n"
       "(uim-db-shell env)\n"
       "Invokes the debugger shell.  The shell will be executed in the "
       "specified environment or toplevel if omitted."))
     (commands
      ("Note: the ones marked with *** aren't available when the debugger "
       "shell is invoked manually.\n"
       "@continue/@cont/@c\n"
       "\tExit the shell and continue execution of the program.\n"
       "@break/@b [<f>] <l>\n"
       "\tA shorthand for (uim-db-set-break! <f> "
       "<l>).\n"
       "@del/@d <n>\n"
       "\tSame as (uim-db-del-break! <n>)\n"
       "@expression/@expr/@exp/@e ***\n"
       "\tPrints the next expression to be "
       "evaluated.\n"
       "@display/@disp/@di <expr>\n"
       "\tEquivalent to (uim-db-add-display! '<expr>) (notice the quote).\n"
       "@help <topic>\n"
       "\tExecutes (uim-db-help).\n"
       "@undisplay/@undisp/@u <i>\n"
       "\tShorthand for (uim-db-del-display! <i>)\n"
       "@hook <proc> ***\n"
       "\tAdds <proc> to the hooks list of the current breakpoint.\n"
       "@unhook/@delhook <hook-id> ***\n"
       "\tRemoves a hook.\n")))))
