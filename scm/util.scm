;;; util.scm: Utility functions for uim.
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

;;
(define control-char?
  (lambda (c)
    (and (integer? c)
	 (or (< c 32)
	     (= c 127)))))
;;
(define alphabet-char?
  (lambda (c)
    (and (integer? c)
	 (or
	  (and (>= c 65) (<= c 90))
	  (and (>= c 97) (<= c 122))))))
;;
(define usual-char?
  (lambda (c)
    (and (integer? c)
	 (and (> c 32) (< c 127)))))
;;
(define numeral-char?
  (lambda (c)
    (and (integer? c)
	  (and (>= c 48)
	       (<= c 57)))))

;;
(define numeral-char->number
  (lambda (c)
    (if (numeral-char? c)
	(- c 48)
	c)))
;;
(define to-lower-char
  (lambda (c)
    (if (and (alphabet-char? c) (< c 91))
	(+ c 32)
	c)))
;;
(define string-list-concat
  (lambda (lst)
    (apply string-append (reverse lst))))

;;
(define string-find
  (lambda (lst str)
    (member str lst)))

;;
(define truncate-list
  (lambda (lst n)
    (if (or (< (length lst)
	       n)
	    (< n 0))
	#f
	(let self ((lst lst)
		   (n n))
	  (if (or (= n 0)
		  (null? lst))
	      ()
	      (cons (car lst)
		    (self (cdr lst) (- n 1))))))))

;; procedural 'or' for use with 'apply'
;; e.g. (apply proc-or boolean-lst)
;; should be deprecated and replaced with a proper, Schemer's way
(define proc-or
  (lambda xs
    (if (null? xs)
	#f
	(or (car xs)
	    (apply proc-or (cdr xs))))))

;; procedural 'and' for use with 'apply'
;; e.g. (apply proc-and boolean-lst)
;; should be deprecated and replaced with a proper, Schemer's way
(define proc-and
  (lambda xs
    (if (null? xs)
	#t
	(and (car xs)
	     (apply proc-and (cdr xs))))))

(define list-head
  (lambda (lst n)
    (if (= n 0)  ;; required due to bug #642
	()
	(or (truncate-list lst n)
	    (error "out of range in list-head")))))

;; local procedure. don't use in outside of util.scm
(define iterate-lists
  (lambda (mapper state lists)
    (let ((runs-out? (apply proc-or (mapcar null? lists))))
      (if runs-out?
	  (cdr (mapper state ()))
	  (let* ((elms (mapcar car lists))
		 (rests (mapcar cdr lists))
		 (pair (mapper state elms))
		 (terminate? (car pair))
		 (new-state (cdr pair)))
	    (if terminate?
		new-state
		(iterate-lists mapper new-state rests)))))))

;; not yet tested -- YamaKen 2004-10-30
(define alist-replace
  (lambda (kons alist)
    (let* ((id (car kons))
	   (preexisting (assoc id alist)))
      (if preexisting
	  (begin
	    (set-cdr! preexisting (cdr kons))
	    alist)
	  (cons kons alist)))))

;;
;; R5RS procedures (don't expect 100% compatibility)
;;

;; definition of 'else' has been moved into slib.c
;(define else #t)

(define boolean?
  (lambda (x)
    (or (eq? x #t)
        (eq? x #f))))

(define integer?
  (lambda (x)
    (number? x)))

(define list?
  (lambda (x)
    (or (null? x)
	(and (pair? x)
	     (list? (cdr x))))))

(define string->symbol intern)

(define map
  (lambda args
    (let ((f (car args))
	  (lists (cdr args)))
      (if (<= (length lists) 3)  ;; uim's siod accepts up to 3 lists
	  (apply mapcar args)    ;; faster native processing
	  (iterate-lists (lambda (state elms)
			   (if (null? elms)
			       (cons #t (reverse state))
			       (let ((mapped (apply f elms)))
				 (cons #f (cons mapped state)))))
			 () lists)))))

(define for-each map)

;;(define list-tail
;;  (lambda (lst n)
;;    (if (= n 0)
;;	lst
;;	(list-tail (cdr lst) (- n 1)))))
(define list-tail
  (lambda (lst n)
    (if (or (< (length lst)
	       n)
	    (< n 0))
	(error "out of range in list-tail")
	(nthcdr n lst))))

;;
;; SRFI procedures (don't expect 100% compatibility)
;;

;;(define take)
;;(define drop)
;;(define take-right)
;;(define drop-right)
;;(define split-at)
;;(define last)
;;(define unfold)

(define list-tabulate
  (lambda (n init-proc)
    (if (< n 0)
	(error "bad length for list-tabulate")
	(let self ((i (- n 1))
		   (res ()))
	  (if (< i 0)
	      res
	      (self (- i 1)
		    (cons (init-proc i) res)))))))

;; This procedure does not conform to the SRFI-1 specification. The
;; argument 'fill' is required.
(define make-list
  (lambda (n fill)
    (list-tabulate n
		   (lambda (i)
		     fill))))

;; This procedure does not conform to the SRFI-1 specification. The
;; optional arguments 'start' and 'step' is not supported.
(define iota
  (lambda (count)
    (list-tabulate count
		   (lambda (i)
		     i))))
    
(define zip
  (lambda lists
      (let ((runs-out? (apply proc-or (map null? lists))))
	(if runs-out?
	    ()
	    (let* ((elms (map car lists))
		   (rests (map cdr lists)))
	      (cons elms (apply zip rests)))))))

(define append-reverse
  (lambda (rev-head tail)
    (fold cons tail rev-head)))

(define find
  (lambda (f lst)
    (cond
     ((null? lst)
      #f)
     ((f (car lst))
      (car lst))
     (else
      (find f (cdr lst))))))

(define any
  (lambda args
    (let* ((pred (car args))
	   (lists (cdr args)))
      (iterate-lists (lambda (state elms)
		       (if (null? elms)
			   '(#t . #f)
			   (let ((res (apply pred elms)))
			     (cons res res))))
		     #f lists))))       

(define every
  (lambda args
    (let* ((pred (car args))
	   (lists (cdr args)))
      (iterate-lists (lambda (state elms)
		       (if (null? elms)
			   '(#t . #t)
			   (let ((res (apply pred elms)))
			     (cons (not res) res))))
		     #f lists))))	       

(define fold
  (lambda args
    (let* ((kons (car args))
	   (knil (cadr args))
	   (lists (cddr args)))
      (iterate-lists (lambda (state elms)
		       (if (null? elms)
			   (cons #t state)
			   (cons #f (apply kons (append elms (list state))))))
		     knil lists))))

(define filter
  (lambda args
    (let ((pred (car args))
	  (lst (cadr args)))
      (iterate-lists (lambda (state elms)
		       (if (null? elms)
			   (cons #t (reverse state))
			   (let ((elm (car elms)))
			     (cons #f (if (pred elm)
					  (cons elm state)
					  state)))))
		     () (list lst)))))

(define filter-map
  (lambda args
    (let ((f (car args))
	  (lists (cdr args)))
      (iterate-lists (lambda (state elms)
		       (if (null? elms)
			   (cons #t (reverse state))
			   (let ((mapped (apply f elms)))
			     (cons #f (if mapped
					  (cons mapped state)
					  state)))))
		     () lists))))

(define remove
  (lambda (pred lst)
    (filter (lambda (elm)
	      (not (pred elm)))
	    lst)))

(define alist-delete
  (lambda args
    (let ((key (car args))
	  (alist (cadr args))
	  (key=? (if (null? (cddr args))
		     =
		     (car (cddr args)))))
    (remove (lambda (elm)
	      (key=? (car elm)
		     key))
	    alist))))

;;
;; uim-specific utilities
;;

;; for eval
(define toplevel-env ())

;; used for dynamic environment substitution of closure
(define enclose-another-env
  (lambda (closure another-env)
    (let* ((code (%%closure-code closure))
	   (args (car code))
	   (body (cdr code))
	   (definition (list 'lambda args body)))
      (eval definition another-env))))

;; See test/test-util.scm to know what define-record does.
;; rec-spec requires list of list rather than alist to keep
;; extensibility (e.g. (nth 2 spec) and so on may be used)
(define define-record
  (lambda (rec-sym rec-spec)
    (let ((i 0))
      (for-each (lambda (spec)
		  (let* ((index i)
			 (elem-sym (nth 0 spec))
			 (default  (nth 1 spec))
			 (getter-sym (symbolconc rec-sym '- elem-sym))
			 (getter (lambda (rec)
				   (nth index rec)))
			 (setter-sym (symbolconc rec-sym '-set- elem-sym '!))
			 (setter (lambda (rec val)
				   (set-car!
				    (nthcdr index rec)
				    val))))
		    (eval (list 'define getter-sym getter)
			  toplevel-env)
		    (eval (list 'define setter-sym setter)
			  toplevel-env)
		    (set! i (+ i 1))))
		rec-spec))
    (let ((creator-sym (symbolconc rec-sym '-new))
	  (creator (lambda init-lst
		     (let ((defaults (map cadr rec-spec)))
		       (cond
			((null? init-lst)
			 (copy-list defaults))
			((<= (length init-lst)
			     (length defaults))
			 (let* ((rest-defaults (nthcdr (length init-lst)
						       defaults))
				(complemented-init-lst (append init-lst
							       rest-defaults)))
			   (copy-list complemented-init-lst)))
			(else
			 #f))))))
      (eval (list 'define creator-sym creator)
	    toplevel-env))))

;; for direct candidate selection
(define number->candidate-index
  (lambda (n)
    (cond
     ((= n 0)
      9)
     ((and (>= n 1)
	   (<= n 9))
      (- n 1))
     (else
      n))))

;; update style-element vars
;; style-spec requires list of (style-element-name . validator)
(define update-style
  (lambda (style-spec style)
    (let* ((elem (car style))
	   (name (car elem))
	   (val (if (symbol? (cdr elem))
		    (symbol-value (cdr elem))
		    (cdr elem)))
	   (spec (assq name style-spec))
	   (valid? (symbol-value (cdr spec))))
      (if (valid? val)
	  (set-symbol-value! name val))
      (if (not (null? (cdr style)))
	  (update-style style-spec (cdr style))))))

;;
;; Preedit color related configurations and functions.
;;
(define reversed-preedit-foreground #f)
(define reversed-preedit-background #f)
(define separator-foreground #f)
(define separator-background #f)
(define reversed-separator-foreground #f)
(define reversed-separator-background #f)

(define uim-color 'uim-color-uim)
(define uim-color-spec
  '((reversed-preedit-foreground   . string?)
    (reversed-preedit-background   . string?)
    (separator-foreground          . string?)
    (separator-background          . string?)
    (reversed-separator-foreground . string?)
    (reversed-separator-background . string?)))

;; predefined color styles
(define uim-color-uim
  '((reversed-preedit-foreground   . "white")
    (reversed-preedit-background   . "black")
    (separator-foreground          . "lightsteelblue")
    (separator-background          . "")
    (reversed-separator-foreground . "white")
    (reversed-separator-background . "black")))
(define uim-color-atok
  '((reversed-preedit-foreground   . "black")
    (reversed-preedit-background   . "blue")
    (separator-foreground          . "lightsteelblue")
    (separator-background          . "")
    (reversed-separator-foreground . "black")
    (reversed-separator-background . "blue")))

(define context-update-preedit
  (lambda (context segments)
    (im-clear-preedit context)
    (for-each (lambda (segment)
		(if segment
		    (let ((attr (car segment))
			  (str (cdr segment)))
		      (im-pushback-preedit context attr str))))
	      segments)
    (im-update-preedit context)))

;; functions for multiple segments input method. used to unify
;; common features of multi-segment IMs such as Canna and Anthy
(define multi-segment-type-hiragana 0)
(define multi-segment-type-katakana 1)
(define multi-segment-type-hankana 2)

(define multi-segment-make-index-list
  (lambda (n old-lst)
    (let ((old-lst-len (length old-lst)))
      (if (< n old-lst-len)
	  (truncate-list old-lst n)
	  (append old-lst
		  (make-list (- n
				old-lst-len)
			     0))))))

(define multi-segment-make-string
  (lambda (sl dir type)
    (let ((get-str-by-type
	   (lambda (l)
	     (cond
	      ((= type multi-segment-type-hiragana)
	       (caar l))
	      ((= type multi-segment-type-katakana)
	       (car (cdar l)))
	      ((= type multi-segment-type-hankana)
	       (cadr (cdar l)))))))
      (if (not (null? sl))
	  (if dir
	      (string-append (multi-segment-make-string (cdr sl) dir type)
			     (get-str-by-type sl))
	      (string-append (get-str-by-type sl)
			     (multi-segment-make-string (cdr sl) dir type)))
	  ""))))

(define multi-segment-make-left-string
  (lambda (sl kana)
    (multi-segment-make-string sl #t kana)))

(define multi-segment-make-right-string
  (lambda (sl kana)
    (multi-segment-make-string sl #f kana)))

(define multi-segment-opposite-kana
  (lambda (kana)
    (cond
     ((= kana multi-segment-type-hiragana)
      multi-segment-type-katakana)
     ((= kana multi-segment-type-katakana)
      multi-segment-type-hiragana)
     ((= kana multi-segment-type-hankana)
      multi-segment-type-hiragana))))


(define candidate-window-position "caret")
