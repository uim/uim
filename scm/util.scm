;;; util.scm: Utility functions for uim.
;;;
;;; Copyright (c) 2003-2005 uim Project http://uim.freedesktop.org/
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

;; Current uim implementation treats char as integer

;; TODO: write test
(define string-escape
  (lambda (s)
    (let ((buf (string-append "\"\"" s s)))
      (print-to-string s buf))))

;; TODO: write test
(define string->char
  (lambda (str)
    (and (= (string-length str)
	    1)
	 (string->charcode str))))

;; TODO: write test
(define string->printable-char
  (lambda (str)
    (let ((c (string->char str)))
      (and (char-printable? c)
	   c))))

(define string->letter
  (lambda (str)
    (let ((c (string->printable-char str)))
      (and (char-alphabetic? c)
	   c))))

(define string-list-concat
  (lambda (lst)
    (apply string-append (reverse lst))))

(define string-find
  (lambda (lst str)
    (member str lst)))

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

(define alist-replace
  (lambda (kons alist)
    (let* ((id (car kons))
	   (preexisting (assoc id alist)))
      (if preexisting
	  (begin
	    (set-cdr! preexisting (cdr kons))
	    alist)
	  (cons kons alist)))))

(define join
  (lambda (sep list)
    (let ((len (length list)))
      (if (= len 0)
	  ()
	  (cdr (apply append (zip (make-list len sep)
				  list)))))))

(define string-join
  (lambda (sep str-list)
    (apply string-append (join sep str-list))))

(define string-append-map
  (lambda args
    (apply string-append (apply map args))))

;; only accepts single-arg functions
;; (define caddr (compose car cdr cdr))
(define compose
  (lambda args
    (let ((funcs (if (null? args)
		     (list (lambda (x) x))
		     args)))
      (fold (lambda (f g)
	      (lambda (arg)
		(f (g arg))))
	    (car (reverse funcs))
	    (cdr (reverse funcs))))))

;; TODO: write test
(define safe-car
  (lambda (pair)
    (and (pair? pair)
	 (car pair))))

;; TODO: write test
(define safe-cdr
  (lambda (pair)
    (and (pair? pair)
	 (cdr pair))))

;; TODO: write test
(define assq-cdr
  (lambda (key alist)
    (safe-cdr (assq key alist))))

(define clamp
  (lambda (x bottom ceiling)
    (max bottom
	 (min x ceiling))))

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

(define zero?
  (lambda (x)
    (if (integer? x)
	(= x 0)
	(error "non-numeric value for zero?"))))

(define positive?
  (lambda (x)
    (> x 0)))

(define negative?
  (lambda (x)
    (< x 0)))

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

(define quotient /)	;; / in siod is quotient actually

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
;; R5RS-like character procedures
;;

(define char-control?
  (lambda (c)
    (and (integer? c)
	 (or (<= c 31)
	     (= c 127)))))

(define char-upper-case?
  (lambda (c)
    (and (integer? c)
	 (>= c 65)
	 (<= c 90))))

(define char-lower-case?
  (lambda (c)
    (and (integer? c)
	 (>= c 97)
	 (<= c 122))))

(define char-alphabetic?
  (lambda (c)
    (or (char-upper-case? c)
	(char-lower-case? c))))

(define char-numeric?
  (lambda (c)
    (and (integer? c)
	 (>= c 48)
	 (<= c 57))))

(define char-printable?
  (lambda (c)
    (and (integer? c)
	 (<= c 127)
	 (not (char-control? c)))))

(define char-graphic?
  (lambda (c)
    (and (char-printable? c)
	 (not (= c 32)))))

;; TODO: write test
(define char-vowel?
  (let ((vowel-chars (map string->char
			  '("a" "i" "u" "e" "o"))))
    (lambda (c)
      (and (char-alphabetic? c)
	   (member (char-downcase c)
		   vowel-chars)))))

;; TODO: write test
(define char-consonant?
  (lambda (c)
    (and (char-alphabetic? c)
	 (not (char-vowel? c)))))

(define numeral-char->number
  (lambda (c)
    (if (char-numeric? c)
	(- c 48)
	c)))

(define char-downcase
  (lambda (c)
    (if (char-upper-case? c)
	(+ c 32)
	c)))

(define char-upcase
  (lambda (c)
    (if (char-lower-case? c)
	(- c 32)
	c)))

;; backward compatibility
(define control-char? char-control?)
(define alphabet-char? char-alphabetic?)
(define numeral-char? char-numeric?)
(define usual-char? char-graphic?)
(define to-lower-char char-downcase)

;;
;; SRFI procedures (don't expect 100% compatibility)
;;

;;(define take)
;;(define drop)
;;(define take-right)
;;(define drop-right)
;;(define split-at)

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
;; optional argument 'step' is not supported.
(define iota
  (lambda args
    (let ((count (car args))
	  (start (if (not (null? (cdr args)))
		     (cadr args)
		     0)))
      (list-tabulate (- count start)
		     (lambda (i)
		       (+ start i))))))

;; TODO: write test
(define last
  (lambda (lst)
    (car (last-pair lst))))

;; only accepts 2 lists
;; TODO: write test
(define append! nconc)
    
(define concatenate
  (lambda (lists)
    (apply append lists)))

(define concatenate!
  (lambda (lists)
    ;;(fold-right append! () lists)
    (fold append! () (reverse lists))))

(define zip
  (lambda lists
      (let ((runs-out? (apply proc-or (map null? lists))))
	(if runs-out?
	    ()
	    (let* ((elms (map car lists))
		   (rests (map cdr lists)))
	      (cons elms (apply zip rests)))))))

(define append-map
  (lambda args
    (concatenate! (apply map args))))

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

;; TODO: write test
;; replaced with faster C version
;;(define find-tail
;;  (lambda (pred lst)
;;    (cond
;;     ((null? lst)
;;      #f)
;;     ((pred (car lst))
;;      lst)
;;     (else
;;      (find-tail pred (cdr lst))))))

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

(define unfold
  (lambda args
    (let ((term? (nth 0 args))
	  (kar (nth 1 args))
	  (kdr (nth 2 args))
	  (seed (nth 3 args))
	  (tail-gen (if (= (length args)
			   5)
			(nth 4 args)
			(lambda (x) ()))))
      (if (term? seed)
	  (tail-gen seed)
	  (cons (kar seed)
		(unfold term? kar kdr (kdr seed) tail-gen))))))

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

;; TODO: write test
(define delete
  (lambda args
    (let ((x (car args))
	  (lst (cadr args))
	  (val=? (if (null? (cddr args))
		     =
		     (car (cddr args)))))
      (filter (lambda (elm)
		(not (val=? elm x)))
	      lst))))

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

;; SRFI-60 procedures
;; Siod's bit operation procedures take only two arguments
;; TODO: write tests
(define bitwise-not bit-not)

(define bitwise-and
  (lambda xs
    (fold bit-and (bitwise-not 0) xs)))

(define bitwise-or
  (lambda xs
    (fold bit-or 0 xs)))

(define bitwise-xor
  (lambda xs
    (fold bit-xor 0 xs)))

;;
;; uim-specific utilities
;;

;; TODO: write test
(define make-scm-pathname
  (lambda (file)
    (or (and (= (string->charcode file)
		(string->charcode "/"))
	     file)
	(string-append (load-path) "/" file))))

;; TODO: write test
;; returns succeeded or not
(define try-load
  (lambda (file)
    (and (file-readable? (make-scm-pathname file))
	 (not (*catch 'errobj (begin (load file)
				     #f))))))

;; TODO: write test
;; returns succeeded or not
(define try-require
  (lambda (file)
    (and (file-readable? (make-scm-pathname file))
	 (eq? (symbolconc '* (string->symbol file) '-loaded*)
	      (*catch 'errobj (require file))))))

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

;; for backward compatibility
(define uim-symbol-value-str
  (lambda (sym)
    (let ((val (if (symbol-bound? sym)
		   (symbol-value sym)
		   "")))
      (if (symbol? val)
	  (symbol->string val)
	  val))))

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
