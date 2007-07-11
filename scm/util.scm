;;; util.scm: Utility functions for uim.
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

(use srfi-1)
(use srfi-6)
(use srfi-34)
(use srfi-60)

;;;;

(define hyphen-sym (string->symbol "-"))

;;
;; generic utilities
;;

;; Make escaped string literal to print a form.
;;
;; (string-escape "a str\n") -> "\"a str\\n\""
;;
;; The following two codes must display same result. See
;; test/test-util.scm for further specification.
;;
;; (display str)
;;
;; (use srfi-6)
;; (define estr (string-append "(display " (string-escape str) ")"))
;; (eval (read (open-input-string estr))
;;       (interaction-environment))
(define string-escape
  (lambda (s)
    (let ((p (open-output-string)))
      (write s p)
      (get-output-string p))))

;; Current uim implementation treats char as integer

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

;; should be obsoleted by 'take'
(define truncate-list
  (lambda (lst n)
    (guard (err
	    (else #f))
      (take lst n))))

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

;; should be obsoleted by 'take'
(define list-head take)

;; TODO: write test
(define sublist
  (lambda (lst start end)
    (list-tail (list-head lst (+ end 1))
	       start)))

;; TODO: write test
(define sublist-rel
  (lambda (lst start len)
    (sublist lst start (+ start len))))

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

;; symbol-append is a de facto standard procedure name
(define symbol-append
  (lambda args
    (string->symbol (string-append-map symbol->string args))))

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

(define method-delegator-new
  (lambda (dest-getter method)
    (lambda args
      (let* ((self (car args))
	     (dest (dest-getter self)))
	(apply method (cons dest (cdr args)))))))

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

(define nconc
  (lambda (lst obj)
    (if (null? lst)
	obj
	(begin
	  (set-cdr! (last-pair lst) obj)
	  lst))))

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
  (let ((vowel-chars (map char->integer
			  '(#\a #\i #\u #\e #\o))))
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

;;
;; backward compatibility: should be obsoleted
;;

(define control-char? char-control?)
(define alphabet-char? char-alphabetic?)
(define numeral-char? char-numeric?)
(define usual-char? char-graphic?)
(define to-lower-char char-downcase)

(define charcode->string
  (lambda (c)
    (list->string (list (integer->char (bitwise-and 255 c))))))

(define string->charcode
  (lambda (s)
    (let ((sl (with-char-codec "ISO-8859-1"
	        (lambda ()
		  (string->list s)))))
      (if (null? sl)
	  0
	  (char->integer (car sl))))))

(define symbolconc symbol-append)

;; should be obsoleted by list-ref
(define nth
  (lambda (k lst)
    (list-ref lst k)))

;; should be obsoleted by list-tail
(define nthcdr
  (lambda (k lst)
    (guard (err
	    (else #f))
      (list-tail lst k))))

;; should be obsoleted by list-copy of SRFI-1
(define copy-list
  (lambda (lst)
    (append lst '())))

(define digit->string
  (lambda (n)
    (and (number? n)
         (number->string n))))

;;
;; SIOD compatibility
;;
(define puts display)

;; TODO: Rename to more appropriate name such as 'inspect' (the name
;; came from debugging terms) or simply 'writeln'. But since I don't
;; know Scheme culture enough, I can't determine what is appropriate.
(define siod-print
  (lambda (obj)
    (write obj)
    (newline)))

(define print siod-print)

(define feature?
  (lambda (sym)
    (provided? (symbol->string sym))))


;;
;; uim-specific utilities
;;

(define do-nothing (lambda args #f))

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
    (guard (err
	    (else
	     #f))
      ;; to suppress error message, check file existence first
      (and (file-readable? (make-scm-pathname file))
	   (load file)))))

;; TODO: write test
;; returns succeeded or not
(define try-require
  (lambda (file)
    (guard (err
	    (else
	     #f))
      ;; to suppress error message, check file existence first
      (and (file-readable? (make-scm-pathname file))
	   (require file)))))

;; used for dynamic environment substitution of closure
(define %%enclose-another-env
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
    (for-each (lambda (spec index)
		(let* ((elem-sym (list-ref spec 0))
		       (default  (list-ref spec 1))
		       (getter-sym (symbolconc rec-sym hyphen-sym elem-sym))
		       (getter (lambda (rec)
				 (list-ref rec index)))
		       (setter-sym (symbolconc rec-sym hyphen-sym 'set- elem-sym '!))
		       (setter (lambda (rec val)
				 (set-car! (nthcdr index rec)
					   val))))
		  (eval (list 'define getter-sym getter)
			(interaction-environment))
		  (eval (list 'define setter-sym setter)
			(interaction-environment))))
	      rec-spec
	      (iota (length rec-spec)))
    (let ((creator-sym (symbolconc rec-sym hyphen-sym 'new))
	  (creator (let ((defaults (map cadr rec-spec)))
		     (lambda init-lst
		       (cond
			((null? init-lst)
			 (copy-list defaults))
			;; fast path
			((= (length init-lst)
			    (length defaults))
			 (copy-list init-lst))
			;; others
			((< (length init-lst)
			    (length defaults))
			 (let* ((rest-defaults (nthcdr (length init-lst)
						       defaults))
				(complemented-init-lst (append init-lst
							       rest-defaults)))
			   (copy-list complemented-init-lst)))
			(else
			 #f))))))
      (eval (list 'define creator-sym creator)
	    (interaction-environment)))))

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
    (reversed-preedit-background   . "cyan")
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

;; FIXME: write test.
(define ucs-to-utf8-string
  (lambda (ucs)
    (let ((utf-8
	   (if (< ucs 128)
	       (list ucs)		; ASCII
	       (let enc ((to-be-split ucs)
			 (threshold 64))
		 (if (< to-be-split threshold)
		     (list (bit-or to-be-split
				   (bit-xor 255 (- (* 2 threshold) 1))))
		     (cons (bit-or 128 (bit-and 63 to-be-split))
			   (enc (/ to-be-split 64) (/ threshold 2))))))))
      (string-append-map charcode->string (reverse utf-8)))))
