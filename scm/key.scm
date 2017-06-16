;;;
;;; Copyright (c) 2003-2013 uim Project https://github.com/uim/uim
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

(require "util.scm")

;; config
(define enable-emacs-like-key-prefix? #t)

;; valid-key-symbols is defined in uim-key.c

(define key-symbol?
  (lambda (sym)
    (member sym valid-key-symbols)))

(define intern-key-symbol
  (lambda (key-str)
    (let ((sym (string->symbol key-str)))
      (if (memq sym valid-key-symbols)
	  sym
	  #f))))

;
(define shift-key-mask
  (lambda (state)
    (= (bitwise-and state 1) 1)))
(define control-key-mask
  (lambda (state)
    (= (bitwise-and state 2) 2)))
(define alt-key-mask
  (lambda (state)
    (= (bitwise-and state 4) 4)))
(define meta-key-mask
  (lambda (state)
    (= (bitwise-and state 8) 8)))
(define super-key-mask
  (lambda (state)
    (= (bitwise-and state 64) 64)))
(define hyper-key-mask
  (lambda (state)
    (= (bitwise-and state 128) 128)))

(define modifier-key-mask
  (lambda (state)
    (> state 0)))

(define modifier-key?
  (lambda (key key-state)
    (or
     (eq? key 'Shift_key)
     (eq? key 'Control_key)
     (eq? key 'Alt_key)
     (eq? key 'Meta_key)
     (eq? key 'Super_key)
     (eq? key 'Hyper_key))))

(define key-state-alist
  '((Shift_key   . 1)
    (Control_key . 2)
    (Alt_key     . 4)
    (Meta_key    . 8)
    (Super_key   . 64)
    (Hyper_key   . 128)))

(define emacs-like-prefix-alist
  '(("S" . Shift_key)
    ("C" . Control_key)
    ("A" . Alt_key)
    ("M" . Meta_key)
    ("Z" . Super_key)
    ("H" . Hyper_key)
    ("I" . IgnoreCase)
    ("J" . IgnoreShift)
    ("K" . IgnoreRegularShift)))

(define tag-prefix-alist
  '(("Shift"              . Shift_key)
    ("Control"            . Control_key)
    ("Alt"                . Alt_key)
    ("Meta"               . Meta_key)
    ("Super"              . Super_key)
    ("Hyper"              . Hyper_key)
    ("IgnoreCase"         . IgnoreCase)
    ("IgnoreShift"        . IgnoreShift)
    ("IgnoreRegularShift" . IgnoreRegularShift)))

(define translator-prefix?
  (lambda (symbol)
    (or (eq? symbol 'IgnoreCase)
	(eq? symbol 'IgnoreShift)
	(eq? symbol 'IgnoreRegularShift))))

;;
(define intern-key-prefix
  (lambda (symbol-str alist)
    (let ((pair (assoc symbol-str alist)))
      (and pair
	   (cdr pair)))))

(define parse-tag-prefix-symbol
  (lambda (parsed char-list)
    (let ((prefix (if (string=? parsed "")
		      #f
		      (intern-key-prefix parsed tag-prefix-alist))))
      (if (not (null? char-list))
	  (let* ((head (car char-list))
		 (head-char (string->charcode head))
		 (rest (cdr char-list)))
	    (if (or (ichar-alphabetic? head-char)
		    (ichar-numeric? head-char)
		    (string=? head "_"))
		(parse-tag-prefix-symbol (string-append parsed head) rest)
		(cons prefix char-list)))
	  (cons prefix ())))))

(define parse-tag-prefix
  (lambda (str)
    (if (not (string=? str ""))
	(let* ((char-list (reverse (string-to-list str)))
	       (head (car char-list)))
	  (if (string=? head "<")
	      (let* ((parsed (parse-tag-prefix-symbol "" (cdr char-list)))
		     (prefix (car parsed))
		     (rest (cdr parsed)))
		(if (and (not (null? rest))
			 (string=? (car rest) ">"))
		    (cons prefix
			  (if (null? (cdr rest))
			      ""
			      (apply string-append (cdr rest))))
		    (cons #f str)))
	      (cons #f str)))
	(cons #f str))))

(define parse-emacs-like-prefix
  (lambda (str)
    (let* ((char-list (reverse (string-to-list str)))
	   (prefix-str (and (<= 2 (length char-list))
			(string=? (nth 1 char-list) "-")
			(car char-list)))
	   (prefix (intern-key-prefix prefix-str emacs-like-prefix-alist))
	   (rest (if prefix
		     (apply string-append (cddr char-list))
		     str)))
      (cons prefix rest))))

(define parse-key-prefix
  (lambda (str)
    (let* ((parsed-as-emacs (parse-emacs-like-prefix str))
	   (emacs-prefix (car parsed-as-emacs)))
      (or (and enable-emacs-like-key-prefix?
	       emacs-prefix
	       parsed-as-emacs)
	  (parse-tag-prefix str)))))

(define parse-key-str
  (lambda (str translators key key-state)
    (let ((str-len (string-length str)))
      (cond
       ((= str-len 0)
	(list "" translators key key-state))
       ((= str-len 1)
	(list "" translators (string->charcode str) key-state))
       ((<= 2 str-len)
	(let* ((parsed (parse-key-prefix str))
	       (prefix (car parsed))
	       (rest (cdr parsed)))
	  (cond
	   ((modifier-key? prefix 0)
	    (let ((key-state (bitwise-ior key-state
					  (cdr (assq prefix key-state-alist)))))
	      (parse-key-str rest translators key key-state)))
	   ((translator-prefix? prefix)
	    (let* ((translator
		    (cond
		     ((eq? prefix 'IgnoreCase)
		      (lambda (key key-state)
			(let ((translated-key (ichar-downcase key)))
			  (list translated-key key-state))))
		     ((eq? prefix 'IgnoreShift)
		      (lambda (key key-state)
			(let ((translated-key-state
			       (bitwise-and key-state
					    (bitwise-not 1))))
			  (list key translated-key-state))))
		     ((eq? prefix 'IgnoreRegularShift)
		      (lambda (key key-state)
			(let ((translated-key-state
			       (if (ichar-graphic? key)
				   (bitwise-and key-state
						(bitwise-not 1))
				   key-state)))
			  (list key translated-key-state))))))
		   (translators (cons translator
				      translators)))
	      (parse-key-str rest translators key key-state)))
	   (else
	    (let* ((key-symbol (intern-key-symbol str))
		   (key (or key-symbol
			    key))
		   (rest (if key-symbol
			     ""
			     rest)))
	      (list rest translators key key-state))))))))))

(define apply-translators
  (lambda (translators key key-state)
    (if (null? translators)
	(list translators key key-state)
	(let* ((translator (car translators))
	       (rest-translators (cdr translators))
	       (translated (translator key key-state))
	       (translated-key (car translated))
	       (translated-state (cadr translated)))
	  (apply-translators
	   rest-translators
	   translated-key
	   translated-state)))))

;; Generates key predicate
;; (make-single-key-predicate "<Control>j")
(define make-single-key-predicate
  (lambda (source)
    (cond
     ((string? source)
      (let* ((key-str source)
	     (parsed (parse-key-str key-str () -1 0))
	     (translated (apply apply-translators (cdr parsed)))
	     (translators  (nth 1 parsed))
	     (target-key   (nth 1 translated))
	     (target-state (nth 2 translated)))
	(lambda (key key-state)
	  (let* ((translated (apply-translators translators key key-state))
		 (key       (nth 1 translated))
		 (key-state (nth 2 translated)))
	    (and (eqv? key target-key)
		 (eqv? key-state target-state))))))
     ((symbol? source)
      (let ((predicate-sym source))
	(lambda (key key-state)
	  ((symbol-value predicate-sym) key key-state))))
     (else
      (let ((maybe-predicate source))
	maybe-predicate)))))

;; Generates or'ed key predicate
;; (make-key-predicate '("<Control>j" "<Alt>k" "<Control>L"))
(define make-key-predicate
  (lambda (sources)
    (cond
     ((list? sources)
      (let ((predicates (map make-single-key-predicate sources)))
	(lambda (key key-state)
	  (apply proc-or
		 (map (lambda (predicate)
			(apply predicate (list key key-state)))
		      predicates)))))
     (else
      (let ((source sources))
	(make-single-key-predicate source))))))

(define modify-key-strs-implicitly
  (lambda (key-strs)
    (cond
     ((list? key-strs)
      (map modify-key-strs-implicitly key-strs))
     ((string? key-strs)
      (let* ((key-str key-strs)
	     (modified-key-str (string-append
				"<IgnoreRegularShift>"
				key-str)))
	modified-key-str))
     (else
      (let ((maybe-predicate key-strs))
	maybe-predicate)))))

;; Generates or'ed key predicate and bind it into
;; toplevel-environment. Use define-key rather than calling this
;; directly.
;; (define-key-internal 'foo-key? '("<Control>j" "<Alt>k" "<Control>L"))
(define define-key-internal
  (lambda (key-predicate-sym key-strs)
    (let* ((modified-key-strs (modify-key-strs-implicitly key-strs))
	   (predicate (make-key-predicate modified-key-strs)))
      (eval (list 'define key-predicate-sym predicate)
	    (interaction-environment)))))
(define-macro define-key
  (lambda (key-predicate-sym key-strs)
    `(define-key-internal ',key-predicate-sym ,key-strs)))

(define valid-key-str?
  (lambda (key-str)
    (let* ((parsed (parse-key-str key-str () -1 0))
	   (rest        (nth 0 parsed))
	   (translators (nth 1 parsed))
	   (key         (nth 2 parsed))
	   (key-state   (nth 3 parsed)))
      (and (string? key-str)
	   (string=? rest "")
	   (not (eqv? key -1))))))

;; 'strict-key-str' stands for key-str without translator-prefixes and
;; emacs like prefix
(define valid-strict-key-str?
  (lambda (key-str)
    (let ((saved-enable-eprefix? enable-emacs-like-key-prefix?)
	  (res #f))
      (set! enable-emacs-like-key-prefix? #f)
      (let* ((parsed (parse-key-str key-str () -1 0))
	     (rest        (nth 0 parsed))
	     (translators (nth 1 parsed))
	     (key         (nth 2 parsed))
	     (key-state   (nth 3 parsed)))
	(set! res (and (string? key-str)
		       (string=? rest "")
		       (null? translators)
		       (not (eqv? key -1)))))
      (set! enable-emacs-like-key-prefix? saved-enable-eprefix?)
      res)))

;;
(define-key left-key? "left")
(define-key right-key? "right")
(define-key switch-im-key? '("<Control>Shift_key" "<Shift>Control_key"))
