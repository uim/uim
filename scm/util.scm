;;; util.scm: Utility functions for uim.
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

(require-extension (srfi 1 6 34))

(require "ichar.scm")
(require "deprecated-util.scm")


(define hyphen-sym (string->symbol "-"))

;;
;; generic utilities
;;

(define writeln
  (lambda args
    (apply write args)
    (newline)))

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

;; procedural 'or' for use with 'apply'
;; e.g. (apply proc-or boolean-lst)
;; should be deprecated and replaced with a proper, Schemer's way
(define proc-or
  (lambda xs
    (reduce (lambda (x y)
	      (or x y))
	    #f xs)))

;; procedural 'and' for use with 'apply'
;; e.g. (apply proc-and boolean-lst)
;; should be deprecated and replaced with a proper, Schemer's way
(define proc-and
  (lambda xs
    (reduce (lambda (x y)
	      (and x y))
	    #t xs)))

;; meaning of 'end' has been changed from uim 1.5.0. See
;; doc/COMPATIBILITY and test-util.scm.
(define sublist
  (lambda (lst start end)
    (take (drop lst start)
	  (- end start))))

;; meaning of 'len' has been changed from uim 1.5.0. See
;; doc/COMPATIBILITY and test-util.scm.
(define sublist-rel
  (lambda (lst start len)
    (take (drop lst start)
	  len)))

;;; Merged from composer branch
;;; TODO: Merge with trunk, write test
;;;
;;(define sublist
;;  (lambda (lst start end)
;;    (list-tail (list-head lst (+ end 1))
;;	       start)))
;;
;;;; .parameter len Length to get. -1 means end of original lst
;;(define sublist-rel
;;  (lambda (lst start len)
;;    (if (negative? len)
;;	(list-tail lst start)
;;	(sublist lst start (+ start len)))))

(define alist-replace
  (lambda (kons alist)
    (let* ((id (car kons))
	   (preexisting (assoc id alist)))
      (if preexisting
	  (begin
	    (set-cdr! preexisting (cdr kons))
	    alist)
	  (cons kons alist)))))

(define list-join
  (lambda (lst sep)
    (if (null? lst)
	'()
	(cdr (fold-right (lambda (kar kdr)
			   (cons* sep kar kdr))
			 '() lst)))))

;; downward compatible with SRFI-13 string-join
(define string-join
  (lambda (str-list sep)
    (apply string-append (list-join str-list sep))))

;; Split pattern has been changed from uim 1.5.0. See
;; doc/COMPATIBILITY and test-uim-util.scm.
(define string-split
  (lambda (str sep)
    (let ((slen (string-length str))
	  (seplen (string-length sep)))
      (let rec ((start 0))
	(let ((next (and (<= start slen)
			 (string-contains str sep start))))
	  (if next
	      (cons (substring str start next)
		    (rec (+ next seplen)))
	      (list (substring str start slen))))))))

(define string-append-map
  (lambda args
    (apply string-append (apply map args))))

;; symbol-append is a de facto standard procedure name
(define symbol-append
  (lambda args
    (string->symbol (string-append-map symbol->string args))))

(define call-with-output-string
  (lambda (proc)
    (let ((out (open-output-string)))
      (proc out)
      (get-output-string out))))

(define call-with-input-string
  (lambda (str proc)
    (let ((in (open-input-string str)))
      (proc in))))

(define call-with-string-io
  (lambda (str proc)
    (let ((out (open-output-string))
          (in  (open-input-string str)))
      (proc in out)
      (get-output-string out))))

(define write-to-string
  (lambda (obj . args)
    (call-with-output-string
     (lambda (port) ((if (pair? args) (car args) write) obj port)))))

(define read-from-string
  (lambda (string)
    (call-with-input-string
     string
     (lambda (port)
       (read port)))))

(define (read-line . args)
  (let-optionals* args ((port (current-input-port)))
    (let loop ((c (read-char port))
               (rest '()))
      (cond ((eq? #\newline c)
             (list->string (reverse rest)))
            ((or (eof-object? c)
                 (not c))
             (if (null? rest)
                 c
                 (list->string (reverse rest))))
            (else
             (loop (read-char port) (cons c rest)))))))

(define (iconv-convert to-code from-code from-str)
  (if (equal? to-code from-code)
      from-str
      (or
       (and-let* ((ic (iconv-open to-code from-code))
                  (to-str (iconv-code-conv ic from-str)))
         (iconv-release ic)
         to-str)
       ;; XXX
       from-str)))

;; only accepts single-arg functions
;; (define caddr (compose car cdr cdr))
;; FIXME: remove the closure overhead
(define compose
  (lambda funcs
    (reduce-right (lambda (f g)
		    (lambda (x)
		      (f (g x))))
		  values funcs)))

(define method-delegator-new
  (lambda (dest-getter method)
    (lambda (self . args)
      (apply method (cons (dest-getter self) args)))))

(define safe-car
  (lambda (pair)
    (and (pair? pair)
	 (car pair))))

(define safe-cdr
  (lambda (pair)
    (and (pair? pair)
	 (cdr pair))))

(define assq-cdr
  (lambda (key alist)
    (safe-cdr (assq key alist))))

(define clamp
  (lambda (x bottom ceiling)
    (max bottom
	 (min x ceiling))))

(define inc
  (lambda (n)
    (+ n 1)))

(define dec
  (lambda (n)
    (- n 1)))

(define *gensym-counter* 0)
(define gensym
  (lambda args
    (set! *gensym-counter* (inc *gensym-counter*))
    (let ((new (string->symbol (format "~a~X" (if (pair? args) (car args) "g") *gensym-counter*))))
      (if (symbol-bound? new)
          (gensym)
          new))))

;; TODO: write test
(define number->symbol
  (compose string->symbol number->string))

;;
;; uim-specific utilities
;;

(define do-nothing (lambda args #f))

(define make-scm-pathname
  (lambda (file)
    (if (string-prefix? "/" file)
	(list file)
        (let ((paths (string-split (load-path) ":")))
          (map (lambda (x) (string-append x "/" file)) paths)))))

;; returns succeeded or not
(define try-load
  (lambda (file)
    (guard (err
             (else #f))
           (let ((paths (make-scm-pathname file)))
             (let loop ((path (car paths))
                        (rest (cdr paths)))
               ;; to suppress error message, check file existence first
               (if (file-readable? path)
                 (begin
                   (load path)
                   #t)
                 (if (not (null? rest))
                   (loop (car rest) (cdr rest))
                   #f)))))))

;; returns succeeded or not
(define try-require
  (lambda (file)
    (guard (err
             (else #f))
           (let ((paths (make-scm-pathname file)))
             (let loop ((path (car paths))
                        (rest (cdr paths)))
               ;; to suppress error message, check file existence first
               (if (file-readable? path)
                 (require path)
                 (if (not (null? rest))
                   (loop (car rest) (cdr rest))
                   #f)))))))

;; used for dynamic environment substitution of closure
(define %%enclose-another-env
  (lambda (closure another-env)
    (let* ((code (%%closure-code closure))
	   (args (car code))
	   (body (cdr code))
	   (definition (list 'lambda args body)))
      (eval definition another-env))))

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

;; TODO: write test
;; Compensates an index number for an enumerable container
;; .parameter idx Item index. Negative value instructs (abs idx)
;; items before from last item
;; .parameter size Size of the enumerable container
(define compensate-index
  (lambda (idx size)
    (if (negative? idx)
        (max 0 (+ size idx))
        (clamp idx 0 (dec size)))))

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
