;;; evmap-csv.scm: CSV exporter/importer for evmap rulesets
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

;; Usage:
;;
;; - Exporting a ruleset
;;
;;   echo '(require "evmap-csv.scm") (ruleset-export-csv ja-romaji-hiragana-ruleset)' | uim-sh -B >ruleset.csv
;;
;; - Importing a ruleset
;;   echo -n '(require "evmap-csv.scm") (ruleset-print-imported-csv "ja-romaji-hiragana-ruleset")' >/tmp/header.txt
;;   cat /tmp/header.txt ruleset.csv | uim-sh -B

(require "util.scm")
(require "physical-key.scm")
(require "evmap.scm")

(require "ng-japanese-romaji.scm")
(require "ng-japanese-kana.scm")
(require "japanese-nicola.scm")
(require "ng-japanese-azik.scm")


(define event-exp-valid-symbol?
  (lambda (exp)
    (and (symbol? exp)
	 (or (modifier-symbol? exp)
	     (assq exp event-exp-predicate-alist)
	     (assq exp event-exp-directive-alist)
	     (assq exp action-exp-preprocess-directive-alist)
	     (assq exp action-exp-postprocess-directive-alist)
	     (logical-key? exp)
	     (physical-key? exp)))))

(define ruleset-longest-event-seq
  (lambda (ruleset)
    (fold (lambda (rule longest)
	    (let ((evseq-len (length (car rule))))
	      (if (> evseq-len longest)
		  evseq-len
		  longest)))
	  0
	  ruleset)))

(define event-exp->csv-cell
  (lambda (exp)
    (cond
     ((string? exp)
      (if (or (string=? exp " ")
	      (string=? exp ",")
	      (string=? exp "\""))
	  (string-escape exp)
	  exp))
     ((symbol? exp)
      (symbol->string exp))
     ((list? exp)
      (string-join " "
		   (map event-exp->csv-cell exp)))
     (else
      (print exp)
      (error "unknown element")))))


(define rule->csv
  (lambda (rule evseq-width)
    (let ((evseq (car rule))
	  (actseq (cadr rule)))
      (string-join ","
		   (map event-exp->csv-cell
			(append
			 (list-copy! (make-list evseq-width "")
				     evseq)
			 '("") ;; separator
			 actseq))))))

(define ruleset-export-csv
  (lambda (ruleset)
    (let ((evseq-width (ruleset-longest-event-seq ruleset)))
      (for-each (lambda (rule)
		  (puts (rule->csv rule evseq-width))
		  (puts "\n"))
		ruleset))))

;;
;; import
;;

(define evmap-csv-eval-token
  (lambda (str token)
    (if (and (string=? str "")
	     (string=? token ""))
	#f
	(let* ((new-token (string-append token str))
	       (head (string->charcode new-token))
	       (evaluated (if (or (char-alphabetic? head)
				  (= head (string->char "\""))
				  (= head (string->char "$")))
			      (read-from-string new-token)
			      new-token)))
	  (or (and (string? evaluated)
		   evaluated)
	      (and (event-exp-valid-symbol? evaluated)
		   evaluated)
	      new-token)))))

(define evmap-csv-state-remain
  (lambda (str token parsed)
    (list token parsed)))

(define evmap-csv-state-append-token
  (lambda (str token parsed)
    (list (string-append token str)
	  parsed)))

(define evmap-csv-state-push-elem
  (lambda (str token parsed)
    (let ((elem (evmap-csv-eval-token str token)))
      (list ""
	    (cons elem parsed)))))

(define evmap-csv-state-push-separated-elem
  (lambda (str token parsed)
    (evmap-csv-state-push-elem "" token parsed)))

(define evmap-csv-state-push-list-elem
  (lambda (str token parsed)
    (let ((elem (evmap-csv-eval-token str token)))
      (list ""
	    (cons (cons elem (car parsed))
		  (cdr parsed))))))

(define evmap-csv-state-push-separated-list-elem
  (lambda (str token parsed)
    (evmap-csv-state-push-list-elem "" token parsed)))

(define evmap-csv-state-push-last-list-elem
  (lambda (str token parsed)
    (let ((next-parsed (cadr
			(evmap-csv-state-push-separated-list-elem "" token parsed))))
      (list ""
	    (cons (reverse (car next-parsed))
		  (cdr next-parsed))))))

(define-record 'evmap-csv-state
  (list
   (list 'str        #f)
   (list 'next-state #f)
   (list 'action     evmap-csv-state-append-token)))

;; TODO: replace with a code generated by sophisticated parser generator
(define evmap-csv-state-map
  (list
   (cons 'initial
	 (list
	  (evmap-csv-state-new ""   'fin evmap-csv-state-remain)
	  (evmap-csv-state-new "\n" 'fin evmap-csv-state-remain)
	  (evmap-csv-state-new " "  'error evmap-csv-state-remain)
	  (evmap-csv-state-new "\"" 'string)
	  (evmap-csv-state-new #f   'elem)))
   (cons 'neutral
	 (list
	  (evmap-csv-state-new ","  'neutral evmap-csv-state-push-separated-elem)
	  (evmap-csv-state-new ""   'fin evmap-csv-state-remain)
	  (evmap-csv-state-new "\n" 'fin evmap-csv-state-remain)
	  (evmap-csv-state-new " "  'error evmap-csv-state-remain)
	  (evmap-csv-state-new "\"" 'string)
	  (evmap-csv-state-new #f   'elem)))
   (cons 'elem
	 (list
	  (evmap-csv-state-new ""   'fin evmap-csv-state-remain)
	  (evmap-csv-state-new "\n" 'fin evmap-csv-state-push-separated-elem)
	  (evmap-csv-state-new ","  'neutral evmap-csv-state-push-separated-elem)
	  (evmap-csv-state-new " "  'list-elem
			       (lambda (str token parsed)
				 (let ((elem (evmap-csv-eval-token "" token)))
				   (list ""
					 (cons (list elem)
					       parsed)))))
	  (evmap-csv-state-new "\"" 'error evmap-csv-state-remain)
	  (evmap-csv-state-new #f   'elem)))
   (cons 'string
	 (list
	  (evmap-csv-state-new "\"" 'neutral evmap-csv-state-push-elem)
	  (evmap-csv-state-new "\\" 'string-escaping)
	  (evmap-csv-state-new #f   'string)))
   (cons 'string-escaping
	 (list
	  (evmap-csv-state-new #f   'string)))
   (cons 'list-neutral
	 (list
	  (evmap-csv-state-new ","  'neutral evmap-csv-state-push-last-list-elem)
	  (evmap-csv-state-new ""   'fin evmap-csv-state-remain)
	  (evmap-csv-state-new "\n" 'fin evmap-csv-state-remain)
	  (evmap-csv-state-new " "  'list-neutral evmap-csv-state-remain)
	  (evmap-csv-state-new "\"" 'list-string)
	  (evmap-csv-state-new #f   'list-elem)))
   (cons 'list-elem
	 (list
	  (evmap-csv-state-new ","  'neutral evmap-csv-state-push-last-list-elem)
	  (evmap-csv-state-new "\n" 'fin evmap-csv-state-push-separated-list-elem)
	  (evmap-csv-state-new " "  'list-neutral evmap-csv-state-push-separated-list-elem)
	  (evmap-csv-state-new "\"" 'error evmap-csv-state-remain)
	  (evmap-csv-state-new #f   'list-elem)))
   (cons 'list-string
	 (list
	  (evmap-csv-state-new "\"" 'list-neutral evmap-csv-state-push-list-elem)
	  (evmap-csv-state-new "\\" 'list-string-escaping)
	  (evmap-csv-state-new #f   'list-string)))
   (cons 'list-string-escaping
	 (list
	  (evmap-csv-state-new #f   'list-string)))
   (cons 'fin
	 (list
	  (evmap-csv-state-new #f   'fin evmap-csv-state-remain)))
   (cons 'error
	 (list
	  (evmap-csv-state-new #f   'error evmap-csv-state-remain)))))

(define evmap-csv-next-state
  (lambda (cur-state str)
    (let* ((cands (assq-cdr cur-state evmap-csv-state-map))
	   (next (or (assoc str cands)
		     (assoc #f cands))))
      next)))

;; returns (parsed . rest-src)
(define evmap-csv-parse-line
  (lambda (kar kdr seed)
    (let transit ((state 'initial)
		  (context '("" ()))  ;; (token parsed)
		  (src seed))
      (let* ((str (kar src))
	     (next (evmap-csv-next-state state str))
	     (next-state (and next
			      (evmap-csv-state-next-state next)))
	     (action (and next
			  (evmap-csv-state-action next)))
	     (next-context (apply action (cons str context))))
	;;(print state)
	;;(print str)
	(cond
	 ((eq? next-state 'fin)
	  (cons (reverse (cadr next-context))
		src))
	 ((eq? next-state 'error)
	  (print "invalid CSV line")
	  #f)
	 ((string=? str "")
	  #f)
	 (else
	  (transit next-state next-context (kdr src))))))))

(define evmap-csv-cells-event-seq
  (lambda (cells)
    (let self ((cells cells)
	       (seq ()))
      (if (or (null? cells)
	      (not (car cells)))
	  (reverse seq)
	  (self (cdr cells)
		(cons (car cells) seq))))))

(define evmap-csv-cells-action-seq
  (lambda (cells)
    (find-tail (lambda (x) x)
	       (find-tail not cells))))

(define evmap-csv-cells->rule
  (lambda (cells)
    (let ((ev-cells (evmap-csv-cells-event-seq cells))
	  (act-cells (evmap-csv-cells-action-seq cells)))
      (list ev-cells act-cells))))

(define evmap-csv-string-car
  (lambda (str-list)
    (if (null? str-list)
	""
	(car str-list))))

(define evmap-csv-file-car
  (lambda (dummy)
    (let ((c (getc)))
      (if (eq? c (eof-val))
	  ""
	  (charcode->string c)))))

;; returns ruleset
(define ruleset-import-csv
  (lambda (kar kdr seed)
    (let parse-line ((parsed ())
		     (src seed))
      (let* ((res (evmap-csv-parse-line kar kdr src))
	     (cells (car res))
	     (rest-src (cdr res)))
	(if cells
	    (parse-line (cons (evmap-csv-cells->rule cells)
			      parsed)
			rest-src)
	    (reverse parsed))))))

(define ruleset-import-csv-from-stdin
  (lambda ()
    (ruleset-import-csv evmap-csv-file-car (lambda (x) x) ())))

(define ruleset-print-imported-csv
  (lambda (ruleset-name)
    (puts (string-append
	   "(define " ruleset-name " '(\n"))
    (for-each (lambda (rule)
		(puts "  ")
		(print rule))
	      (ruleset-import-csv-from-stdin))
    (puts "))\n")))
