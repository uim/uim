;;;
;;; Copyright (c) 2003-2010 uim Project http://code.google.com/p/uim/
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

;;
; following functions are implemented within C
;  rk-lib-find-seq
;  rk-lib-find-partial-seq
;  rk-lib-expect-seq
;  rk-lib-expect-key?
;
; back match is mainly used for Hangul
;
; if "table" is provided as a rule, ct-lib-find-seq and ;
; ct-lib-find-partial-seq are used to search words from sorted text
; file.

(define-record 'rk-context
  '((rule             ())
    (seq              ())
    (immediate-commit #f)
    (back-match       #f)
    (find-seq         #f)
    (find-partial-seq #f)))
(define rk-context-new-internal rk-context-new)

(define rk-context-new
  (lambda (rule immediate-commit back)
    (if (string? rule)
      (require "ct.scm"))
    (let ((find-seq (if (string? rule)
                      ct-lib-find-seq
                      rk-lib-find-seq))
          (find-partial-seq (if (string? rule)
                              ct-lib-find-partial-seq
                              rk-lib-find-partial-seq)))
    (rk-context-new-internal rule () immediate-commit back find-seq find-partial-seq))))

;; back match
(define rk-find-longest-back-match
  (lambda (rule seq find-seq)
    (if (not (null? seq))
	(if (find-seq seq rule)
	    seq
	    (rk-find-longest-back-match rule (cdr seq) find-seq))
	'())))
;; back match
(define rk-find-longest-head
  (lambda (rseq rule find-seq)
    (let ((seq (reverse rseq)))
      (if (find-seq seq rule)
	  seq
	  (if (not (null? rseq))
	      (rk-find-longest-head (cdr rseq) rule find-seq)
	      '())))))
;; back match
(define rk-check-back-commit
  (lambda (rkc rule rseq)
    (let* ((seq (reverse rseq))
	   (len (length seq))
           (find-seq (rk-context-find-seq rkc))
           (find-partial-seq (rk-context-find-partial-seq rkc))
	   (longest-tail (rk-find-longest-back-match rule seq find-seq))
	   (longest-head (reverse (rk-find-longest-head rseq rule find-seq)))
	   (head
	    (truncate-list seq
			   (- len (length longest-tail))))
	   (partial (find-partial-seq seq rule))
	   (tail-partial
	    (if (not (null? longest-tail))
		(find-partial-seq longest-tail rule)
		#f))
	   (c (find-seq longest-tail rule))
	   (t (find-seq seq rule))
	   (res #f))
      (and
       (if (> len 0)
	   #t
	   #f)
       (if partial
	   #f
	   #t)
       (if (and c t)
	   #f
	   #t)
       (if (not tail-partial)
	   (let ((matched (find-seq (reverse longest-head) rule))
		 (tail (reverse (truncate-list (reverse seq)
					       (- len
						  (length longest-head))))))
	     (if matched
		 (set! res (cadr matched)))
	     (if (and
		  res
		  (or
		   (not (null? longest-tail))
		   (find-partial-seq tail rule)))
		 (rk-context-set-seq! rkc tail)
		 (rk-context-set-seq! rkc '())) ;; no match in rule
	     #f)
	   #t)
       (let ((matched (find-seq head rule)))
	 (if matched
	     (set! res (cadr matched)))
	 (rk-context-set-seq! rkc (reverse longest-tail))))
      res)))
;;
(define rk-partial-seq?
  (lambda (rkc s)
    (if (null? s)
        #f
        ((rk-context-find-partial-seq rkc) (reverse s) (rk-context-rule rkc)))))

;; API
(define rk-partial?
  (lambda (rkc)
    (if (rk-context-back-match rkc)
	(if (not (null? (rk-context-seq rkc)))
	    #t
	    #f)
	(rk-partial-seq?
	 rkc
	 (rk-context-seq rkc)))))

;; API
(define rk-current-seq
  (lambda (rkc)
    (let* ((s (rk-context-seq rkc))
	   (rule (rk-context-rule rkc))
           (find-seq (rk-context-find-seq rkc)))
      (find-seq (reverse s) rule))))

;; API
(define rk-flush
  (lambda (context)
    (rk-context-set-seq! context ())))

;; API
(define rk-backspace
  (lambda (context)
    (if
     (pair? (rk-context-seq context))
     (begin
       (rk-context-set-seq! context
		 (cdr (rk-context-seq context)))
       ;; If the sequence contains only non-representable keysyms after
       ;; the deletion, flush them.
       (if (and
	    (pair? (rk-context-seq context))
	    (null? (remove
		    (lambda (x)
		     (and
		      (intern-key-symbol x)
		      (not (symbol-bound? (string->symbol x)))))
		    (rk-context-seq context))))
	   (rk-flush context))
       #t)
     #f)))
 
;; API
(define rk-delete
  (lambda (context)
    (if
     (pair? (rk-context-seq context))
     (begin
       (rk-context-set-seq! context
		 (cdr (rk-context-seq context)))
       ;; If the sequence contains only non-representable keysyms after
       ;; the deletion, flush them.
       (if (and
	    (not (null? (rk-context-seq context)))
	    (null? (remove
		    (lambda (x)
		     (and
		      (intern-key-symbol x)
		      (not (symbol-bound? (string->symbol x)))))
		    (rk-context-seq context))))
	   (rk-flush context))

       #t)
     #f)))


; Merges two strings that have been converted, for example
; ("ん" "ン" "　") ("1" "1" "1") --> ("ん1" "ン1" "　1").
; SEQ1 and SEQ2 must be proper lists having the same length.
; A disgusting hack for implementing ("n" "1") --> ("ん1").
; Anyone with the time, skill and passion, please clean this up :-(
(define rk-merge-seqs
  (lambda (seq1 seq2)
    (if (and (pair? seq1) (pair? seq2))
	(cons (string-append (car seq1) (car seq2))
	      (rk-merge-seqs (cdr seq1) (cdr seq2)))
	; This should be () when we reach the end of the lists, or
	; whatever passed as SEQ1 if SEQ2 is #f
	seq1)))

;; front match
(define rk-proc-tail
  (lambda (context seq)
    (let* ((rule (rk-context-rule context))
           (find-seq (rk-context-find-seq context))
	   (old-seq
	    (find-seq
	     (reverse (rk-context-seq context)) rule))
	   (res #f))
      (if old-seq
	  (begin
	    (rk-flush context)
	    ;; Comment out the code using rk-merge-seqs for the
	    ;; moment because of rk-backspace problem -- ekato
	    ;;(set! res
	    ;;	  (rk-merge-seqs
	    ;;	   (cadr old-seq)
	    ;;	   (rk-push-key! context (car seq)))))
	    ;;
	    (rk-push-key! context (car seq))
	    (set! res (cadr old-seq)))
	    ;;
	  (if (not (null? (rk-context-seq context)))
	      (begin
		(rk-flush context)
		(set! res
		      (rk-push-key! context (car seq))))))
      res)))


(define rk-proc-end-seq
  (lambda (context seq s)
    (if (rk-context-immediate-commit context)
	(if seq
	    (let ((latter (cadr seq)))
	      (rk-context-set-seq! context (cdar seq))
	      (if (not (null? latter))
		  latter
		  #f))
	    (begin
	      (rk-context-set-seq! context '())
	      #f))
	(begin
	  (rk-context-set-seq! context s)
	  #f))))
;; API
;; return list of all expected next characters for the current partial sequence
;; return '() if current rkc is not partial
(define rk-expect
  (lambda (rkc)
    (let
	((s (reverse (rk-context-seq rkc)))
	 (rule (rk-context-rule rkc)))
      (rk-lib-expect-seq s rule))))

;; API
;; return #t if the key is expected as a next character in the partial sequence
;; this should be faster than rk-expect
(define rk-expect-key?
  (lambda (rkc key)
    (let
	((s (reverse (rk-context-seq rkc)))
	 (rule (rk-context-rule rkc)))
      (rk-lib-expect-key-for-seq? s rule key))))


;; back match
(define rk-push-key-back-match
  (lambda (rkc key)
    (let*
	((cur-seq (rk-context-seq rkc))
	 (new-seq (cons key cur-seq))
	 (rule (rk-context-rule rkc)))
      (rk-context-set-seq! rkc new-seq)
      (rk-check-back-commit rkc rule new-seq))))

;; front match
(define rk-push-key-front-match
  (lambda (rkc key)
    (let* ((s (cons key (rk-context-seq rkc)))
           (rule (rk-context-rule rkc))
           (find-seq (rk-context-find-seq rkc))
           (res (if (rk-partial-seq? rkc s)
                  (begin
                    (rk-context-set-seq! rkc s)
                    #f)
                  (let ((seq (find-seq (reverse s) rule)))
                    (if seq
                      (rk-proc-end-seq rkc seq s)
                      (rk-proc-tail rkc s))))))
      res)))

;; API
;; returns the rule entry that exactly matches with current pending
;; key sequence. rkc will not be altered.
(define rk-peek-terminal-match
  (lambda (rkc)
    (let ((rule-entry (rk-current-seq rkc)))
      (and rule-entry
	   (cadr rule-entry)))))

;; API
;;超適当。rk.scmを理解するか、あとでごっそり設計しなおす事。
;;
;; The procedure name is confusable. I suggest rk-terminate-input!
;;  -- YamaKen 2004-10-25
(define rk-push-key-last!
  (lambda (rkc)
    (let*
	((s (rk-context-seq rkc))
	 (rule (rk-context-rule rkc))
         (find-seq (rk-context-find-seq rkc))
	 (seq (find-seq (reverse s) rule)))
      (rk-proc-end-seq rkc seq s)
      )))

;; API
;; returns string list or #f
(define rk-push-key!
  (lambda (rkc key)
    (if (rk-context-back-match rkc)
	;; mainly for Hangul
	(rk-push-key-back-match rkc key)
	;; for other languages
	(rk-push-key-front-match rkc key))))
;; API
(define rk-pending
  (lambda (c)
    (string-list-concat
     ;; remove keysyms not representable in IM
     (filter-map
      (lambda (x) (if (intern-key-symbol x)
		      (if (symbol-bound? (string->symbol x))
		          (symbol-value (string->symbol x))
			  "")
		      x))
      (rk-context-seq c)))))
