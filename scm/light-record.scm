;;; light-record.scm: Lightweight record types
;;;
;;; Copyright (c) 2007-2013 uim Project https://github.com/uim/uim
;;;
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;; 3. Neither the name of authors nor the names of its contributors
;;;    may be used to endorse or promote products derived from this software
;;;    without specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS
;;; IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
;;; THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;;; PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR
;;; CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;;; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;; In contrast to SRFI-9 standard, this record library features:
;;
;;   - Automatic accessor name generation (but lost naming scheme
;;     selectability)
;;
;;   - No memory overhead on each record instance such as record marker
;;     and type information (but lost type detectability)
;;
;;   - Selectable data backend implementation such as vector and list.
;;     List backend enables sharing some tail part between multiple
;;     record instances
;;
;;   - Composable field-specs (since record definers are not syntax)
;;
;;
;; Specification:
;;
;; <record definition> must be placed on toplevel env.
;;
;; <record definition> ::= (define-vector-record <record name> <field specs>)
;;                         | (define-list-record <record name> <field specs>)
;;                         | (define-record-generic <record name> <field specs>
;;                            <list2record> <record-copy>
;;                            <record-ref> <record-set!>)
;;
;; <record name>   ::= <identifier>
;; <list2record>   ::= <procedure>
;; <record-copy>   ::= <procedure>
;; <record-ref>    ::= <procedure>
;; <record-set!>   ::= <procedure>
;;
;; <field specs>   ::= ()
;;                     | (<field spec> . <field specs>)
;;
;; <field spec>    ::= <symbol>
;;                     | (<symbol>)
;;                     | (<symbol> <default value>)
;;
;; <default value> ::= <any Scheme object>

(require-extension (srfi 1 23))
(cond-expand
 (uim)
 (else
  (require-extension (srfi 43)))) ;; vector-copy

(require "util.scm")


(define %HYPHEN-SYM (string->symbol "-"))

(define %list-set!
  (lambda (lst index val)
    (set-car! (list-tail lst index)
	      val)))

(define vector-copy
  (if (symbol-bound? 'vector-copy)
      vector-copy
      (lambda (v)
	(list->vector (vector->list v)))))

(define record-field-spec-name
  (lambda (fld-spec)
    (let ((name (or (safe-car fld-spec)
		    fld-spec)))
      (if (symbol? name)
	  name
	  (error "invalid field spec")))))

(define record-field-spec-default-value
  (compose safe-car safe-cdr))

(define make-record-spec-name
  (lambda (rec-name)
    (symbol-append 'record-spec- rec-name)))

(define make-record-constructor-name
  (lambda (rec-name)
    (symbol-append 'make- rec-name)))

(define make-record-duplicator-name
  (lambda (rec-name)
    (symbol-append rec-name %HYPHEN-SYM 'copy)))

(define make-record-getter-name
  (lambda (rec-name fld-name)
    (symbol-append rec-name %HYPHEN-SYM fld-name)))

(define make-record-setter-name
  (lambda (rec-name fld-name)
    (symbol-append rec-name %HYPHEN-SYM 'set- fld-name '!)))

(define %make-record-constructor
  (lambda (rec-name fld-specs list->record)
    (let ((defaults (map record-field-spec-default-value fld-specs))
	  (defaults-len (length fld-specs)))
      (lambda init-lst
	(if (null? init-lst)
	    (list->record defaults)
	    (let ((init-lst-len (length init-lst)))
	      (cond
	       ((= init-lst-len defaults-len)
		(list->record init-lst))
	       ((< init-lst-len defaults-len)
		(let* ((rest-defaults (list-tail defaults init-lst-len))
		       (complemented-init-lst (append init-lst rest-defaults)))
		  (list->record complemented-init-lst)))
	       (else
		(error "invalid initialization list for record"
		       rec-name)))))))))

;; To suppress redundant closure allocation, accessors for same
;; <index, type> share identical procedure. And faster short-cut
;; procedures such as car are predefined.
(define %retrieve-record-accessor
  (let ((pool `(((0 . ,list-ref)   . ,car)
		((1 . ,list-ref)   . ,cadr)
		((2 . ,list-ref)   . ,caddr)
		((0 . ,%list-set!) . ,set-car!)
		((1 . ,%list-set!) . ,(lambda (l v)
					(set-car! (cdr l) v)))
		((2 . ,%list-set!) . ,(lambda (l v)
					(set-car! (cddr l) v))))))
    (lambda (index key accessor)
      (let ((pool-key (cons index key)))
	(cond
	 ((assoc pool-key pool) => cdr)
	 (else
	  (set! pool (alist-cons pool-key accessor pool))
	  accessor))))))

(define %make-record-getter
  (lambda (index record-ref)
    (let ((getter (lambda (rec)
		    (record-ref rec index))))
      (%retrieve-record-accessor index record-ref getter))))

(define %make-record-setter
  (lambda (index record-set!)
    (let ((setter (lambda (rec val)
		    (record-set! rec index val))))
      (%retrieve-record-accessor index record-set! setter))))

(define-macro %define-record-getter
  (lambda (rec-name fld-name index record-ref)
    (let ((getter-name (make-record-getter-name rec-name fld-name)))
      `(define ,getter-name (%make-record-getter ,index ,record-ref)))))

(define-macro %define-record-setter
  (lambda (rec-name fld-name index record-set!)
    (let ((setter-name (make-record-setter-name rec-name fld-name)))
      `(define ,setter-name (%make-record-setter ,index ,record-set!)))))

;;(define-macro %define-record-accessors
;;  (lambda (rec-name fld-specs record-ref record-set!)
;;    (cons 'begin
;;	  (map (lambda (fld-name index)
;;		 `(begin
;;		    (%define-record-getter ,rec-name ,fld-name ,index
;;					   ,record-ref)
;;		    (%define-record-setter ,rec-name ,fld-name ,index
;;					   ,record-set!)))
;;	       (map record-field-spec-name fld-specs)
;;	       (iota (length fld-specs))))))

(define-macro define-record-generic
  (lambda (rec-name fld-specs list->record record-copy record-ref record-set!)
    `(begin
       ;; define record field specs
       (define ,(make-record-spec-name rec-name) ,fld-specs)
       ;; define record object constructor
       (define ,(make-record-constructor-name rec-name)
	 (%make-record-constructor ',rec-name ,fld-specs ,list->record))
       ;; define record object duplicator
       (define ,(make-record-duplicator-name rec-name) ,record-copy)
       ;; define record field accessors
       ,(cons 'begin
	      (map (lambda (fld-name index)
		     `(begin
			(%define-record-getter ,rec-name ,fld-name ,index
					       ,record-ref)
			(%define-record-setter ,rec-name ,fld-name ,index
					       ,record-set!)))
		   (map record-field-spec-name (eval fld-specs (interaction-environment)))
		   (iota (length (eval fld-specs (interaction-environment)))))))))

(define-macro define-vector-record
  (lambda (rec-name fld-specs)
    `(define-record-generic
       ,rec-name ,fld-specs
       list->vector vector-copy vector-ref vector-set!)))

(define-macro define-list-record
  (lambda (rec-name fld-specs)
    `(define-record-generic
       ,rec-name ,fld-specs
       list-copy list-copy list-ref %list-set!)))
