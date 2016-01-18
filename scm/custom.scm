;;; custom.scm: Customization support
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

;; There are complex definitions to experiment the customization
;; mechanism. Will be simplified once the requirement is cleared up.
;; -- YamaKen

(require "i18n.scm")
(require "util.scm")
(require "key.scm")

;; config
(define key-list->gui-key-list 'key-list-export-as-basic)
(define gui-key-list->key-list 'key-list-import-as-basic)
;;(define key-list->gui-key-list 'key-list-export-as-traditional)
;;(define gui-key-list->key-list 'key-list-import-as-traditional)

;; public
(define custom-activity-hooks ())
(define custom-get-hooks ())
(define custom-set-hooks ())
(define custom-literalize-hooks ())
(define custom-update-hooks ())
(define custom-group-update-hooks ())
(define custom-group-list-update-hooks ())

;; private
(define custom-full-featured? #t)
(define custom-rec-alist ())
(define custom-group-rec-alist ())
(define custom-subgroup-alist ())

(define custom-validator-alist
  '((boolean      . custom-boolean?)
    (integer      . custom-integer?)
    (string       . custom-string?)
    (pathname     . custom-pathname?)
    (choice       . custom-valid-choice?)
    (ordered-list . custom-ordered-list?)
    (key          . custom-key?)
    (table        . custom-table?)))

(define anything?
  (lambda (x)
    #t))

(define custom-boolean?
  (lambda (x)
    #t))

(define custom-integer?
  (lambda (x min max)
    (and (integer? x)
	 (<= min x)
	 (<= x max))))

(define custom-string?
  (lambda (x regex)
    (string? x)))

(define custom-pathname?
  (lambda (str type)
    (and (string? str)
	 (symbol? type)
	 (memq type '(regular-file directory)))))

(define custom-valid-choice?
  (lambda arg
    (let ((sym (car arg))
	  (choice-rec-alist (cdr arg)))
      (and (symbol? sym)
	   (assq sym choice-rec-alist)
	   #t))))

(define custom-ordered-list?
  (lambda arg
    (let ((syms (car arg))
	  (choice-rec-alist (cdr arg)))
      (and (list? syms)
	   (every (lambda (sym)
		    (apply custom-valid-choice? (cons sym choice-rec-alist)))
		  syms)))))

(define custom-key?
  (lambda (key-repls)
    (and (list? key-repls)
	 (every (lambda (key)
		  (or (and (string? key)  ;; "<Control>a"
			   ;;(valid-strict-key-str? key)
			   (valid-key-str? key))  ;; acceps translators
		      (and (symbol? key)  ;; 'generic-cancel-key
			   (custom-exist? key 'key))))
		key-repls))))

(define custom-table?
  (lambda arg
    (let ((syms (car arg))
          (choice-rec-alist (cdr arg)))
      (and (list? syms)
           (every (lambda (row)
                    (and (list? row)
                         (every (lambda (cell)
                                  (string? cell))
                                row)))
                  syms)))))

(define custom-pathname-type
  (lambda (custom-sym)
    (car (custom-type-attrs custom-sym))))

(define custom-expand-key-references
  (lambda (key)
    (cond
     ((string? key)
      (list (key-str->gui-key-str key)))
     ((list? key)
      (append-map custom-expand-key-references key))
     ((and (symbol? key)
	   (custom-exist? key 'key))
      (custom-expand-key-references (custom-value key)))
     (else
      ()))))

;; TODO
(define custom-key-advanced-editor?
  (lambda (custom-sym)
    #f))

(define reversed-tag-prefix-alist
  (map (lambda (pair)
	 (cons (cdr pair)
	       (car pair)))
       tag-prefix-alist))

;; TODO: write test
;; (key-str->key-list "<Control><Shift><IgnoreRegularShift>return")
;;   -> (Control_key Shift_key IgnoreRegularShift "return")
;; (key-str->key-list "C-M-a")
;;   -> (Control_key Meta_key "a")
(define key-str->key-list
  (lambda (key-str)
    (unfold (compose not car parse-key-prefix)
	    (compose car parse-key-prefix)
	    (compose cdr parse-key-prefix)
	    key-str
	    (compose list cdr parse-key-prefix))))

;; TODO: write test
(define key-list->key-str
  (lambda (key-list)
    (string-append-map
     (lambda (elem)
       (if (symbol? elem)
	   (let ((mod (cdr (assq elem reversed-tag-prefix-alist))))
	     (string-append "<" mod ">"))
	   elem))
     key-list)))

;; TODO: write test
(define map-key-list-body
  (lambda (body-mapper key-list)
    (map (lambda (elem)
	   (if (string? elem)
	       (body-mapper elem)
	       elem))
	 key-list)))

;; TODO: write test
(define map-key-list-letter
  (lambda (letter-mapper key-list)
    (let ((letter (string->alphabetic-ichar (find string? key-list))))
      (map-key-list-body (lambda (elem)
			   (if letter
			       (charcode->string (letter-mapper letter))
			       elem))
			 key-list))))

;; TODO: write test
(define map-key-str
  (lambda (key-list-mapper key-str)
    (if (string? key-str)
	(let ((key-list (key-str->key-list key-str)))
	  (key-list->key-str (key-list-mapper key-list)))
	key-str)))

;; TODO: write test
(define key-list-upcase
  (lambda (key-list)
    (map-key-list-letter ichar-upcase key-list)))

;; TODO: write test
(define key-list-downcase
  (lambda (key-list)
    (map-key-list-letter ichar-downcase key-list)))

;; TODO: write test
(define key-list-visualize-space
  (lambda (key-list)
    (map-key-list-body (lambda (elem)
			 (if (string=? elem " ")
			     "space"
			     elem))
		      key-list)))

;; TODO: write test
(define key-list-characterize-space
  (lambda (key-list)
    (map-key-list-body (lambda (elem)
			 (if (string=? elem "space")
			     " "
			     elem))
		      key-list)))

;; TODO: write test
(define key-list-encode-shift
  (lambda (key-list)
    (let* ((has-shift? (memq 'Shift_key key-list))
	   (str (find string? key-list))
	   (printable (string->printable-ichar str))
	   (letter (string->alphabetic-ichar str)))
      (filter-map (lambda (elem)
		    (cond
		     ((and (eq? elem 'Shift_key)
			   (ichar-graphic? printable))
		      #f)
		     ((and (string? elem)
			   has-shift?
			   letter)
		      (charcode->string (ichar-upcase letter)))
		     ((and (string? elem)
			   has-shift?
			   (ichar-graphic? printable))
		      str)
		     (else
		      elem)))
		  key-list))))

;; TODO: write test
(define key-list-decode-shift
  (lambda (key-list)
    (let* ((letter (string->alphabetic-ichar (find string? key-list)))
	   (upper-case? (and letter
			     (ichar-upper-case? letter)))
	   (has-shift? (memq 'Shift_key key-list))
	   (stripped (key-list-downcase key-list)))
      (if (and (not has-shift?)
	       upper-case?)
	  (cons 'Shift_key stripped)
	  stripped))))

;; TODO: write test
(define key-list-ignore-regular-shift
  (lambda (key-list)
    (let ((printable (string->printable-ichar (find string? key-list))))
      (if (ichar-graphic? printable)
	  (cons 'IgnoreRegularShift key-list)
	  key-list))))

;; TODO: write test
(define key-list-ignore-letter-shift
  (lambda (key-list)
    (let ((letter (string->alphabetic-ichar (find string? key-list))))
      (if letter
	  (cons 'IgnoreShift key-list)
	  key-list))))

;; TODO: write test
(define key-list-ignore-punct-numeric-shift
  (lambda (key-list)
    (let* ((str (find string? key-list))
	   (c (string->printable-ichar str)))
      (if (and (ichar-graphic? c)
	       (not (ichar-alphabetic? c)))
	  (cons 'IgnoreShift key-list)
	  key-list))))

;; TODO: write test
(define key-list-ignore-case
  (lambda (key-list)
    (let ((letter (string->alphabetic-ichar (find string? key-list))))
      (if letter
	  (cons 'IgnoreCase key-list)
 	  key-list))))

;; TODO: write test
(define key-list-strip-shift
  (lambda (key-list)
    (delete 'Shift_key key-list eq?)))

;; TODO: write test
(define key-list-strip-regular-shift
  (lambda (key-list)
    (let* ((str (find string? key-list))
	   (printable (string->printable-ichar str)))
      (if (ichar-graphic? printable)
	  (key-list-strip-shift key-list)
	  key-list))))

;; TODO: write test
(define key-list-strip-translators
  (lambda (key-list)
    (remove translator-prefix? key-list)))

;; TODO: write test
(define key-list-export-as-basic (compose key-list-visualize-space
					  key-list-encode-shift
					  key-list-strip-translators))

;; TODO: write test
(define key-list-import-as-basic (compose key-list-characterize-space
					  key-list-ignore-punct-numeric-shift
					  key-list-ignore-case
					  key-list-decode-shift
					  key-list-strip-regular-shift))

;; TODO: write test
(define key-list-export-as-traditional (compose key-list-visualize-space
						key-list-strip-translators))

;; TODO: write test
(define key-list-import-as-traditional (compose key-list-characterize-space
						key-list-ignore-regular-shift))

;; TODO: write test
(define key-str->gui-key-str
  (lambda (key-str)
    (map-key-str (symbol-value key-list->gui-key-list)
		 key-str)))

;; TODO: write test
(define gui-key-str->key-str
  (lambda (key-str)
    (map-key-str (symbol-value gui-key-list->key-list)
		 key-str)))

(define custom-choice-label
  (lambda (custom-sym val-sym)
    (let* ((sym-rec-alist (custom-type-attrs custom-sym))
	   (srec (assq val-sym sym-rec-alist)))
      (if srec
	  (custom-choice-rec-label srec)
	  (symbol->string val-sym)))))

(define custom-choice-desc
  (lambda (custom-sym val-sym)
    (let* ((sym-rec-alist (custom-type-attrs custom-sym))
	   (srec (assq val-sym sym-rec-alist)))
      (if srec
	  (custom-choice-rec-desc srec)
	  (symbol->string val-sym)))))

(define custom-choice-range-reflect-olist-val
  (lambda (dst-sym src-sym indication-alist)
    (custom-set-type-info!
     dst-sym
     (cons (custom-type dst-sym)
	   (action-id-list->choice (custom-value src-sym)
				   indication-alist)))))

(define-record 'custom-group-rec
  '((sym   #f)
    (label "")
    (desc  "")))

(define define-custom-group
  (lambda (gsym label desc)
    (let ((grec (custom-group-rec-new gsym label desc)))
      (if (not (custom-group-rec gsym))
	  (begin
	    (set! custom-group-rec-alist (cons grec custom-group-rec-alist))
	    (custom-call-hook-procs 'global
				    custom-group-list-update-hooks))))))

(define custom-group-rec
  (lambda (gsym)
    (assq gsym custom-group-rec-alist)))

;; API
(define custom-group-label
  (lambda (gsym)
    (custom-group-rec-label (custom-group-rec gsym))))

;; API
(define custom-group-desc
  (lambda (gsym)
    (custom-group-rec-desc (custom-group-rec gsym))))

;; API
(define custom-group-subgroups
  (lambda (gsym)
    (let ((groups (filter-map (lambda (pair)
				(let ((primary-grp (car pair))
				      (subgrp (cdr pair)))
				  (and (eq? gsym primary-grp)
				       subgrp)))
			      custom-subgroup-alist)))
      (reverse groups))))

;; API
(define custom-list-groups
  (lambda ()
    (let ((groups (map custom-group-rec-sym custom-group-rec-alist)))
      (reverse groups))))

;; API
(define custom-list-primary-groups
  (lambda ()
    (let ((groups (filter-map
		   (lambda (grec)
		     (let ((grp (custom-group-rec-sym grec)))
		       (and (assq grp custom-subgroup-alist)
			    grp)))
		   custom-group-rec-alist)))
      (reverse groups))))

;; TODO: rewrite test for 'AND' expression
;; API
;; #f means 'any group'
(define custom-collect-by-group
  (lambda groups
    (reverse
     (filter-map (lambda (crec)
		   (let ((custom-groups (custom-rec-groups crec)))
		     (and (or (not (car groups))
			      (every (lambda (group)
				       (memq group custom-groups))
				     groups))
			  (custom-rec-sym crec))))
		 custom-rec-alist))))

;; API
(define custom-add-hook
  (lambda (custom-sym hook-sym proc)
    (set-symbol-value! hook-sym (cons (cons custom-sym proc)
				      (symbol-value hook-sym)))))

;; #f for custom-sym means 'any entries'
(define custom-remove-hook
  (lambda (custom-sym hook-sym)
    (let ((removed (if custom-sym
		       (alist-delete custom-sym (symbol-value hook-sym) eq?)
		       ()))
	  (removed? (if custom-sym
			(assq custom-sym (symbol-value hook-sym))
			(not (null? (symbol-value hook-sym))))))
      (set-symbol-value! hook-sym removed)
      removed?)))

(define custom-hook-procs
  (lambda (sym hook)
    (let* ((filter (lambda (pair)
		     (let ((custom (car pair))
			   (proc (cdr pair)))
		       (and (eq? sym custom)
			    proc))))
	   (procs (filter-map filter
			      hook)))
      procs)))

(define custom-call-hook-procs
  (lambda (sym hook)
    (let ((procs (custom-hook-procs sym hook)))
      (map (lambda (proc)
	     (proc))
	   procs))))

(define-record 'custom-rec
  '((sym     #f)
    (default #f)
    (groups  ())
    (type    ())
    (label   "")
    (desc    "")))

(define custom-rec
  (lambda (sym)
    (assq sym custom-rec-alist)))

;; TODO: rewrite test for overwriting and 'main' subgroup
;; API
(define define-custom
  (lambda (sym default groups type label desc)
    (let* ((primary-grp (car groups))
	   (subgrps (if (null? (cdr groups))
			'(main)
			(cdr groups)))
	   (modified-groups (cons primary-grp subgrps))
	   (crec (custom-rec-new sym default modified-groups type label desc)))
      ;; See also require-custom for error handling TODO
      (for-each (lambda (gsym)
		  (or (custom-group-rec gsym)
		      (error (string-append "undefined group '"
					    (symbol->string gsym)
					    "' is referred by "
					    (symbol->string sym)))))
		modified-groups)
      (set! custom-rec-alist (alist-replace crec custom-rec-alist))
      (custom-call-hook-procs primary-grp custom-group-update-hooks)
      (if (not (symbol-bound? sym))
	  (let ((quoted-default (if (or (symbol? default)
					(list? default))
				    (list 'quote default)
				    default)))
	    (eval (list 'define sym quoted-default)
		  (interaction-environment))
	    (custom-set-value! sym default)))  ;; to apply hooks
      (for-each (lambda (subgrp)
		  (let ((registered (custom-group-subgroups primary-grp)))
		    (if (not (memq subgrp registered))
			(set! custom-subgroup-alist
			      (cons (cons primary-grp subgrp)
				    custom-subgroup-alist)))))
		subgrps))))

;; #f as type means 'any type'
(define custom-exist?
  (lambda (sym type)
    (and (assq sym custom-rec-alist)
	 (or (not type)
	     (eq? type
		  (custom-type sym))))))

;; API
(define custom-valid?
  (lambda (sym val)
    (let* ((type-name (custom-type sym))
	   (type-attrs (custom-type-attrs sym))
	   (valid? (symbol-value (cdr (assq type-name
					    custom-validator-alist)))))
      (apply valid? (cons val type-attrs)))))

;; API
(define custom-value
  (lambda (sym)
    (custom-call-hook-procs sym custom-get-hooks)
    (let ((val (symbol-value sym)))
      (if (custom-valid? sym val)
	  val
	  (custom-default-value sym)))))

;; TODO: rewrite test
;; API
(define custom-set-value!
  (lambda (sym val)
    (and (custom-valid? sym val)
	 (let* ((custom-syms (custom-collect-by-group #f))
		(map-activities (lambda ()
				  (map (lambda (pair)
					 ((cdr pair)))
				       custom-activity-hooks)))
		(pre-activities (map-activities)))
	   (set-symbol-value! sym val)
	   (if (eq? (custom-type sym)
		    'key)
	       (let ((key-val (custom-modify-key-predicate-names val)))
		 (eval (list 'define (symbol-append sym '?)
			     (list 'make-key-predicate (list 'quote key-val)))
		       (interaction-environment))))
	   (custom-call-hook-procs sym custom-set-hooks)
	   (custom-call-hook-procs sym custom-update-hooks)
	   (let ((post-activities (map-activities)))
	     (for-each (lambda (another-sym pre post)
			 (if (and (not (eq? another-sym sym))
				  (not (eq? (not pre)     ;; normalize bool
					    (not post)))) ;; normalize bool
			     (custom-call-hook-procs another-sym
						     custom-update-hooks)))
		       (map car custom-activity-hooks)
		       pre-activities
		       post-activities)
	     #t)))))

;; API
(define custom-touch-value!
  (lambda (sym)
    (custom-set-value! sym
		       (custom-value sym))))

(define custom-active?
  (lambda (sym)
    (let* ((procs (custom-hook-procs sym custom-activity-hooks))
	   (activities (map (lambda (proc)
			      (proc))
			    procs))
	   (active? (apply proc-and activities)))
      active?)))

;; API
(define custom-default?
  (lambda (sym)
    (equal? (symbol-value sym)
	    (custom-default-value sym))))

;; API
(define custom-default-value
  (lambda (sym)
    (custom-rec-default (custom-rec sym))))

;; API
(define custom-groups
  (lambda (sym)
    (custom-rec-groups (custom-rec sym))))

;; API
(define custom-type
  (lambda (sym)
    (car (custom-rec-type (custom-rec sym)))))

(define custom-type-attrs
  (lambda (sym)
    (let* ((crec (custom-rec sym))
	   (typedef (custom-rec-type crec)))
      (cdr typedef))))

;; TODO: write test
;; API for temporary solution
(define custom-type-info
  (lambda (sym)
    (custom-rec-type (custom-rec sym))))

;; TODO: write test
;; API for temporary solution
(define custom-set-type-info!
  (lambda (sym info)
    (custom-rec-set-type! (custom-rec sym)
			  info)
    (custom-call-hook-procs sym custom-update-hooks)))

;; API
(define custom-range
  (lambda (sym)
    (let* ((type (custom-type sym))
	   (attrs (custom-type-attrs sym)))
      (case type
	((choice ordered-list table)
	 (map custom-choice-rec-sym attrs))
	((integer string)
	 attrs)
	(else
	 ())))))

;; API
(define custom-label
  (lambda (sym)
    (custom-rec-label (custom-rec sym))))

;; API
(define custom-desc
  (lambda (sym)
    (custom-rec-desc (custom-rec sym))))

(define custom-list-as-literal
  (lambda (lst)
    (let ((canonicalized (map (lambda (elem)
				(cond
				 ((symbol? elem)
				  (symbol->string elem))
				 ((string? elem)
				  (string-escape elem))
				 (else
				  "")))
			      lst)))
      (string-append "'(" (string-join canonicalized " ") ")"))))

; (custom-list-as-table  '(("a" "b" "c") ("d" "e")))
;   -> "'((\"a\" \"b\" \"c\") (\"d\" \"e\"))"
(define custom-list-as-table
  (lambda (tbl)
    (string-append "'("
      (string-join
        (map (lambda (lst)
          (string-append "(" 
            (string-join
              (map (lambda (elem)
                (string-escape elem)) lst) " ") ")")) tbl) " ") ")")))

;; API
(define custom-value-as-literal
  (lambda (sym)
    (let ((val (custom-value sym))
	  (type (custom-type sym)))
      (cond
       ((eq? type 'integer)
	(number->string val))
       ((eq? type 'string)
	(string-escape val))
       ((eq? type 'pathname)
	(string-escape val))
       ((eq? type 'choice)
	(string-append "'" (symbol->string val)))
       ((or (eq? type 'ordered-list)
	    (eq? type 'key))
	(custom-list-as-literal val))
       ((eq? type 'table)
	(custom-list-as-table val))
       ((or (eq? val #f)
	    (eq? type 'boolean))
	(if (eq? val #f)
	    "#f"
	    "#t"))))))

;; Don't invoke this from a literalize-hook. It will cause infinite loop
(define custom-definition-as-literal
  (lambda (sym)
    (let ((var (symbol->string sym))
	  (val (custom-value-as-literal sym))
	  (hooked (custom-call-hook-procs sym custom-literalize-hooks)))
      (if (not (null? hooked))
	  (string-join hooked "\n")
	  (apply string-append
		 (append
		  (list "(define " var " " val ")")
		  (if (eq? (custom-type sym)
			   'key)
		      (let ((key-val (custom-list-as-literal
				      (custom-modify-key-predicate-names
				       (custom-value sym)))))
			(list "\n(define " var "? "
			      "(make-key-predicate " key-val "))"))
		      ())))))))

;; API
;; TODO: implement after uim 0.4.6 depending on scm-nested-eval
(define custom-broadcast-custom
  (lambda (sym)
  #f))

;; API
;; #f means 'any group'
;; TODO: support "AND" expression
(define custom-broadcast-customs
  (lambda (group)
    (let ((custom-syms (custom-collect-by-group group)))
      (for-each custom-broadcast-custom custom-syms))))

(define custom-register-cb
  (lambda (hook valid? custom-sym ptr gate-func func)
    (and (valid? custom-sym)
	 (let ((cb (lambda () (gate-func func ptr custom-sym))))
	   (custom-add-hook custom-sym hook cb)))))

;;
;; predefined subgroups
;;

(define-custom-group 'main
		     (N_ "-")
		     (N_ "Main settings of this group"))

(define-custom-group 'hidden
		     (N_ "Hidden settings")
		     (N_ "Hidden settings of this group. This group is invisible from uim_custom clients. Exists for internal variable management."))


(prealloc-heaps-for-heavy-job)
(custom-reload-customs)
