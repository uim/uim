;;; evmap.scm: Multipurpose event mapper
;;;
;;; Copyright (c) 2004-2005 uim Project http://uim.freedesktop.org/
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

;; The evmap is designed to be used as table-based character composition, key
;; binding, key substitution, action binding and so on.
;;
;; Current implementation is using dumb expanded tree of event sequences as
;; internal representaion. In different to ordinary finite state machine (or
;; automaton), cyclic state transitions are intentionally limited in evmap to
;; achieve:
;;
;;   - merging multiple state machines into single one
;;   - represent mapping rules as simple table definition for user convenience
;;
;; Former issue means merging event sequences such as ("k" "a") and ("k" "k"
;; "o") into single tree ("k" ("a" ("k" "o"))). This tree representation
;; requirement will be withdrawn if sophisticated state machine compiler is
;; introduced. But I recommend that using the m17n library to create such
;; cyclic state machine based input method since the library has
;; well-experienced facility for such input methods.
;;
;; From the viewpoint of character composition feature, the evmap itself only
;; provides simple table-based character composition roughly as same layer as
;; traditional rk.scm although it can handle more complex composition rules
;; than rk. More complex features such as word or phrase-level editing will be
;; implemented in higher layer using the composer framework in cooperation
;; with evmap's own composer interface evmap-composer.scm.
;;
;; Don't blame me about efficiency or resource consumption of current
;; implementation. This is only a prototype. Interfaces, data formats and
;; behaviors will be changed without notice until next stable release.
;;   -- YamaKen 2005-02-18

(require "util.scm")
(require "ustr.scm")
(require "event.scm")
(require "ng-key.scm")


(define event-exp-list?
  (lambda (x)
    (or (pair? x)
	(null? x))))

;;
;; event expression
;;

(define event-exp-predicate-alist
  (list
   (cons 'press           key-event-press)
   (cons 'release         (compose not key-event-press))
   (cons 'autorepeat      key-event-autorepeat)
   (cons 'nonrepeat       (compose not key-event-autorepeat))
   (cons 'char-printable  (compose char-printable?  key-event-char))
   (cons 'char-graphic    (compose char-graphic?    key-event-char))
   (cons 'char-control    (compose char-control?    key-event-char))
   (cons 'char-numeric    (compose char-numeric?    key-event-char))
   (cons 'char-alphabetic (compose char-alphabetic? key-event-char))
   (cons 'char-upper-case (compose char-upper-case? key-event-char))
   (cons 'char-lower-case (compose char-lower-case? key-event-char))
   (cons 'char-vowel      (compose char-vowel?      key-event-char))
   (cons 'char-nonvowel   (compose not char-vowel?  key-event-char))
   (cons 'char-consonant  (compose char-consonant?  key-event-char))))

(define event-exp-directive-alist
  (list
   (cons 'consume         (lambda (ev) (event-set-consumed! ev #t) #t))
   (cons 'peek            (lambda (ev) (event-set-consumed! ev 'peek) #t))
   (cons 'loopback        (lambda (ev) (event-set-loopback! ev #t) #t))))

(define event-exp-predicate
  (lambda (sym)
    (or (assq-cdr sym event-exp-predicate-alist)
	(assq-cdr sym event-exp-directive-alist))))

;; #f means "don't care"
;; TODO: replace default value of modifier with #f to allow
;; overwriting by mod_None
(define-record 'event-exp-collector
  (list
   (list 'str        #f)        ;; precomposed string
   (list 'lkey       #f)        ;; logical keysym
   (list 'pkey       #f)        ;; physical keysym
   (list 'modifier   mod_None)  ;; set of modifiers
   (list 'predicates ())        ;; ordered list of predicates
   (list 'pred-alist ())))
(define event-exp-collector-new-internal event-exp-collector-new)

(define event-exp-collector-new
  (let ((pred-alist (append event-exp-predicate-alist
			    event-exp-directive-alist)))
    (lambda args
      (let ((evc (apply event-exp-collector-new-internal args)))
	(event-exp-collector-set-pred-alist! evc pred-alist)
	evc))))

(define event-exp-collector-find-predicate
  (lambda (evc sym)
    (assq-cdr sym (event-exp-collector-pred-alist evc))))

(define event-exp-collector-add-modifier!
  (lambda (evc mod)
    (let ((modifier (event-exp-collector-modifier evc)))
      (event-exp-collector-set-modifier! evc (bitwise-or mod modifier)))))

(define event-exp-collector-add-predicate!
  (lambda (evc pred)
    (let ((predicates (event-exp-collector-predicates evc)))
      (event-exp-collector-set-predicates! evc (cons pred predicates)))))

(define event-exp-collector-normalize-predicates!
  (lambda (evc)
    (let* ((pred-alist (event-exp-collector-pred-alist evc))
	   (predicates (event-exp-collector-predicates evc))
	   (normalized (or ;; fast path
			   (and (= (length predicates)
				   1)
				(find-tail (lambda (pair)
					     (eq? (car predicates)
						  (cdr pair)))
					   pred-alist)
				predicates)
			   ;; ordinary path
			   (filter-map (lambda (pair)
					 (let ((pred (cdr pair)))
					   (and (memq pred predicates)
						pred)))
				       pred-alist))))
      (event-exp-collector-set-predicates! evc normalized))))

;; returns normalized event-exp expression
(define event-exp-collector-exp
  (lambda (evc)
    (event-exp-collector-normalize-predicates! evc)
    (let* ((modifier (event-exp-collector-modifier evc))
	   (exp-list (remove not
			     (append!
			      (list
			       (event-exp-collector-str evc)
			       (event-exp-collector-lkey evc)
			       (event-exp-collector-pkey evc)
			       (and (not (= modifier 0))
				    modifier))
			      (event-exp-collector-predicates evc)))))
      (if (= (length exp-list)
	     1)
	  (car exp-list)
	  exp-list))))

(define event-exp-collector-fold-elem
  (let ((evc-error (lambda (msg)
		     (error (string-append "invalid event-exp expression: "
					   msg)))))
    (lambda (evc exp)
      (cond
       ((string? exp)
	(if (event-exp-collector-str evc)
	    (evc-error "duplicated str"))
	(event-exp-collector-set-str! evc exp))
       ((symbol? exp)
	(let ((pred-ent (assq exp (event-exp-collector-pred-alist evc))))
	  (cond
	   (pred-ent
	    (let ((match? (cdr pred-ent)))
	      (event-exp-collector-add-predicate! evc match?)))
	   ((modifier-symbol? exp)
	    (event-exp-collector-add-modifier! evc (symbol-value exp)))
	   ((logical-key? exp)
	    (if (event-exp-collector-lkey evc)
		(evc-error "duplicated logical key"))
	    (event-exp-collector-set-lkey! evc exp))
	   ((physical-key? exp)
	    (if (event-exp-collector-pkey evc)
		(evc-error "duplicated physical key"))
	    (event-exp-collector-set-pkey! evc exp))
	   (else
	    (evc-error (string-append "unknown symbol '" exp))))))
       ((pair? exp)
	(evc-error "invalid nested list"))
       (else
	(evc-error "invalid element")))
      evc)))

(define event-exp-collector-fold-internal
  (lambda (exp evc-creator)
    (let ((evc (evc-creator)))
      (fold (lambda (exp evc)
	      (event-exp-collector-fold-elem evc exp))
	    evc
	    (if (event-exp-list? exp)
		exp
		(list exp))))))

(define event-exp-collector-fold
  (lambda (exp)
    (event-exp-collector-fold-internal exp event-exp-collector-new)))


(define event-exp-add-elem
  (lambda (exp elem)
    (if (event-exp-list? exp)
	(cons elem exp)
	(list elem exp))))

(define event-exp-list-add-elem
  (lambda (exp-list elem)
    (map (lambda (exp)
	   (event-exp-add-elem exp elem))
	 exp-list)))

(define event-exp-has-elem?
  (lambda (exp elem)
    (if (event-exp-list? exp)
	(member elem exp)
	(equal? elem exp))))

(define event-exp-list-has-elem?
  (lambda (exp-list elem)
    (any (lambda (exp)
	   (event-exp-has-elem? exp elem))
	 exp-list)))

;; side effect: event-consumed of passed ev is modified when matched
;; and explicit directive 'consume' or 'peek' is specified. The
;; event-consumed field will not be modified if any other elements of
;; the event-exp do not match.
;;
;; TODO: distinguish key-event and others
(define event-exp-match?
  (lambda (exp ev)
    (let ((modifier-explicitly-matched? #f))
      (and (not (null? exp))
	   (every (lambda (elem)
		    (cond
		     ((string? elem)
		      (and (key-event-str ev)
			   (string=? (key-event-str ev)
				     elem)))
		     ;; modifier
		     ((integer? elem)
		      (set! modifier-explicitly-matched? #t)
		      (modifier-match? elem (key-event-modifier ev)))
		     ((symbol? elem)
		      (cond
		       ;; logical key
		       ((eq? (key-event-lkey ev)
			     elem)
			;;(logical-key? elem)  ;; already validated when parsed
			#t)
		       ;; physical key
		       ((eq? (key-event-pkey ev)
			     elem)
			;;(physical-key? elem)  ;; already validated when parsed
			#t)
		       (else
			#f)))
		     ((procedure? elem)
		      (elem ev))
		     (else
		      #f)))
		  (if (event-exp-list? exp)
		      exp
		      (list exp)))
	   (or modifier-explicitly-matched?
	       (= (key-event-modifier ev)
		  0))))))

;;
;; event expression macros
;;

;; macro expansion should be performed on rule rather than
;; event-exp-seq to allow flexible expressions.  -- YamaKen 2005-02-23

;; abbreviation of press-release macro
(define event-exp-implicit-macro?
  (let ((implicit-macro-body? (lambda (elem)
				(or (string? elem)
				    (logical-key? elem)
				    (physical-key? elem)))))
    (lambda (exp)
      (if (pair? exp)
	  (and (not (memq 'press exp))
	       (not (memq 'release exp))
	       (not (assq (car exp) event-exp-macro-alist))
	       (find implicit-macro-body? exp))
	  (implicit-macro-body? exp)))))

(define event-exp-formal-macro?
  (lambda (exp)
    (let ((macro-sym (safe-car exp)))
      (and macro-sym
	   (symbol? macro-sym)
	   (assq-cdr macro-sym event-exp-macro-alist)))))

(define event-exp-macro?
  (lambda (exp)
    (or (event-exp-implicit-macro? exp)
	(event-exp-formal-macro? exp))))

;; 'press-release' macro
;; Collects corresponding release edge of the key. Default behavior.
(define event-exp-expand-macro-press-release
  (lambda (exp-list)
    (list
     ;; Composed character should be appeared on pressing key
     ;;(list 
     ;;  (event-exp-add-elem exp-list 'press))
     ;; Composed character must not be disappeared on releasing
     ;; key. These duplicated action mapping prevents mapping
     ;; ordinary named action instead of characters. This problem
     ;; should be resolved by rule-based macro expansion which
     ;; replacing current event-exp-seq based one.
     (list
      (event-exp-add-elem exp-list 'press)
      (event-exp-add-elem exp-list 'release)))))

;; 'set' macro
(define event-exp-expand-macro-set
  (lambda (exp-list)
    (cond
     ((null? exp-list)
      ())
     ((= (length exp-list)
	 1)
      (list exp-list))
     (else
      (append-map (lambda (exp)
		    (let* ((others (delete exp exp-list equal?))
			   (expandeds (event-exp-expand-macro-set others)))
		      (map (lambda (expanded)
			     (cons exp expanded))
			   expandeds)))
		  exp-list)))))

;; 'ordered-chord' macro
;;
;; Since current implementation produces explosional (length exp-list)!
;; rules, arguments of ordered-chord keys should be limited to 3, which
;; produces 6 rules. 4 keys produces 24 rules, and 5 keys produces 120
;; rules. This rule explosion problem will be resolved by dynamic state
;; transition and ruleset composer(merger) that aware of the dynamic
;; transition.  -- YamaKen 2005-02-15
;;
;; TODO: ensure that exp-list does not contain 'press' and 'release'
(define event-exp-expand-macro-ordered-chord
  (lambda (exp-list)
    (if (or (null? exp-list)
	    (event-exp-list-has-elem? exp-list 'press)
	    (event-exp-list-has-elem? exp-list 'release))
	()
	(let* ((presses (event-exp-list-add-elem exp-list 'press))
	       (releases (event-exp-list-add-elem exp-list 'release))
	       (release-seqs (event-exp-expand-macro-set releases)))
	  (map (lambda (release-seq)
		 (append presses release-seq))
	       release-seqs)))))

;; 'chord' macro
;;
;; Since current implementation produces explosional ((length exp-list)! ^ 2)
;; rules, arguments of chord keys should be limited to 2, which produces 4
;; rules. 3 keys produces 36 rules, and 4 keys produces 576 rules. This rule
;; explosion problem will be resolved by dynamic state transition and ruleset
;; composer(merger) that aware of the dynamic transition.
;;   -- YamaKen  2005-02-15
;;
;; TODO: ensure that exp-list does not contain 'press' and 'release'
(define event-exp-expand-macro-chord
  (lambda (exp-list)
    (if (or (null? exp-list)
	    (event-exp-list-has-elem? exp-list 'press)
	    (event-exp-list-has-elem? exp-list 'release))
	()
	(let* ((presses (event-exp-list-add-elem exp-list 'press))
	       (releases (event-exp-list-add-elem exp-list 'release))
	       (release-seqs (event-exp-expand-macro-set releases)))
	  (append-map (lambda (press-seq)
			(map (lambda (release-seq)
			       (append press-seq release-seq))
			     release-seqs))
		      (event-exp-expand-macro-set presses))))))

;; press-release, set, and ordered-chord are very bad name. should be
;; replaced with short and meaningful names.
(define event-exp-macro-alist
  (list
   (cons 'press-release event-exp-expand-macro-press-release)
   (cons 'set           event-exp-expand-macro-set)
   (cons 'ordered-chord event-exp-expand-macro-ordered-chord)
   (cons 'chord         event-exp-expand-macro-chord)
   ;;(cons 'interval      event-exp-expand-macro-interval)
   ))

;;
;; event expression sequence
;;

;; returns list of ev-exps
(define event-exp-list-expand-macro
  (lambda (ev-exps parsed)
    (if (null? ev-exps)
	(list (reverse parsed))
	(let ((exp (car ev-exps))
	      (rest (cdr ev-exps)))
	  (cond
	   ;; fast path for implicit press-release macro
	   ((event-exp-implicit-macro? exp)
	    (let ((expanded (car (event-exp-expand-macro-press-release exp))))
	      (event-exp-list-expand-macro
	       rest
	       (append-reverse expanded parsed))))
	   ;; ordinary macros
	   ((event-exp-formal-macro? exp)
	    (let* ((macro-sym (car exp))
		   (macro-args (cdr exp))
		   (macro (assq-cdr macro-sym event-exp-macro-alist)))
	      (append-map (lambda (expanded)
			    (event-exp-list-expand-macro
			     rest
			     (append-reverse expanded parsed)))
			  (macro macro-args))))
	   ;; AND expression, other simple elements
	   (else
	    (event-exp-list-expand-macro rest (cons exp parsed))))))))

(define event-exp-list-expand-macro
  (lambda (ev-exps parsed)
    (if (null? ev-exps)
	(list (concatenate (reverse parsed)))
	(let ((exp (car ev-exps))
	      (rest (cdr ev-exps)))
	  (cond
	   ;; fast path for implicit press-release macro
	   ((event-exp-implicit-macro? exp)
	    (let ((expanded (car (event-exp-expand-macro-press-release exp))))
	      (event-exp-list-expand-macro
	       rest
	       (cons expanded parsed))))
	   ;; ordinary macros
	   ((event-exp-formal-macro? exp)
	    (let* ((macro-sym (car exp))
		   (macro-args (cdr exp))
		   (macro (assq-cdr macro-sym event-exp-macro-alist)))
	      (append-map (lambda (expanded)
			    (event-exp-list-expand-macro
			     rest
			     (cons expanded parsed)))
			  (macro macro-args))))
	   ;; AND expression, other simple elements
	   (else
	    (event-exp-list-expand-macro rest (cons (list exp) parsed))))))))

;; returns list of ev-exps
(define event-exp-seq-parse
  (let* ((list-canonicalize (compose event-exp-collector-exp
				     event-exp-collector-fold))
	 (canonicalize (lambda (exp)
			 (cond
			  ;; fast path for simple press-release elements
			  ((and (pair? exp)
				(= (length exp)
				   2)
				(memq (car exp)
				      '(press release))
				(string? (cadr exp)))
			   (list (cadr exp)
				 (event-exp-predicate (car exp))))
			  ;; other expressions
			  (else
			   (list-canonicalize exp))))))
    (lambda (ev-exp-seq)
      (let ((expandeds (event-exp-list-expand-macro ev-exp-seq ())))
	(map (lambda (expanded)
	       (map canonicalize expanded))
	     expandeds)))))

;;
;; action expressions
;;

;; - an event is interpreted as implicit commit-event action
;; - event can be expressed as event-exp
;; - positional matched event references $1, $2, ... $9 are available
;; - raw string as action such as "a" represents commit-event
;; - raw string list as action such as '("a" "b") represents predit-event
;; - an action-id
;;
;; special directives:
;;   - loopback
;;   - return
;;   - char-upcase
;;   - char-downcase

(define list-copy!
  (lambda (dst src)
    (let self ((rest-dst dst)
	       (rest-src src))
      (if (or (null? rest-dst)
	      (null? rest-src))
	  dst
	  (begin
	    (set-car! rest-dst (car rest-src))
	    (self (cdr rest-dst)
		  (cdr rest-src)))))))

(define action-exp-directive-positional-var
  (lambda (pos)
    (lambda (emc ev)
      (list-copy! ev (evmap-context-positional-var emc pos))
      #t)))

;; These directives are processed at first of action extraction
(define action-exp-preprocess-directive-alist
  (list
   ;; Positional variable references
   (cons '$1            (action-exp-directive-positional-var 1))
   (cons '$2            (action-exp-directive-positional-var 2))
   (cons '$3            (action-exp-directive-positional-var 3))
   (cons '$4            (action-exp-directive-positional-var 4))
   (cons '$5            (action-exp-directive-positional-var 5))
   (cons '$6            (action-exp-directive-positional-var 6))
   (cons '$7            (action-exp-directive-positional-var 7))
   (cons '$8            (action-exp-directive-positional-var 8))
   (cons '$9            (action-exp-directive-positional-var 9))))

(define action-exp-postprocess-directive-alist
  (list
   (cons 'press         (lambda (emc ev) (key-event-set-press! ev #t) #t))
   (cons 'release       (lambda (emc ev) (key-event-set-press! ev #f) #t))
   (cons 'autorepeat    (lambda (emc ev) (key-event-set-autorepeat! ev #t) #t))
   (cons 'nonrepeat     (lambda (emc ev) (key-event-set-autorepeat! ev #t) #t))
   (cons 'char-upcase   (lambda (emc ev) (key-event-char-upcase! ev) #t))
   (cons 'char-downcase (lambda (emc ev) (key-event-char-downcase! ev) #t))

   (cons 'loopback      (lambda (emc ev) (event-set-loopback! ev #t) #t))
   (cons 'return        (lambda (emc ev) #t))))

(define action-exp-directive
  (lambda (sym)
    (or (assq-cdr sym action-exp-preprocess-directive-alist)
	(assq-cdr sym action-exp-postprocess-directive-alist))))

(define action-exp-collector-new
  (let ((directive-alist (append action-exp-preprocess-directive-alist
				 action-exp-postprocess-directive-alist)))
    (lambda args
      (let ((actc (apply event-exp-collector-new args)))
	(event-exp-collector-set-pred-alist! actc directive-alist)
	actc))))

(define action-exp-collector-fold
  (lambda (exp)
    (event-exp-collector-fold-internal exp action-exp-collector-new)))

(define action-exp-seq-parse
  (let ((action-symbol? (lambda (sym)
			  (and (symbol? sym)
			       (string-prefix? "action_"
					       (symbol->string sym))
			       sym)))
	(canonicalize (compose event-exp-collector-exp
			       action-exp-collector-fold)))
    (lambda (act-exps)
      (map (lambda (exp)
	     (or (and (string? exp)
		      exp)
		 (action-symbol? exp)
		 (canonicalize exp)))
	   act-exps))))

;; presumes normalized
;; TODO:
;;   - support named action (e.g. action_anthy_hiragana)
;;   - support alternative actions ('alt' macro) to express candidates
;;   - support arbitrary event object construction
(define action-exp-seq-extract
  (let* ((member-proc? (lambda (proc alist)
			 (find (lambda (pair)
				 (eq? proc (cdr pair)))
			       alist)))
	(preproc? (lambda (proc)
		    (member-proc? proc
				  action-exp-preprocess-directive-alist)))
	(extract-exp
	 (lambda (act-exp emc)
	   (cond
	    ((or (string? act-exp)
		 (symbol? act-exp)
		 (integer? act-exp))
	     act-exp)
	    ((procedure? act-exp)
	     (let ((ev (key-event-new)))
	       (act-exp emc ev)
	       ev))
	    ((pair? act-exp)
	     (let* ((ev (key-event-new))
		    (pre-procs (filter preproc? act-exp))
		    (rest-elems (remove (lambda (elem)
					  (memq elem pre-procs))
					act-exp)))
	       (for-each (lambda (preproc)
			   (preproc emc ev))
			 pre-procs)
	       (for-each (lambda (elem)
			   (cond
			    ((string? elem)
			     (key-event-set-str! ev elem))
			    ((integer? elem)
			     (key-event-set-modifier! ev elem))
			    ((procedure? elem)
			     (elem emc ev))
			    ((symbol? elem)
			     (cond
			      ((logical-key? elem)
			       (key-event-set-lkey! ev elem))
			      ((physical-key? elem)
			       (key-event-set-pkey! ev elem))
			      (else
			       (error "invalid symbol in action expression"))))
			    (else
			     (error "invalid element in action expression"))))
			 rest-elems)
	       ev))
	    (else
	     (error "invalid element in action expression"))))))
    (lambda (act-exps emc)
      (cond
       ((not act-exps)
	#f)
       ((pair? act-exps)
	(filter-map (lambda (act-exps)
		      (extract-exp act-exps emc))
		    act-exps))
       (else
	(extract-exp act-exps emc))))))

;;
;; evmap-tree
;;

(define-record 'evmap-rule
  '((event-seq  ())
    (action-seq ())))

;; internal representation
(define-record 'evmap-tree
  '((event      #f)
    (action-seq #f)
    (branches   ())))  ;; list of nodes

(define evmap-tree-leaf?
  (lambda (tree)
    (null? (evmap-tree-branches tree))))

(define evmap-tree-node?
  (lambda (tree)
    (not (evmap-tree-leaf? tree))))

;; API
;; returns branches
(define evmap-tree-find-branches
  (lambda args
    (let ((tree (car args))
	  (ev (cadr args))
	  (ev=? (if (null? (cddr args))
		    event-exp-match?
		    (car (cddr args)))))
      (find-tail (lambda (child)
		   (ev=? (evmap-tree-event child) ev))
		 (evmap-tree-branches tree)))))

(define evmap-tree-insert-node!
  (lambda (tree node)
    (evmap-tree-set-branches! tree
			      (cons node (evmap-tree-branches tree)))
    node))

;; presumes normalized
(define evmap-tree-insert-rule!
  (lambda (tree ev-exps act-exps)
    (if (null? ev-exps)
	(error "invalid null event expression in rule")
	(let* ((ev-exp (car ev-exps))
	       (rest (cdr ev-exps))
	       (child (or (safe-car (evmap-tree-find-branches tree
							      ev-exp
							      equal?))
			  (evmap-tree-insert-node! tree
						   (evmap-tree-new ev-exp)))))
	  (if (null? rest)
	      (evmap-tree-set-action-seq! child act-exps)
	      (evmap-tree-insert-rule! child rest act-exps))))))

;; simple but slower
;;(define evmap-tree-insert-rule!
;;  (lambda (tree ev-exps act-exps)
;;    (if (null? ev-exps)
;;	(error "invalid null event expression in rule")
;;	(evmap-tree-set-action-seq!
;;	 (fold (lambda (ev-exp node)
;;		 (or (safe-car (evmap-tree-find-branches node ev-exp equal?))
;;		     (evmap-tree-insert-node! node
;;					      (evmap-tree-new ev-exp))))
;;	       tree
;;	       ev-exps)
;;	 act-exps))))

;; API
;; returns evmap-tree
(define evmap-parse-ruleset
  (lambda (ruleset)
    (let ((tree (evmap-tree-new)))
      (for-each (lambda (rule)
		  (let ((ev-seq-list (event-exp-seq-parse
				      (evmap-rule-event-seq rule)))
			(act-seq (action-exp-seq-parse
				  (evmap-rule-action-seq rule))))
		    (for-each (lambda (ev-seq)
				(evmap-tree-insert-rule! tree ev-seq act-seq))
			      ev-seq-list)))
		ruleset)
      tree)))

(define rk-rule->evmap-ruleset
  (lambda (rk-rule)
    (map (lambda (pair)
	   (let ((seq (caar pair))
		 (composed (cadr pair)))
	     (print
	      (list seq
		    (if (string? (car composed))
			composed
			(apply zip composed))))))
	 rk-rule)
    #f))

;;
;; evmap-context
;;

(define ustr-end-elem
  (lambda (ustr)
    (and (not (ustr-empty? ustr))
	 (ustr-nth ustr (- (ustr-length ustr)
			   1)))))

(define-record 'evmap-context
  '((root #f)
    (seq  #f)))  ;; ustr of evmap-tree
(define evmap-context-new-internal evmap-context-new)

(define evmap-context-new
  (lambda (ruletree)
    (evmap-context-new-internal ruletree (ustr-new))))

(define evmap-context-flush!
  (lambda (emc)
    (ustr-clear! (evmap-context-seq emc))))

(define evmap-context-current-tree
  (lambda (emc)
    (let ((seq (evmap-context-seq emc)))
      (ustr-end-elem seq))))

;; TODO: write test
(define evmap-context-initial?
  (lambda (emc)
    (ustr-empty? (evmap-context-seq emc))))

(define evmap-context-complete?
  (lambda (emc)
    (let ((current-tree (evmap-context-current-tree emc)))
      (and current-tree
	   (evmap-tree-leaf? current-tree)))))

(define evmap-context-partial?
  (lambda (emc)
    (let ((current-tree (evmap-context-current-tree emc)))
      (and current-tree
	   (evmap-tree-node? current-tree)
	   (evmap-tree-action-seq current-tree)))))

;;(define evmap-context-terminate!
;;  (lambda (emc)
;;    (let ((composed-str (evmap-context-composed-string emc)))
;;      (evmap-context-flush! emc)
;;      composed-str)))

;;(define evmap-context-commit!
;;  (lambda (emc)
;;    (let ((commit-str (evmap-context-composed-string emc)))
;;      (ustr-clear-former! (evmap-context-seq emc))
;;      commit-str)))

(define evmap-context-event-seq
  (lambda (emc)
    (let ((seq (evmap-context-seq emc)))
      (map-ustr-whole evmap-tree-event seq))))

;; returns string list
(define evmap-context-event-seq-string
  (lambda (emc)
    (filter-map key-event-extract-press-str
		(evmap-context-event-seq emc))))

;; TODO: write test
(define evmap-context-action-seq
  (lambda (emc)
    (let* ((tree (evmap-context-current-tree emc))
	   (ev (and tree
		    (evmap-tree-event tree))))
      (and tree
	   (evmap-tree-action-seq tree)))))

;; returns string list
;; can be used as rk-peek-terminal-match
(define evmap-context-composed-string
  (lambda (emc)
    (let ((tree (evmap-context-current-tree emc)))
      (and tree
	   (let* ((act-seq (evmap-tree-action-seq tree))
		  (str-list (if (string? act-seq)
				(list act-seq)
				(filter string? act-seq))))
	     (and (not (null? str-list))
		  str-list))))))

(define evmap-context-preedit-string
  (lambda (emc)
    (or (evmap-context-composed-string emc)
	(evmap-context-event-seq-string emc))))

;; pos starts from 1
(define evmap-context-positional-var
  (lambda (emc pos)
    (let* ((seq (evmap-context-seq emc))
	   (tree (ustr-nth seq (- pos 1))))
      (evmap-tree-event tree))))

;; 'ev' may be consumed
;; returns closer-tree
(define evmap-context-input!
  (lambda (emc ev)
    (let* ((seq (evmap-context-seq emc))
	   (prev-tree (if (ustr-cursor-at-beginning? seq)
			  (evmap-context-root emc)
			  (ustr-cursor-backside seq)))
	   (closer-tree (safe-car (evmap-tree-find-branches prev-tree ev))))
      (and closer-tree
	   (let* ((peek (eq? (event-consumed ev)
			     'peek))
		  (branches (evmap-tree-branches closer-tree))
		  (substituted (evmap-tree-new (if peek
						   (key-event-new) ;; dummy
						   ev)
					       #f
					       branches))
		  (act-exps (evmap-tree-action-seq closer-tree)))
	     (ustr-insert-elem! seq substituted)
	     (evmap-tree-set-action-seq! substituted
					 (action-exp-seq-extract act-exps emc))
	     (event-set-consumed! ev (if peek #f #t))
	     closer-tree)))))

;; Current implementation only supports these undo behaviors.
;;
;; "ch"   -> backspace -> "c"
;; "ちゃ" -> backspace -> "ch"
;;
;; TODO: Support following alternative undo behaviors.
;;
;; "ちゃ" -> backspace -> ""
;; "ちゃ" -> backspace -> "ち"
(define evmap-context-undo!
  (lambda (emc)
    (let undo ((seq (evmap-context-seq emc)))
      (cond
       ((ustr-cursor-at-beginning? seq)
	#f)
       ;; remove until most recent press
       ((key-event-extract-press-str (car (ustr-cursor-backside seq)))
	(ustr-cursor-delete-backside! seq)
	#t)
       (else
	(ustr-cursor-delete-backside! seq)
	(undo seq))))))
