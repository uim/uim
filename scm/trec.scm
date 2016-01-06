;;; trec.scm: A tree-based generic recursive sequence-to-sequence mapper
;;;
;;; Copyright (c) 2005-2013 uim Project https://github.com/uim/uim
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
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS
;;; IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
;;; THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;;; PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR
;;; CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;;; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
;;; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
;;; OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
;;; ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;;

;; trec (pronounced as 'trek') is a tree (in exactly, de la Briandais trie)
;; -based generic sequence-to-sequence mapper. It maps an arbitrary abstract
;; object sequence to another arbitrary object sequence, based on an
;; user-defined ruleset.
;;
;; This file only provides the generic data structure and algorithms. To use
;; trec as a concrete character composition engine, some supplemental parts
;; such as event handlings and preedit string extractor are
;; required. trec-composer.scm will provide them.
;;
;; The name 'trec' comes from:
;; - TREe-based RECursive Converter
;; - The meaning and pronunciation of the word 'trek'

(require-extension (srfi 1 2 8 23))

(require "util.scm")

;; Resource-consumption sensitive environment may enable only deterministic
;; search. This variable only affects on startup-time.
(or (symbol-bound? 'trec-enable-reroutable-search?)
    (eval '(define trec-enable-reroutable-search? #f)
	  (interaction-environment)))


;;
;; generic utilities
;;

(define last-pair?
  (lambda (lst)
    (null? (cdr lst))))

(define find-map
  (lambda (f lst)
    (and (not (null? lst))
	 (or (f (car lst))
	     (find-map f (cdr lst))))))
  
(define remove-once
  (lambda (pred lst)
    (receive (head tail) (break pred lst)
      (if (null? tail)
	  lst
	  (append head (cdr tail))))))


;;
;; trec-rule
;;

;; (path . value)
(define trec-rule-path car)
(define trec-rule-value cdr)
(define trec-rule-new cons)

(define trec-vnode-directive? vector?)

;; .parameter ruleset A list of trec-rule
;; .parameter backward-match Bool value indicates that the ruletree shall be
;; built for backward-match
;; .returns A trec-node as a compiled ruletree
(define trec-parse-ruleset
  (lambda (key=? backward-match ruleset)
    (let ((root (trec-node-new)))
      (trec-node-merge-ruleset! root key=? backward-match ruleset)
      (if (trec-node-val root)
	  (error "root node cannot hold value")
      root))))


;;
;; trec-node
;;

(define TREC-NULL-KEY #f)
(define TREC-NULL-VALUE #f)

(define trec-node-rec-spec
  '((key #f)
    (val #f)
    ;; . (branches ())
    ))
(define-record 'trec-node trec-node-rec-spec)
(define trec-node-key car)   ;; optimization
(define trec-node-val cadr)  ;; optimization
(define trec-node-branches cddr)
(define trec-node-set-branches!
  (lambda (node new-branches)
    (set-cdr! (cdr node) new-branches)))

(define trec-vnode? procedure?)

(define trec-node-root?
  (lambda (node)
    (and (not (trec-node-key node))
	 (not (trec-node-val node)))))

(define trec-node-leaf?
  (compose null? trec-node-branches))

;; TODO: return 'branch' only
(define trec-node-insert-branch!
  (lambda (node branch)
    (let ((new-branches (cons branch (trec-node-branches node))))
      (trec-node-set-branches! node new-branches)
      new-branches)))

;; TODO: follow the return value change of trec-node-insert-branch!
(define trec-node-descend!
  (lambda (node key=? key)
    (or (find (lambda (branch)
		(key=? (trec-node-key branch) key))
	      (trec-node-branches node))
	(car (trec-node-insert-branch! node (trec-node-new key))))))

(define trec-node-merge-rule!
  (lambda (node key=? backward-match rule)
    (let* ((path (if backward-match
		     (reverse (trec-rule-path rule))
		     (trec-rule-path rule)))
	   (val (trec-rule-value rule))
	   (descend! (lambda (keys cur-node)
		       (if (trec-vnode-directive? (cdr keys))
			   (let ((vnode (trec-vnode-new
					 (cdr keys) (car keys) val)))
			     (trec-node-insert-branch! cur-node vnode)
			     #f)
			   (trec-node-descend! cur-node key=? (car keys)))))
	   (leaf (pair-fold descend! node path)))
      (if leaf
	  (trec-node-set-val! leaf val))
      node)))

;; TODO: Backtrack from the leaf of previous rule to reduce full search
(define trec-node-merge-ruleset!
  (lambda (node key=? backward-match ruleset)
    (let ((merge! (lambda (rule node)
		    (trec-node-merge-rule! node key=? backward-match rule))))
      (fold merge! node ruleset))))


;;
;; trec-route
;;

(define trec-route-new
  (lambda (initial-node)
    (if trec-enable-reroutable-search?
	(list (list initial-node))
	(list initial-node))))

;; 'route point' is each route element backtrackable to
(define trec-route-point-node
  (if trec-enable-reroutable-search?
      car
      values))

(define trec-route-last-node
  ;;(compose trec-route-point-node car))
  ;; optimzation
  (if trec-enable-reroutable-search?
      caar
      car))

(define trec-route-initial? last-pair?)

(define trec-route-initial last-pair)

;; root node may be appeared more than once in a route, as a result of
;; recursive joining
(define trec-route-root?
  (compose trec-node-root? trec-route-last-node))

(define trec-route-last-root
  (lambda (route)
    (find-tail trec-node-root? route)))

(define trec-route-goal?
  (compose trec-node-leaf? trec-route-last-node))

(define trec-route-next-descendants
  (compose trec-node-branches trec-route-last-node))

(define trec-route-last-key
  (compose trec-node-key trec-route-last-node))

(define trec-route-nth-key
  (lambda (route idx interesting-key?)
    (let* ((keys (trec-route-filter-keys route interesting-key?))
	   (len (length keys)))
      (list-ref keys (compensate-index idx len)))))

(define trec-route-keys
  (let ((not-root-key? values))
    (lambda (route)
      (trec-route-filter-keys route not-root-key?))))

(define trec-route-filter-keys
  (lambda (route pred)
    (let ((f (lambda (rest)
	       (let ((key (trec-route-last-key rest)))
		 (and (pred key)
		      key)))))
      (filter-map-trec-route f route))))

(define trec-route-value
  (compose trec-node-val trec-route-last-node))

(define trec-route-values
  (lambda (route)
    (let ((f (lambda (rest)
	       (or (and (trec-route-root? rest)
			(not (last-pair? rest))
			(trec-route-value (cdr rest)))
		   (and (eq? rest route)
			(trec-route-value rest))))))
      (filter-map-trec-route f route))))

;; .returns (new-route . rejected-keys)
(define trec-route-advance
  (lambda (route router key)
    (router route (trec-route-next-descendants route) key)))

;; .returns (new-route . rejected-keys)
(define trec-route-route
  (lambda (route router keys)
    (or (and-let* (((not (null? keys)))
		   (rt.rej (trec-route-advance route router (car keys)))
		   ((null? (cdr rt.rej))))  ;; successfully consumed the key
	  (trec-route-route (car rt.rej) router (cdr keys)))
	(cons route keys))))

(define trec-route-backtrack
  (lambda (route)
    (if (trec-route-initial? route)
	(cons route #f)
	(cons (cdr route)
	      (trec-route-last-key route)))))

(define filter-map-trec-route
  (lambda (f route)
    (pair-fold (lambda (rest filtered)
		 (let ((mapped (f rest)))
		   (or (and mapped
			    (cons mapped filtered))
		       filtered)))
	       ()
	       route)))


;;
;; trec-context
;;

(define trec-context-rec-spec
  '((route ())))
(define-record 'trec-context trec-context-rec-spec)

(define trec-context-initial? (compose trec-route-initial? trec-context-route))
(define trec-context-goal?    (compose trec-route-goal?    trec-context-route))
(define trec-context-keys     (compose trec-route-keys     trec-context-route))
(define trec-context-values   (compose trec-route-values   trec-context-route))

(define trec-context-reset!
  (lambda (tc)
    (trec-context-set-route! tc (trec-route-initial (trec-context-route tc)))))

;; .returns rejected keys (null if no rejected keys), or #f if matching failed
(define trec-context-advance!
  (lambda (tc router key)
    (and-let* ((rt.rej (trec-route-advance (trec-context-route tc) router key)))
      (trec-context-set-route! tc (car rt.rej))
      (cdr rt.rej))))

;; .returns Retrieved key (not keys). #f if initial context
(define trec-context-backtrack!
  (lambda (tc)
    (let ((rt.rej (trec-route-backtrack (trec-context-route tc))))
      (trec-context-set-route! tc (car rt.rej))
      (cdr rt.rej))))


;;
;; route transition drivers (router)
;;

;; a router returns (route . rejected-keys) or #f

;; no vkey and vnode
(define trec-router-vanilla-advance-new
  (lambda (match?)
    (define advance
      (lambda (route cands key)
	(and (not (null? cands))
	     (let ((node (car cands))
		   (rest (cdr cands)))
	       (or (and (match? (trec-node-key node) key)
			(cons (cons (cons key (cdr node)) route)
			      ()))
		   (advance route rest key))))))
    advance))

(define trec-router-advance-with-fallback-new
  (lambda (base-router fallback-router)
    (lambda (route cands key)
      (or (base-router route cands key)
	  (fallback-router route cands key)))))

;; FIXME: rename appropriately
(define trec-make-node
  (lambda (node matched key)
    (cond
     ((eq? matched TREC-MATCHER-FIN)
      (cons key (cdr node)))
     ((eq? matched TREC-MATCHER-RETRY)
      (cons TREC-NULL-KEY (cdr node)))
     (else
      (let ((next-node (cons matched (cdr node))))
	(list key TREC-NULL-VALUE next-node))))))

;; FIXME: node
;; TODO: simplify
(define trec-router-std-advance-new
  (lambda (matcher)
    (define advance
      (lambda (route cands key)
	(and (not (null? cands))
	     (let ((node (car cands))
		   (rest (cdr cands)))
	       (or (if (trec-vnode? node)
		       (node advance route matcher key)
		       (and-let* ((matched (matcher (trec-node-key node) key))
				  (new-node (trec-make-node node matched key))
				  (advanced (cons (cons new-node rest)
						  route)))
			 (if (eq? matched TREC-MATCHER-RETRY)
			     (advance advanced
				      (trec-node-branches new-node) key)
			     (cons advanced ()))))
		   (advance route rest key))))))
    advance))


;;
;; key matchers
;;

;; A matcher returns:
;;
;; TREC-MATCHER-FIN   matched and the state transition has been finished
;; TREC-MATCHER-RETRY matched and finished, and instructs that
;;                    re-injecting of the last key for next key-matching
;; <others>           matched and transited to an intermediate state
;; #f                 unmatched

;; Dummy pairs are used to allocate unique ID without polluting namespace
;; of symbols, or value space of numbers.
(define TREC-MATCHER-FIN   (cons #f #f))
(define TREC-MATCHER-RETRY (cons #f #f))

(define trec-matcher-terminal-state
  (lambda (state)
    (safe-car (memq state (list TREC-MATCHER-FIN
				TREC-MATCHER-RETRY)))))

(define trec-vkey? procedure?)

(define trec-matcher-std-new
  (lambda (match?)
    (lambda (key-exp key)
      (if (trec-vkey? key-exp)
	  (key-exp key-exp key)
	  (and (match? key-exp key)
	       TREC-MATCHER-FIN)))))


;;
;; virtual keys
;;

;; TODO: simplify
;; .pre-condition matcher must be a trec-matcher-std
(define trec-vkey-keyset-new
  (lambda (matcher keyset)
    (lambda (dummy-key-exp key)
      (let retry ((rest-keys keyset))
	(and-let* ((transit (lambda (key-exp)
			      (matcher key-exp key)))
		   (matched (find-map transit rest-keys))
		   (rest-keys (remove-once matcher rest-keys)))
	  (or (and (trec-matcher-terminal-state matched)
		   (or (and (null? rest-keys)
			    matched)
		       (and (eq? matched TREC-MATCHER-FIN)
			    (trec-vkey-keyset-new matcher rest-keys))
		       (and (eq? matched TREC-MATCHER-RETRY)
			    (retry rest-keys))))
	      (let ((rest-vkey (trec-vkey-keyset-new matcher rest-keys)))
		(trec-vkey-keyseq-new matcher (list matched rest-vkey)))))))))

;; TODO: simplify
;; .pre-condition matcher must be a trec-matcher-std
(define trec-vkey-keyseq-new
  (lambda (matcher keyseq)
    (lambda (dummy-key-exp key)
      (let retry ((rest-seq keyseq))
	(and-let* ((key-exp (safe-car rest-seq))
		   (matched (matcher key-exp key))
		   (rest-seq (cdr rest-seq)))
	  (or (and (trec-matcher-terminal-state matched)
		   (or (and (null? rest-seq)
			    matched)
		       (and (eq? matched TREC-MATCHER-FIN)
			    (trec-vkey-keyseq-new matcher rest-seq))
		       (and (eq? matched TREC-MATCHER-RETRY)
			    (retry rest-seq))))
	      (trec-vkey-keyseq-new matcher (cons matched rest-seq))))))))


;;
;; virtual nodes
;;

(define trec-vnode-new
  (lambda (directive-vec rule-key rule-val)
    (let* ((directive (vector->list directive-vec))
	   (directive-sym (car directive))
	   (pregiven-keys (cdr directive))
	   (make-vnode (or (assq-cdr directive-sym trec-vnode-directive-alist)
			   (error "invalid vnode directive"))))
      (make-vnode pregiven-keys rule-key rule-val))))

;; TODO: simplify
(define trec-vnode-peek-new
  (lambda (pregiven-keys rule-key rule-val)
    (if (not (null? pregiven-keys))
	(error "'peek' does not take arguments"))
    (lambda (router route matcher key)
      (and-let* ((matched (matcher rule-key key)))
	(cond
	 ((eq? matched TREC-MATCHER-FIN)
	  (cons (cons (list TREC-NULL-KEY rule-val)
		      route)
		(list key)))
	 ((eq? matched TREC-MATCHER-RETRY)
	  (router (cons (list TREC-NULL-KEY rule-val)
			route)
		  ()
		  key))
	 (else
	  (let ((next-node (trec-vnode-peek-new pregiven-keys matched rule-val)))
	    (cons (cons (list TREC-NULL-KEY TREC-NULL-VALUE next-node)
			route)
		  (list key)))))))))

;; TODO: simplify
(define trec-make-vnode-recur-new
  (lambda (join retry)
    (define vnode-new
      (lambda (pregiven-keys rule-key rule-val)
	(lambda (router route matcher key)
	  (and-let* ((matched (matcher rule-key key)))
	    (if (not (trec-matcher-terminal-state matched))
		(let ((next-node (vnode-new matched rule-val)))
		  (cons (list key TREC-NULL-VALUE next-node)
			route))
		(let ((root (trec-route-last-root route))
		      (keys (if retry
				(append pregiven-keys (list key))
				pregiven-keys))
		      (node (cond
			     ((eq? matched TREC-MATCHER-FIN)
			      (list key rule-val))
			     ((eq? matched TREC-MATCHER-RETRY)
			      (list TREC-NULL-KEY rule-val)))))
		  (trec-route-route router
				    (if join
					(cons* root node route)
					root)
				    keys)))))))
    vnode-new))

(define trec-vnode-join-new        (trec-make-vnode-recur-new #f #f))
(define trec-vnode-join-retry-new  (trec-make-vnode-recur-new #f #t))
(define trec-vnode-recur-new       (trec-make-vnode-recur-new #t #f))
(define trec-vnode-recur-retry-new (trec-make-vnode-recur-new #t #t))

(define trec-vnode-directive-alist
  (list
   (cons 'peek        trec-vnode-peek-new)
   (cons 'join        trec-vnode-join-new)
   (cons 'join-retry  trec-vnode-join-retry-new)
   (cons 'recur       trec-vnode-recur-new)
   (cons 'recur-retry trec-vnode-recur-retry-new)))


(if trec-enable-reroutable-search?
    (require "trec-reroutable.scm"))
