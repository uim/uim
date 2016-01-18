;;; ng-action.scm: Action handling framework for uim (next generation)
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

;; TODO:
;; - write test
;; - describe naming conventions and standard actions such as act_std_commit

(require-extension (srfi 2))

(require "util.scm")
(require "i18n.scm")
(require "event.scm")


;; CAUTION: Use ng-action-new, ng-action-indicate and action-actid
;; instead of action-new, action-indicate and action-id until legacy
;; action.scm has been obsoleted.  -- YamaKen 2005-07-08

;;
;; indication
;;

;; keep same as load-action.scm until legacy action.scm has been obsoleted
(define indication-rec-spec
  '((id           #f)    ;; must be first field. will be renamed to figure-id
    (iconic-label #f)    ;; utext or #f
    (label        #f)    ;; utext or #f
    (short-desc   #f)))  ;; utext or #f
(define-record 'indication indication-rec-spec)
(define indication-figure-id indication-id)
(define indication-set-figure-id! indication-set-id!)

(define label-indication-new
  (lambda (utext)
    (list 'none  ;; id
	  #f     ;; iconic-label
	  utext  ;; label
	  #f)))  ;; short-desc

(define internal-construct-indication-new
  (lambda (label)
    (indication-new 'null
		    ""
		    label
		    (N_ "*An internal construct*"))))

;;
;; action
;;

;; TODO: rename 'actid' to 'id'
(define action-skeleton-rec-spec
  '((actid         #f)  ;; must be first field
    (indicate-proc #f)
    (activate-proc #f)
    (precond-pred  #f)
    (status-proc   #f)))
(define-record 'action-skeleton action-skeleton-rec-spec)

;; TODO: rename ng-action-new to action-new
(define action-rec-spec
  '(append
    (owner #f)  ;; must be first field
    action-skeleton-rec-spec))
(define legacy-action-new (and (symbol-bound? 'action-new)
			       action-new))
(define-record 'action action-rec-spec)
(define ng-action-new action-new)
(define action-new legacy-action-new)

;; .returns Concrete action object
(define action-skeleton-bless
  (lambda (skeleton owner)
    (cons owner skeleton)))

;; .returns A bool value indicates succeeded or not
(define action-activate!
  (lambda (act)
    (and (action-ready? act)
	 (let ((proc (action-activate-proc act)))
	   (if proc
	       (proc act))
	   #t))))

(define action-ready?
  (lambda (act)
    (let ((pred (action-precond-pred act)))
      (or (not pred)
	  (pred act)))))

;; TODO: rename to action-indicate
(define ng-action-indicate
  (lambda (act)
    (let ((proc (action-indicate-proc act)))
      (if proc
	  (proc act)
	  std-indication-fallback))))

;; Provide visual status information for human about an action
;; intended to be used via chooser UI and so on.
;;
;; .returns A symbol indicates status of the action 'selected 'checked
;; or #f. 'selected indicates that the action is exclusively selected
;; from a related group of actions. 'checked indicates that the action
;; has some activity regardless of status of other actions. #f
;; indicates no status. The two symbols are expected to be visualized
;; as appropriate different figure.
(define action-status
  (lambda (act)
    (and-let* ((proc (action-status-proc act)))
      (proc act))))

;; usage: (action-status-encoder-selected anthy-direct-mode?)
(define action-status-encoder-selected
  (lambda (stat?)
    (lambda (act)
      (and stat?
	   (stat? act)
	   'selected))))

(define action-status-encoder-checked
  (lambda (stat?)
    (lambda (act)
      (and stat?
	   (stat? act)
	   'checked))))


;;
;; actionset
;;

;; actionset is an abstract action-skeleton repository formed as
;; (fetcher . opaque).
;;
;; usage: (actionset-new assq (list skeleton0 skeleton1 skeleton2))

(define actionset-fetcher car)
(define actionset-opaque cdr)

(define actionset-new
  (lambda (fetcher opaque)
    (if (procedure? fetcher)
	(cons fetcher opaque)
	(error "actionset-new: invalid fetcher"))))

(define actionset-fetch-action-skeleton
  (lambda (actset act-id)
    (and actset
	 ((actionset-fetcher actset) act-id (actionset-opaque actset)))))

(define actionset-fetch-action
  (lambda (actset owner act-id)
    (and-let* ((skeleton (actionset-fetch-action-skeleton actset act-id)))
      (action-skeleton-bless skeleton owner))))

(define actionset-handle-event
  (lambda (actset owner ev)
    (and actset
	 (case (event-type ev)
	   ((action)
	    (and-let* ((act-id (action-event-action-id ev))
		       (act (actionset-fetch-action actset owner act-id)))
	      (action-activate! act)))

	   (else
	    #f)))))


;;
;; standard definitions
;;

(define std-action-skeleton-new
  (lambda (act-id label short-desc activate! ready?)
    (action-skeleton-new act-id
			 (lambda (act)
			   (indication-new 'none
					   ""
					   label
					   short-desc))
			 activate!
			 ready?)))

(define std-indication-null
  (indication-new 'null
		  ""
		  ""
		  ""))

(define std-indication-fallback
  (indication-new 'unknown
		  (N_ "?")
		  (N_ "unknown")
		  (N_ "unknown")))

;; any UI should replace an indication that has 'separator as
;; indication-figure-id with real separator rather than label and icon
(define std-indication-separator
  (indication-new 'separator
		  (N_ "--")
		  (N_ "--------")
		  ""))

(define std-action-separator
  (action-skeleton-new 'act_separator
		       (lambda (act)
			 std-indication-separator)))
