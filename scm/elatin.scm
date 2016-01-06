;;; elatin.scm -- Emacs-style Latin characters translation
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

;; This input method implements character composition rules for the
;; Latin letters used in European languages.  The rules, defined in
;; the file elatin-rules.scm, have been adapted from GNU Emacs 23.1.

(require "util.scm")
(require "rk.scm")
(require "elatin-rules.scm")
(require-custom "generic-key-custom.scm")
(require-custom "elatin-custom.scm")

(define elatin-context-rec-spec
  (append
   context-rec-spec
   '((on?		  #f)
     (rkc		  ())
     (completions	  #f)
     (completion-no	  0)
     (jump-to-top-on-tab? #f)
     (translations	  #f)
     (translation-no	  0)
     (preedit		  ""))))
(define-record 'elatin-context elatin-context-rec-spec)
(define elatin-context-new-internal elatin-context-new)

(define (elatin-context-new id im)
  (let* ((lc (elatin-context-new-internal id im))
	 (rules (begin (or (symbol-bound? elatin-rules)
			   (begin (display "uim-elatin: [warning] ")
				  (write elatin-rules)
				  (display " unavailable, falling back on ")
				  (write elatin-default-rules) (newline)
				  (display "uim-elatin: [warning] use customization tool and fix the latin characters keyboard layout\n")
				  (custom-set-value!
				   'elatin-rules elatin-default-rules)))
		       (symbol-value elatin-rules)))
	 (rkc (rk-context-new rules #f #f)))
    (elatin-context-set-widgets! lc elatin-widgets)
    (elatin-context-set-rkc! lc rkc)
    lc))

(define (elatin-current-translations lc)
  (let ((rkc (elatin-context-rkc lc)))
    (or (rk-peek-terminal-match rkc)
	(and (not (null? (rk-context-seq rkc)))
	     (list (rk-pending rkc))))))

(define (elatin-close-window lc)
  (if (or (elatin-context-translations lc)
	  (elatin-context-completions lc))
      (begin (im-deactivate-candidate-selector lc)
	     (elatin-context-set-translations! lc #f)
	     (elatin-context-set-completions! lc #f))))

(define (elatin-context-reset lc)
  (rk-flush (elatin-context-rkc lc))
  (elatin-close-window lc))

(define (elatin-commit lc str)
  (elatin-close-window lc)
  (im-commit lc str))

(define (elatin-reset-and-commit lc str)
  (elatin-context-reset lc)
  (im-commit lc str))

(define (elatin-context-flush lc)
  (let ((trans (elatin-current-translations lc)))
    (elatin-context-reset lc)
    (if trans (im-commit lc (car trans)))))

(define (elatin-update-preedit lc)
  (let* ((trans (or (elatin-context-translations lc)
		    (elatin-current-translations lc)))
	 (new-preedit (if trans (car trans) "")))
    (or (string=? new-preedit (elatin-context-preedit lc))
	(begin (im-clear-preedit lc)
	       (or (string=? new-preedit "")
		   (im-pushback-preedit lc
					preedit-underline
					new-preedit))
	       (im-pushback-preedit lc
				    preedit-cursor
				    "")
	       (im-update-preedit lc)
	       (elatin-context-set-preedit! lc new-preedit)))))

(define (elatin-prepare-activation lc)
  (elatin-context-flush lc)
  (elatin-update-preedit lc))

(register-action 'action_elatin_off
		 (lambda (lc)
		   (list
		    'off
		    "a"
		    (N_ "ELatin mode off")
		    (N_ "ELatin composition off")))
		 (lambda (lc)
		   (not (elatin-context-on? lc)))
		 (lambda (lc)
		   (elatin-prepare-activation lc)
		   (elatin-context-set-on?! lc #f)))

(register-action 'action_elatin_on
		 (lambda (lc)
		   (list
		    'on
		    "à"
		    (N_ "ELatin mode on")
		    (N_ "ELatin composition on")))
		 (lambda (lc)
		   (elatin-context-on? lc))
		 (lambda (lc)
		   (elatin-prepare-activation lc)
		   (elatin-context-set-on?! lc #t)))

(define elatin-input-mode-actions
  '(action_elatin_off action_elatin_on))

(define elatin-widgets '(widget_elatin_input_mode))

(define default-widget_elatin_input_mode 'action_elatin_off)

(register-widget 'widget_elatin_input_mode
		 (activity-indicator-new elatin-input-mode-actions)
		 (actions-new elatin-input-mode-actions))

(define elatin-context-list '())

(define (elatin-init-handler id im arg)
  (let ((lc (elatin-context-new id im)))
    (set! elatin-context-list (cons lc elatin-context-list))
    lc))

(define (elatin-release-handler lc)
  (let ((rkc (elatin-context-rkc lc)))
    (set! elatin-context-list
	  ;; (delete lc elatin-context-list eq?) does not work
	  (remove (lambda (c) (eq? (elatin-context-rkc c) rkc))
		  elatin-context-list))))

(define (elatin-open-translations-window lc trans)
  (let ((ntrans (length trans)))
    (if (elatin-context-completions lc)
	(im-deactivate-candidate-selector lc))
    (elatin-context-set-translations! lc trans)
    (elatin-context-set-translation-no! lc 0)
    (im-activate-candidate-selector lc ntrans ntrans)
    (im-select-candidate lc 0)))

(define (elatin-set-completions lc matches)
  (let* ((seq (reverse (rk-context-seq (elatin-context-rkc lc))))
	 (completions (elatin-context-completions lc))
	 (completion-no (elatin-context-completion-no lc))
	 (n (or (list-index (lambda (elt) (equal? (caar elt) seq))
			    matches)
		(and completions
		     (let ((highlighted (list-ref completions completion-no)))
		       (list-index (lambda (elt) (eq? elt highlighted))
				   matches)))
		0)))
    (elatin-context-set-completions! lc matches)
    (elatin-context-set-completion-no! lc n)
    (elatin-context-set-jump-to-top-on-tab?! lc #t)))

(define (elatin-find-partial-matches seq rule)
  (let ((partials (rk-lib-find-partial-seqs seq rule))
	(full (rk-lib-find-seq seq rule)))
    (if full (cons full partials) partials)))

(define (elatin-update-completions lc)
  (if (elatin-context-completions lc)
      (let* ((rkc (elatin-context-rkc lc))
	     (seq (reverse (rk-context-seq rkc)))
	     (rule (rk-context-rule rkc))
	     (matches (elatin-find-partial-matches seq rule)))
	(elatin-set-completions lc matches)
	(or (elatin-context-translations lc)
	    (begin
	      (im-deactivate-candidate-selector lc)
	      (im-activate-candidate-selector
	       lc (length matches) elatin-nr-candidates-max)
	      (im-select-candidate lc (elatin-context-completion-no lc)))))))

(define (elatin-push-keys lc str-list)
  (let ((rkc (elatin-context-rkc lc)))
    (for-each (lambda (str) (rk-push-key! rkc str)) str-list)))

(define (elatin-do-rkc lc)
  (let ((rkc (elatin-context-rkc lc))
	(trans (elatin-current-translations lc)))
    (if (rk-partial? rkc)
	(begin (if (and trans (> (length trans) 1))
		   (elatin-open-translations-window lc trans))
	       (elatin-update-completions lc))
	(if (> (length trans) 1)
	    (begin (elatin-context-reset lc)
		   (elatin-open-translations-window lc trans))
	    (elatin-reset-and-commit lc (car trans))))))

(define (elatin-take-common-head matches)
  (let ((common '())
	(keys-list (map caar matches)))
    (let loop-horiz ((first (car keys-list)))
      (cond ((null? first)
	     (reverse common))
	    ((let loop-vert ((rest (cdr keys-list)))
	       (cond ((null? rest)
		      #t)
		     ((null? (car rest))
		      #f)
		     ((equal? (car first) (caar rest))
		      (set-car! rest (cdar rest))
		      (loop-vert (cdr rest)))
		     (else
		      #f)))
	     (set! common (cons (car first) common))
	     (loop-horiz (cdr first)))
	    (else
	     (reverse common))))))

(define (elatin-open-completions-window lc matches)
  (elatin-set-completions lc matches)
  (im-activate-candidate-selector lc (length matches) elatin-nr-candidates-max)
  (im-select-candidate lc (elatin-context-completion-no lc)))

(define (elatin-start-completion lc)
  (let* ((rkc (elatin-context-rkc lc))
	 (seq (reverse (rk-context-seq rkc)))
	 (lseq (length seq))
	 (rule (rk-context-rule rkc))
	 (matches (elatin-find-partial-matches seq rule)))
    (if (= (length matches) 1)
	(begin (elatin-push-keys lc (drop (caaar matches) lseq))
	       (elatin-do-rkc lc))
	(let ((common (elatin-take-common-head matches)))
	  (if (equal? common seq)
	      (elatin-open-completions-window lc matches)
	      (begin
		(elatin-push-keys lc (drop common lseq))
		(let ((trans (elatin-current-translations lc)))
		  (if (and elatin-show-all-if-ambiguous?
			   (not (and trans (> (length trans) 1)
				     (begin (elatin-set-completions lc matches)
					    #t))))
		      (elatin-open-completions-window lc matches)
		      (elatin-do-rkc lc)))))))))

(define (elatin-commit-completion lc)
  (let* ((highlighted (list-ref (elatin-context-completions lc)
				(elatin-context-completion-no lc)))
	 (trans (cadr highlighted)))
    (if (> (length trans) 1)
	(begin (elatin-context-reset lc)
	       (elatin-open-translations-window lc trans))
	(elatin-reset-and-commit lc (car trans)))))

(define (elatin-move-completion-highlight lc offset)
  (let* ((completions (elatin-context-completions lc))
	 (max (length completions))
	 (n (+ (elatin-context-completion-no lc) offset))
	 (compensated-n (cond
			 ((>= n max) 0)
			 ((< n 0) (- max 1))
			 (else n))))
    (elatin-context-set-completion-no! lc compensated-n)
    (im-select-candidate lc compensated-n)))

(define (elatin-move-translation-highlight lc offset)
  (let* ((translations (elatin-context-translations lc))
	 (max (length translations))
	 (n (+ (elatin-context-translation-no lc) offset))
	 (compensated-n (cond
			 ((>= n max) 0)
			 ((< n 0) (- max 1))
			 (else n))))
    (elatin-context-set-translation-no! lc compensated-n)
    (im-select-candidate lc compensated-n)))

(define elatin-control-key?
  (let ((shift-or-no-modifier? (make-key-predicate '("<Shift>" ""))))
    (lambda (key key-state)
      (not (shift-or-no-modifier? -1 key-state)))))

(define (elatin-cond-default lc key key-state)
  (let ((rkc (elatin-context-rkc lc))
	(cur-trans (elatin-current-translations lc)))
    (cond

     ((or (elatin-off-key? key key-state)
	  (and elatin-esc-turns-off? (eq? key 'escape)))
      (elatin-context-flush lc)
      (if (eq? key 'escape)
	  (im-commit-raw lc))
      (elatin-context-set-on?! lc #f))

     ((elatin-backspace-key? key key-state)
      (if (rk-backspace rkc)
	  (if (null? (rk-context-seq rkc))
	      (elatin-close-window lc)
	      (elatin-do-rkc lc))
	  (im-commit-raw lc)))

     ((elatin-control-key? key key-state)
      (elatin-context-flush lc)
      (im-commit-raw lc))

     (else
      (let* ((key-str (if (symbol? key)
			  (symbol->string key)
			  (charcode->string key)))
	     (cur-seq (rk-context-seq rkc))
	     (res (rk-push-key! rkc key-str))
	     (new-seq (rk-context-seq rkc)))
	(if (equal? new-seq (cons key-str cur-seq))
	    (elatin-do-rkc lc)
	    (begin (or (null? cur-seq)
		       (elatin-commit lc (car cur-trans)))
		   (if (null? new-seq) (im-commit-raw lc)))))))))

(define (elatin-proc-off-state lc key key-state)
  (if (elatin-on-key? key key-state)
      (elatin-context-set-on?! lc #t)
      (im-commit-raw lc)))

(define (elatin-proc-normal-state lc key key-state)
  (cond
   ((and elatin-use-completion?
	 (elatin-completion-key? key key-state)
	 (not (null? (rk-context-seq (elatin-context-rkc lc)))))
    (elatin-start-completion lc))
   (else
    (elatin-cond-default lc key key-state))))

(define (elatin-proc-completion-state lc key key-state)
  (let* ((rkc (elatin-context-rkc lc))
	 (completions (elatin-context-completions lc))
	 (ncompletions (length completions))
	 (completion-no (elatin-context-completion-no lc))
	 (jump? (elatin-context-jump-to-top-on-tab? lc)))

    (elatin-context-set-jump-to-top-on-tab?! lc #f)

    (cond

     ((elatin-completion-key? key key-state)
      (let* ((seq (reverse (rk-context-seq rkc)))
	     (lseq (length seq)))
	(if (= ncompletions 1)
	    (begin (elatin-push-keys lc (drop (caaar completions) lseq))
		   (elatin-do-rkc lc))
	    (let ((common (elatin-take-common-head completions)))
	      (if (equal? common seq)
		  (if (and jump? (>= completion-no elatin-nr-candidates-max))
		      (begin (elatin-context-set-completion-no! lc 0)
			     (im-select-candidate lc 0))
		      (im-shift-page-candidate lc #t))
		  (begin (elatin-push-keys lc (drop common lseq))
			 (elatin-do-rkc lc)))))))

     ((elatin-cancel-key? key key-state)
      (im-deactivate-candidate-selector lc)
      (elatin-context-set-completions! lc #f))
     ((elatin-prev-page-key? key key-state)
      (im-shift-page-candidate lc #f))
     ((elatin-next-page-key? key key-state)
      (im-shift-page-candidate lc #t))
     ((elatin-prev-candidate-key? key key-state)
      (elatin-move-completion-highlight lc -1))
     ((elatin-next-candidate-key? key key-state)
      (elatin-move-completion-highlight lc 1))
     ((elatin-commit-key? key key-state)
      (elatin-commit-completion lc))
     ((and (ichar-numeric? key)
	   (let* ((keyidx (- (numeric-ichar->integer key) 1))
		  (page (if (= elatin-nr-candidates-max 0)
			    0
			    (quotient completion-no elatin-nr-candidates-max)))
		  (idx (* page elatin-nr-candidates-max)))
	     (if (= keyidx -1) (set! keyidx 9))
	     (set! idx (+ idx keyidx))
	     (and (>= idx 0) (< idx ncompletions)
		  (begin (elatin-context-set-completion-no! lc idx)
			 (elatin-commit-completion lc)
			 #t)))))

     (else
      (elatin-cond-default lc key key-state)))))

(define (elatin-proc-translation-state lc key key-state)
  (let ((seq (rk-context-seq (elatin-context-rkc lc)))
	(trans (elatin-context-translations lc))
	(translation-no (elatin-context-translation-no lc)))

    (cond

     ((and elatin-use-completion?
	   (elatin-completion-key? key key-state)
	   (not (null? seq)))
      (elatin-context-set-translations! lc #f)
      (if (elatin-context-completions lc)
	  (elatin-update-completions lc)
	  (begin
	    (im-deactivate-candidate-selector lc)
	    (elatin-start-completion lc))))

     ((elatin-cancel-key? key key-state)
      (if (null? seq)
	  (elatin-commit lc (car trans))
	  (begin
	    (elatin-context-set-translations! lc #f)
	    (if (elatin-context-completions lc)
		(elatin-update-completions lc)
		(im-deactivate-candidate-selector lc)))))
     ((elatin-prev-page-key? key key-state)
      (im-shift-page-candidate lc #f))
     ((elatin-next-page-key? key key-state)
      (im-shift-page-candidate lc #t))
     ((elatin-prev-candidate-key? key key-state)
      (elatin-move-translation-highlight lc -1))
     ((elatin-next-candidate-key? key key-state)
      (elatin-move-translation-highlight lc 1))
     ((elatin-commit-key? key key-state)
      (elatin-reset-and-commit lc (list-ref trans translation-no)))
     ((and (ichar-numeric? key)
	   (let* ((keyidx (- (numeric-ichar->integer key) 1))
		  (page (if (= elatin-nr-candidates-max 0)
			    0
			    (quotient translation-no elatin-nr-candidates-max)))
		  (idx (* page elatin-nr-candidates-max)))
	     (if (= keyidx -1) (set! keyidx 9))
	     (set! idx (+ idx keyidx))
	     (and (>= idx 0) (< idx (length trans))
		  (begin (elatin-reset-and-commit lc (list-ref trans idx))
			 #t)))))

     (else
      (if (null? seq)
	  (elatin-commit lc (car trans))
	  (begin (elatin-context-set-translations! lc #f)
		 (or (elatin-context-completions lc)
		     (im-deactivate-candidate-selector lc))))
      (elatin-cond-default lc key key-state)))))

(define (elatin-key-press-handler lc key key-state)
  (if (elatin-context-on? lc)
      (cond
       ((elatin-context-translations lc)
	(elatin-proc-translation-state lc key key-state))
       ((elatin-context-completions lc)
	(elatin-proc-completion-state lc key key-state))
       (else
	(elatin-proc-normal-state lc key key-state)))
      (elatin-proc-off-state lc key key-state))
  (elatin-update-preedit lc))

(define (elatin-key-release-handler lc key key-state)
  ;; don't discard any key release event for apps
  (im-commit-raw lc))

(define (elatin-reset-handler lc)
  (elatin-context-reset lc))

(define (elatin-get-candidate-handler lc idx accel-enum-hint)
  (let ((candidates (or (elatin-context-translations lc)
			(elatin-context-completions lc))))
    (let* ((item (list-ref candidates idx))
	   (desc
	    (if (string? item)
		item
		(apply string-append
		       (append (caar item) '("  ")
			       (append-map (lambda (str) (list "  " str))
					   (cadr item))))))
	   (num (number->string (+ idx 1))))
      (list desc num ""))))

(define (elatin-set-candidate-index-handler lc idx)
  (if (elatin-context-translations lc)
      (elatin-context-set-translation-no! lc idx)
      (elatin-context-set-completion-no! lc idx)))

(define (elatin-focus-out-handler lc)
  (elatin-context-flush lc)
  (elatin-update-preedit lc))

(define (elatin-place-handler lc)
  (elatin-update-preedit lc))

(define (elatin-displace-handler lc)
  (elatin-context-flush lc)
  (elatin-update-preedit lc))

(register-im
 'elatin
 ""
 "UTF-8"
 elatin-im-name-label
 elatin-im-short-desc
 #f
 elatin-init-handler
 elatin-release-handler
 context-mode-handler
 elatin-key-press-handler
 elatin-key-release-handler
 elatin-reset-handler
 elatin-get-candidate-handler
 elatin-set-candidate-index-handler
 context-prop-activate-handler
 #f
 #f
 elatin-focus-out-handler
 elatin-place-handler
 elatin-displace-handler
)

;; Local Variables:
;; mode: scheme
;; coding: utf-8
;; End:
