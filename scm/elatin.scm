;;; elatin.scm -- Emacs-style Latin characters translation
;;;
;;; Copyright (c) 2003-2008 uim Project http://code.google.com/p/uim/
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
;; the file elatin-rules.scm, have been adapted from GNU Emacs 22.

(require "util.scm")
(require "rk.scm")
(require "elatin-rules.scm")
(require-custom "generic-key-custom.scm")
(require-custom "elatin-custom.scm")

(define elatin-context-rec-spec
  (append
   context-rec-spec
   '((on	 #f)
     (rkc	 #f)
     (show-cands #f))))
(define-record 'elatin-context elatin-context-rec-spec)
(define elatin-context-new-internal elatin-context-new)

(define (elatin-context-new id im)
  (let ((lc (elatin-context-new-internal id im))
	(rkc (rk-context-new (symbol-value elatin-rules) #f #f)))
    (elatin-context-set-widgets! lc elatin-widgets)
    (elatin-context-set-rkc! lc rkc)
    lc))

(define (elatin-current-translation lc)
  (let ((rkc (elatin-context-rkc lc)))
    (or (rk-peek-terminal-match rkc)
	(and (not (null? (rk-context-seq rkc)))
	     (list (rk-pending rkc))))))

(define (elatin-current-string lc)
  (let ((trans (elatin-current-translation lc)))
    (if trans (car trans) "")))

(define (elatin-context-clear lc)
  (rk-flush (elatin-context-rkc lc)))

(define (elatin-context-flush lc)
  (let ((str (elatin-current-string lc)))
    (if (not (equal? str "")) (im-commit lc str))
    (elatin-context-clear lc)))

(define (elatin-open-candidates-window lc height)
  (if (elatin-context-show-cands lc)
      (im-deactivate-candidate-selector lc))
  (im-activate-candidate-selector lc height height)
  (im-select-candidate lc 0)
  (elatin-context-set-show-cands! lc #t))

(define (elatin-close-candidates-window lc)
  (if (elatin-context-show-cands lc)
      (im-deactivate-candidate-selector lc))
  (elatin-context-set-show-cands! lc #f))

(define (elatin-update-preedit lc)
  (if (elatin-context-on lc)
      (let ((trans (elatin-current-translation lc))
	    (ltrans 0))
	(im-clear-preedit lc)
	(if trans
	    (begin (im-pushback-preedit lc
					preedit-underline
					(car trans))
		   (set! ltrans (length trans))))
	(im-pushback-preedit lc
			     preedit-cursor
			     "")
	(im-update-preedit lc)
	(if (> ltrans 1)
	    (elatin-open-candidates-window lc ltrans)
	    (elatin-close-candidates-window lc)))))

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
		   (not (elatin-context-on lc)))
		 (lambda (lc)
		   (elatin-prepare-activation lc)
		   (elatin-context-set-on! lc #f)))

(register-action 'action_elatin_on
		 (lambda (lc)
		   (list
		    'on
		    "à"
		    (N_ "ELatin mode on")
		    (N_ "ELatin composition on")))
		 (lambda (lc)
		   (elatin-context-on lc))
		 (lambda (lc)
		   (elatin-prepare-activation lc)
		   (elatin-context-set-on! lc #t)))

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

(define elatin-control-key?
  (let ((shift-or-no-modifier? (make-key-predicate '("<Shift>" ""))))
    (lambda (key key-state)
      (not (shift-or-no-modifier? -1 key-state)))))

(define (elatin-proc-on-state lc key key-state)
  (let ((rkc (elatin-context-rkc lc))
	(cur-trans (elatin-current-translation lc)))
    (cond

     ((or (elatin-off-key? key key-state)
	  (and elatin-esc-turns-off? (eq? key 'escape)))
      (elatin-context-flush lc)
      (if (eq? key 'escape)
	  (im-commit-raw lc))
      (elatin-context-set-on! lc #f)
      (elatin-close-candidates-window lc)
      (im-clear-preedit lc)
      (im-update-preedit lc))

     ((elatin-backspace-key? key key-state)
      (if (not (rk-backspace rkc))
	  (im-commit-raw lc)))

     ((elatin-control-key? key key-state)
      (elatin-context-flush lc)
      (im-commit-raw lc))

     ((and (ichar-numeric? key)
	   (elatin-context-show-cands lc)
	   (let ((idx (- (numeric-ichar->integer key) 1)))
	     (if (= idx -1) (set! idx 9))
	     (and (>= idx 0) (< idx (length cur-trans))
		  (begin
		    (im-commit lc (nth idx cur-trans))
		    (elatin-context-clear lc)
		    #t)))))

     (else
      (let* ((key-str (if (symbol? key)
			  (symbol->string key)
			  (charcode->string key)))
	     (cur-seq (rk-context-seq rkc))
	     (res (rk-push-key! rkc key-str))
	     (new-seq (rk-context-seq rkc))
	     (new-trans (elatin-current-translation lc)))
	(if (equal? new-seq (cons key-str cur-seq))
	    (if (not (or (rk-partial? rkc) (> (length new-trans) 1)))
		(begin (im-commit lc (car (rk-peek-terminal-match rkc)))
		       (elatin-context-clear lc)))
	    (begin (if (not (null? cur-seq)) (im-commit lc (car cur-trans)))
		   (if (null? new-seq) (im-commit-raw lc)))))))))

(define (elatin-proc-off-state lc key key-state)
  (if (elatin-on-key? key key-state)
      (elatin-context-set-on! lc #t)
      (im-commit-raw lc)))

(define (elatin-key-press-handler lc key key-state)
  (if (elatin-context-on lc)
      (elatin-proc-on-state lc key key-state)
      (elatin-proc-off-state lc key key-state))
  (elatin-update-preedit lc))

(define (elatin-key-release-handler lc key key-state)
  (if (or (ichar-control? key)
	  (not (elatin-context-on lc)))
      ;; don't discard key release event for apps
      (im-commit-raw lc)))

(define (elatin-reset-handler lc)
  (elatin-context-clear lc))

(define (elatin-get-candidate-handler lc idx accel-enum-hint)
  (let* ((candidates (elatin-current-translation lc))
	 (candidate (nth idx candidates)))
    (list candidate (digit->string (+ idx 1)) "")))

;; Emacs does nothing on focus-out
;; TODO: this should be configurable
(define (elatin-focus-out-handler lc)
  #f)

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
 #f
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
