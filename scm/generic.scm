;;;
;;; Copyright (c) 2003,2004 uim Project http://uim.freedesktop.org/
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

(require "rk.scm")
(require "generic-key.scm")

;;; user configs

(define generic-use-candidate-window? #t)
(define generic-candidate-op-count 1)
(define generic-nr-candidate-max 10)

;; widgets and actions

;; widgets
(define generic-widgets '(widget_generic_input_mode))

;; default activity for each widgets
(define default-widget_generic_input_mode 'action_generic_off)

;; actions of widget_generic_input_mode
(define generic-input-mode-actions
  '(action_generic_off
    action_generic_on))


;;; implementations

(define generic-prepare-activation
  (lambda (gc)
    (let ((rkc (generic-context-rk-context gc)))
      (rk-flush rkc)
      (generic-update-preedit gc))))

(register-action 'action_generic_off
		 (lambda (gc)
		   (list
		    'figure_generic_off
		    "o"
		    (N_ "off")
		    (N_ "Direct Input Mode")))
		 (lambda (gc)
		   (not (generic-context-on gc)))
		 (lambda (gc)
		   (generic-prepare-activation gc)
		   (generic-context-set-on! gc #f)))

(register-action 'action_generic_on
		 (lambda (gc)
		   (let* ((im (generic-context-im gc))
			  (name (symbol->string (im-name im))))
		     (list
		      'figure_generic_on
		      "O"
		      (N_ "on")
		      (string-append name (N_ " Mode")))))
		 (lambda (gc)
		   (generic-context-on gc))
		 (lambda (gc)
		   (generic-prepare-activation gc)
		   (generic-context-set-on! gc #t)))

;; Update widget definitions based on action configurations. The
;; procedure is needed for on-the-fly reconfiguration involving the
;; custom API
(define generic-configure-widgets
  (lambda ()
    (register-widget 'widget_generic_input_mode
		     (activity-indicator-new generic-input-mode-actions)
		     (actions-new generic-input-mode-actions))))

(define generic-context-rec-spec
  (append
   context-rec-spec
   '((rk-context         #f)
     (rk-nth             0)
     (on                 #f)
     (candidate-op-count 0)
     (raw-commit         #f)
     (converting         #f))))
(define-record 'generic-context generic-context-rec-spec)
(define generic-context-new-internal generic-context-new)

(define generic-context-new
  (lambda (id im rule back)
    (let ((gc (generic-context-new-internal id im))
	  (rkc (rk-context-new rule #f back)))
      (generic-context-set-widgets! gc generic-widgets)
      (generic-context-set-rk-context! gc rkc)
      gc)))

(define generic-context-flush
  (lambda (pc)
    (generic-context-set-rk-nth! pc 0)
    (generic-context-set-candidate-op-count! pc 0)
    (generic-context-set-converting! pc #f)))

(define generic-update-preedit
  (lambda (pc)
    (if (generic-context-raw-commit pc)
	(generic-context-set-raw-commit! pc #f)
	(let* ((rkc (generic-context-rk-context pc))
	       (cs (rk-current-seq rkc))
	       (n (generic-context-rk-nth pc)))
	  (im-clear-preedit pc)
	  (im-pushback-preedit
	   pc preedit-reverse
	   (if cs
	       (nth n (cadr cs))
	       (rk-pending rkc)))
	  (im-update-preedit pc)))))

(define generic-commit-raw
  (lambda (pc)
    (im-commit-raw pc)
    (generic-context-set-raw-commit! pc #t)))


;; XXX: This function is too long, we should split into several functions..
;; 2004-11-05 TOKUNAGA Hiroyuki
(define generic-proc-input-state
  (lambda (pc key state)
    (let* ((rkc (generic-context-rk-context pc))
	   (n (generic-context-rk-nth pc))
	   (cs (cadr (rk-current-seq rkc)))
	   (res))
      (and
       (if (generic-off-key? key state)
	   (begin
	     (rk-flush rkc)
	     (generic-context-set-on! pc #f)
	     #f)
	   #t)
       (if (generic-prev-candidate-key? key state)
	   (begin
	     (generic-context-set-converting! pc #t)
	     (generic-proc-converting-state pc key state)
	     #f)
	   #t)
       (if (generic-next-candidate-key? key state)
	   (begin
	     (generic-context-set-converting! pc #t)
	     (generic-proc-converting-state pc key state)
	     #f)
	   #t)
       (if (or
	    (generic-prev-page-key? key state)
	    (generic-next-page-key? key state))
	   #f
	   #t)
       (if (generic-backspace-key? key state)
	   (begin
	     (if (not (rk-backspace rkc))
		 (generic-commit-raw pc))
	     (generic-context-set-rk-nth! pc 0)
	     (im-deactivate-candidate-selector pc)
	     #f)
	   #t)
       (if (generic-commit-key? key state)
	   (let ((cs (rk-current-seq rkc)))
	     (if (> (length (cadr cs)) 0)
		 (begin
		   (im-commit pc (nth (generic-context-rk-nth pc) (cadr cs)))
		   (im-deactivate-candidate-selector pc)
		   (rk-flush rkc)
		   (generic-context-flush pc))
		 (begin
		   (im-commit-raw pc)
		   (rk-flush rkc)
		   (im-update-preedit pc)))
	     #f)
	   #t)
       (if (symbol? key)
	   (begin
	     (rk-flush rkc)
	     (generic-commit-raw pc)
	     (generic-context-flush pc)
	     #f)
	   #t)
       (if (and (modifier-key-mask state)
		(not (shift-key-mask state)))
	   (begin
	     (generic-commit-raw pc)
	     #f)
	   #t)
       (begin
	 (set! res
	       (rk-push-key!
		rkc
		(charcode->string key)))
	 #t))
      (if (not (rk-partial? rkc))
	  (let ((cs (rk-current-seq rkc)))
	    (if (= (length (cadr cs)) 1)
		(begin
		  (im-commit pc
			     (nth (generic-context-rk-nth pc) (cadr cs)))
		  (generic-context-set-rk-nth! pc 0)
		  (generic-context-set-candidate-op-count! pc 0)
		  (im-deactivate-candidate-selector pc)
		  (rk-flush rkc)))))
      (if res
	  (begin
	    (im-commit pc (nth (generic-context-rk-nth pc) res))
	    (generic-context-set-rk-nth! pc 0)
	    (generic-context-set-candidate-op-count! pc 0)
	    (im-deactivate-candidate-selector pc))
	  ))))

(define generic-proc-converting-state
  (lambda (pc key state)
    (let* ((rkc (generic-context-rk-context pc))
	   (n (generic-context-rk-nth pc))
	   (cs (cadr (rk-current-seq rkc)))
	   (res))
      (and
       (if (generic-prev-candidate-key? key state)
	   (if (> (length (cadr cs)) 0)
	       (begin
		 (set! n (- n 1))
		 (generic-context-set-rk-nth! pc n)
		 (if (< n 0)
		     (begin
		       (generic-context-set-rk-nth! pc (- (length cs) 1))
		       (set! n (- (length cs) 1))))
		 (generic-context-set-candidate-op-count!
		  pc
		  (+ 1 (generic-context-candidate-op-count pc)))
		 (if (>= (generic-context-candidate-op-count pc)
			 generic-candidate-op-count)
		     (im-select-candidate pc n))
		 #f)
	       (begin
		 (im-commit-raw pc)
		 (rk-flush rkc)
		 (im-update-preedit pc)
		 #f))
	   #t)
       (if (generic-next-candidate-key? key state)
	   (if (> (length (cadr cs)) 0)
	       (begin
		 (generic-context-set-rk-nth! pc (+ 1 n))
		 (if (<= (length cs) (+ n 1))
		     (generic-context-set-rk-nth! pc 0))
		 (generic-context-set-candidate-op-count!
		  pc
		  (+ 1 (generic-context-candidate-op-count pc)))
		 (if (and
		      (= (generic-context-candidate-op-count pc)
			 generic-candidate-op-count)
		      generic-use-candidate-window?)
		     (im-activate-candidate-selector pc (length cs) generic-nr-candidate-max))
		 (if (>= (generic-context-candidate-op-count pc)
			 generic-candidate-op-count)
		     (begin
		       (if (>= (+ n 1) (length cs))
			   (set! n -1))
		       (im-select-candidate pc (+ n 1))))
		 #f)
	       (begin
		 (im-commit-raw pc)
		 (rk-flush rkc)
		 (im-update-preedit pc)
		 #f))
	   #t)
       (if (and (generic-prev-page-key? key state)
		(<= generic-candidate-op-count (generic-context-candidate-op-count pc)))
	   (begin
	     (im-shift-page-candidate pc #f)
	     #f)
	   #t)
       (if (and (generic-next-page-key? key state)
		(<= generic-candidate-op-count (generic-context-candidate-op-count pc)))
	   (begin
	     (im-shift-page-candidate pc #t)
	     #f)
	   #t)
       (if (generic-backspace-key? key state)
	   (begin
	     (if (not (rk-backspace rkc))
		 (generic-commit-raw pc))
	     (generic-context-set-rk-nth! pc 0)
	     (im-deactivate-candidate-selector pc)
	     #f)
	   #t)
       (if (generic-commit-key? key state)
	   (let ((cs (rk-current-seq rkc)))
	     (if (> (length (cadr cs)) 0)
		 (begin
		   (im-commit pc (nth (generic-context-rk-nth pc) (cadr cs)))
		   (generic-context-flush pc)
		   (rk-flush rkc)
		   (im-deactivate-candidate-selector pc))
		 (begin
		   (im-commit-raw pc)
		   (rk-flush rkc)
		   (im-update-preedit pc)))
	     #f)
	   #t)
       (if (symbol? key)
	   (if (>= (generic-context-candidate-op-count pc)
		   generic-candidate-op-count)
	       #f
	       (begin
		 (rk-flush rkc)
		 (generic-commit-raw pc)
		 (generic-context-set-rk-nth! pc 0)
		 #f))
	   #t)
       (if (and (modifier-key-mask state)
		(not (shift-key-mask state)))
	   (begin
	     (generic-commit-raw pc)
	     #f)
	   #t)
       (let ((cs (rk-current-seq rkc)))
	 (if (> (length (cadr cs)) 0)
	     (im-commit pc
			(nth (generic-context-rk-nth pc) (cadr cs))))
	     (generic-context-flush pc)
	     (im-deactivate-candidate-selector pc)
	     (rk-flush rkc)
	     (generic-proc-input-state pc key state)
	 )))))
  

(define generic-proc-off-mode
  (lambda (pc key state)
    (and
     (if (generic-on-key? key state)
	 (begin
	   (generic-context-set-on! pc #t)
	   #f)
	 #t)
     ;;
     (generic-commit-raw pc))))

(define generic-key-press-handler
  (lambda (pc key state)
    (if (control-char? key)
	(im-commit-raw pc)
	(if (generic-context-on pc)
	    (if (generic-context-converting pc)
		(generic-proc-converting-state pc key state)
		(generic-proc-input-state pc key state))
	    (generic-proc-off-mode pc key state)))
    (generic-update-preedit pc)
    ()))

(define generic-key-release-handler
  (lambda (pc key state)
    (if (or (control-char? key)
	    (not (generic-context-on pc)))
	;; don't discard key release event for apps
	(generic-commit-raw pc))))

(define generic-reset-handler
  (lambda (pc)
    (let ((rkc (generic-context-rk-context pc)))
      (rk-flush rkc))))

(define generic-get-candidate-handler
  (lambda (pc idx accel-enum-hint)
    (let* ((rkc (generic-context-rk-context pc))
	   (cs (cadr (rk-current-seq rkc)))
	   (cand (car (nthcdr idx cs))))
      (list cand (digit->string (+ idx 1)) ""))))

(define generic-set-candidate-index-handler
  (lambda (pc idx)
    (let ((rkc (generic-context-rk-context pc)))
      (generic-context-set-rk-nth! pc idx))))

(define generic-init-handler
  (lambda (id im init-handler)
    (init-handler id im #f)))

(define generic-register-im
  (lambda (name lang code short-desc init-arg)
    (register-im
     name
     lang
     code
     short-desc
     init-arg
     generic-init-handler
     #f  ;; release-handler
     context-mode-handler
     generic-key-press-handler
     generic-key-release-handler
     generic-reset-handler
     generic-get-candidate-handler
     generic-set-candidate-index-handler
     context-prop-activate-handler)))

(generic-configure-widgets)
