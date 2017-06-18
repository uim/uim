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

(require "util.scm")
(require "rk.scm")
(require-custom "generic-key-custom.scm")
(require-custom "generic-custom.scm")


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

(define ascii-rule
  (map (compose (lambda (entry)
		  (list (list entry) entry))
		list
		charcode->string)
       (iota 95 32)))

(define generic-prepare-activation
  (lambda (gc)
    (let ((rkc (generic-context-rk-context gc)))
      (rk-flush rkc)
      (generic-update-preedit gc))))

(register-action 'action_generic_off
		 (lambda (gc)
		   (list
		    'off
		    "-"
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
		      'on
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
   '((rk-context            #f)
     (rk-nth                0)
     (on                    #f)
     (candidate-op-count    0)
     (raw-commit            #f)
     (multi-cand-input      #f)
     (cands                 ()))))
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
    (generic-context-set-multi-cand-input! pc #f)
    (generic-context-set-cands! pc '())
    (rk-flush (generic-context-rk-context pc))))

(define generic-update-preedit
  (lambda (pc)
    (if (generic-context-raw-commit pc)
	(generic-context-set-raw-commit! pc #f)
	(let* ((rkc (generic-context-rk-context pc))
	       (cands (generic-context-cands pc))
	       (n (generic-context-rk-nth pc)))
	  (im-clear-preedit pc)
	  (im-pushback-preedit
	   pc preedit-reverse
	   (if (and
                 (not (null? cands))
                 (> n -1))
             (if (pair? (car cands))
               (car (nth n cands))
               (nth n cands))
             (rk-pending rkc)))
	  (im-update-preedit pc)))))

(define generic-commit-raw
  (lambda (pc)
    (im-commit-raw pc)
    (generic-context-set-raw-commit! pc #t)))

(define generic-commit
  (lambda (pc)
    (let ((rkc (generic-context-rk-context pc))
          (cands (generic-context-cands pc))
          (n (generic-context-rk-nth pc)))
      (if (not (null? cands))
	  (begin
            (if (> n -1)
              (if (pair? (car cands))
                (im-commit pc (car (nth (generic-context-rk-nth pc) cands)))
                (im-commit pc (nth (generic-context-rk-nth pc) cands)))
              ;(im-commit pc (rk-pending rkc))
              )
	    (im-deactivate-candidate-selector pc)
	    (generic-context-flush pc))
	  (begin
	    (im-commit-raw pc)
	    (rk-flush rkc))))))

(define generic-commit-by-numkey
  (lambda (pc key)
    (let* ((rkc (generic-context-rk-context pc))
	   (cands (generic-context-cands pc))
	   (n (generic-context-rk-nth pc))
	   (nr (length cands))
	   (cur-page (if (= generic-nr-candidate-max 0)
			 0
			 (quotient n generic-nr-candidate-max)))
	   (pageidx (- (numeric-ichar->integer key) 1))
	   (compensated-pageidx (cond
				 ((< pageidx 0) ; pressing key_0
				  (+ pageidx 10))
				 (else
				  pageidx)))
	   (idx (+ (* cur-page generic-nr-candidate-max) compensated-pageidx)))
      (if (< idx nr)
	  (begin
            (if (pair? (car cands))
              (im-commit pc (car (nth idx cands)))
              (im-commit pc (nth idx cands)))
	    (im-deactivate-candidate-selector pc)
	    (generic-context-flush pc)
	    #t)
	  #f))))

(define generic-proc-input-state-without-preedit
  (lambda (pc key state rkc)
    (cond
     ((generic-off-key? key state)
      (generic-context-set-on! pc #f)
      #f)
     ((generic-backspace-key? key state)
      (generic-commit-raw pc)
      #f)
     ((symbol? key)
      (generic-commit-raw pc)
      #f)
     ((and (modifier-key-mask state)
	   (not (shift-key-mask state)))
      (generic-commit-raw pc)
      #f)
     (else
      #t))))

(define generic-proc-input-state-with-preedit
  (lambda (pc key state rkc)
    (cond
     ((generic-off-key? key state)
      (let ((cands (generic-context-cands pc))
            (n (generic-context-rk-nth pc)))
	(if (and
              (not (null? cands))
              (> n -1))
	    (begin
              (if (pair? (car cands))
                (im-commit pc (car (nth n cands)))
                (im-commit pc (nth n cands)))
	      (generic-context-flush pc))
	    (if (not (string=? (rk-pending rkc) "")) ;; flush pending rk
		(generic-context-flush pc)))
	(generic-context-set-on! pc #f)
	#f))
     ((generic-prev-candidate-key? key state)
      (generic-handle-convert-key pc key state)
      #f)
     ((generic-next-candidate-key? key state)
      (generic-handle-convert-key pc key state)
      #f)
     ((generic-backspace-key? key state)
      (rk-backspace rkc)
      (generic-context-set-rk-nth! pc 0)
      (generic-update-input-state-cands pc key state
					rkc (rk-context-seq rkc) #f)
      #f)
     ((generic-commit-key? key state)
      (generic-commit pc)
      #f)
     ((generic-cancel-key? key state)
      (generic-context-flush pc)
      #f)
     ((symbol? key)
      (generic-commit pc)
      (im-commit-raw pc)
      #f)
     ((and (modifier-key-mask state)
	   (not (shift-key-mask state)))
      (generic-commit pc)
      (im-commit-raw pc)
      #f)
     (else
      #t))))

(define generic-flatten
  (lambda (lst)
    (cond ((null? lst) '())
          ((list? (car lst))
           (append (generic-flatten (car lst))
                   (generic-flatten (cdr lst))))
          (else
            (cons (car lst) (generic-flatten (cdr lst)))))))

(define generic-update-input-state-cands
  (lambda (pc key state rkc prev-seq res)
    (if (not (rk-partial? rkc))
      ;; exact match or no-match
      (let* ((cs (rk-current-seq rkc))
             (cands (if cs (cadr cs) '())))
        (generic-context-set-cands! pc cands)
        (if cs
          (if (null? (cdr cands)) 
            ;; single candidate
            (begin
              (im-commit pc (nth 0 cands))
              (generic-context-flush pc)
              (im-deactivate-candidate-selector pc))
            ;; show candidates for the Pinyin like input method
            (if (and generic-use-candidate-window?
                     generic-show-candidate-implicitly?)
              (begin
                (im-activate-candidate-selector
                  pc (length cands) generic-nr-candidate-max)
                (im-select-candidate pc 0)
                (generic-context-set-multi-cand-input! pc #t)
                (generic-context-set-candidate-op-count!
                  pc
                  (+ 1 (generic-context-candidate-op-count pc)))))))
        ;; commit no-matching key
        (if (and
              (not cs)
              (null? (rk-context-seq rkc))
              (or
                (null? prev-seq)
                res)
              (not (generic-backspace-key? key state))) ;; mmm...
          (im-commit-raw pc)))
      ;; partial match, including exact match
      (let* ((ret (if generic-show-prediction-candidates?
                    (rk-cands-with-minimal-partial rkc)
                    '()))
             (expand-cands
               (lambda (lst)
                 (map (lambda (x)
                        (let ((head (car x))
                              (tail (cdr x)))
                          (map (lambda (y) (cons y tail)) head))) lst)))
             (cands/keys (generic-flatten (expand-cands ret))))
        (if (not generic-show-prediction-candidates?)
          (let* ((cs (rk-current-seq rkc))
                 (cands (if cs (cadr cs) '())))
            (set! cands/keys cands)))
        (generic-context-set-cands! pc cands/keys)
        (if (not (null? cands/keys))
          ;; show candidates even in input-state
          (begin
            (if (and generic-use-candidate-window?
                     generic-show-candidate-implicitly?)
              (begin
                (im-activate-candidate-selector
                  pc (length cands/keys) generic-nr-candidate-max)
                (if (and
                      generic-show-prediction-candidates?
                      (not (string=? (cdr (car cands/keys)) "")))
                  (generic-context-set-rk-nth! pc -1)
                  (begin
                    (generic-context-set-rk-nth! pc 0)
                    (im-select-candidate pc 0)
                    (generic-context-set-candidate-op-count! pc (+ 1 (generic-context-candidate-op-count pc)))))
                (generic-context-set-multi-cand-input! pc #t)))))))))

(define generic-proc-input-state
  (lambda (pc key state)
    (let* ((rkc (generic-context-rk-context pc))
    	   (seq (rk-context-seq rkc))
	   (res #f))
      (and
       (if (string=? (rk-pending rkc) "")
	   (generic-proc-input-state-without-preedit pc key state rkc)
	   (generic-proc-input-state-with-preedit pc key state rkc))
       (begin
	 (set! res
	       (rk-push-key!
		rkc
		(charcode->string key)))
         (if res
	     (begin
	       (im-commit pc (nth (generic-context-rk-nth pc) res))
	       (generic-context-set-rk-nth! pc 0)
	       (generic-context-set-candidate-op-count! pc 0)
	       (generic-context-set-cands! pc '())
	       (im-deactivate-candidate-selector pc)))
	 (generic-update-input-state-cands pc key state rkc seq res))))))

(define generic-proc-specific-multi-cand-input-state
  (lambda (pc key state rkc)
    (cond
     ((generic-off-key? key state)
      (let ((cands (generic-context-cands pc))
            (n (generic-context-rk-nth pc)))
	(if (and
              (not (null? cands))
              (> n -1))
	    (begin
              (if (pair? (car cands))
                (im-commit pc (car (nth n cands)))
                (im-commit pc (nth n cands)))
	      (generic-context-flush pc))
	    (if (not (string=? (rk-pending rkc) "")) ;; flush pending rk
		(generic-context-flush pc)))
	(generic-context-set-on! pc #f)
	(im-deactivate-candidate-selector pc)
	#f))
     ((generic-prev-candidate-key? key state)
      (generic-handle-convert-key pc key state)
      #f)
     ((generic-next-candidate-key? key state)
      (generic-handle-convert-key pc key state)
      #f)
     ((generic-prev-page-key? key state)
      (generic-handle-convert-key pc key state)
      #f)
     ((generic-next-page-key? key state)
      (generic-handle-convert-key pc key state)
      #f)
     ((generic-backspace-key? key state)
      (rk-backspace rkc)
      (generic-context-set-rk-nth! pc 0)
      (generic-update-multi-cand-state-cands pc key state rkc)
      #f)
     ((generic-commit-key? key state)
      (generic-context-set-multi-cand-input! pc #f)
      (generic-commit pc)
      #f)
     ((generic-cancel-key? key state)
      (im-deactivate-candidate-selector pc)
      (generic-context-flush pc)
      #f)
     ((symbol? key)
      (generic-context-set-multi-cand-input! pc #f)
      (generic-commit pc)
      (im-commit-raw pc)
      #f)
     ((and generic-commit-candidate-by-numeral-key?
	   (ichar-numeric? key)
           (not (rk-expect-key? rkc (charcode->string key))))
      (if (generic-commit-by-numkey pc key)
	  (generic-context-set-multi-cand-input! pc #f))
      #f)

     ((and (modifier-key-mask state)
	   (not (shift-key-mask state)))
      (generic-context-set-multi-cand-input! pc #f)
      (generic-commit pc)
      (im-commit-raw pc)
      #f)
     (else
      #t))))

(define generic-update-multi-cand-state-cands
  (lambda (pc key state rkc)
    (if (not (rk-partial? rkc)) ;; exact match or no-match
      (let* ((cs (rk-current-seq rkc))
             (cands (if cs (cadr cs) '())))
        (generic-context-set-cands! pc cands)
        (generic-context-set-rk-nth! pc 0)
        (if cs
          (if (null? (cdr cands))
            (begin
              (im-commit pc
                         (nth 0 cands))
              (generic-context-flush pc)
              (im-deactivate-candidate-selector pc))
            ;; show candidates for the Pinyin like input method
            (if generic-use-candidate-window?
              (begin
                (im-activate-candidate-selector
                  pc (length cands) generic-nr-candidate-max)
                (im-select-candidate pc 0))))
          ;; perhaps backspace to clear preedit
          (begin
            (im-deactivate-candidate-selector pc)
            (generic-context-flush pc))))
      ;; partial match, including exact match
      (let* ((ret (if generic-show-prediction-candidates?
                    (rk-cands-with-minimal-partial rkc)
                    '()))
             (expand-cands
               (lambda (lst)
                 (map (lambda (x)
                        (let ((head (car x))
                              (tail (cdr x)))
                          (map (lambda (y) (cons y tail)) head))) lst)))
             (cands/nexts (generic-flatten (expand-cands ret))))

        (if (not generic-show-prediction-candidates?)
          (let* ((cs (rk-current-seq rkc))
                 (cands (if cs (cadr cs) '())))
            (set! cands/nexts cands)))
        (generic-context-set-cands! pc cands/nexts)
        (if (not (null? cands/nexts))
          (if (not (null? (cdr cands/nexts)))
            (begin
              (im-activate-candidate-selector
                pc (length cands/nexts) generic-nr-candidate-max)
              (if (and
                    generic-show-prediction-candidates?
                    (not (string=? (cdr (car cands/nexts)) "")))
                (generic-context-set-rk-nth! pc -1)
                (begin
                  (generic-context-set-rk-nth! pc 0)
                  (im-select-candidate pc 0))))
            ;; single candidate
            (begin
              (im-deactivate-candidate-selector pc)
              (generic-context-set-rk-nth! pc 0)
              (generic-context-set-candidate-op-count! pc 0)
              (generic-context-set-multi-cand-input! pc #f)))
          ;; no-candidate
          (begin
            (im-deactivate-candidate-selector pc)
            (generic-context-set-candidate-op-count! pc 0)
            (generic-context-set-multi-cand-input! pc #f)))))))

(define generic-proc-multi-cand-input-state
  (lambda (pc key state)
    (let* ((rkc (generic-context-rk-context pc))
           (seq (rk-context-seq rkc))
           (cands (generic-context-cands pc))
           (res #f))
      (and
       (generic-proc-specific-multi-cand-input-state pc key state rkc)
       (begin
	 (set! res
	       (rk-push-key!
		rkc
		(charcode->string key)))
         (if res
	     ;; commit matched word and continue new rk
	     (begin
               (if (< (generic-context-rk-nth pc) (length res))
                 (im-commit pc (nth (generic-context-rk-nth pc) res))
	         ;(im-commit pc (car (nth (generic-context-rk-nth pc) cands)))
	         ;(im-commit pc (nth 0 res))
                 ) ;; XXX: what is the expected behavior here?
	       (generic-context-set-rk-nth! pc 0)
	       (generic-context-set-candidate-op-count! pc 0)
	       (im-deactivate-candidate-selector pc)
               (generic-context-set-multi-cand-input! pc #f)
               (generic-update-input-state-cands pc key state rkc seq res))
             (generic-update-multi-cand-state-cands pc key state rkc)))))))

(define generic-handle-convert-key
  (lambda (pc key state)
    (let* ((rkc (generic-context-rk-context pc))
	   (n (generic-context-rk-nth pc))
	   (cands (generic-context-cands pc))
	   (nr (length cands)))
      (if (and 
            (not (generic-context-multi-cand-input pc))
            (not (null? cands))
            (not (null? (cdr cands))))
        (generic-context-set-multi-cand-input! pc #t))
      (and
       (if (generic-prev-candidate-key? key state)
	   (if (not (null? cands))
	     (if (pair? (cdr cands))
	       ;; multiple candidates
	       (begin
		 (set! n (- n 1))
		 (generic-context-set-rk-nth! pc n)
		 (if (< n 0)
		     (begin
		       (generic-context-set-rk-nth! pc (- nr 1))
		       (set! n (- nr 1))))
		 (generic-context-set-candidate-op-count!
		  pc
		  (+ 1 (generic-context-candidate-op-count pc)))
		 (if (and
                      (= (generic-context-candidate-op-count pc)
                         generic-candidate-op-count)
                      generic-use-candidate-window?)
                     (im-activate-candidate-selector
		      pc nr generic-nr-candidate-max))
		 (if (and
		      (>= (generic-context-candidate-op-count pc)
			  generic-candidate-op-count)
                      generic-use-candidate-window?)
		     (im-select-candidate pc n))
		 #f)
	       ;; single candidate
	       (begin
		 (generic-commit pc)
		 #f))
	     ;; no candidate
	     (begin
	       (generic-context-flush pc)
	       #f))
	   #t)
       (if (generic-next-candidate-key? key state)
	   (if (not (null? cands))
	     (if (pair? (cdr cands))
	       ;; multiple candidates
	       (begin
		 (generic-context-set-rk-nth! pc (+ 1 n))
		 (if (<= nr (+ n 1))
		     (generic-context-set-rk-nth! pc 0))
		 (generic-context-set-candidate-op-count!
		  pc
		  (+ 1 (generic-context-candidate-op-count pc)))
		 (if (and
		      (= (generic-context-candidate-op-count pc)
			 generic-candidate-op-count)
		      generic-use-candidate-window?)
		     (im-activate-candidate-selector pc nr
						     generic-nr-candidate-max))
		 (if (and
		      (>= (generic-context-candidate-op-count pc)
			  generic-candidate-op-count)
		      generic-use-candidate-window?)
		     (begin
		       (if (>= (+ n 1) nr)
			   (set! n -1))
		       (im-select-candidate pc (+ n 1))))
		 #f)
	       ;; single candidate
	       (begin
		 (generic-commit pc)
		 #f))
	     ;; no candidate
	     (begin
	       (generic-context-flush pc)
	       #f))
	   #t)
       (if (and (generic-prev-page-key? key state)
		(<= generic-candidate-op-count
		    (generic-context-candidate-op-count pc))
		generic-use-candidate-window?)
	   (begin
	     (im-shift-page-candidate pc #f)
	     #f)
	   #t)
       (if (and (generic-next-page-key? key state)
		(<= generic-candidate-op-count
		    (generic-context-candidate-op-count pc))
		generic-use-candidate-window?)
	   (begin
	     (im-shift-page-candidate pc #t)
	     #f)
	   #t)))))


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
    (if (ichar-control? key)
	(im-commit-raw pc)
	(if (generic-context-on pc)
          (if (generic-context-multi-cand-input pc)
            (generic-proc-multi-cand-input-state pc key state)
            (generic-proc-input-state pc key state))
          (generic-proc-off-mode pc key state)))
    (generic-update-preedit pc)
    ()))

(define generic-key-release-handler
  (lambda (pc key state)
    (if (or (ichar-control? key)
	    (not (generic-context-on pc)))
	;; don't discard key release event for apps
	(generic-commit-raw pc))))

(define generic-reset-handler
  (lambda (pc)
    (let ((rkc (generic-context-rk-context pc)))
      (rk-flush rkc))))

(define generic-focus-in-handler
  (lambda (pc)
    #f))

(define generic-focus-out-handler
  (lambda (pc)
    (generic-commit pc)
    (generic-update-preedit pc)))

(define generic-place-handler generic-focus-in-handler)
(define generic-displace-handler generic-focus-out-handler)

(define generic-get-candidate-handler
  (lambda (pc idx accel-enum-hint)
    (let* ((cands (generic-context-cands pc))
	   (cell (nth idx cands))
           (cand (if (pair? cell) (string-append (car cell) "\t" (cdr cell))
                   cell)))
      (list cand (digit->string (+ idx 1)) ""))))

(define generic-set-candidate-index-handler
  (lambda (pc idx)
    (let ((rkc (generic-context-rk-context pc)))
      (generic-context-set-rk-nth! pc idx)
      (generic-update-preedit pc))))

(define generic-init-handler
  (lambda (id im init-handler)
    (init-handler id im #f)))

(define generic-register-im
  (lambda (name lang code name-label short-desc init-arg)
    (register-im
     name
     lang
     code
     name-label
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
     context-prop-activate-handler
     #f
     generic-focus-in-handler
     generic-focus-out-handler
     generic-place-handler
     generic-displace-handler
     )))

(generic-configure-widgets)
