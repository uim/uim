;;; action.scm: Handles user-operable actions of input methods
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

;; This user-operable action handling framework have eliminated all of
;; property related handlings such as im-update-prop-list or
;; im-update-prop-label from each input method. Input method developer
;; can forget about the property (and legacy 'mode') feature except
;; for context creation process.
;;
;; The term 'widget' used in this framework is still under
;; discussion. It may be renamed in accordance with result of the
;; discussion.
;;
;; Input method developer must not use all procedures or records in
;; this file except for the 7 procedures described below. This is
;; required to ensure isolation of the property/action feature from
;; uim core. It is important for future extensibility and wide-range
;; platform support. Please satisfy following conditions.

;; - Use only following 7 procedures.
;;   * register-action
;;   * register-widget
;;   * indicator-new
;;   * activity-indicator-new
;;   * actions-new
;;   * context-prop-activate-handler
;;   * context-mode-handler
;;
;; - Don't insert (require "action.scm") in your input method
;;   file. All necessary procedures described above are appropriately
;;   prepared in im.scm.
;;
;; - Name the widget used for choose input mode as
;;   "widget_*_input_mode" (replace "*" with your input method name
;;   such as "your_im"). This naming convention is used to display
;;   only the primary input state for user by some helper applets, or
;;   support a legacy 'mode' API.
;;
;; -- 2004-10-30 YamaKen

(require "util.scm")

;;
;; action
;;

(define action-list ())

(define action-rec-spec
  '((id                 #f)
    (indication-handler #f)
    (activity-pred      #f)
    (handler            #f)))
(define-record 'action action-rec-spec)

;; indicator is restricted version of action
(define indicator-rec-spec action-rec-spec)
(define-record 'indicator indicator-rec-spec)
(define indicator-set-id! #f)
(define indicator-set-activity-pred! #f)
(define indicator-set-activate-handler! #f)
(define indicator-new-internal indicator-new)
(define indicator-new
  (lambda (indication-handler)
    (indicator-new-internal #f indication-handler)))

;; API for input method developers
(define register-action
  (lambda action
    (set! action-list (alist-replace action action-list))))

(define fetch-action
  (lambda (action-sym)
    (assq action-sym action-list)))

(define action-active?
  (lambda (action owner)
    (let ((active? (action-activity-pred action)))
      (and active?
	   (active? owner)))))

(define action-indicate
  (lambda (action owner)
    (let ((indicate (and action
			 (action-indication-handler action))))
      (if indicate
	  (indicate owner)
	  fallback-indication))))

;; API for input method developers
(define actions-new
  (lambda (action-syms)
    (filter-map fetch-action action-syms)))

;; API for input method developers
(define activity-indicator-new
  (lambda (action-syms)
    (let ((actions (actions-new action-syms)))
      (indicator-new
       (lambda (owner)
	 (let ((active-action (find (lambda (action)
				      (and action
					   (action-active? action owner)))
				      actions)))
	       (if active-action
		   (action-indicate active-action owner)
		   fallback-indication)))))))

;;
;; widget
;;

(define widget-proto-list ())

(define widget-rec-spec
  '((id          #f) ;; must be first member
    (indicator   #f)
    (actions     ())
    (owner       #f)
    (prev-config #f)
    (prev-state  #f)))
(define-record 'widget widget-rec-spec)
(define widget-new-internal widget-new)

;; API for input method developers
(define register-widget
  (lambda widget-proto
    (let ((duplicated (copy-list widget-proto)))
      (if (not (widget-actions duplicated))
	  (widget-set-actions! duplicated ()))
      (set! widget-proto-list (alist-replace duplicated widget-proto-list)))))

(define widget-new
  (lambda (wid owner)
    (let ((widget-proto (assq wid widget-proto-list)))
      (if widget-proto
	  (let* ((actions (widget-actions widget-proto))
		 (default-activity-sym (symbolconc 'default- wid))
		 (default-activity (and (symbol-bound? default-activity-sym)
					(assq (symbol-value
					       default-activity-sym)
					      actions)))
		 (init-list (append widget-proto
				    (list owner)))
		 (widget (apply widget-new-internal init-list)))
	    (if default-activity
		(widget-activate! widget default-activity))
	    widget)
	  #f))))

(define widget-activity
  (lambda (widget)
    (let* ((owner (widget-owner widget))
	   (active? (lambda (action)
		      (action-active? action owner)))
	   (candidates (filter active?
			       (widget-actions widget))))
      (cond
       ((= (length candidates)
	   1)
	(car candidates))
       ((null? candidates)
	#f)
       (else
	(if (>= (verbose)
		5)
	    (print (widget-debug-message widget
					 "widget-activity"
					 "ambiguous activity")))
	#f)))))

;; 'action' accepts both actual action or action-id
(define widget-activate!
  (lambda (widget action)
    (let* ((action (if (symbol? action)
		       (assq action (widget-actions widget))
		       action))
	   (handler (and action
			 (action-handler action))))
      (and handler
	   (begin
	     (handler (widget-owner widget))
	     #t)))))

(define widget-configuration
  (lambda (widget)
    (let* ((owner (widget-owner widget))
	   (indicator (widget-indicator widget))
	   (indicate (lambda (action)
		       (action-indicate action owner))))
      (cons (or (and indicator
		     (indicator-id indicator))
		'action_unknown)
	    (map indicate
		 (widget-actions widget))))))

(define widget-state
  (lambda (widget)
    (let* ((activity (widget-activity widget))
	   (owner (widget-owner widget))
	   (indicator (widget-indicator widget))
	   (indicator-indication (action-indicate indicator owner)))
      (list activity indicator-indication))))

(define widget-update-configuration!
  (lambda (widget)
    (let* ((new-config (widget-configuration widget))
	   (config-updated? (not (equal? (widget-prev-config widget)
					 new-config))))
      (widget-set-prev-config! widget new-config)
      config-updated?)))

(define widget-update-state!
  (lambda (widget)
    (let* ((new-state (widget-state widget))
	   (state-updated? (not (equal? (widget-prev-state widget)
					new-state))))
      (widget-set-prev-state! widget new-state)
      state-updated?)))

(define widget-debug-message
  (lambda (widget location defect)
    (let* ((wid (widget-id widget))
	   (widget-id-str (symbol->string wid)))
      (string-append
       defect " in " location ". debug " widget-id-str "."))))

;;
;; helper protocol message handlings
;;

;; See doc/HELPER-PROTOCOL for the protocol specification

(define indication-compose-label
  (lambda (indication)
    (string-append (symbol->string (indication-id indication)) "\t"
		   (indication-iconic-label indication) "\t"
		   (indication-label indication) "\n")))

(define indication-compose-branch
  (lambda (indication)
    (string-append "branch\t"
		   (indication-compose-label indication))))

(define indication-compose-leaf
  (lambda (indication act-id active?)
    (string-append "leaf\t"
		   (symbol->string (indication-id indication)) "\t"
		   (indication-iconic-label indication) "\t"
		   (indication-label indication) "\t"
		   (indication-short-desc indication) "\t"
		   (symbol->string act-id) "\t"
		   (if active?
		       "*\n"
		       "\n"))))

(define widget-compose-live-branch
  (lambda (widget)
    (let* ((owner (widget-owner widget))
	   (activity (widget-activity widget))
	   (indicator (widget-indicator widget))
	   (branch (indication-compose-branch (action-indicate indicator owner)))
	   (leaves (map (lambda (action)
			  (let ((active? (eq? action activity))
				(indication (action-indicate action owner))
				(act-id (action-id action)))
			    (indication-compose-leaf indication act-id active?)))
			(widget-actions widget))))
      (apply string-append (cons branch leaves)))))

;; API for uim developers
;;
;; Developers must use this procedure to reconfigure order or
;; existence of widgets. Don't use context-set-widgets! directly. The
;; framework can't detect the configuration information invalidation
;; when violently reconfigured by context-set-widgets!.
(define context-init-widgets!
  (lambda (context widget-id-list)
    (let* ((widget-id-list (if (or (null? widget-id-list)
				   (not widget-id-list))
			       '(widget_fallback)
			       widget-id-list))
	   (widgets (filter-map (lambda (wid)
				  (if (symbol? wid)
				      (widget-new wid context)
				      wid)) ;; already actualized
				widget-id-list)))
      (context-set-widgets! context widgets)
      (context-propagate-widget-configuration context))))

;; TODO: write test
;; API for uim developers
(define context-list-replace-widgets!
  (lambda (target-im-name widget-id-list)
    (for-each (lambda (context)
		(let* ((im (context-im context))
		       (name (im-name im)))
		  (and (eq? name
			    target-im-name)
		       (context-init-widgets! context widget-id-list))))
	      context-list)))

;; API for uim developers
;; returns action-id list that can be passed to context-update-widget-states!
;; TODO: write test
(define context-current-widget-states
  (let ((widget-act-id (compose
                         (lambda (activity) (if activity
                                              (action-id activity)
                                              #f))
                         widget-activity)))
    (lambda (context)
      (map widget-act-id (context-widgets context)))))

;; API for uim developers
;; TODO: write test
(define context-update-widget-states!
  (lambda (context act-ids)
    (for-each widget-activate!
	      (context-widgets context)
	      act-ids)))

;; API for uim developers
(define context-update-widgets
  (lambda (context)
    (let ((widgets (context-widgets context)))
      (if (not (null? (filter-map widget-update-configuration! widgets)))
          (context-propagate-widget-configuration context))
      (if (not (null? (filter-map widget-update-state! widgets)))
          (context-propagate-widget-states context)))))

(define bridge-show-input-state-mode-on? #f)

(define context-propagate-prop-list-update
  (lambda (context)
    (let* ((widgets (context-widgets context))
	   (branches (map widget-compose-live-branch
			  widgets))
	   (widget-config-tree (apply string-append branches)))
      (if (eq? bridge-show-with?
               'mode)
          (if (eq? (context-current-mode context) 0)
              (set! bridge-show-input-state-mode-on? #f)
              (set! bridge-show-input-state-mode-on? #t)))
      (im-update-prop-list context widget-config-tree))))

;; API for uim developers
(define context-propagate-widget-states
  (lambda (context)
    ;; Sending prop_list every time costs all uim participant
    ;; processes slightly heavy resource consumptions. Although it is
    ;; not a problem for the rich desktop environment today, we should
    ;; also consider resource sensitive embedded environments if it is
    ;; not hard.
    ;;
    ;; We should adopt another message to send lightweight status
    ;; update, and revise prop_list as initial configuration message
    ;; (i.e. remove the flag field) -- 2004-10-08 YamaKen
    (context-propagate-prop-list-update context)
    (context-update-mode context)))

;; API for uim developers
(define context-propagate-widget-configuration
  (lambda (context)
    (context-propagate-prop-list-update context)
    (context-update-mode-list context)))

;; API for input method developers
;; ready to use for register-im
(define context-prop-activate-handler
  (lambda (context message)
    (let* ((widgets (context-widgets context))
	   (act-id (string->symbol message))
	   (activate! (lambda (widget)
			(let* ((actions (widget-actions widget))
			       (action (assq act-id actions))
			       (indicator (widget-indicator widget)))
			  (or (widget-activate! widget action)
			      (widget-activate! widget indicator))))))
      (find activate! widgets))))


;;
;; legacy 'mode' handlings for backward compatibility
;;

;; find the property that has "_input_mode" suffix
(define context-find-mode-widget
  (lambda (context)
    (let* ((widgets (context-widgets context))
	   (extract-suffix (lambda (widget)
			     (let* ((wid (widget-id widget))
				    (as-str (symbol->string wid))
				    (rev-words (reverse
						(string-split as-str "_"))))
			       (and (>= (length rev-words) 2)
				    (list-head rev-words 2)))))
	   (mode-widget? (lambda (widget)
			   (let ((suffix (extract-suffix widget)))
			     (equal? suffix
				     '("mode" "input"))))))
      (find mode-widget? widgets))))

(define widget-action-id->mode-value
  (lambda (mode-widget aid)
    (let ((index (lambda (val lst)
		   (let ((found (memq val (reverse lst))))
		     (if found
			 (- (length found)
			    1)
			 (error "invalid action-id for mode-widget")))))
	  (act-ids (map action-id
			(widget-actions mode-widget))))
      (index aid act-ids))))

(define widget-mode-value->action-id
  (lambda (mode-widget mode)
    (let* ((actions (widget-actions mode-widget))
	   (act-ids (map action-id actions)))
      (and (>= mode
	       0)
	   (< mode
	      (length actions))
	   (nth mode act-ids)))))

(define context-current-mode
  (lambda (context)
    (let* ((mode-widget (context-find-mode-widget context))
	   (activity (and mode-widget
			  (widget-activity mode-widget))))
      (if activity
	  (widget-action-id->mode-value mode-widget
					(action-id activity))
	  0))))

;; don't invoke directly. use context-propagate-widget-states instead
(define context-update-mode
  (lambda (context)
    (im-update-mode context (context-current-mode context))))

;; don't invoke directly. use context-propagate-widget-configuration instead
(define context-update-mode-list
  (lambda (context)
    (im-clear-mode-list context)
    (let ((mode-widget (context-find-mode-widget context)))
      (if mode-widget
	  (for-each (lambda (action)
		      (let* ((indication (action-indicate action context))
			     (label (indication-label indication)))
			(im-pushback-mode-list context label)))
		    (widget-actions mode-widget))
	  (im-pushback-mode-list context
				 (indication-label fallback-indication))))
    (im-update-mode-list context)
    (context-update-mode context)))

;; API for input method developers
;; ready to use for register-im
(define context-mode-handler
  (lambda (context mode)
    (let* ((mode-widget (context-find-mode-widget context))
	   (act-id (and mode-widget
			   (widget-mode-value->action-id mode-widget mode))))
      (and act-id
	   (widget-activate! mode-widget act-id)))))

;;
;; builtin entities
;;

(define fallback-indication
  (list 'unknown
	"?"
	(N_ "unknown")
	(N_ "unknown")))

(register-widget
 'widget_fallback
 (indicator-new (lambda (owner)
		  fallback-indication))
 #f) ;; has no actions

;; should be replaced with real separator by helper tool implementations
(register-action 'action_separator
		 (list 'separator ;; dummy indication
		       "--"
		       "--------"
		       "")
		 #f  ;; has no activity
		 #f) ;; has no handler


;;
;; widget definitions for example
;;

;;; user configs

;; controls:
;; - what widgets will be shown for user
;; - shown in what order
(define test-widgets '(;;widget_example_im_name
		       ;;widget_example_exec_im_switcher
		       ;;widget_example_arbitrary_info
		       ;;widget_example_non_selectable_item
		       ))

;;; internal definitions

(define example-im-name-indication
  (list 'im_name_example
	"example"
	"example (ja)"
	(N_ "Japanese Kana Kanji Conversion Engine, Example")))

(define example-exec-im-switcher-indication
  (list 'im_switcher
	"sw"
	(N_ "exec im-switcher")
	(N_ "exec im-switcher")))

(register-action 'action_exec_im_switcher
		 (lambda (ac)
		   example-exec-im-switcher-indication)
		 #f ;; has no activity
		 (lambda (ac)
		   (print "exec uim-im-switcher")))

;; Update widget definitions based on action configurations. The
;; procedure is needed for on-the-fly reconfiguration involving the
;; custom API
(define example-configure-widgets
  (lambda ()
    (register-widget 'widget_example_im_name
		     (indicator-new (lambda (ac)
				      example-im-name-indication))
		     #f) ;; has no actions

    (register-widget 'widget_example_arbitrary_info
		     (indicator-new (let ((count 0))
				      (lambda (ac)
					(set! count (+ count 1))
					(list (digit->string count)
					      "an arbitrary information"
					      "an arbitrary information"))))
		     ;; indicator and actions are isolated
		     (actions-new example-input-mode-actions))

    ;; requires revised protocol message to activate indicator button
    (register-widget 'widget_example_exec_im_switcher
		     ;; indicator can be an action
		     (fetch-action 'action_exec_im_switcher)
		     #f) ;; has no actions

    ;; actions can contain non-selectable but activatable item
    ;; (exec-im-switcher)
    (register-widget 'widget_example_non_selectable_item
		     (activity-indicator-new example-input-mode-actions)
		     (actions-new (cons 'action_exec_im_switcher
					example-input-mode-actions)))))
