;;; im-custom.scm: Customization variables for im.scm
;;;
;;; Copyright (c) 2003-2013 uim Project http://code.google.com/p/uim/
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

(require-extension (srfi 1 34))

(require "i18n.scm")
(require "xkb.scm")

(define custom-im-list-as-choice-rec
  (lambda (lst)
     (filter-map (lambda (im)
		   (and im
			(let ((sym (im-name im))
			      (name-label (im-name-label im))
			      (desc (im-short-desc im)))
			  (custom-choice-rec-new sym name-label desc))))
		 lst)))
  
(define-custom-group 'global
		     (N_ "Global settings")
		     (N_ "long description will be here."))

(define-custom-group 'toolbar
		     (N_ "Toolbar")
		     (N_ "long description will be here."))

(define-custom-group 'annotation
                     (N_ "Annotation")
                     (N_ "long description will be here."))

(define-custom-group 'xim
		     (N_ "XIM")
		     (N_ "long description will be here."))

(define-custom-group 'xkb
		     (N_ "Xkb")
		     (N_ "long description will be here."))

(define-custom-group 'notify
		     (N_ "Notify")
		     (N_ "long description will be here."))

(define-custom-group 'http
		     (N_ "Http")
		     (N_ "long description will be here."))

;; subgroup
(define-custom-group 'advanced
		     (N_ "Advanced settings")
		     (N_ "long description will be here."))

;; subgroup
(define-custom-group 'buttons
		     (N_ "Buttons")
		     (N_ "long description will be here."))

;; subgroup
(define-custom-group 'menu-imsw
		     (N_ "Menu-based IM switcher")
		     (N_ "long description will be here."))

;; subgroup
(define-custom-group 'candwin
		     (N_ "Candidate window")
		     (N_ "long description will be here."))

;; subgroup
(define-custom-group 'dictionary
                     (N_ "Dictionary")
                     (N_ "long description will be here."))

;; subgroup
(define-custom-group 'segment-sep
                     (N_ "Segment separator")
                     (N_ "long description will be here."))

;; subgroup
(define-custom-group 'mode-transition
                     (N_ "Mode transition")
                     (N_ "long description will be here."))

;; subgroup
(define-custom-group 'special-op
                     (N_ "Special operation")
                     (N_ "long description will be here."))

;; subgroup
(define-custom-group 'default-im-name
		     (N_ "Default input method")
		     (N_ "long description will be here."))

;; subgroup
(define-custom-group 'im-deployment
		     (N_ "Input method deployment")
		     (N_ "long description will be here."))

;; subgroup
(define-custom-group 'visual-preference
		     (N_ "Visual preference")
		     (N_ "long description will be here."))

;; subgroup
(define-custom-group 'xim-preedit
		     (N_ "Preedit settings of XIM")
		     (N_ "long description will be here."))

;; subgroup
(define-custom-group 'xkb-preference
		     (N_ "Settings of X Keyboard Extension")
		     (N_ "long description will be here."))

;; subgroup
(define-custom-group 'toolbar-help
                     (N_ "Help")
                     (N_ "long description will be here."))

;; subgroup
(define-custom-group 'toolbar-icon
                     (N_ "Icon")
                     (N_ "long description will be here."))

;; subgroup
(define-custom-group 'toolbar-display
                     (N_ "Display behavior")
                     (N_ "long description will be here."))

;; subgroup
(define-custom-group 'toolbar-widget
                     (N_ "Toolbar")
                     (N_ "long description will be here."))


;; 
;; default-im-name
;;

;; warning: must be defined before custom-preserved-default-im-name
(define-custom 'custom-activate-default-im-name? #f
  '(global im-deployment)
  '(boolean)
  (N_ "Specify default IM")
  (N_ "long description will be here."))

(define-custom 'custom-preserved-default-im-name
  (and (not (null? im-list))
       (im-name (find-default-im #f)))
  '(global im-deployment)
  (cons
   'choice
   (custom-im-list-as-choice-rec (reverse im-list)))
  (N_ "Default input method")
  (N_ "long description will be here."))

;; activity dependency
(custom-add-hook 'custom-preserved-default-im-name
		 'custom-activity-hooks
		 (lambda ()
		   custom-activate-default-im-name?))

(define custom-hook-get-default-im-name
  (lambda ()
    (set! custom-activate-default-im-name? default-im-name)
    (set! custom-preserved-default-im-name (or default-im-name
					       custom-preserved-default-im-name
					       (im-name (find-default-im #f))))))

;; decode #f from default-im-name
(custom-add-hook 'custom-activate-default-im-name?
		 'custom-get-hooks
		 custom-hook-get-default-im-name)
(custom-add-hook 'custom-preserved-default-im-name
		 'custom-get-hooks
		 custom-hook-get-default-im-name)

(define custom-hook-set-default-im-name
  (lambda ()
    (set! default-im-name
	  (and custom-activate-default-im-name?
	       custom-preserved-default-im-name))))

;; encode #f into default-im-name
(custom-add-hook 'custom-activate-default-im-name?
		 'custom-set-hooks
		 custom-hook-set-default-im-name)
(custom-add-hook 'custom-preserved-default-im-name
		 'custom-set-hooks
		 custom-hook-set-default-im-name)

(define custom-hook-literalize-preserved-default-im-name
  (lambda ()
    (string-append
     "(define custom-preserved-default-im-name "
     (custom-value-as-literal 'custom-preserved-default-im-name)
     ")\n"
     "(define default-im-name "
     (if default-im-name
	 (string-append "'" (symbol->string default-im-name))
	 "#f")
     ")")))

(custom-add-hook 'custom-preserved-default-im-name
		 'custom-literalize-hooks
		 custom-hook-literalize-preserved-default-im-name)

;;
;; Enabled IM list
;;

(define usable-im-list
  (lambda ()
    (let ((imlist (filter (lambda (name)
			    (memq name installed-im-list))
			  enabled-im-list)))
	 (if (not (null? imlist))
	     imlist
	     '(direct)))))

(define-custom 'enabled-im-list (usable-im-list)
  '(global im-deployment)
  (cons
   'ordered-list
   (if custom-full-featured?
       (custom-im-list-as-choice-rec (alist-delete 'direct stub-im-list))
       ()))
  (N_ "Enabled input methods")
  (N_ "long description will be here."))

(custom-add-hook 'enabled-im-list
		 'custom-get-hooks
		 (lambda ()
		   (set! enabled-im-list (delete 'direct enabled-im-list))))

(define retrieve-im-for-custom-choice
  (lambda (name)
    (and name
	 (if (eq? name 'direct) ;; 'direct is not in the stub-im-list
	     (assq 'direct im-list)
	     (let ((im (assq name stub-im-list)))
	       im)))))

(define update-imsw-widget-of-context-widgets
  (lambda ()
    ;; update im-list
    (for-each (lambda (stub)
		(if (memq (stub-im-name stub) enabled-im-list)
		    (if enable-lazy-loading?
			(apply register-stub-im stub)
			(require-module (stub-im-module-name stub)))))
	      stub-im-list)
    (set! im-list
      (remove (lambda (im)
		(and
		  (not (memq (im-name im) enabled-im-list))
		  (not (eq? (im-name im) 'direct))))
	      im-list))

    ;; update imsw widget
    (if toolbar-show-action-based-switcher-button?
	(let ((acts (imsw-actions)))
	  (register-widget 'widget_im_switcher
			   (activity-indicator-new acts)
			   (actions-new acts))
	  (for-each (lambda (ctx)
		      (let* ((widgets (context-widgets ctx))
			     (widget-ids (map car widgets)))
			(context-init-widgets! ctx widget-ids)))
		    context-list)))))

;; value dependency
(if custom-full-featured?
    (custom-add-hook 'enabled-im-list
		     'custom-set-hooks
		     (lambda ()
		       (custom-set-type-info!
			'custom-preserved-default-im-name
			(cons 'choice
			      (custom-im-list-as-choice-rec
			       (map retrieve-im-for-custom-choice
			            (if (not (memq 'direct enabled-im-list))
				        (append enabled-im-list '(direct))
				        enabled-im-list)))))
		       (custom-set-type-info!
			'toggle-im-alt-im
			(cons 'choice
			      (custom-im-list-as-choice-rec
			       (map retrieve-im-for-custom-choice
			            (if (not (memq 'direct enabled-im-list))
				        (append enabled-im-list '(direct))
				        enabled-im-list)))))))
    ;; for non- full-featured
    (custom-add-hook 'enabled-im-list
		     'custom-set-hooks
		     update-imsw-widget-of-context-widgets))

;;
;; im-switching
;;
(define-custom-group 'im-switching
  (N_ "Input method switching")
  (N_ "long description will be here."))

(define-custom 'enable-im-switch? #f
  '(global im-switching)
  '(boolean)
  (N_ "Enable IM switching by hotkey")
  (N_ "long description will be here."))

(define-custom 'switch-im-key '("<Control>Shift_key" "<Shift>Control_key")
  '(global im-switching)
  '(key)
  (N_ "IM switching key")
  (N_ "long description will be here."))

(define-custom 'switch-im-skip-direct-im? #f
  '(global im-switching)
  '(boolean)
  (N_ "Skip direct method for IM switching by hotkey")
  (N_ "long description will be here."))

;; activity dependency
(custom-add-hook 'switch-im-key
		 'custom-activity-hooks
		 (lambda ()
		   enable-im-switch?))

(custom-add-hook 'switch-im-skip-direct-im?
		 'custom-activity-hooks
		 (lambda ()
		   enable-im-switch?))

;; im-toggle 
(define-custom-group 'im-toggle
		     (N_ "Input method toggle")
		     (N_ "long description will be here."))

(define-custom 'enable-im-toggle? #t
  '(global im-toggle)
  '(boolean)
  (N_ "Enable input method toggle by hot keys")
  (N_ "long description will be here."))

(define-custom 'toggle-im-key '("<Meta> ")
  '(global im-toggle)
  '(key)
  (N_ "Input method toggle key")
  (N_ "long description will be here."))

(define-custom 'toggle-im-alt-im 'direct
  '(global im-toggle)
  (cons
   'choice
   (if custom-full-featured?
       (custom-im-list-as-choice-rec (reverse im-list))
       ()))
  (N_ "Alternative input method")
  (N_ "long description will be here."))

(custom-add-hook 'toggle-im-alt-im
		 'custom-set-hooks
		 (lambda ()
		   (for-each (lambda (ctx)
			       (reset-toggle-context! (context-id ctx) ctx))
			     context-list)))


;; activity dependency
(custom-add-hook 'toggle-im-key
		 'custom-activity-hooks
		 (lambda ()
		   enable-im-toggle?))

(custom-add-hook 'toggle-im-alt-im
		 'custom-activity-hooks
		 (lambda ()
		   enable-im-toggle?))


(define-custom 'uim-color 'uim-color-uim
  '(global visual-preference)
  (list 'choice
	(list 'uim-color-uim (N_ "uim") (N_ "uim native"))
	(list 'uim-color-atok (N_ "ATOK like") (N_ "Similar to ATOK")))
  (N_ "Preedit color")
  (N_ "long description will be here."))

(custom-add-hook 'uim-color
		 'custom-set-hooks
		 (lambda ()
		   (update-style uim-color-spec (symbol-value uim-color))))

(define-custom 'candidate-window-style 'vertical
  '(global visual-preference)
  (list 'choice
	(list 'vertical
	      (N_ "Vertical")
	      (N_ "long description will be here"))
	(list 'horizontal
	      (N_ "Horizontal")
	      (N_ "long description will be here"))
	(list 'table
	      (N_ "Table style")
	      (N_ "long description will be here")))
  (N_ "Candidate window type")
  (N_ "long description will be here."))

;; referred by some bridges
(define-custom 'candidate-window-position 'caret
  '(global visual-preference)
  (list 'choice
	(list 'caret
	      (N_ "Adjacent to cursor")
	      (N_ "Adjacent to cursor"))
	(list 'left
	      (N_ "Left end of preedit area")
	      (N_ "Left end of preedit area"))
	(list 'right
	      (N_ "Right end of preedit area")
	      (N_ "Right end of preedit area")))
  (N_ "Candidate window position")
  (N_ "long description will be here."))

(define-custom 'enable-lazy-loading? #t
  '(global advanced)
  '(boolean)
  (N_ "Enable lazy input method loading for fast startup")
  (N_ "long description will be here."))

(custom-add-hook 'enable-lazy-loading?
		 'custom-set-hooks
		 (lambda ()
		   (if enable-lazy-loading?
		       (require "lazy-load.scm"))))

(define-custom 'toolbar-display-time 'always
  '(toolbar toolbar-display)
  (list 'choice
        (list 'always
              (N_ "Always")
              (N_ "long description will be here."))
        (list 'mode
              (N_ "Based on mode")
              (N_ "long description will be here."))
        (list 'never
              (N_ "Never")
              (N_ "long description will be here.")))
  (N_ "Display")
  (N_ "long description will be here."))

;;
;; toolbar buttons
;;

(define imsw-reconfigure
  (lambda ()
    (if toolbar-show-action-based-switcher-button?
	(require "im-switcher.scm"))
    ;; Since context-list is empty on start-up, imsw-register-widget
    ;; is not called here.
    (if (and
	 (symbol-bound? 'context-refresh-switcher-widget!)
	 toolbar-show-action-based-switcher-button?)
	(for-each context-refresh-switcher-widget!
		  context-list))))

;; must be hooked before 'toolbar-show-action-based-switcher-button?
;; definition
(custom-add-hook 'toolbar-show-action-based-switcher-button?
		 'custom-set-hooks
		 imsw-reconfigure)

(define-custom 'toolbar-show-action-based-switcher-button? #t
  '(toolbar menu-imsw)
  '(boolean)
  (N_ "Enable menu-based input method switcher")
  (N_ "Show menu-based IM switcher on toolbar."))

(define-custom 'imsw-coverage 'system-global
  '(toolbar menu-imsw)
  (list 'choice
	(list 'system-global
	      (N_ "whole desktop")
	      (N_ "All input method of text areas on the system are changed."))
	(list 'app-global
	      (N_ "focused application only")
	      (N_ "Only the input method of the focused application is changed. Other text areas remain untouched."))
	(list 'focused-context
	      (N_ "focused text area only")
	      (N_ "Only the input method of the focused text area is changed. Other text areas remain untouched.")))
  (N_ "Effective coverage")
  (N_ "Specify where the IM switching takes effect."))

;; activity dependency
(custom-add-hook 'imsw-coverage
		 'custom-activity-hooks
		 (lambda ()
		   toolbar-show-action-based-switcher-button?))

(define-custom 'toolbar-show-switcher-button? #f
  '(toolbar buttons)
  '(boolean)
  (N_ "full-featured input method switcher")
  (N_ "Show the button on toolbar that invokes uim-im-switcher application for IM switching."))

(define-custom 'toolbar-show-pref-button? #t
  '(toolbar buttons)
  '(boolean)
  (N_ "preference tool")
  (N_ "long description will be here."))

(define-custom 'toolbar-show-dict-button? #f
  '(toolbar buttons)
  '(boolean)
  (N_ "Japanese dictionary tool")
  (N_ "long description will be here."))

(define-custom 'toolbar-show-input-pad-button? #f
  '(toolbar buttons)
  '(boolean)
  (N_ "input pad")
  (N_ "long description will be here."))

(define-custom 'toolbar-show-handwriting-input-pad-button? #f
  '(toolbar buttons)
  '(boolean)
  (N_ "handwriting-input pad")
  (N_ "long description will be here."))

(define-custom 'toolbar-show-help-button? #f
  '(toolbar buttons)
  '(boolean)
  (N_ "help")
  (N_ "long description will be here."))

(define-custom 'toolbar-help-browser 'system
  '(toolbar toolbar-help)
  (list 'choice
        (list 'system
              (N_ "System")
              (N_ "long description will be here."))
        (list 'manual
              (N_ "Manual")
              (N_ "long description will be here.")))
  (N_ "Document browser")
  (N_ "long description will be here."))

(define-custom 'toolbar-help-browser-name "firefox"
  '(toolbar toolbar-help)
  '(string ".*")
  (N_ "Browser name")
  (N_ "long description will be here."))

(custom-add-hook 'toolbar-help-browser-name
		 'custom-activity-hooks
		 (lambda ()
                   (eq? toolbar-help-browser 'manual)))

(define-custom 'toolbar-icon-for-dark-background? #f
  '(toolbar toolbar-icon)
  '(boolean)
  (N_ "Use icon for dark background")
  (N_ "long description will be here."))

(define-custom 'bridge-show-input-state? #f
  '(global visual-preference)
  '(boolean)
  (N_ "Show input mode nearby cursor")
  (N_ "long description will be here."))

(define-custom 'bridge-show-with? 'time
  '(global visual-preference)
  (list 'choice
        (list 'mode
              (N_ "With mode")
              (N_ "long description will be here."))
        (list 'time
              (N_ "With time")
              (N_ "long description will be here.")))
  (N_ "Show input mode")
  (N_ "long description will be here."))

(custom-add-hook 'bridge-show-with?
		 'custom-activity-hooks
		 (lambda ()
                   bridge-show-input-state?))

(define-custom 'bridge-show-input-state-time-length 3
  '(global visual-preference)
  '(integer 0 100)
  (N_ "Time length for showing input mode nearby the cursor")
  (N_ "Set 0 to show indicator always."))

(custom-add-hook 'bridge-show-input-state-time-length
		 'custom-activity-hooks
		 (lambda ()
		   (and bridge-show-input-state?
                        (eq? bridge-show-with?
                             'time))))

;;
;; uim-xim specific custom
;;

(define-custom 'uim-xim-use-xft-font? #f
  '(xim xim-preedit)
  '(boolean)
  (N_ "Use anti-aliased fonts for Over-the-Spot/Root-Window preedit")
  (N_ "long description will be here."))

(define-custom 'uim-xim-xft-font-name "Sans"
  '(xim xim-preedit)
  '(string ".*")
  (N_ "Font name for preedit area (anti-aliased)")
  (N_ "long description will be here."))

(custom-add-hook 'uim-xim-xft-font-name
		 'custom-activity-hooks
		 (lambda ()
		   uim-xim-use-xft-font?))

;;
;; X Keyboard Extension specific custom
;;

(define-custom 'xkb-save-map? #f
  '(xkb xkb-preference)
  '(boolean)
  (N_ "Save keyboard map to load in Xkb-less environments")
  (N_ "long description will be here."))

(custom-add-hook 'xkb-save-map?
		 'custom-activity-hooks
		 (lambda ()
		   (and xkb-plugin-ready? (xkb-lib-display-ready?))))

(custom-add-hook 'xkb-save-map?
		 'custom-set-hooks
		 (lambda ()
		   (if xkb-save-map?
		       (let ((m (xkb-lib-get-map)))
			 (if m (guard (err
				       (else #f))
				      (get-config-path! #t)
				      (call-with-output-file xkb-map-path
					(lambda (p) (write m p)))))))))

(define-custom 'xkb-map-path
  (string-append (or (get-config-path #t) "") "/xkb-map")
  '(xkb xkb-preference)
  '(pathname regular-file)
  (N_ "Location of keyboard map file")
  (N_ "long description will be here."))

(custom-add-hook 'xkb-map-path
		 'custom-activity-hooks
		 (lambda ()
		   xkb-plugin-ready?))

;;
;; Notify
;;

(define-custom 'notify-agent 'stderr
  '(notify)
  `(choice
    ,@(uim-notify-get-plugins))
  (N_ "Notify agent name")
  (N_ "long description will be here."))

(custom-add-hook 'notify-agent
                 'custom-set-hooks
                 (lambda ()
                   (if (symbol-bound? 'uim-notify-load)
                     (uim-notify-load (symbol->string
                                        notify-agent)))))

;;
;; Http
;;

(define-custom 'http-proxy-setting 'direct
  '(http)
  (list 'choice
        (list 'direct
              (N_ "Direct connection")
              (N_ "Direct connection, no proxy."))
        (list 'user
              (N_ "User")
              (N_ "Use proxy."))
        ;;(list 'system
        ;;      (N_ "System")
        ;;      (N_ "Use proxy with system setting.")))
        )
  (N_ "Proxy setting")
  (N_ "long description will be here."))

(define-custom 'http-proxy-hostname "localhost"
  '(http)
  '(string ".*")
  (N_ "Http proxy hostname")
  (N_ "long description will be here."))

(custom-add-hook 'http-proxy-hostname
		 'custom-activity-hooks
		 (lambda ()
                   (eq? http-proxy-setting
                        'user)))

(define-custom 'http-proxy-port 8080
  '(http)
  '(integer 0 65535)
  (N_ "Http proxy port")
  (N_ "long description will be here."))

(custom-add-hook 'http-proxy-port
		 'custom-activity-hooks
		 (lambda ()
                   (eq? http-proxy-setting
                        'user)))

(define-custom 'http-timeout 3000
  '(http)
  '(integer 0 65535)
  (N_ "Timeout (msec)")
  (N_ "Timeout of http connection (msec)."))

(load "predict-custom.scm")


(if custom-full-featured?
    (for-each require-module installed-im-module-list))
