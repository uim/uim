;;; im-custom.scm: Customization variables for im.scm
;;;
;;; Copyright (c) 2003-2007 uim Project http://uim.freedesktop.org/
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

(require "i18n.scm")

;; ugettext should be performed higher layer
(define custom-im-list-as-choice-rec
  (lambda (lst)
     (filter-map (lambda (im)
		   (and im
			(let ((sym (im-name im))
			      (name-label (ugettext (im-name-label im)))
			      (desc (ugettext (im-short-desc im))))
			  (custom-choice-rec-new sym name-label desc))))
		 lst)))
  
(define-custom-group 'global
		     (_ "Global settings")
		     (_ "long description will be here."))

(define-custom-group 'toolbar
		     (_ "Toolbar")
		     (_ "long description will be here."))

;; subgroup
(define-custom-group 'advanced
		     (_ "Advanced settings")
		     (_ "long description will be here."))

;; subgroup
(define-custom-group 'buttons
		     (_ "Buttons")
		     (_ "long description will be here."))

;; subgroup
(define-custom-group 'menu-imsw
		     (_ "Menu-based IM switcher")
		     (_ "long description will be here."))

;; subgroup
(define-custom-group 'candwin
		     (_ "Candidate window")
		     (_ "long description will be here."))

;; subgroup
(define-custom-group 'annotation
                     (_ "Annotation")
                     (_ "long description will be here."))

;; subgroup
(define-custom-group 'dictionary
                     (_ "Dictionary")
                     (_ "long description will be here."))

;; subgroup
(define-custom-group 'segment-sep
                     (_ "Segment separator")
                     (_ "long description will be here."))

;; subgroup
(define-custom-group 'mode-transition
                     (_ "Mode transition")
                     (_ "long description will be here."))

;; subgroup
(define-custom-group 'special-op
                     (_ "Special operation")
                     (_ "long description will be here."))

;; subgroup
(define-custom-group 'default-im-name
		     (_ "Default input method")
		     (_ "long description will be here."))

;; subgroup
(define-custom-group 'im-deployment
		     (_ "Input method deployment")
		     (_ "long description will be here."))

;; subgroup
(define-custom-group 'visual-preference
		     (_ "Visual preference")
		     (_ "long description will be here."))

;; 
;; default-im-name
;;

;; warning: must be defined before custom-preserved-default-im-name
(define-custom 'custom-activate-default-im-name? #f
  '(global im-deployment)
  '(boolean)
  (_ "Specify default IM")
  (_ "long description will be here."))

(define-custom 'custom-preserved-default-im-name
  (and (not (null? im-list))
       (im-name (find-default-im #f)))
  '(global im-deployment)
  (cons
   'choice
   (custom-im-list-as-choice-rec (reverse im-list)))
  (_ "Default input method")
  (_ "long description will be here."))

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
  (_ "Enabled input methods")
  (_ "long description will be here."))

(custom-add-hook 'enabled-im-list
		 'custom-get-hooks
		 (lambda ()
		   (set! enabled-im-list (delete 'direct enabled-im-list))))

;; value dependency
(if custom-full-featured?
    (custom-add-hook 'enabled-im-list
		     'custom-set-hooks
		     (lambda ()
		       (custom-set-type-info!
			'custom-preserved-default-im-name
			(cons 'choice
			      (custom-im-list-as-choice-rec
			       (map retrieve-im
				    enabled-im-list)))))))

;;
;; im-switching
;;

;; I think that current "im-switching by hotkey" feature is not
;; useful. So commented out them to avoid confusion of users.
;;   -- YamaKen 2005-02-01

;;(define-custom-group 'im-switching
;;		     (_ "Input method switching")
;;		     (_ "long description will be here."))
;;
;;(define-custom 'enable-im-switch #f
;;  '(global im-switching)
;;  '(boolean)
;;  (_ "Enable IM switching by hotkey")
;;  (_ "long description will be here."))
;;
;;(define-custom 'switch-im-key '("<Control>Shift_key" "<Shift>Control_key")
;;  '(global im-switching)
;;  '(key)
;;  (_ "IM switching key")
;;  (_ "long description will be here."))
;;
;;;; activity dependency
;;(custom-add-hook 'switch-im-key?
;;		 'custom-activity-hooks
;;		 (lambda ()
;;		   enable-im-switch))

(define-key switch-im-key? '())

;; im-toggle 
(define-custom-group 'im-toggle
		     (_ "Input method toggle")
		     (_ "long description will be here."))

(define-custom 'enable-im-toggle? #t
  '(global im-toggle)
  '(boolean)
  (_ "Enable input method toggle by hot keys")
  (_ "long description will be here."))

(define-custom 'toggle-im-key '("<Meta> ")
  '(global im-toggle)
  '(key)
  (_ "Input method toggle key")
  (_ "long description will be here."))

(define-custom 'toggle-im-alt-im 'direct
  '(global im-toggle)
  (cons
   'choice
   (if custom-full-featured?
       (custom-im-list-as-choice-rec stub-im-list)
       ()))
  (_ "Alternative input method")
  (_ "long description will be here."))

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
	(list 'uim-color-uim (_ "uim") (_ "uim native"))
	(list 'uim-color-atok (_ "ATOK like") (_ "Similar to ATOK")))
  (_ "Preedit color")
  (_ "long description will be here."))

(custom-add-hook 'uim-color
		 'custom-set-hooks
		 (lambda ()
		   (update-style uim-color-spec (symbol-value uim-color))))

;; referred by some bridges
(define-custom 'candidate-window-position 'caret
  '(global visual-preference)
  (list 'choice
	(list 'caret
	      (_ "Adjacent to cursor")
	      (_ "Adjacent to cursor"))
	(list 'left
	      (_ "Left end of preedit area")
	      (_ "Left end of preedit area"))
	(list 'right
	      (_ "Right end of preedit area")
	      (_ "Right end of preedit area")))
  (_ "Candidate window position")
  (_ "long description will be here."))

(define-custom 'enable-lazy-loading? #t
  '(global advanced)
  '(boolean)
  (_ "Enable lazy input method loading for fast startup")
  (_ "long description will be here."))


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
  (_ "Enable menu-based input method switcher")
  (_ "Show menu-based IM switcher on toolbar."))

(define-custom 'imsw-coverage 'system-global
  '(toolbar menu-imsw)
  (list 'choice
	(list 'system-global
	      (_ "whole desktop")
	      (_ "All input method of text areas on the system are changed."))
	(list 'app-global
	      (_ "focused application only")
	      (_ "Only the input method of the focused application is changed. Other text areas remain untouched."))
	(list 'focused-context
	      (_ "focused text area only")
	      (_ "Only the input method of the focused text area is changed. Other text areas remain untouched.")))
  (_ "Effective coverage")
  (_ "Specify where the IM switching takes effect."))

;; activity dependency
(custom-add-hook 'imsw-coverage
		 'custom-activity-hooks
		 (lambda ()
		   toolbar-show-action-based-switcher-button?))

(define-custom 'toolbar-show-switcher-button? #f
  '(toolbar buttons)
  '(boolean)
  (_ "full-featured input method switcher")
  (_ "Show the button on toolbar that invokes uim-im-switcher application for IM switching."))

(define-custom 'toolbar-show-pref-button? #t
  '(toolbar buttons)
  '(boolean)
  (_ "preference tool")
  (_ "long description will be here."))

(define-custom 'toolbar-show-dict-button? #f
  '(toolbar buttons)
  '(boolean)
  (_ "Japanese dictionary tool")
  (_ "long description will be here."))

(define-custom 'toolbar-show-input-pad-button? #f
  '(toolbar buttons)
  '(boolean)
  (_ "input pad")
  (_ "long description will be here."))

(define-custom 'toolbar-show-handwriting-input-pad-button? #f
  '(toolbar buttons)
  '(boolean)
  (_ "handwriting-input pad")
  (_ "long description will be here."))

(define-custom 'toolbar-show-help-button? #f
  '(toolbar buttons)
  '(boolean)
  (_ "help")
  (_ "long description will be here."))

(define-custom 'bridge-show-input-state? #f
  '(global visual-preference)
  '(boolean)
  (_ "Show input mode nearby cursor")
  (_ "long description will be here."))

(define-custom 'bridge-show-input-state-time-length 3
  '(global visual-preference)
  '(integer 0 100)
  (_ "Time length for showing input mode nearby the cursor")
  (_ "Set 0 to show indicator always."))

(custom-add-hook 'bridge-show-input-state-time-length
		 'custom-activity-hooks
		 (lambda ()
		   bridge-show-input-state?))

;; EB Library support
;; 2005-02-08 Takuro Ashie <ashie@homa.ne.jp>
;; FIXME! Here isn't suitable position for EB support preference
(define-custom-group 'eb
		     (_ "EB library")
		     (_ "long description will be here."))

(define-custom 'eb-enable-for-annotation? #f
  '(eb candwin)
  '(boolean)
  (_ "Use EB library to search annotations")
  (_ "long description will be here."))

(define-custom 'eb-dic-path
  (string-append (sys-datadir) "/dict")
  '(eb candwin)
  '(pathname directory)
  (_ "The directory which contains EB dictionary file")
  (_ "long description will be here."))

(custom-add-hook 'eb-dic-path
		 'custom-activity-hooks
		 (lambda ()
		   eb-enable-for-annotation?))

;; uim-xim specific custom
(define-custom-group 'xim
		     (_ "XIM")
		     (_ "long description will be here."))

(define-custom-group 'preedit
		     (_ "Preedit settings of XIM")
		     (_ "long description will be here."))

(define-custom 'uim-xim-use-xft-font? #f
  '(xim preedit)
  '(boolean)
  (_ "Use anti-aliased fonts for Over-the-Spot/Root-Window preedit")
  (_ "long description will be here."))

(define-custom 'uim-xim-xft-font-name "Sans"
  '(xim preedit)
  '(string ".*")
  (_ "Font name for preedit area (anti-aliased)")
  (_ "long description will be here."))

(custom-add-hook 'uim-xim-xft-font-name
		 'custom-activity-hooks
		 (lambda ()
		   uim-xim-use-xft-font?))


(if custom-full-featured?
    (for-each require-module installed-im-module-list))
