;;; im-custom.scm: Customization variables for im.scm
;;;
;;; Copyright (c) 2003-2005 uim Project http://uim.freedesktop.org/
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

(define custom-installed-im-list
  (lambda ()
    (let ((orig-enabled-im-list enabled-im-list)
	  (orig-require require))
      (set! enabled-im-list ())  ;; enable all IMs
      ;; XXX temporary solution to register all IM in a file
      (set! require
	    (lambda (file)
	      (let* ((file-sym (string->symbol file))
		     (loaded-sym (symbolconc '* file-sym '-loaded*))
		     (reloaded-sym (symbolconc '* file-sym '-reloaded*)))
		(cond
		 ((symbol-bound? reloaded-sym)
		  loaded-sym)
		 ((try-load file)
		  (eval (list 'define loaded-sym #t)
			toplevel-env)
		  (eval (list 'define reloaded-sym #t)
			toplevel-env)
		  loaded-sym)
		 (else
		  #f)))))
      (for-each require-module installed-im-module-list)
      (set! require orig-require)
      (set! enabled-im-list orig-enabled-im-list)
      (custom-im-list-as-choice-rec (reverse
				     (alist-delete 'direct im-list eq?))))))

(define-custom 'enabled-im-list '(direct)
  '(global im-deployment)
  (cons
   'ordered-list
   (if custom-full-featured?
       (custom-installed-im-list)
       ()))
  (_ "Enabled input methods")
  (_ "long description will be here."))

(custom-add-hook 'enabled-im-list
		 'custom-get-hooks
		 (lambda ()
		   (set! enabled-im-list (remove (lambda (name)
						   (eq? name
							'direct))
						 enabled-im-list))))

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

(define-custom 'uim-color 'uim-color-uim
  '(global visual-preference)
  (list 'choice
	(list 'uim-color-uim (_ "uim") (_ "uim native"))
	(list 'uim-color-atok (_ "ATOK like") (_ "Similar to ATOK")))
  (_ "Preedit color")
  (_ "long description will be here."))

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

;; toolbar buttons
(define-custom 'toolbar-show-switcher-button? #t
  '(toolbar buttons)
  '(boolean)
  (_ "Show input method switcher button on toolbar")
  (_ "long description will be here."))

(define-custom 'toolbar-show-pref-button? #t
  '(toolbar buttons)
  '(boolean)
  (_ "Show uim preference tool button on toolbar")
  (_ "long description will be here."))

(define-custom 'toolbar-show-dict-button? #f
  '(toolbar buttons)
  '(boolean)
  (_ "Show uim Japanese dictionary tool button on toolbar")
  (_ "long description will be here."))

(define-custom 'toolbar-show-input-pad-button? #f
  '(toolbar buttons)
  '(boolean)
  (_ "Show uim input pad button on toolbar")
  (_ "long description will be here."))

(define-custom 'toolbar-show-handwriting-input-pad-button? #f
  '(toolbar buttons)
  '(boolean)
  (_ "Show uim handwriting-input pad button on toolbar")
  (_ "long description will be here."))

(define-custom 'toolbar-show-help-button? #f
  '(toolbar buttons)
  '(boolean)
  (_ "Show uim help button on toolbar")
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
  (_ "long description will be here."))

;; EB Library support
;; 2005-02-08 Takuro Ashie <ashie@homa.ne.jp>
;; FIXME! Here isn't suitable position for EB support preference
(define-custom-group 'eb
		     (_ "EB library settings")
		     (_ "long description will be here."))

(define-custom 'eb-enable-for-annotation? #f
  '(eb candwin)
  '(boolean)
  (_ "Use EB library to search annotations")
  (_ "long description will be here."))

(define-custom 'eb-dic-path
  (string-append (getenv "HOME") "/dict")
  '(eb candwin)
  '(pathname)
  (_ "The directory which contains EB dictionary file")
  (_ "long description will be here."))


;; uim-xim specific custom
(define-custom-group 'xim
		     (_ "XIM settings")
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
