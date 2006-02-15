;;; im-switcher.scm: Action-based IM switcher
;;;
;;; Copyright (c) 2006 uim Project http://uim.freedesktop.org/
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

;; To enable the action-based IM switching, add (require "im-switcher.scm") to
;; ~/.uim

(require "util.scm")
(require "im.scm")
(require "i18n.scm")
(require "load-action.scm")

(define imsw-indication-id-alist
  '())

(define imsw-iconic-label-alist
  '((direct . "-")
    (anthy  . "A")
    (canna  . "C")
    (mana   . "M")
    (skk    . "S")
    (tcode  . "T")
    (byeoru . "B")))

(define imsw-default-iconic-label "IM")

(define imsw-indication-id
  (lambda (idname)
    (or (assq-cdr idname imsw-indication-id-alist)
	idname)))

(define imsw-iconic-label
  (lambda (idname)
    (or (assq-cdr idname imsw-iconic-label-alist)
	imsw-default-iconic-label)))

(define imsw-actions
  (lambda ()
    (reverse
     (map (lambda (im)
	    (let* ((idname (im-name im))
		   (act-name (symbolconc 'action_imsw_ idname))
		   (label (ugettext (im-name-label im)))
		   (desc (ugettext (im-short-desc im)))
		   (indication (list (imsw-indication-id idname)
				     (imsw-iconic-label idname)
				     label
				     desc)))
	      (register-action act-name
			       (lambda (ctx) ;; indication handler
				 indication)

			       (lambda (ctx) ;; activity predicate
				 (= (im-name (context-im ctx))
				    idname))

			       (lambda (ctx) ;; action handler
				 (uim-switch-im (context-id ctx) idname)
				 ;; FIXME: Switch IM of all contexts to the
				 ;; idname. It should be performed by each
				 ;; bridges via new callback, since some IM
				 ;; environments do not have the concept 'all
				 ;; context' (i.e. single-context system).
				 ;;
				 ;; FIXME: im-change-whole-desktop should not
				 ;; be called directly.
				 ;; 'im-switch-system-global-im' is
				 ;; appropriate?
				 ;;
				 ;; (im-change-whole-desktop idname)
				 ))
	      act-name))
	  im-list))))

(define imsw-register-widget
  (lambda ()
    (let ((acts (imsw-actions)))
      (register-widget 'widget_im_switcher
		       (activity-indicator-new acts)
		       (actions-new acts)))))

(define context-init-widgets-orig context-init-widgets!)
(define context-init-widgets!
  (lambda (context widget-id-list)
    (context-init-widgets-orig context
			       (cons 'widget_im_switcher widget-id-list))))

(define context-list-replace-widgets-orig context-list-replace-widgets!)
(define context-list-replace-widgets!
  (lambda (target-im-name widget-id-list)
    (context-list-replace-widgets-orig
     target-im-name
     (cons 'widget_im_switcher widget-id-list))))

(imsw-register-widget)
