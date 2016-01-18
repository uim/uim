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

;;; user configs

;; widgets and actions

;; widgets
(define direct-widgets '(widget_direct_input_mode))

;; default activity for each widgets
(define default-widget_direct_input_mode 'action_direct_direct)

;; actions of widget_direct_input_mode
(define direct-input-mode-actions
  '(action_direct_direct))


;;; implementations

(register-action 'action_direct_direct
		 (lambda (dc)
		   (list
		    'direct_input
		    "-"
		    (N_ "direct")
		    (N_ "Direct Input Mode")))
		 (lambda (dc)
		   #t)
		 #f)  ;; no action handler

;; Update widget definitions based on action configurations. The
;; procedure is needed for on-the-fly reconfiguration involving the
;; custom API
(define direct-configure-widgets
  (lambda ()
    (register-widget 'widget_direct_input_mode
		     (activity-indicator-new direct-input-mode-actions)
		     (actions-new direct-input-mode-actions))))

(define direct-context-rec-spec context-rec-spec)
(define-record 'direct-context direct-context-rec-spec)
(define direct-context-new-internal direct-context-new)

(define direct-context-new
  (lambda args
    (let ((dc (apply direct-context-new-internal args)))
      (direct-context-set-widgets! dc direct-widgets)
      dc)))

(define direct-push-back-mode
  (lambda (dc lst)
    (if (car lst)
	(begin
	  (im-pushback-mode-list dc (caar lst))
	  (direct-push-back-mode dc (cdr lst))))))

(define direct-init-handler
  (lambda (id im arg)
    (let ((dc (direct-context-new id im)))
      ;;(im-clear-mode-list dc)
      ;;(direct-push-back-mode dc im-list)
      ;;(im-update-mode-list dc)
      ;;(im-update-mode dc (- (length im-list) 1))
      dc)))

(define direct-release-handler
  (lambda (dc)
    #f))

(define direct-key-press-handler
  (lambda (dc key state)
    (im-commit-raw dc)))

(define direct-key-release-handler
  (lambda (dc key state)
    (im-commit-raw dc)))

(define direct-reset-handler
  (lambda (dc)
    #f))

;;(define direct-mode-handler
;;  (lambda (dc mode)
;;    (create-context (direct-context-id dc)
;;    		    #f
;;    		    (car (nth mode im-list)))
;;    #f))

(define direct-get-candidate-handler
  (lambda (dc idx)
    #f))

(define direct-set-candidate-index-handler
  (lambda (dc idx)
    #f))

(direct-configure-widgets)

(register-im
 'direct
 "*"  ;; wildcard language. see i18n.scm
 "UTF-8"
 (N_ "Direct")
 (N_ "Pass through all user input without any modification")
 #f
 direct-init-handler
 direct-release-handler
 context-mode-handler
 direct-key-press-handler
 direct-key-release-handler
 direct-reset-handler
 direct-get-candidate-handler
 direct-set-candidate-index-handler
 context-prop-activate-handler
 #f
 #f
 #f
 #f
 #f
 )
