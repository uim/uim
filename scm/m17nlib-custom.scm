;;;
;;; Copyright (c) 2012-2013 uim Project https://github.com/uim/uim
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

(define-custom-group 'm17nlib
                     (N_ "m17nlib")
                     (N_ "A multilingual text processing library engine"))

;;
;; toolbar
;;

;; Can't be unified with action definitions in skk.scm until uim
;; 0.4.6.
(define m17nlib-input-mode-indication-alist
  (list
   (list 'action_m17nlib_off
	 'off
	 "-"
	 (N_ "off")
	 (N_ "Direct input mode"))
   (list 'action_m17nlib_on
	 'on
	 "O"
	 (N_ "on")
	 (N_ "m17nlib mode"))))

;;; Buttons

(define-custom 'm17nlib-widgets '(widget_m17nlib_input_mode)
  '(m17nlib toolbar-widget)
  (list 'ordered-list
	(list 'widget_m17nlib_input_mode
	      (N_ "Input mode")
	      (N_ "Input mode")))
  (N_ "Enabled toolbar buttons")
  (N_ "long description will be here."))

;; dynamic reconfiguration
;; m17nlib-configure-widgets is not defined at this point. So wrapping
;; into lambda.
(custom-add-hook 'm17nlib-widgets
		 'custom-set-hooks
		 (lambda ()
		   (m17nlib-configure-widgets)))

;;; Input mode

(define-custom 'default-widget_m17nlib_input_mode 'action_m17nlib_off
  '(m17nlib toolbar-widget)
  (cons 'choice
	(map indication-alist-entry-extract-choice
	     m17nlib-input-mode-indication-alist))
  (N_ "Default input mode")
  (N_ "long description will be here."))

(define-custom 'm17nlib-input-mode-actions
               (map car m17nlib-input-mode-indication-alist)
  '(m17nlib toolbar-widget)
  (cons 'ordered-list
	(map indication-alist-entry-extract-choice
	     m17nlib-input-mode-indication-alist))
  (N_ "Input mode menu items")
  (N_ "long description will be here."))

;; value dependency
(if custom-full-featured?
    (custom-add-hook 'm17nlib-input-mode-actions
		     'custom-set-hooks
		     (lambda ()
		       (custom-choice-range-reflect-olist-val
			'default-widget_m17nlib_input_mode
			'm17nlib-input-mode-actions
			m17nlib-input-mode-indication-alist))))

;; activity dependency
(custom-add-hook 'default-widget_m17nlib_input_mode
		 'custom-activity-hooks
		 (lambda ()
		   (memq 'widget_m17nlib_input_mode m17nlib-widgets)))

(custom-add-hook 'm17nlib-input-mode-actions
		 'custom-activity-hooks
		 (lambda ()
		   (memq 'widget_m17nlib_input_mode m17nlib-widgets)))

;; dynamic reconfiguration
;; m17nlib-configure-widgets is not defined at this point. So wrapping
;; into lambda.
(custom-add-hook 'default-widget_m17n_input_mode
		 'custom-set-hooks
		 (lambda ()
		   (m17nlib-configure-widgets)))

(custom-add-hook 'm17nlib-input-mode-actions
		 'custom-set-hooks
		 (lambda ()
		   (m17nlib-configure-widgets)))

;;
;; candidate window
;;

(define-custom 'm17nlib-use-candidate-window? #t
  '(m17nlib candwin)
  '(boolean)
  (N_ "Use candidate window")
  (N_ "long description will be here."))
