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

(require "generic-key.scm")

;;; user configs

(define m17nlib-candidate-max 10)

;; key defs
(define-key m17nlib-on-key? 'generic-on-key?)
(define-key m17nlib-off-key? 'generic-off-key?)

;; widgets and actions

;; widgets
(define m17nlib-widgets '(widget_m17nlib_input_mode))

;; default activity for each widgets
(define default-widget_m17nlib_input_mode 'action_m17nlib_off)

;; actions of widget_m17nlib_input_mode
(define m17nlib-input-mode-actions
  '(action_m17nlib_off
    action_m17nlib_on))


;;; implementations

(register-action 'action_m17nlib_off
		 (lambda (mc)
		   (list
		    'figure_m17nlib_off
		    "o"
		    (N_ "off")
		    (N_ "Direct Input Mode")))
		 (lambda (mc)
		   (not (m17nlib-context-on mc)))
		 (lambda (mc)
		   (m17nlib-context-set-on! mc #f)))

(register-action 'action_m17nlib_on
		 (lambda (mc)
		   (let* ((im (m17nlib-context-im mc))
			  (name (symbol->string (im-name im))))
		     (list
		      'figure_m17nlib_on
		      "O"
		      (N_ "on")
		      (string-append name (N_ " Mode")))))
		 (lambda (mc)
		   (m17nlib-context-on mc))
		 (lambda (mc)
		   (m17nlib-context-set-on! mc #t)))

;; Update widget definitions based on action configurations. The
;; procedure is needed for on-the-fly reconfiguration involving the
;; custom API
(define m17nlib-configure-widgets
  (lambda ()
    (register-widget 'widget_m17nlib_input_mode
		     (activity-indicator-new m17nlib-input-mode-actions)
		     (actions-new m17nlib-input-mode-actions))))

(define m17nlib-context-rec-spec
  (append
   context-rec-spec
   ;; renamed from 'id' to avoid conflict with context-id
   '((mc-id             #f)
     (on                #f)
     (showing-candidate #f))))
(define-record 'm17nlib-context m17nlib-context-rec-spec)
(define m17nlib-context-new-internal m17nlib-context-new)

(define m17nlib-context-new
  (lambda (id im name)
    (let ((mc (m17nlib-context-new-internal id im))
	  (mc-id (m17nlib-lib-alloc-context name)))
      (m17nlib-context-set-widgets! mc m17nlib-widgets)
      (m17nlib-context-set-mc-id! mc mc-id)
      mc)))

(define m17nlib-update-preedit
  (lambda (mc)
    (let* ((mid (m17nlib-context-mc-id mc)))
      (if (m17nlib-lib-preedit-changed? mid)
	  (if (m17nlib-lib-compose-mode? mid)
	      (begin
		(im-clear-preedit mc)
		(im-pushback-preedit mc
				     preedit-underline
				     (m17nlib-lib-get-left-of-candidate mid))
		(im-pushback-preedit mc
				     (+ preedit-reverse preedit-cursor)
				     (m17nlib-lib-get-selected-candidate mid))
		(im-pushback-preedit mc
				     preedit-underline
				     (m17nlib-lib-get-right-of-candidate mid))
		(im-update-preedit mc))
	      (begin
		(im-clear-preedit mc)
		(im-pushback-preedit mc
				     preedit-underline
				     (m17nlib-lib-get-left-of-cursor mid))
		(im-pushback-preedit mc
				     preedit-cursor "")
		(im-pushback-preedit mc
				     preedit-underline
				     (m17nlib-lib-get-right-of-cursor mid))
		(im-update-preedit mc))
	      )))))

(define m17nlib-update-candidate
  (lambda (mc)
    (let* ((mid (m17nlib-context-mc-id mc))
	   (max (m17nlib-lib-get-nr-candidates mid))
	   (showing-candidate? (m17nlib-context-showing-candidate mc)))
      (if showing-candidate?
	  (if (m17nlib-lib-candidate-show? mid)
	      (im-select-candidate mc (m17nlib-lib-get-candidate-index mid))
	      (begin
		(m17nlib-context-set-showing-candidate! mc #f)
		(im-deactivate-candidate-selector mc mid))) ;end showing candidate
	  (if (m17nlib-lib-candidate-show? mid)
	      (begin
		(im-activate-candidate-selector
		 mc
		 max m17nlib-candidate-max)
		(im-select-candidate mc (m17nlib-lib-get-candidate-index mid))
		(m17nlib-context-set-showing-candidate! mc #t)))
	  ))));nothing to do

(define m17nlib-append-modifiers
  (lambda (key key-state key-str)
    (if (shift-key-mask key-state)
	(set! key-str (string-append "S-" key-str)))
    (if (control-key-mask key-state)
	(set! key-str (string-append "C-" key-str)))
    (if (alt-key-mask key-state)
	(set! key-str (string-append "A-" key-str)))
    (if (meta-key-mask key-state)
	(set! key-str (string-append "M-" key-str)))
    key-str))

(define m17nlib-proc-direct-state
  (lambda (mc key key-state)
   (if (m17nlib-on-key? key key-state)
       (m17nlib-context-set-on! mc #t)
       (m17nlib-commit-raw mc))))

(define m17nlib-commit-raw
  (lambda (mc)
    (im-commit-raw mc)))


; Unfortunatelly, we don't have simple way to translate...
(define m17nlib-translate-ukey-to-mkey
  (lambda (key key-state)
    (m17nlib-append-modifiers
     key key-state
     (cdr (assq key
		'((backspace       . "BackSpace")
		  (delete          . "Delete")
		  (escape          . "Escape")
		  (return          . "Return")
		  (tab             . "Tab")
		  (left            . "Left")
		  (up              . "Up")
		  (right           . "Right")
		  (down            . "Down")
		  (prior           . "Page_Down")
		  (next            . "Page_Up")
		  (home            . "Home")
		  (end             . "End")
		  (zenkaku-hankaku . "")
		  (Multi_key       . "")
		  (Mode_switch     . "")
		  (Henkan_Mode     . "")
		  (Muhenkan        . "")
		  (F1              . "F1")
		  (F2              . "F2")
		  (F3              . "F3")
		  (F4              . "F4")
		  (F5              . "F5")
		  (F6              . "F6")
		  (F7              . "F7")
		  (F8              . "F8")
		  (F9              . "F9")
		  (F10             . "F10")
		  (F11             . "F11")
		  (F12             . "F12")
		  (F13             . "F13")
		  (F14             . "F14")
		  (F15             . "F15")
		  (F16             . "F16")
		  (F17             . "F17")
		  (F18             . "F18")
		  (F19             . "F19")
		  (F20             . "F20")))))))
  
(define m17nlib-init-handler
  (lambda (id im arg)
    (m17nlib-context-new id im arg)))

(define m17nlib-release-handler
  (lambda (mc)
    #f))

(define m17nlib-push-key
  (lambda (mc key key-state)
    (let* ((mid (m17nlib-context-mc-id mc)))
      (cond
       ((m17nlib-off-key? key key-state)
	(m17nlib-context-set-on! mc #f)
	#t) ;; #t means key event was consumed.
       ((symbol? key)
	(let ((mkey (m17nlib-translate-ukey-to-mkey key key-state)))
	  (m17nlib-lib-push-symbol-key mid mkey)))
       (else
	(m17nlib-lib-push-key mid key key-state))))))

(define m17nlib-press-key-handler
  (lambda (mc key key-state)
    (let* ((mid (m17nlib-context-mc-id mc)))
      (if (m17nlib-context-on mc)
	  (if (m17nlib-push-key mc key key-state)
	      #f ;; Discard key event
	      (let ((commit-str (m17nlib-lib-get-commit-string mid)))
		(if (string=? commit-str "")
		    (im-commit-raw mc)
		    (begin
		      (im-commit mc commit-str)
		      (m17nlib-lib-commit mid)))))
	  (m17nlib-proc-direct-state mc key key-state))
      (m17nlib-update-preedit mc)
      (m17nlib-update-candidate mc))))

(define m17nlib-release-key-handler
  (lambda (mc key key-state)
    #f))

(define m17nlib-reset-handler
  (lambda (mc)
    #f))

(define m17nlib-get-candidate-handler
  (lambda (mc idx accel-enum-hint)
    (let* ((mid (m17nlib-context-mc-id mc))
	   (cand (m17nlib-lib-get-nth-candidate
		  mid idx)))
      (list cand (digit->string (+ idx 1)) ""))))

(define m17nlib-set-candidate-index-handler
  (lambda (mc idx)
    #f))

;; Developer specified IM rejection should be withdrawn after we got
;; user-manageable enabled IM list feature. These redundant IMs should
;; be "disabled by default, but can be enabled" under the feature.
;;   -- YamaKen 2005-01-11
(define duplicated-im-list
  '("m17n-ja-anthy"
    "m17n-ja-tcode"
    "m17n-zh-pinyin"
    "m17n-zh-py"))

(define duplicated-im?
  (lambda (name)
    (member name duplicated-im-list)))

(define m17nlib-register
  (lambda (i nr-im)
    (if (> nr-im i)
	(begin
	  (if (not (duplicated-im? (m17nlib-lib-nth-input-method-name i)))
	      (register-im
	       (string->symbol (m17nlib-lib-nth-input-method-name i))
	       (m17nlib-lib-nth-input-method-lang i)
	       "UTF-8"
	       (m17nlib-lib-nth-input-method-name i)
	       (N_ "An input method provided by the m17n library")
	       (m17nlib-lib-nth-input-method-name i)
	       m17nlib-init-handler
	       m17nlib-release-handler
	       context-mode-handler
	       m17nlib-press-key-handler
	       m17nlib-release-key-handler
	       m17nlib-reset-handler
	       m17nlib-get-candidate-handler
	       m17nlib-set-candidate-index-handler
	       context-prop-activate-handler
	       ))
	      (m17nlib-register (+ i 1) nr-im))
	())))

(m17nlib-lib-init)
(m17nlib-register 0 (m17nlib-lib-nr-input-methods))
(m17nlib-configure-widgets)
