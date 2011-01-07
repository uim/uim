;;;
;;; Copyright (c) 2003-2011 uim Project http://code.google.com/p/uim/
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

(require-custom "generic-key-custom.scm")

;;; user configs

(define scim-candidate-max 10)

;; key defs
(define-key scim-on-key? 'generic-on-key?)
(define-key scim-off-key? 'generic-off-key?)

;; widgets and actions

;; widgets
(define scim-widgets '(widget_scim_input_mode))

;; default activity for each widgets
(define default-widget_scim_input_mode 'action_scim_off)

;; actions of widget_scim_input_mode
(define scim-input-mode-actions
  '(action_scim_off
    action_scim_on))


;;; implementations

(register-action 'action_scim_off
		 (lambda (mc)
		   (list
		    'off
		    "-"
		    (N_ "off")
		    (N_ "Direct Input Mode")))
		 (lambda (mc)
		   (not (scim-context-on mc)))
		 (lambda (mc)
		   (scim-context-set-on! mc #f)))

(register-action 'action_scim_on
		 (lambda (mc)
		   (let* ((im (scim-context-im mc))
			  (name (symbol->string (im-name im))))
		     (list
		      'on
		      "O"
		      (N_ "on")
		      (string-append name (N_ " Mode")))))
		 (lambda (mc)
		   (scim-context-on mc))
		 (lambda (mc)
		   (scim-context-set-on! mc #t)))

;; Update widget definitions based on action configurations. The
;; procedure is needed for on-the-fly reconfiguration involving the
;; custom API
(define scim-configure-widgets
  (lambda ()
    (register-widget 'widget_scim_input_mode
		     (activity-indicator-new scim-input-mode-actions)
		     (actions-new scim-input-mode-actions))))

(define scim-context-rec-spec
  (append
   context-rec-spec
   ;; renamed from 'id' to avoid conflict with context-id
   '((mc-id             #f)
     (on                #f)
     (showing-candidate #f))))

(define-record 'scim-context scim-context-rec-spec)
(define scim-context-new-internal scim-context-new)

(define scim-context-new
  (lambda (id im name)
    (let ((mc (scim-context-new-internal id im))
	  (mc-id (scim-lib-alloc-context name)))
      (scim-context-set-widgets! mc scim-widgets)
      (scim-context-set-mc-id! mc mc-id)
      mc)))

(define scim-update-preedit
  (lambda (mc)
    (let* ((mid (scim-context-mc-id mc)))
      (if (scim-lib-preedit-changed? mid)
	  (if (scim-lib-compose-mode? mid)
	      (begin
		(im-clear-preedit mc)
		(im-pushback-preedit mc
				     preedit-underline
				     (scim-lib-get-left-of-candidate mid))
		(im-pushback-preedit mc
				     (+ preedit-reverse preedit-cursor)
				     (scim-lib-get-selected-candidate mid))
		(im-pushback-preedit mc
				     preedit-underline
				     (scim-lib-get-right-of-candidate mid))
		(im-update-preedit mc))
	      (begin
		(im-clear-preedit mc)
		(im-pushback-preedit mc
				     preedit-underline
				     (scim-lib-get-left-of-cursor mid))
		(im-pushback-preedit mc
				     preedit-cursor "")
		(im-pushback-preedit mc
				     preedit-underline
				     (scim-lib-get-right-of-cursor mid))
		(im-update-preedit mc))
	      )))))

(define scim-update-candidate
  (lambda (mc)
    (scim-lib-fill-new-candidates! (scim-context-mc-id mc))
    (let* ((mid (scim-context-mc-id mc))
	   (max (scim-lib-get-nr-candidates mid))
	   (showing-candidate? (scim-context-showing-candidate mc))
	   (candidates-changed? (scim-lib-candidates-changed? mid)))

      ;; FIXME: Rewrite this seriese of if with cond.
      ;; close candidate window
      (if (or
	   (and showing-candidate? candidates-changed?)
	   (and showing-candidate? (not (scim-lib-candidate-show? mid))))
	  (begin
	    (im-deactivate-candidate-selector mc mid)
	    (scim-context-set-showing-candidate! mc #f)))

      (if (and
	   (or
	    candidates-changed?
	    (and 
	     (not showing-candidate?)
	     (scim-lib-candidate-show? mid)))
	   (not (= max 0)))
	  (begin
	    (im-activate-candidate-selector
	     mc max scim-candidate-max)
	    (im-select-candidate
	     mc (scim-lib-get-candidate-index mid))
	    (scim-context-set-showing-candidate! mc #t)))

      (if (and showing-candidate?
	       (scim-lib-candidate-show? mid))
	  (im-select-candidate mc (scim-lib-get-candidate-index mid))))))


(define scim-append-modifiers
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

(define scim-proc-direct-state
  (lambda (mc key key-state)
   (if (scim-on-key? key key-state)
       (scim-context-set-on! mc #t)
       (scim-commit-raw mc))))

(define scim-commit-raw
  (lambda (mc)
    (im-commit-raw mc)))


; Unfortunatelly, we don't have simple way to translate...
(define scim-translate-ukey-to-mkey
  (lambda (key key-state)
    (scim-append-modifiers
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
		  (Kanji           . "")
		  (hiragana-katakana . "")
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
  
(define scim-init-handler
  (lambda (id im arg)
    (scim-context-new id im arg)))

(define scim-release-handler
  (lambda (mc)
    #f))

(define scim-push-key
  (lambda (mc key key-state)
    (let* ((mid (scim-context-mc-id mc)))
      (cond
       ((scim-off-key? key key-state)
	(scim-context-set-on! mc #f)
	#t) ;; #t means key event was consumed.
       ((symbol? key)
	(let ((mkey (scim-translate-ukey-to-mkey key key-state)))
	  (scim-lib-push-symbol-key mid mkey)))
       (else
	(scim-lib-push-key mid key key-state))))))

(define scim-press-key-handler
  (lambda (mc key key-state)
    (let* ((mid (scim-context-mc-id mc)))
      (if (scim-context-on mc)
	  (if (scim-push-key mc key key-state)
	      #f ;; Discard key event
	      (let* ((result (scim-lib-get-result mid))
		     (consumed? (car result))
		     (commit-str (cdr result)))
		(if (string=? commit-str "")
		    (im-commit-raw mc)
		    (begin
		      (im-commit mc commit-str)
		      (scim-lib-commit mid)
		      (if (not consumed?)
			  (im-commit-raw mc))))))
	  (scim-proc-direct-state mc key key-state))
      (scim-update-preedit mc)
      (scim-update-candidate mc))))

(define scim-release-key-handler
  (lambda (mc key key-state)
    #f))

(define scim-reset-handler
  (lambda (mc)
    #f))

(define scim-get-candidate-handler
  (lambda (mc idx accel-enum-hint)
    (let* ((mid (scim-context-mc-id mc))
	   (cand (scim-lib-get-nth-candidate
		  mid idx)))
      (list cand (digit->string (+ idx 1)) ""))))

(define scim-set-candidate-index-handler
  (lambda (mc idx)
    #f))

;; Developer specified IM rejection should be completely withdrawn
;; after we got flexible install-time IM preference list. These
;; redundant IMs should be "disabled by default, but can be enabled"
;; under the feature. But we should invest our time for more valuable
;; issues.  -- YamaKen 2005-01-25
(define duplicated-im-list
  '("scim-ja-anthy"
    "scim-ja-tcode"
    "scim-zh-pinyin"
    "scim-zh-py"))

;; At now, simply enable all IMs. Although they are redundant and
;; unconfortable, they can be disabled by uim-pref.
;;   -- YamaKen 2005-01-25
(define duplicated-im?
  (if #t
      (lambda (name)
	#f)
      (lambda (name)
	(member name duplicated-im-list))))

(define scim-register
  (lambda (i nr-im)
    (if (> nr-im i)
	(begin
	  (if (not (duplicated-im? (scim-lib-nth-input-method-name i)))
	      (register-im
	       (string->symbol (scim-lib-nth-input-method-name i))
	       (scim-lib-nth-input-method-lang i)
	       "UTF-8"
	       (scim-lib-nth-input-method-name i)
	       (N_ "An input method provided by the scim library")
	       (scim-lib-nth-input-method-name i)
	       scim-init-handler
	       scim-release-handler
	       context-mode-handler
	       scim-press-key-handler
	       scim-release-key-handler
	       scim-reset-handler
	       scim-get-candidate-handler
	       scim-set-candidate-index-handler
	       context-prop-activate-handler
	       #f
	       #f
	       #f
	       #f
	       #f
	       ))
	      (scim-register (+ i 1) nr-im))
	())))

(scim-lib-init)
(scim-register 0 (scim-lib-nr-input-methods))
(scim-configure-widgets)
