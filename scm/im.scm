;;; im.scm: Core IM management functions for uim
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

(require-extension (srfi 2 6 23 34))

; Comment should be written in English, UTF-8.
;
(require "util.scm")
(require "i18n.scm")
(require "load-action.scm")
(require "annotation.scm")

;; config
(define default-im-name #f)

;; preedit attributes: should be moved to another file
(define preedit-none      0)
(define preedit-underline 1)
(define preedit-reverse   2)
(define preedit-cursor    4)
(define preedit-separator 8)
(define preedit-attr?
  (lambda (attr)
    (memv attr (list preedit-none
		     preedit-underline
		     preedit-reverse
		     preedit-cursor
		     preedit-separator))))

(define text-area-id-alist
  '((primary   . 1)
    (selection . 2)
    (clipboard . 4)))

(define text-origin-alist
  '((cursor    . 1)
    (beginning . 2)
    (end       . 3)))

(define text-extent-alist
  '((full       . -2)
    (paragraph  . -3)
    (sentence   . -5)
    (word       . -9)
    (char-frags . -17)
    (disp-rect  . -33)
    (disp-line  . -65)
    (line       . -129)))

;;
;; im-management
;;
(define im-list ())

(define installed-im-list ())
(define enabled-im-list ())

(define-record 'im
  (list
   (list 'name                        #f)  ;; must be first member
   (list 'lang                        "")
   (list 'encoding                    "")
   (list 'name-label                  "")  ;; under discussion
   (list 'short-desc                  "")
   (list 'init-arg                    #f)
   (list 'init-handler                list)
   (list 'release-handler             list)
   (list 'mode-handler                list)
   (list 'key-press-handler           list)
   (list 'key-release-handler         list)
   (list 'reset-handler               list)
   (list 'get-candidate-handler       list)
   (list 'set-candidate-index-handler list)
   (list 'prop-activate-handler       list)
   (list 'input-string-handler        list)
   (list 'focus-in-handler            list)
   (list 'focus-out-handler           list)
   (list 'place-handler               list)
   (list 'displace-handler            list)
   (list 'module-name                 "")
   (list 'delay-activating-handler    #f)))

(define im-custom-set-handler
  (lambda (im)
    (if (symbol-bound? 'custom-prop-update-custom-handler)
	custom-prop-update-custom-handler
	list)))

(define normalize-im-list
  (lambda ()
    (let ((ordinary-im-list (alist-delete 'direct im-list eq?))
	  (direct-im (retrieve-im 'direct)))
      (if direct-im
	  (set! im-list (cons direct-im
			      ordinary-im-list))))))

;; TODO: rewrite test
;; accepts overwrite register
;; returns whether initial register or not
(define register-im
  (lambda (name lang encoding name-label short-desc init-arg init release
		mode key-press key-release reset
		get-candidate set-candidate-index prop input-string
		focus-in focus-out place displace)
    ;; Rejects symbols that cannot be valid external representation such
    ;; as "3foo", "#foo", ...
    (if (guard (err (else #t))
	  (not (eq? name
		    (read (open-input-string (symbol->string name))))))
	(begin
	  (if (symbol-bound? 'uim-notify-fatal)
	      (uim-notify-fatal (N_ "invalid IM name")))
	  (error "invalid IM name")))
    (and (or (null? enabled-im-list)  ;; bootstrap
	     (memq name enabled-im-list)
	     (eq? name 'direct))  ;; direct IM must always be enabled
	 (let ((im (im-new name lang encoding name-label short-desc
			   init-arg init release
			   mode key-press key-release reset
			   get-candidate set-candidate-index prop
			   input-string focus-in focus-out place displace
			   currently-loading-module-name))
	       (initial-registration? (not (assq name im-list))))
	   (set! im-list (alist-replace im im-list))
	   (normalize-im-list)
           initial-registration?))))

;; strictly find out im by name
(define retrieve-im
  (lambda (name)
    (and name
	 (let ((im (assq name im-list)))
	   im))))

(define default-im-for-debug
  (lambda ()
    (and (provided? "debug")
	 (let* ((str (getenv "UIM_IM_ENGINE"))
		(sym (and str
			  (string->symbol str))))
	   (retrieve-im sym)))))

(define find-im-for-locale
  (lambda (localestr)
    (let* ((lang (locale-zh-awared-lang (locale-new localestr)))
	   (ims-for-lang (filter (lambda (im)
				   (langgroup-covers? (im-lang im)
						      lang))
				 im-list))
	   (preference-ordered (and (not (null? ims-for-lang))
				    (reverse ims-for-lang))))
      (and (not (null? preference-ordered))
	   (car preference-ordered)))))

(define find-default-im
  (lambda (localestr)
    (or (default-im-for-debug)
	(retrieve-im default-im-name)
	(find-im-for-locale localestr))))

;; find most suitable im by im-name and lang
(define find-im
  (lambda (name localestr)
    (or (retrieve-im name)
	(find-default-im localestr))))

(define uim-filter-convertible-ims
  (lambda (uc)
    (filter (lambda (im)
              (im-convertible? uc (im-encoding im)))
            im-list)))

(define uim-n-convertible-ims
  (lambda (uc)
    (length (uim-filter-convertible-ims uc))))

(define uim-nth-convertible-im
  (lambda (uc n)
    (guard (err
            (else #f))
      (list-ref (uim-filter-convertible-ims uc) n))))

;; called from uim_get_default_im_name()
(define uim-get-default-im-name
  (lambda (localestr)
    (let ((name (im-name (find-default-im localestr))))
      (symbol->string name))))

;; called from uim_get_im_name_for_locale()
(define uim-get-im-name-for-locale
  (lambda (localestr)
    (let ((name (im-name (find-im-for-locale localestr))))
      (symbol->string name))))

;;
;; im-switching
;;

;; for C
(define uim-switch-im
  (lambda (uc name)
    (reset-handler uc)
    ;; Don't use remove-context. old and new context must (eq? old new)
    (invoke-handler im-release-handler uc)
    (let ((cur-context (im-retrieve-context uc))
          (new-context (create-context uc #f name)))
      (remove-context new-context)
      (set-cdr! cur-context (cdr new-context))
      (setup-context cur-context))))

;; for Scheme
(define im-switch-im
  (lambda (c name)
    (let ((uc (if (pair? c)
                  (context-uc c)
                  c)))
      (uim-switch-im uc name)
      (im-raise-configuration-change uc))))

(define next-im
  (lambda (name)
    (let* ((im-names enabled-im-list)
	   (im-rest (memq name im-names)))
      (or (and im-rest
	       (pair? (cdr im-rest))
	       (cadr im-rest))
	  (car im-names)))))

(define next-im-for-switch-im
  (lambda (name)
    (let ((im (next-im name)))
      (or
       (and
	switch-im-skip-direct-im?
	(eq? im 'direct)
	(next-im im))
       im))))

;; 'switch-im' is not a API but an IM-switching method. Don't confuse with
;; im-switch-im
(define switch-im
  (lambda (uc name)
    (im-switch-im uc (next-im-for-switch-im name))))

;; FIXME: Input states are kept only if the state is appeared in the
;; toolbar.
(define toggle-im
  (lambda (uc c)
    (let* ((cur-state (toggle-state-new (context-primary-im? c)
					(im-name (context-im c))
					(context-current-widget-states c)))
	   (saved-state (context-toggle-state c)))
      (im-switch-im uc (if saved-state
			   (toggle-state-im-name saved-state)
			   toggle-im-alt-im))
      ;; retrieve new context replaced by im-switch-im
      (let ((c (im-retrieve-context uc)))
	(if saved-state
	    (let ((orig-wstates (toggle-state-widget-states saved-state)))
	      (context-update-widget-states! c orig-wstates)))
	(context-set-toggle-state! c cur-state)))))

(define reset-toggled-im
  (lambda (uc c)
    (let ((saved-state (context-toggle-state c)))
      (im-switch-im uc (if saved-state
			   (toggle-state-im-name saved-state)
			   default-im-name)))))

(define reset-toggle-context!
  (lambda (uc ctx)
    (if (not (context-primary-im? ctx))
	(reset-toggled-im uc ctx))
    ;; ctx may be expired by the toggle-im
    (context-set-toggle-state! (im-retrieve-context uc) #f)))

;;
;; context-management
;;
(define context-list ())

(define context-rec-spec
  '((uc           #f)  ;; Scheme-wrapped uim_context. must be first member
    (im           #f)
    (widgets      ())  ;; may be renamed
    (toggle-state #f)
    (key-passthrough #f)))
(define-record 'context context-rec-spec)
;; backward compatibility: should be replaced with context-uc and
;; context-set-uc!
(define context-id context-uc)
(define context-set-id! context-set-uc!)

(define toggle-state-rec-spec
  '((primary?      #f)
    (im-name       #f)
    (widget-states ())))
(define-record 'toggle-state toggle-state-rec-spec)

(define context-primary-im?
  (lambda (c)
    (let ((toggle-state (context-toggle-state c)))
      (or (not toggle-state)
	  (not (toggle-state-primary? toggle-state))))))

(define context-primary-im-name
  (lambda (c)
    (if (context-primary-im? c)
	(im-name (context-im c))
	(toggle-state-im-name (context-toggle-state c)))))

;; FIXME: implement
(define context-focused?
  (lambda (c)
    #t))

(define remove-context
  (lambda (c)
    (set! context-list
	  (delete c context-list eq?))))

(define register-context
  (lambda (c)
    (set! context-list
	  (cons c context-list))))

(define create-context
  (lambda (uc lang name)
    (let* ((im (find-im name lang))
           (arg (and im (im-init-arg im))))
      (im-set-encoding uc (im-encoding im))
      (let* ((handler (im-init-handler im))
             (c (handler uc im arg)))
        (register-context c)
        ;; im-* procedures that require uc->sc must not called here since it
        ;; is not filled yet. Place such procedures to setup-context.
        c))))

;; post create-context setup
(define setup-context
  (lambda (c)
    (let ((widget-ids (context-widgets c)))
      (update-style uim-color-spec (symbol-value uim-color))
      (context-init-widgets! c widget-ids))))

(define release-context
  (lambda (uc)
    (invoke-handler im-release-handler uc)
    (remove-context (im-retrieve-context uc))
    #f))

(define uim-context-im
  (lambda (uc)
    (let ((c (im-retrieve-context uc)))
      (and c
           (context-im c)))))

(define uim-context-encoding
  (lambda (uc)
    (and-let* ((im (uim-context-im uc)))
      (im-encoding im))))

(define context-update-preedit
  (lambda (context segments)
    (im-clear-preedit context)
    (for-each (lambda (segment)
		(if segment
		    (let ((attr (car segment))
			  (str (cdr segment)))
		      (im-pushback-preedit context attr str))))
	      segments)
    (im-update-preedit context)))

;; Backward compatibility. The term 'commit' is incorrect. No commit
;; operation is performed by this. This actually instructs 'pass-through' the
;; input key. The key filtering interface will be replaced with 'filtered'
;; boolean value returned by key-*-handler of each IM in some future. Current
;; semantics is not an ordinary design for IM and felt unnatural.
;;   -- YamaKen 2007-01-10
(define im-commit-raw
  (lambda (c)
    (context-set-key-passthrough! (if (pair? c)
                                      c
                                      (im-retrieve-context c))
                                  #t)))

;; Deprecated
(define im-get-raw-key-str
  (lambda (key state)
    (and (integer? key)
	 (<= key 255)
	 (integer? state)
	 (cond
	  ((= state 0)
	   (charcode->string key))
	  ((= state (assq-cdr 'Shift_key key-state-alist))
	   (charcode->string (ichar-upcase key)))
	  (else
	   #f)))))

;;
;; dispatchers
;;
(define invoke-handler
  (lambda args
    (let* ((handler-reader (car args))
	   (uc (cadr args))
	   (c (im-retrieve-context uc))
	   (handler-args (cons c (cddr args)))
	   (im (and c (context-im c)))
	   (handler (and im (handler-reader im)))
	   (result (and handler
			(apply handler handler-args))))
      (context-update-widgets c)
      result)))

;; Returns #t if input is filtered.
;; Don't discard unnecessary key events. They are necessary for
;; proper GUI widget handling. More correction over entire uim
;; codes is needed.
(define key-press-handler
  (lambda (uc key state)
    (let* ((c (im-retrieve-context uc))
	   (im (and c (context-im c))))
      (context-set-key-passthrough! c #f)
      (cond
       ((and enable-im-toggle?
	     (toggle-im-key? key state))
	(toggle-im uc c))
       ((and enable-im-switch?
	     (switch-im-key? key state))
	(switch-im uc (im-name im)))
       ((modifier-key? key state)
	;; don't discard modifier press/release edge for apps
	(im-commit-raw c))
       (else
	(invoke-handler im-key-press-handler uc key state)))
      (not (context-key-passthrough c)))))

;; Returns #t if input is filtered.
(define key-release-handler
  (lambda (uc key state)
    (let ((c (im-retrieve-context uc)))
      (context-set-key-passthrough! c #f)
      (cond
       ((modifier-key? key state)
	;; don't discard modifier press/release edge for apps
	(im-commit-raw c))
       (else
	(invoke-handler im-key-release-handler uc key state)))
      (not (context-key-passthrough c)))))

(define reset-handler
  (lambda (uc)
    (invoke-handler im-reset-handler uc)))

(define focus-in-handler
  (lambda (uc)
    (invoke-handler im-focus-in-handler uc)))

(define focus-out-handler
  (lambda (uc)
    (invoke-handler im-focus-out-handler uc)))

(define place-handler
  (lambda (uc)
    (invoke-handler im-place-handler uc)))

(define displace-handler
  (lambda (uc)
    (invoke-handler im-displace-handler uc)))

(define mode-handler
  (lambda (uc mode)
    (invoke-handler im-mode-handler uc mode)))

(define prop-activate-handler
  (lambda (uc message)
    (invoke-handler im-prop-activate-handler uc message)))

(define input-string-handler
  (lambda (uc str)
    (invoke-handler im-input-string-handler uc str)))

(define custom-set-handler
  (lambda (uc custom-sym custom-val)
    (invoke-handler im-custom-set-handler uc custom-sym custom-val)))

(define get-candidate
  (lambda (uc idx accel-enum-hint)
    (let ((c (invoke-handler im-get-candidate-handler uc idx accel-enum-hint)))
      (if (and (not enable-annotation?)
               (not (string=? (last c) "")))
          (set-cdr! (cdr c) (list ""))
          (and (string=? (last c) "")
               (set-cdr! (cdr c) (list (annotation-get-text (car c) (uim-context-encoding uc))))))
      c)))

(define set-candidate-index
  (lambda (uc idx)
    (invoke-handler im-set-candidate-index-handler uc idx)))

(define delay-activating-handler
  (lambda (uc)
    (invoke-handler im-delay-activating-handler uc)))

(define im-acquire-text
  (lambda (c id origin former-len latter-len)
    (let ((text-id (cdr (assq id text-area-id-alist)))
          (text-origin (cdr (assq origin text-origin-alist)))
	  (text-extent-former (if (symbol? former-len)
				  (cdr (assq former-len text-extent-alist))
				  former-len))
	  (text-extent-latter (if (symbol? latter-len)
				  (cdr (assq latter-len text-extent-alist))
				  latter-len)))
      (im-acquire-text-internal
       c text-id text-origin text-extent-former text-extent-latter))))

(define im-delete-text
  (lambda (c id origin former-len latter-len)
    (let ((text-id (cdr (assq id text-area-id-alist)))
          (text-origin (cdr (assq origin text-origin-alist)))
	  (text-extent-former (if (symbol? former-len)
				  (cdr (assq former-len text-extent-alist))
				  former-len))
	  (text-extent-latter (if (symbol? latter-len)
				  (cdr (assq latter-len text-extent-alist))
				  latter-len)))
      (im-delete-text-internal
       c text-id text-origin text-extent-former text-extent-latter))))
