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

(require "i18n.scm")

(define custom-im-list-as-choice-rec
  (lambda (lst)
    (map (lambda (im)
	   (let ((sym (im-name im))
		 (label-name (im-label-name im))
		 (desc (im-short-desc im)))
	     (custom-choice-rec-new sym label-name desc)))
	 lst)))

(define-custom-group 'global
		     (_ "Global settings")
		     (_ "long description will be here."))

;; subgroup
(define-custom-group 'advanced
		     (_ "Advanced settings")
		     (_ "long description will be here."))

;; 
;; default-im-name
;;
(define-custom-group 'default-im-name
		     (_ "Default input method")
		     (_ "long description will be here."))

(define-custom 'custom-activate-default-im-name? #f
  '(global default-im-name)
  '(boolean)
  (_ "Specify default IM")
  (_ "long description will be here."))

(define-custom 'custom-preserved-default-im-name
  (and (not (null? im-list))
       (im-name (find-default-im #f)))
  '(global default-im-name)
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
	  (if custom-activate-default-im-name?
	      custom-preserved-default-im-name
	      #f))))

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
  (begin
    (if (symbol-bound? 'installed-im-module-list)
	(for-each require-module installed-im-module-list))
    (custom-im-list-as-choice-rec (reverse im-list))))

(define-custom 'enabled-im-list
               (map custom-choice-rec-sym custom-installed-im-list)
  '(global)
  (cons
   'ordered-list
   custom-installed-im-list)
  (_ "Enabled input methods")
  (_ "long description will be here."))

;; bootstrap
(if (and (symbol-bound? 'installed-im-module-list)
	 (null? enabled-im-list))
    (custom-set-value! 'enabled-im-list
		       (map custom-choice-rec-sym custom-installed-im-list)))

(define custom-hook-literalize-enabled-im-list
  (lambda ()
    (require "lazy-load.scm")
    (string-append
     "(define enabled-im-list "
     (custom-value-as-literal 'enabled-im-list)
     ")\n"
     "(require \"lazy-load.scm\")\n\n"
     (string-join "\n" (stub-im-generate-stub-im-list enabled-im-list)))))

(custom-add-hook 'enabled-im-list
		 'custom-literalize-hooks
		 custom-hook-literalize-enabled-im-list)

;;
;; im-switching
;;
(define-custom-group 'im-switching
		     (_ "Input method switching")
		     (_ "long description will be here."))

(define-custom 'enable-im-switch #f
  '(global im-switching advanced)
  '(boolean)
  (_ "Enable IM switching by hotkey")
  (_ "long description will be here."))

(define-custom 'switch-im-key '("<Control>Shift_key" "<Shift>Control_key")
  '(global im-switching advanced)
  '(key)
  (_ "IM switching key")
  (_ "long description will be here."))

;; activity dependency
(custom-add-hook 'switch-im-key?
		 'custom-activity-hooks
		 (lambda ()
		   enable-im-switch))

(define-custom 'uim-color 'uim-color-uim
  '(global)
  (list 'choice
	(list 'uim-color-uim (_ "uim") (_ "uim native"))
	(list 'uim-color-atok (_ "ATOK like") (_ "Similar to ATOK")))
  (_ "Preedit color")
  (_ "long description will be here."))

;; referred by some bridges
(define-custom 'candidate-window-position 'caret
  '(global)
  (list 'choice
	(list 'caret
	      (_ "Adjacent to caret")
	      (_ "Adjacent to caret"))
	(list 'left
	      (_ "Left end of preedit area")
	      (_ "Left end of preedit area"))
	(list 'right
	      (_ "Right end of preedit area")
	      (_ "Right end of preedit area")))
  (_ "Candidate window position")
  (_ "long description will be here."))
