;;; custom.scm: Customization support
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

;; WARNING: This file is existing for only testing purpose. more
;; discussion is required.

;; There are complex definitions to experiment the customization
;; mechanism. Will be simplified once the requirement is cleared up.
;; -- YamaKen

(require "i18n.scm")
(require "util.scm")

;; private
(define custom-rec-alist ())
(define custom-group-rec-alist ())
(define custom-subgroup-alist ())

(define custom-activity-hook ())
(define custom-update-hook ())
(define custom-get-hook ())
(define custom-set-hook ())
(define custom-literalize-hook ())

(define custom-validator-alist
  '((boolean   . custom-boolean?)
    (integer   . custom-integer?)
    (string    . custom-string?)
    (pathname  . pathname?)
    (symbol    . custom-valid-symbol?)
    (key       . key-definition?)))

(define anything?
  (lambda (x)
    #t))

(define custom-boolean?
  (lambda (x)
    #t))

(define custom-integer?
  (lambda (x min max)
    (and (integer? x)
	 (<= min x)
	 (<= x max))))

(define custom-string?
  (lambda (x regex)
    (string? x)))

(define pathname?
  (lambda (str)
    (string? str)))

(define custom-valid-symbol?
  (lambda arg
    (let* ((sym (car arg))
	   (symbol-rec-alist (cdr arg)))
      (and (symbol? sym)
	   (assq sym symbol-rec-alist)
	   #t))))

(define-record 'custom-symbol-rec
  '((sym   #f)
    (label "")
    (desc  "")))

(define custom-symbol-label
  (lambda (custom-sym val-sym)
    (let* ((sym-rec-alist (custom-type-attrs custom-sym))
	   (srec (assq val-sym sym-rec-alist))
	   (label (custom-symbol-rec-label srec)))
      label)))

(define custom-symbol-desc
  (lambda (custom-sym val-sym)
    (let* ((sym-rec-alist (custom-type-attrs custom-sym))
	   (srec (assq val-sym sym-rec-alist))
	   (desc (custom-symbol-rec-desc srec)))
      desc)))

;; only accepts single strict-key-str (not or'ed, not a variable reference)
(define key-definition?
  (lambda (def)
    (valid-strict-key-str? def)))

(define-record 'custom-group-rec
  '((sym   #f)
    (label "")
    (desc  "")))

(define custom-define-group
  (lambda (gsym label desc)
    (let ((grec (custom-group-rec-new gsym label desc)))
      (if (not (custom-group-rec gsym))
	  (set! custom-group-rec-alist (cons grec
					     custom-group-rec-alist))))))

(define custom-group-rec
  (lambda (gsym)
    (assq gsym custom-group-rec-alist)))

;; API
(define custom-group-label
  (lambda (gsym)
    (custom-group-rec-label (custom-group-rec gsym))))

;; API
(define custom-group-desc
  (lambda (gsym)
    (custom-group-rec-desc (custom-group-rec gsym))))

;; API
(define custom-group-subgroups
  (lambda (gsym)
    (let ((groups (filter-map (lambda (pair)
				(let ((primary-grp (car pair))
				      (subgrp (cdr pair)))
				  (and (eq? gsym primary-grp)
				       subgrp)))
			      custom-subgroup-alist)))
      (reverse groups))))

;; API
(define custom-list-groups
  (lambda ()
    (let ((groups (map custom-group-rec-sym custom-group-rec-alist)))
      (reverse groups))))

;; API
(define custom-list-primary-groups
  (lambda ()
    (let ((groups (filter-map
		   (lambda (grec)
		     (let ((grp (custom-group-rec-sym grec)))
		       (and (assq grp custom-subgroup-alist)
			    grp)))
		   custom-group-rec-alist)))
      (reverse groups))))

;; API
;; #f means 'any group'
;; TODO: support "AND" expression
(define custom-collect-by-group
  (lambda (group)
    (filter-map (lambda (crec)
		  (and (or (not group)
			   (memq group (custom-rec-groups crec)))
		       (custom-rec-sym crec)))
		custom-rec-alist)))

(define custom-add-hook
  (lambda (custom-sym hook-sym proc)
    (set-symbol-value! hook-sym (cons (cons custom-sym proc)
				      (symbol-value hook-sym)))))

;; #f for custom-sym means 'any entries'
(define custom-remove-hook
  (lambda (custom-sym hook-sym)
    (let ((removed (if custom-sym
		       (alist-delete custom-sym (symbol-value hook-sym) eq?)
		       ()))
	  (removed? (if custom-sym
			(assq custom-sym (symbol-value hook-sym))
			(not (null? (symbol-value hook-sym))))))
      (set-symbol-value! hook-sym removed)
      removed?)))

(define custom-hook-procs
  (lambda (sym hook)
    (let* ((filter (lambda (pair)
		     (let ((custom (car pair))
			   (proc (cdr pair)))
		       (and (eq? sym custom)
			    proc))))
	   (procs (filter-map filter
			      hook)))
      procs)))

(define custom-call-hook-procs
  (lambda (sym hook)
    (let ((procs (custom-hook-procs sym hook)))
      (map (lambda (proc)
	     (proc))
	   procs))))

(define-record 'custom-rec
  '((sym     #f)
    (default #f)
    (groups  ())
    (type    ())
    (label   "")
    (desc    "")))

(define custom-rec
  (lambda (sym)
    (assq sym custom-rec-alist)))

(define define-custom
  (lambda (sym default groups type label desc)
    (let ((crec (custom-rec-new sym default groups type label desc))
	  (primary-grp (car groups))
	  (subgrps (cons 'main (cdr groups))))
      (if (not (custom-rec sym))
	  (set! custom-rec-alist (cons crec
				       custom-rec-alist)))
      (if (not (symbol-bound? sym))
	  (let ((default (if (symbol? default)
			     (list 'quote default)
			     default)))
	    (eval (list 'define sym default)
		  toplevel-env)))
      (for-each (lambda (subgrp)
		  (let ((registered (custom-group-subgroups primary-grp)))
		    (if (not (memq subgrp registered))
			(set! custom-subgroup-alist
			      (cons (cons primary-grp subgrp)
				    custom-subgroup-alist)))))
		subgrps))))

;; API
(define custom-valid?
  (lambda (sym val)
    (let* ((type-name (custom-type sym))
	   (type-attrs (custom-type-attrs sym))
	   (valid? (symbol-value (cdr (assq type-name
					    custom-validator-alist)))))
      (apply valid? (cons val type-attrs)))))

;; API
(define custom-value
  (lambda (sym)
    (custom-call-hook-procs sym custom-get-hook)
    (let ((val (symbol-value sym)))
      (if (custom-valid? sym val)
	  val
	  (custom-default-value sym)))))

;; API
(define custom-set!
  (lambda (sym val)
    (and (custom-valid? sym val)
	 (let* ((custom-syms (custom-collect-by-group #f))
		(pre-activities (map custom-active? custom-syms)))
	   (set-symbol-value! sym val)
	   (custom-call-hook-procs sym custom-set-hook)
	   (let ((post-activities (map custom-active? custom-syms)))
	     (for-each (lambda (key pre post)
			 (if (or (eq? key sym)
				 (not (eq? pre post)))
			     (custom-call-hook-procs sym custom-update-hook)))
		       custom-syms
		       pre-activities
		       post-activities)
	     #t)))))

(define custom-active?
  (lambda (sym)
    (let* ((procs (custom-hook-procs sym custom-activity-hook))
	   (activities (map (lambda (proc)
			      (proc))
			    procs))
	   (active? (apply proc-and activities)))
      active?)))

;; API
(define custom-default?
  (lambda (sym)
    (equal? (symbol-value sym)
	    (custom-default-value sym))))

;; API
(define custom-default-value
  (lambda (sym)
    (custom-rec-default (custom-rec sym))))

;; API
(define custom-groups
  (lambda (sym)
    (custom-rec-groups (custom-rec sym))))

;; API
(define custom-type
  (lambda (sym)
    (car (custom-rec-type (custom-rec sym)))))

(define custom-type-attrs
  (lambda (sym)
    (let* ((crec (custom-rec sym))
	   (typedef (custom-rec-type crec)))
      (cdr typedef))))

;; API
(define custom-range
  (lambda (sym)
    (let* ((type (custom-type sym))
	   (attrs (custom-type-attrs sym)))
      (cond
       ((eq? type 'symbol)
	(map custom-symbol-rec-sym attrs))
       (else
	attrs)))))

;; API
(define custom-label
  (lambda (sym)
    (custom-rec-label (custom-rec sym))))

;; API
(define custom-desc
  (lambda (sym)
    (custom-rec-desc (custom-rec sym))))

;; API
(define custom-canonical-value-as-string
  (lambda (sym)
    (let ((val (custom-value sym))
	  (type (custom-type sym))
	  (as-string (lambda (s)
		       (string-append
			"\""
			s
			"\""))))
      (cond
       ((or (eq? val #f)
	    (eq? type 'boolean))
	(if (eq? val #f)
	    "#f"
	    "#t"))
       ((eq? type 'integer)
	(digit->string val))
       ((eq? type 'string)
	(as-string val))
       ((eq? type 'pathname)
	(as-string val))
       ((eq? type 'symbol)
	(string-append "'" (symbol->string val)))
       ((eq? type 'key)
	"")))))  ;; TODO

(define custom-canonical-definition-as-string
  (lambda (sym)
    (let ((var (symbol->string sym))
	  (val (custom-canonical-value-as-string sym)))
      (string-append
       "(define " var " " val ")"))))

(define custom-as-string
  (lambda (sym)
    (let ((hooked (custom-call-hook-procs sym custom-literalize-hook)))
      (if (not (null? hooked))
	  (apply string-append hooked)
	  (custom-canonical-definition-as-string sym)))))

;; API
(define custom-broadcast-custom
  (lambda (sym)
    ))

;; API
;; #f means 'any group'
;; TODO: support "AND" expression
(define custom-broadcast-customs
  (lambda (group)
    (let ((custom-syms (custom-collect-by-group group)))
      (for-each custom-broadcast-custom custom-syms))))

(define custom-prop-update-custom-handler
  (lambda (context custom-sym val)
    (custom-set! custom-sym val)))

(define custom-register-update-cb
  (lambda (custom-sym ptr gate-func func)
    (and (custom-rec sym)
	 (let ((cb (lambda () (gate-func func ptr custom-sym))))
	   (custom-add-hook custom-sym 'custom-update-hook cb)))))


;;
;; Particular definitions: may be split into independent file(s)
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Global

(define direct-im-canonical-name (N_ "Direct"))
(define direct-im-desc (N_ "Direct input method mainly used for latin languages"))

(define anthy-im-canonical-name (N_ "Anthy"))
(define anthy-im-desc (N_ "long description will be here."))

(define canna-im-canonical-name (N_ "Canna"))
(define canna-im-desc (N_ "long description will be here."))

(define skk-im-canonical-name (N_ "SKK"))
(define skk-im-desc (N_ "long description will be here."))

(define prime-im-canonical-name (N_ "PRIME"))
(define prime-im-desc (N_ "long description will be here."))

(define pyunihan-im-canonical-name (N_ "pyunihan"))
(define pyunihan-im-desc (N_ "long description will be here."))

(define pinyin-big5-im-canonical-name (N_ "pinyin-big5"))
(define pinyin-big5-im-desc (N_ "long description will be here."))

(define py-im-canonical-name (N_ "Pinyin"))
(define py-im-desc (N_ "long description will be here."))

(define ipa-im-canonical-name (N_ "International Phonetic Alphabet"))
(define ipa-im-desc (N_ "long description will be here."))

(define romaja-im-canonical-name (N_ "Romaja"))
(define romaja-im-desc (N_ "long description will be here."))

(define hangul3-im-canonical-name (N_ "Hangul3"))
(define hangul3-im-desc (N_ "long description will be here."))

(define hangul2-im-canonical-name (N_ "Hangul2"))
(define hangul2-im-desc (N_ "long description will be here."))

(define viqr-im-canonical-name (N_ "Viqr"))
(define viqr-im-desc (N_ "long description will be here."))

(define tutcode-im-canonical-name (N_ "TUT-Code"))
(define tutcode-im-desc (N_ "long description will be here."))

(define tcode-im-canonical-name (N_ "T-Code"))
(define tcode-im-desc (N_ "long description will be here."))

(define spellcheck-im-canonical-name (N_ "Spellcheck"))
(define spellcheck-im-desc (N_ "long description will be here."))

(custom-define-group 'global
		     (N_ "Global settings")
		     (N_ "long description will be here."))

(custom-define-group 'advanced
		     (N_ "Advanced settings")
		     (N_ "long description will be here."))

(define-custom 'uim-color 'uim-color-uim
  '(global)
  '(symbol
    (uim-color-uim "uim" "uim native")
    (uim-color-atok "ATOK like" "Similar to ATOK"))
  (N_ "Preedit color")
  (N_ "long description will be here."))

;; TODO: configure loader.scm

;; 
;; default-im-name
;;
(custom-define-group 'default-im-name
		     (N_ "Default input method")
		     (N_ "long description will be here."))

(define-custom 'custom-activate-default-im-name? #f
  '(global default-im-name)
  '(boolean)
  (N_ "Specify default IM")
  (N_ "long description will be here."))

;; requires predefined *-im-canonical-name and *-im-desc
(define-custom 'custom-preserved-default-im-name (im-name (find-default-im #f))
  '(global default-im-name)
  (cons
   'symbol
   (reverse (map (lambda (im)
		   (let* ((sym (im-name im))
			  (cname-proc (symbolconc sym '-im-canonical-name))
			  (name (or (and (symbol-bound? cname-proc)
					 (symbol-value cname-proc))
				    (symbol->string sym)))
			  (desc-proc (symbolconc sym '-im-desc))
			  (desc (or (and (symbol-bound? desc-proc)
					 (symbol-value desc-proc))
				    (symbol->string sym))))
		     (list sym name desc)))
		 im-list)))
  (N_ "Default input method")
  (N_ "long description will be here."))

;; activity dependency
(custom-add-hook 'custom-preserved-default-im-name
		 'custom-activity-hook
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
		 'custom-get-hook
		 custom-hook-get-default-im-name)
(custom-add-hook 'custom-preserved-default-im-name
		 'custom-get-hook
		 custom-hook-get-default-im-name)

(define custom-hook-set-default-im-name
  (lambda ()
    (set! default-im-name
	  (if custom-activate-default-im-name?
	      custom-preserved-default-im-name
	      #f))))

;; encode #f into default-im-name
(custom-add-hook 'custom-activate-default-im-name?
		 'custom-set-hook
		 custom-hook-set-default-im-name)
(custom-add-hook 'custom-preserved-default-im-name
		 'custom-set-hook
		 custom-hook-set-default-im-name)

(define custom-hook-literalize-preserved-default-im-name
  (lambda ()
    (string-append
     (custom-canonical-definition-as-string 'custom-preserved-default-im-name)
     "\n"
     "(define default-im-name "
     (if default-im-name
	 (string-append "'" (symbol->string default-im-name))
	 "#f")
     ")")))

(custom-add-hook 'custom-preserved-default-im-name
		 'custom-literalize-hook
		 custom-hook-literalize-preserved-default-im-name)

;;
;; im-switching
;;
(custom-define-group 'im-switching
		     (N_ "Input method switching")
		     (N_ "long description will be here."))

(define-custom 'enable-im-switch #f
  '(global im-switching advanced)
  '(boolean)
  (N_ "Enable IM switching by hotkey")
  (N_ "long description will be here."))

;;(define-custom 'switch-im-key? '("<Control>Shift_key" "<Shift>Control_key")
;;  '(global im-switching advanced)
;;  '(key)
;;  "IM switching key"
;;  "long description will be here.")

;; activity dependency
(custom-add-hook 'switch-im-key?
		 'custom-activity-hook
		 (lambda ()
		   enable-im-switch))

(define-custom 'candidate-window-position "caret"
  '(global)
  '(string "^(caret|left|right)$")
  (N_ "Candidate window position")
  (N_ "long description will be here."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; anthy

(custom-define-group 'anthy
		     anthy-im-canonical-name
		     anthy-im-desc)

(define-custom 'anthy-use-candidate-window? #t
  '(anthy)
  '(boolean)
  (N_ "Use candidate window")
  (N_ "long description will be here."))

(define-custom 'anthy-candidate-op-count 1
  '(anthy)
  '(integer 0 99)
  (N_ "Conversion key press count to show candidate window")
  (N_ "long description will be here."))

(define-custom 'anthy-nr-candidate-max 10
  '(anthy)
  '(integer 1 20)
  (N_ "Number of candidates in candidate window at a time")
  (N_ "long description will be here."))

(define-custom 'anthy-show-segment-separator? #f
  '(anthy advanced)
  '(boolean)
  (N_ "Show segment separator")
  (N_ "long description will be here."))

(define-custom 'anthy-segment-separator "|"
  '(anthy advanced)
  '(string ".*")
  (N_ "Segment separator")
  (N_ "long description will be here."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; canna

(custom-define-group 'canna
		     canna-im-canonical-name
		     canna-im-desc)

(define-custom 'canna-use-candidate-window? #t
  '(canna)
  '(boolean)
  (N_ "Use candidate window")
  (N_ "long description will be here."))

(define-custom 'canna-candidate-op-count 1
  '(canna)
  '(integer 0 99)
  (N_ "Conversion key press count to show candidate window")
  (N_ "long description will be here."))

(define-custom 'canna-nr-candidate-max 10
  '(canna)
  '(integer 1 20)
  (N_ "Number of candidates in candidate window at a time")
  (N_ "long description will be here."))

(define-custom 'canna-show-segment-separator? #f
  '(canna advanced)
  '(boolean)
  (N_ "Show segment separator")
  (N_ "long description will be here."))

(define-custom 'canna-segment-separator "|"
  '(canna advanced)
  '(string ".*")
  (N_ "Segment separator")
  (N_ "long description will be here."))

;;
;; canna-server-name
;;
(custom-define-group 'cannaserver
		     (N_ "Canna server")
		     (N_ "long description will be here."))

(define-custom 'custom-activate-canna-server-name? #f
  '(canna cannaserver)
  '(boolean)
  (N_ "Use Canna server")
  (N_ "long description will be here."))

(define-custom 'custom-preserved-canna-server-name ""
  '(canna cannaserver)
  '(string ".*")
  (N_ "Canna server name")
  (N_ "long description will be here."))

(define-custom 'canna-server-name ""
  '(canna cannaserver)
  '(string ".*")
  (N_ "Canna server name")
  (N_ "long description will be here."))

;; activity dependency
(custom-add-hook 'custom-preserved-canna-server-name
		 'custom-activity-hook
		 (lambda ()
		   custom-activate-canna-server-name?))

(define custom-hook-get-canna-server-name
  (lambda ()
    (set! custom-activate-canna-server-name? canna-server-name)
    (set! custom-preserved-canna-server-name (or canna-server-name
						 custom-preserved-canna-server-name
						 ""))))

;; decode #f from canna-server-name
(custom-add-hook 'custom-activate-canna-server-name?
		 'custom-get-hook
		 custom-hook-get-canna-server-name)
(custom-add-hook 'canna-server-name
		 'custom-get-hook
		 custom-hook-get-canna-server-name)

(define custom-hook-set-canna-server-name
  (lambda ()
    (set! canna-server-name
	  (and custom-activate-canna-server-name?
	       custom-preserved-canna-server-name))))

;; encode #f into canna-server-name
(custom-add-hook 'custom-activate-canna-server-name?
		 'custom-set-hook
		 custom-hook-set-canna-server-name)
(custom-add-hook 'custom-preserved-canna-server-name
		 'custom-set-hook
		 custom-hook-set-canna-server-name)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; skk

(custom-define-group 'skk
		     skk-im-canonical-name
		     skk-im-desc)

(define-custom 'skk-dic-file-name "/usr/share/skk/SKK-JISYO.L"
  '(skk)
  '(pathname)
  (N_ "Dictionary file")
  (N_ "long description will be here."))

(define-custom 'skk-personal-dic-filename
  (string-append (getenv "HOME") "/.skk-jisyo")
  '(skk)
  '(pathname)
  (N_ "Personal dictionary file")
  (N_ "long description will be here."))

(define-custom 'skk-uim-personal-dic-filename
  (string-append (getenv "HOME") "/.skk-uim-jisyo")
  '(skk)
  '(pathname)
  (N_ "Personal dictionary file (dedicated to uim)")
  (N_ "long description will be here."))

(define-custom 'skk-use-candidate-window? #t
  '(skk)
  '(boolean)
  (N_ "Use candidate window")
  (N_ "long description will be here."))

(define-custom 'skk-candidate-op-count 0
  '(skk)
  '(integer 0 99)
  (N_ "Conversion key press count to show candidate window")
  (N_ "long description will be here."))

(define-custom 'skk-nr-candidate-max 10
  '(skk)
  '(integer 1 20)
  (N_ "Number of candidates in candidate window at a time")
  (N_ "long description will be here."))

(define-custom 'skk-use-recursive-learning? #t
  '(skk advanced)
  '(boolean)
  (N_ "Use recursive learning")
  (N_ "long description will be here."))

(define-custom 'skk-egg-like-newline? #f
  '(skk advanced)
  '(boolean)
  (N_ "Use Enter key as just committing (egg-like operation)")
  (N_ "long description will be here."))

(define-custom 'skk-commit-newline-explicitly? #f
  '(skk advanced)
  '(boolean)
  (N_ "Commit newline as ASCII string instead of native key-event")
  (N_ "long description will be here."))

(define-custom 'skk-style 'skk-style-ddskk-like
  '(skk advanced)
  '(symbol
    (skk-style-ddskk-like "ddskk" "Similar to ddskk")
    (skk-style-uim "uim" "uim native"))
  (N_ "Visual style")
  (N_ "long description will be here."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; prime

(custom-define-group 'prime
		     prime-im-canonical-name
		     prime-im-desc)

;(define-custom 'prime-use-candidate-window? #t
;  '(prime)
;  '(boolean)
;  "Use candidate window"
;  "long description will be here.")

;(define-custom 'prime-candidate-op-count 1
;  '(prime)
;  '(integer 0 99)
;  "Conversion key press count to show candidate window"
;  "long description will be here.")

(define-custom 'prime-nr-candidate-max 10
  '(prime)
  '(integer 1 20)
  (N_ "Number of candidates in candidate window at a time")
  (N_ "long description will be here."))

(define-custom 'prime-always-show-window? #t
  '(prime)
  '(boolean)
  (N_ "Always showing candidate window")
  (N_ "long description will be here."))

(define-custom 'prime-preedit-immediate-commit? #f
  '(prime)
  '(boolean)
  (N_ "prime-preedit-immediate-commit?")
  (N_ "long description will be here."))

(define-custom 'prime-mask-pending-preedit? #f
  '(prime)
  '(boolean)
  (N_ "prime-mask-pending-preedit?")
  (N_ "long description will be here."))

;(define-custom 'prime-use-numeral-key-to-select-cand? #t
;  '(prime)
;  '(boolean)
;  "Use numeral key to select candidate directly"
;  "long description will be here.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Other IMs

(custom-define-group 'other-ims
		     (N_ "Other input methods")
		     (N_ "long description will be here."))

(define-custom 'generic-use-candidate-window? #t
  '(other-ims)
  '(boolean)
  (N_ "Use candidate window")
  (N_ "long description will be here."))

(define-custom 'generic-candidate-op-count 1
  '(other-ims)
  '(integer 0 99)
  (N_ "Conversion key press count to show candidate window")
  (N_ "long description will be here."))

(define-custom 'generic-nr-candidate-max 10
  '(other-ims)
  '(integer 1 20)
  (N_ "Number of candidates in candidate window at a time")
  (N_ "long description will be here."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Spellcheck

(custom-define-group 'spellcheck
		     spellcheck-im-canonical-name
		     spellcheck-im-desc)

(define-custom 'spellcheck-use-candidate-window? #t
  '(spellcheck)
  '(boolean)
  (N_ "Use candidate window")
  (N_ "long description will be here."))

(define-custom 'spellcheck-candidate-op-count 1
  '(spellcheck)
  '(integer 0 99)
  (N_ "Conversion key press count to show candidate window")
  (N_ "long description will be here."))

(define-custom 'spellcheck-preedit-immediate-commit? #f
  '(spellcheck)
  '(boolean)
  (N_ "spellcheck-preedit-immediate-commit?")
  (N_ "long description will be here."))

(define-custom 'spellcheck-always-show-window? #t
  '(spellcheck)
  '(boolean)
  (N_ "Always showing candidate window")
  (N_ "long description will be here."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
