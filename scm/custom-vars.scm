;;; custom-vars.scm: Customization variables
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

;;
;; Particular definitions: may be distributed into appropriate files
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Global

(define direct-im-canonical-name (_ "Direct"))
(define direct-im-desc (_ "Direct input method mainly used for latin languages"))

(define anthy-im-canonical-name (_ "Anthy"))
(define anthy-im-desc (_ "long description will be here."))

(define canna-im-canonical-name (_ "Canna"))
(define canna-im-desc (_ "long description will be here."))

(define skk-im-canonical-name (_ "SKK"))
(define skk-im-desc (_ "long description will be here."))

(define prime-im-canonical-name (_ "PRIME"))
(define prime-im-desc (_ "long description will be here."))

(define pyunihan-im-canonical-name (_ "pyunihan"))
(define pyunihan-im-desc (_ "long description will be here."))

(define pinyin-big5-im-canonical-name (_ "pinyin-big5"))
(define pinyin-big5-im-desc (_ "long description will be here."))

(define py-im-canonical-name (_ "Pinyin"))
(define py-im-desc (_ "long description will be here."))

(define ipa-im-canonical-name (_ "International Phonetic Alphabet"))
(define ipa-im-desc (_ "long description will be here."))

(define romaja-im-canonical-name (_ "Romaja"))
(define romaja-im-desc (_ "long description will be here."))

(define hangul3-im-canonical-name (_ "Hangul3"))
(define hangul3-im-desc (_ "long description will be here."))

(define hangul2-im-canonical-name (_ "Hangul2"))
(define hangul2-im-desc (_ "long description will be here."))

(define viqr-im-canonical-name (_ "Viqr"))
(define viqr-im-desc (_ "long description will be here."))

(define tutcode-im-canonical-name (_ "TUT-Code"))
(define tutcode-im-desc (_ "long description will be here."))

(define tcode-im-canonical-name (_ "T-Code"))
(define tcode-im-desc (_ "long description will be here."))

(define spellcheck-im-canonical-name (_ "Spellcheck"))
(define spellcheck-im-desc (_ "long description will be here."))

(define-custom-group 'global
		     (_ "Global settings")
		     (_ "long description will be here."))

(define-custom-group 'advanced
		     (_ "Advanced settings")
		     (_ "long description will be here."))

(define-custom 'uim-color 'uim-color-uim
  '(global)
  '(choice
    (uim-color-uim "uim" "uim native")
    (uim-color-atok "ATOK like" "Similar to ATOK"))
  (_ "Preedit color")
  (_ "long description will be here."))

;; TODO: configure loader.scm

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

;; requires predefined *-im-canonical-name and *-im-desc
(define-custom 'custom-preserved-default-im-name (im-name (find-default-im #f))
  '(global default-im-name)
  (cons
   'choice
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

;;(define-custom 'switch-im-key? '("<Control>Shift_key" "<Shift>Control_key")
;;  '(global im-switching advanced)
;;  '(key)
;;  "IM switching key"
;;  "long description will be here.")

;; activity dependency
(custom-add-hook 'switch-im-key?
		 'custom-activity-hooks
		 (lambda ()
		   enable-im-switch))

(define-custom 'candidate-window-position "caret"
  '(global)
  '(string "^(caret|left|right)$")
  (_ "Candidate window position")
  (_ "long description will be here."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; anthy

(define-custom-group 'anthy
		     anthy-im-canonical-name
		     anthy-im-desc)

(define-custom 'anthy-use-candidate-window? #t
  '(anthy)
  '(boolean)
  (_ "Use candidate window")
  (_ "long description will be here."))

(define-custom 'anthy-candidate-op-count 1
  '(anthy)
  '(integer 0 99)
  (_ "Conversion key press count to show candidate window")
  (_ "long description will be here."))

(define-custom 'anthy-nr-candidate-max 10
  '(anthy)
  '(integer 1 20)
  (_ "Number of candidates in candidate window at a time")
  (_ "long description will be here."))

(define-custom 'anthy-select-candidate-by-numeral-key? #f
  '(anthy)
  '(boolean)
  (_ "Select candidate by numeral keys")
  (_ "long description will be here."))

(define-custom 'anthy-show-segment-separator? #f
  '(anthy advanced)
  '(boolean)
  (_ "Show segment separator")
  (_ "long description will be here."))

(define-custom 'anthy-segment-separator "|"
  '(anthy advanced)
  '(string ".*")
  (_ "Segment separator")
  (_ "long description will be here."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; canna

(define-custom-group 'canna
		     canna-im-canonical-name
		     canna-im-desc)

(define-custom 'canna-use-candidate-window? #t
  '(canna)
  '(boolean)
  (_ "Use candidate window")
  (_ "long description will be here."))

(define-custom 'canna-candidate-op-count 1
  '(canna)
  '(integer 0 99)
  (_ "Conversion key press count to show candidate window")
  (_ "long description will be here."))

(define-custom 'canna-nr-candidate-max 10
  '(canna)
  '(integer 1 20)
  (_ "Number of candidates in candidate window at a time")
  (_ "long description will be here."))

(define-custom 'canna-show-segment-separator? #f
  '(canna advanced)
  '(boolean)
  (_ "Show segment separator")
  (_ "long description will be here."))

(define-custom 'canna-segment-separator "|"
  '(canna advanced)
  '(string ".*")
  (_ "Segment separator")
  (_ "long description will be here."))

;;
;; canna-server-name
;;
(define-custom-group 'cannaserver
		     (_ "Canna server")
		     (_ "long description will be here."))

(define-custom 'custom-activate-canna-server-name? #f
  '(canna cannaserver)
  '(boolean)
  (_ "Use Canna server")
  (_ "long description will be here."))

(define-custom 'custom-preserved-canna-server-name ""
  '(canna cannaserver)
  '(string ".*")
  (_ "Canna server name")
  (_ "long description will be here."))

(define-custom 'canna-server-name ""
  '(canna cannaserver)
  '(string ".*")
  (_ "Canna server name")
  (_ "long description will be here."))

;; activity dependency
(custom-add-hook 'custom-preserved-canna-server-name
		 'custom-activity-hooks
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
		 'custom-get-hooks
		 custom-hook-get-canna-server-name)
(custom-add-hook 'canna-server-name
		 'custom-get-hooks
		 custom-hook-get-canna-server-name)

(define custom-hook-set-canna-server-name
  (lambda ()
    (set! canna-server-name
	  (and custom-activate-canna-server-name?
	       custom-preserved-canna-server-name))))

;; encode #f into canna-server-name
(custom-add-hook 'custom-activate-canna-server-name?
		 'custom-set-hooks
		 custom-hook-set-canna-server-name)
(custom-add-hook 'custom-preserved-canna-server-name
		 'custom-set-hooks
		 custom-hook-set-canna-server-name)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; skk

(define-custom-group 'skk
		     skk-im-canonical-name
		     skk-im-desc)

(define-custom 'skk-dic-file-name (string-append (sys-datadir)
						 "/skk/SKK-JISYO.L")
  '(skk)
  '(pathname)
  (_ "Dictionary file")
  (_ "long description will be here."))

(define-custom 'skk-personal-dic-filename
  (string-append (getenv "HOME") "/.skk-jisyo")
  '(skk)
  '(pathname)
  (_ "Personal dictionary file")
  (_ "long description will be here."))

(define-custom 'skk-uim-personal-dic-filename
  (string-append (getenv "HOME") "/.skk-uim-jisyo")
  '(skk)
  '(pathname)
  (_ "Personal dictionary file (dedicated to uim)")
  (_ "long description will be here."))

(define-custom 'skk-use-candidate-window? #t
  '(skk)
  '(boolean)
  (_ "Use candidate window")
  (_ "long description will be here."))

(define-custom 'skk-candidate-op-count 2
  '(skk)
  '(integer 0 99)
  (_ "Conversion key press count to show candidate window")
  (_ "long description will be here."))

(define-custom 'skk-nr-candidate-max 10
  '(skk)
  '(integer 1 20)
  (_ "Number of candidates in candidate window at a time")
  (_ "long description will be here."))

(define-custom 'skk-use-recursive-learning? #t
  '(skk advanced)
  '(boolean)
  (_ "Use recursive learning")
  (_ "long description will be here."))

(define-custom 'skk-egg-like-newline? #f
  '(skk advanced)
  '(boolean)
  (_ "Use Enter key as just committing (egg-like operation)")
  (_ "long description will be here."))

(define-custom 'skk-commit-newline-explicitly? #f
  '(skk advanced)
  '(boolean)
  (_ "Commit newline as ASCII string instead of native key-event")
  (_ "long description will be here."))

(define-custom 'skk-style 'skk-style-ddskk-like
  '(skk advanced)
  '(choice
    (skk-style-ddskk-like "ddskk" "Similar to ddskk")
    (skk-style-uim "uim" "uim native"))
  (_ "Visual style")
  (_ "long description will be here."))

(define-custom 'skk-use-numeric-conversion? #t
  '(skk advanced)
  '(boolean)
  (_ "Use numeric conversion")
  (_ "long description will be here."))

(define-custom 'skk-use-with-vi? #f
  '(skk advanced)
  '(boolean)
  (_ "Frendly for vi user")
  (_ "long description will be here."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; prime

(define-custom-group 'prime
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
  (_ "Number of candidates in candidate window at a time")
  (_ "long description will be here."))

(define-custom 'prime-always-show-window? #t
  '(prime)
  '(boolean)
  (_ "Always showing candidate window")
  (_ "long description will be here."))

(define-custom 'prime-auto-register-mode? #t
  '(prime)
  '(boolean)
  (_ "Enable auto register mode")
  (_ "long description will be here."))

(define-custom 'prime-pseudo-mode-cursor? #f
  '(prime)
  '(boolean)
  (_ "Enable pseudo mode cursor")
  (_ "long description will be here."))

(define-custom 'prime-char-annotation? #t
  '(prime)
  '(boolean)
  (_ "Show candidate annotations")
  (_ "long description will be here."))


(define-custom 'prime-mask-pending-preedit? #f
  '(prime)
  '(boolean)
  (_ "Mask preedit strings (For tcode users)")
  (_ "long description will be here."))

;(define-custom 'prime-use-numeral-key-to-select-cand? #t
;  '(prime)
;  '(boolean)
;  "Use numeral key to select candidate directly"
;  "long description will be here.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Other IMs

(define-custom-group 'other-ims
		     (_ "Other input methods")
		     (_ "long description will be here."))

(define-custom 'generic-use-candidate-window? #t
  '(other-ims)
  '(boolean)
  (_ "Use candidate window")
  (_ "long description will be here."))

(define-custom 'generic-candidate-op-count 1
  '(other-ims)
  '(integer 0 99)
  (_ "Conversion key press count to show candidate window")
  (_ "long description will be here."))

(define-custom 'generic-nr-candidate-max 10
  '(other-ims)
  '(integer 1 20)
  (_ "Number of candidates in candidate window at a time")
  (_ "long description will be here."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Spellcheck

(define-custom-group 'spellcheck
		     spellcheck-im-canonical-name
		     spellcheck-im-desc)

(define-custom 'spellcheck-use-candidate-window? #t
  '(spellcheck)
  '(boolean)
  (_ "Use candidate window")
  (_ "long description will be here."))

(define-custom 'spellcheck-candidate-op-count 1
  '(spellcheck)
  '(integer 0 99)
  (_ "Conversion key press count to show candidate window")
  (_ "long description will be here."))

(define-custom 'spellcheck-preedit-immediate-commit? #f
  '(spellcheck)
  '(boolean)
  (_ "spellcheck-preedit-immediate-commit?")
  (_ "long description will be here."))

(define-custom 'spellcheck-always-show-window? #t
  '(spellcheck)
  '(boolean)
  (_ "Always showing candidate window")
  (_ "long description will be here."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
