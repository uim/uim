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
;; All contains of this file may be distributed into appropriate files
;;

(require "i18n.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Global

(define custom-im-list-as-choice-rec
  (lambda (lst)
    (reverse (map (lambda (im)
		    (let ((sym (im-name im))
			  (label-name (im-label-name im))
			  (desc (im-short-desc im)))
		      (custom-choice-rec-new sym label-name desc)))
		  lst))))

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

(define-custom 'custom-preserved-default-im-name (im-name (find-default-im #f))
  '(global default-im-name)
  (cons
   'choice
   (custom-im-list-as-choice-rec im-list))
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

(define custom-default-enabled-im-list
  (custom-im-list-as-choice-rec im-list))

(define-custom 'enabled-im-list
               (map custom-choice-rec-sym custom-default-enabled-im-list)
  '(global)
  (cons
   'ordered-list
   custom-default-enabled-im-list)
  (_ "Enabled input methods")
  (_ "long description will be here."))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; global-keys

(define-custom-group 'global-keys
		     (_ "Global key bindings")
		     (_ "long description will be here."))

(define-custom 'generic-on-key '("zenkaku-hankaku" "<Shift> ")
               '(global-keys)
	       '(key)
	       (_ "[Global] on")
	       (_ "long description will be here"))

(define-custom 'generic-off-key '("zenkaku-hankaku" "<Shift> ")
               '(global-keys)
	       '(key)
	       (_ "[Global] off")
	       (_ "long description will be here"))

(define-custom 'generic-begin-conv-key '(" ")
               '(global-keys)
	       '(key)
	       (_ "[Global] begin conversion")
	       (_ "long description will be here"))

(define-custom 'generic-commit-key '("<Control>j" "<Control>J" generic-return-key)
               '(global-keys)
	       '(key)
	       (_ "[Global] commit")
	       (_ "long description will be here"))

(define-custom 'generic-cancel-key '("escape" "<Control>g" "<Control>G")
               '(global-keys)
	       '(key)
	       (_ "[Global] cancel")
	       (_ "long description will be here"))

(define-custom 'generic-next-candidate-key '(" " "down" "<Control>n" "<Control>N")
               '(global-keys)
	       '(key)
	       (_ "[Global] next candidate")
	       (_ "long description will be here"))

(define-custom 'generic-prev-candidate-key '("up" "<Control>p" "<Control>P")
               '(global-keys)
	       '(key)
	       (_ "[Global] previous candidate")
	       (_ "long description will be here"))

(define-custom 'generic-next-page-key '("next")
               '(global-keys)
	       '(key)
	       (_ "[Global] next page of candidate window")
	       (_ "long description will be here"))

(define-custom 'generic-prev-page-key '("prior")
               '(global-keys)
	       '(key)
	       (_ "[Global] previous page of candidate window")
	       (_ "long description will be here"))

(define-custom 'generic-beginning-of-preedit-key '("home" "<Control>a" "<Control>A")
               '(global-keys)
	       '(key)
	       (_ "[Global] beginning of preedit")
	       (_ "long description will be here"))

(define-custom 'generic-end-of-preedit-key '("end" "<Control>e" "<Control>E")
               '(global-keys)
	       '(key)
	       (_ "[Global] end of preedit")
	       (_ "long description will be here"))

(define-custom 'generic-kill-key '("<Control>k" "<Control>K")
               '(global-keys advanced)
	       '(key)
	       (_ "[Global] erase after cursor")
	       (_ "long description will be here"))

(define-custom 'generic-kill-backward-key '("<Control>u" "<Control>U")
               '(global-keys advanced)
	       '(key)
	       (_ "[Global] erase before cursor")
	       (_ "long description will be here"))

(define-custom 'generic-backspace-key '("backspace" "<Control>h" "<Control>H")
               '(global-keys advanced)
	       '(key)
	       (_ "[Global] backspace")
	       (_ "long description will be here"))

(define-custom 'generic-delete-key '("delete" "<Control>d" "<Control>D")
               '(global-keys advanced)
	       '(key)
	       (_ "[Global] delete")
	       (_ "long description will be here"))

(define-custom 'generic-go-left-key '("left" "<Control>b" "<Control>B")
               '(global-keys advanced)
	       '(key)
	       (_ "[Global] left")
	       (_ "long description will be here"))

(define-custom 'generic-go-right-key '("right" "<Control>f" "<Control>F")
               '(global-keys advanced)
	       '(key)
	       (_ "[Global] right")
	       (_ "long description will be here"))

(define-custom 'generic-return-key '("return" "<Control>m" "<Control>M")
               '(global-keys advanced)
	       '(key)
	       (_ "[Global] return")
	       (_ "long description will be here"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; anthy

(define anthy-im-label-name (N_ "Anthy"))
(define anthy-im-short-desc (N_ "Japanese Kana Kanji Conversion Engine, Anthy"))

(define-custom-group 'anthy
                     (ugettext anthy-im-label-name)
                     (ugettext anthy-im-short-desc))

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

(define canna-im-label-name (N_ "Canna"))
(define canna-im-short-desc (N_ "Japanese Kana Kanji Conversion Engine, Canna"))

(define-custom-group 'canna
                     (ugettext canna-im-label-name)
                     (ugettext canna-im-short-desc))

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

(define custom-hook-literalize-preserved-canna-server-name
  (lambda ()
    (string-append
     "(define custom-preserved-canna-server-name "
     (custom-value-as-literal 'custom-preserved-canna-server-name)
     ")\n"
     "(define canna-server-name "
     (if canna-server-name
	 (string-append "\"" canna-server-name "\"")
	 "#f")
     ")")))

(custom-add-hook 'custom-preserved-canna-server-name
		 'custom-literalize-hooks
		 custom-hook-literalize-preserved-canna-server-name)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; skk

(define skk-im-label-name (N_ "SKK"))
(define skk-im-short-desc (N_ "Uim's SKK like input method"))

(define-custom-group 'skk
                     (ugettext skk-im-label-name)
                     (ugettext skk-im-short-desc))

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

(define-custom 'skk-style 'skk-style-ddskk-like
  '(skk advanced)
  (list 'choice
	(list 'skk-style-ddskk-like (_ "ddskk") (_ "Similar to ddskk"))
	(list 'skk-style-uim (_ "uim") (_ "uim native")))
  (_ "Visual style")
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

(define-custom 'skk-use-numeric-conversion? #t
  '(skk advanced)
  '(boolean)
  (_ "Use numeric conversion")
  (_ "long description will be here."))

(define-custom 'skk-use-with-vi? #f
  '(skk advanced)
  '(boolean)
  (_ "Friendly for vi user")
  (_ "long description will be here."))

(define-custom 'skk-commit-candidate-by-label-key? #t
  '(skk advanced)
  '(boolean)
  (_ "Commit candidate by heading label keys")
  (_ "long description will be here."))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; prime

(define prime-im-label-name (N_ "PRIME"))
(define prime-im-short-desc (N_ "Japanese predictable input method"))

(define-custom-group 'prime
                     (ugettext prime-im-label-name)
                     (ugettext prime-im-short-desc))

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
  (_ "Mask preedit strings (For T-Code users)")
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

;; spellcheck IM is not available yet

;;(define spell-im-label-name (N_ "Spellcheck"))
;;(define spell-im-short-desc (N_ "Spellcheck"))
;;
;;(define-custom-group 'spellcheck
;;                     (ugettext spell-im-label-name)
;;                     (ugettext spell-im-short-desc))
;;
;;(define-custom 'spell-use-candidate-window? #t
;;  '(spellcheck)
;;  '(boolean)
;;  (_ "Use candidate window")
;;  (_ "long description will be here."))
;;
;;(define-custom 'spell-candidate-op-count 1
;;  '(spellcheck)
;;  '(integer 0 99)
;;  (_ "Conversion key press count to show candidate window")
;;  (_ "long description will be here."))
;;
;;(define-custom 'spell-preedit-immediate-commit? #f
;;  '(spellcheck)
;;  '(boolean)
;;  (_ "spell-preedit-immediate-commit?")
;;  (_ "long description will be here."))
;;
;;(define-custom 'spell-always-show-window? #t
;;  '(spellcheck)
;;  '(boolean)
;;  (_ "Always showing candidate window")
;;  (_ "long description will be here."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
