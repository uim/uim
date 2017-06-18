;;; elatin-custom.scm -- customization variables for elatin.scm
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

(require "i18n.scm")

(define elatin-im-name-label (N_ "ELatin"))
(define elatin-im-short-desc (N_ "Emacs-style Latin characters input"))
(define elatin-im-long-desc (N_ "An input method for entering Latin letters used in European languages with the key translations adopted in Emacs."))

(define-custom-group 'elatin
  elatin-im-name-label
  elatin-im-short-desc)

(define-custom-group 'elatin-properties
  (N_ "Properties")
  (N_ "long description will be here."))

(define elatin-default-rules 'elatin-rules-latin-prefix)

(define-custom 'elatin-rules elatin-default-rules
  '(elatin elatin-properties)
  (list 'choice
	(list 'elatin-rules-british
	      (N_ "British")
	      (N_ "long description will be here."))
	(list 'elatin-rules-catalan-prefix
	      (N_ "Catalan prefix")
	      (N_ "long description will be here."))
	(list 'elatin-rules-danish-postfix
	      (N_ "Danish postfix")
	      (N_ "long description will be here."))
	(list 'elatin-rules-danish-keyboard
	      (N_ "Danish keyboard")
	      (N_ "long description will be here."))
	(list 'elatin-rules-dutch
	      (N_ "Dutch")
	      (N_ "long description will be here."))
	(list 'elatin-rules-english-dvorak
	      (N_ "English Dvorak")
	      (N_ "long description will be here."))
	(list 'elatin-rules-esperanto-prefix
	      (N_ "Esperanto prefix")
	      (N_ "long description will be here."))
	(list 'elatin-rules-esperanto-postfix
	      (N_ "Esperanto postfix")
	      (N_ "long description will be here."))
	(list 'elatin-rules-finnish-postfix
	      (N_ "Finnish postfix")
	      (N_ "long description will be here."))
	(list 'elatin-rules-finnish-keyboard
	      (N_ "Finnish keyboard")
	      (N_ "long description will be here."))
	(list 'elatin-rules-french-prefix
	      (N_ "French prefix")
	      (N_ "long description will be here."))
	(list 'elatin-rules-french-postfix
	      (N_ "French postfix")
	      (N_ "long description will be here."))
	(list 'elatin-rules-french-alt-postfix
	      (N_ "French alternative postfix")
	      (N_ "long description will be here."))
	(list 'elatin-rules-french-keyboard
	      (N_ "French keyboard")
	      (N_ "long description will be here."))
	(list 'elatin-rules-french-azerty
	      (N_ "French AZERTY")
	      (N_ "long description will be here."))
	(list 'elatin-rules-german-prefix
	      (N_ "German prefix")
	      (N_ "long description will be here."))
	(list 'elatin-rules-german-postfix
	      (N_ "German postfix")
	      (N_ "long description will be here."))
	(list 'elatin-rules-german
	      (N_ "German")
	      (N_ "long description will be here."))
	(list 'elatin-rules-icelandic-postfix
	      (N_ "Icelandic postfix")
	      (N_ "long description will be here."))
	(list 'elatin-rules-icelandic-keyboard
	      (N_ "Icelandic keyboard")
	      (N_ "long description will be here."))
	(list 'elatin-rules-irish-prefix
	      (N_ "Irish prefix")
	      (N_ "long description will be here."))
	(list 'elatin-rules-italian-postfix
	      (N_ "Italian postfix")
	      (N_ "long description will be here."))
	(list 'elatin-rules-italian-alt-postfix
	      (N_ "Italian alternative postfix")
	      (N_ "long description will be here."))
	(list 'elatin-rules-italian-keyboard
	      (N_ "Italian keyboard")
	      (N_ "long description will be here."))
	(list 'elatin-rules-latin-prefix
	      (N_ "Latin prefix")
	      (N_ "long description will be here."))
	(list 'elatin-rules-latin-postfix
	      (N_ "Latin postfix")
	      (N_ "long description will be here."))
	(list 'elatin-rules-latin-alt-postfix
	      (N_ "Latin alternative postfix")
	      (N_ "long description will be here."))
	(list 'elatin-rules-latin-1-prefix
	      (N_ "Latin-1 prefix")
	      (N_ "long description will be here."))
	(list 'elatin-rules-latin-1-postfix
	      (N_ "Latin-1 postfix")
	      (N_ "long description will be here."))
	(list 'elatin-rules-latin-1-alt-postfix
	      (N_ "Latin-1 alternative postfix")
	      (N_ "long description will be here."))
	(list 'elatin-rules-latin-2-prefix
	      (N_ "Latin-2 prefix")
	      (N_ "long description will be here."))
	(list 'elatin-rules-latin-2-postfix
	      (N_ "Latin-2 postfix")
	      (N_ "long description will be here."))
	(list 'elatin-rules-latin-2-alt-postfix
	      (N_ "Latin-2 alternative postfix")
	      (N_ "long description will be here."))
	(list 'elatin-rules-latin-3-prefix
	      (N_ "Latin-3 prefix")
	      (N_ "long description will be here."))
	(list 'elatin-rules-latin-3-postfix
	      (N_ "Latin-3 postfix")
	      (N_ "long description will be here."))
	(list 'elatin-rules-latin-3-alt-postfix
	      (N_ "Latin-3 alternative postfix")
	      (N_ "long description will be here."))
	(list 'elatin-rules-latin-4-postfix
	      (N_ "Latin-4 postfix")
	      (N_ "long description will be here."))
	(list 'elatin-rules-latin-4-alt-postfix
	      (N_ "Latin-4 alternative postfix")
	      (N_ "long description will be here."))
	(list 'elatin-rules-latin-5-postfix
	      (N_ "Latin-5 postfix")
	      (N_ "long description will be here."))
	(list 'elatin-rules-latin-5-alt-postfix
	      (N_ "Latin-5 alternative postfix")
	      (N_ "long description will be here."))
	(list 'elatin-rules-latin-8-prefix
	      (N_ "Latin-8 prefix")
	      (N_ "long description will be here."))
	(list 'elatin-rules-latin-9-prefix
	      (N_ "Latin-9 prefix")
	      (N_ "long description will be here."))
	(list 'elatin-rules-latvian-keyboard
	      (N_ "Latvian keyboard")
	      (N_ "long description will be here."))
	(list 'elatin-rules-lithuanian-keyboard
	      (N_ "Lithuanian keyboard")
	      (N_ "long description will be here."))
	(list 'elatin-rules-lithuanian-numeric
	      (N_ "Lithuanian numeric")
	      (N_ "long description will be here."))
	(list 'elatin-rules-norwegian-postfix
	      (N_ "Norwegian postfix")
	      (N_ "long description will be here."))
	(list 'elatin-rules-norwegian-keyboard
	      (N_ "Norwegian keyboard")
	      (N_ "long description will be here."))
	(list 'elatin-rules-polish-slash
	      (N_ "Polish slash")
	      (N_ "long description will be here."))
	(list 'elatin-rules-portuguese-prefix
	      (N_ "Portuguese prefix")
	      (N_ "long description will be here."))
	(list 'elatin-rules-romanian-prefix
	      (N_ "Romanian prefix")
	      (N_ "long description will be here."))
	(list 'elatin-rules-romanian-alt-prefix
	      (N_ "Romanian alternative prefix")
	      (N_ "long description will be here."))
	(list 'elatin-rules-scandinavian-postfix
	      (N_ "Scandinavian postfix")
	      (N_ "long description will be here."))
	(list 'elatin-rules-slovenian
	      (N_ "Slovenian")
	      (N_ "long description will be here."))
	(list 'elatin-rules-spanish-prefix
	      (N_ "Spanish prefix")
	      (N_ "long description will be here."))
	(list 'elatin-rules-spanish-postfix
	      (N_ "Spanish postfix")
	      (N_ "long description will be here."))
	(list 'elatin-rules-spanish-keyboard
	      (N_ "Spanish keyboard")
	      (N_ "long description will be here."))
	(list 'elatin-rules-swedish-postfix
	      (N_ "Swedish postfix")
	      (N_ "long description will be here."))
	(list 'elatin-rules-swedish-keyboard
	      (N_ "Swedish keyboard")
	      (N_ "long description will be here."))
	(list 'elatin-rules-TeX
	      (N_ "TeX")
	      (N_ "long description will be here."))
	(list 'elatin-rules-turkish-postfix
	      (N_ "Turkish postfix")
	      (N_ "long description will be here."))
	(list 'elatin-rules-turkish-alt-postfix
	      (N_ "Turkish alternative postfix")
	      (N_ "long description will be here.")))
  (N_ "Latin characters keyboard layout")
  (N_ "long description will be here."))

(custom-add-hook 'elatin-rules
		 'custom-set-hooks
		 (lambda ()
		   (map (lambda (lc)
			  (let ((new-rkc (rk-context-new
					  (symbol-value elatin-rules) #f #f)))
			    (elatin-context-flush lc)
			    (elatin-update-preedit lc)
			    (elatin-context-set-rkc! lc new-rkc)))
			elatin-context-list)))

;; For VI users.
(define-custom 'elatin-esc-turns-off? #f
  '(elatin elatin-properties)
  '(boolean)
  (N_ "ESC turns off composition mode (for vi users)")
  (N_ "long description will be here."))

(define-custom 'elatin-nr-candidates-max 10
  '(elatin elatin-properties)
  '(integer 1 20)
  (N_ "Candidate window size")
  (N_ "long description will be here."))

(define-custom 'elatin-numeral-key-selects-candidate? #t
  '(elatin elatin-properties)
  '(boolean)
  (N_ "Select candidate by numeral keys")
  (N_ "long description will be here."))

(define-custom-group 'elatin-completion
  (N_ "Completion")
  (N_ "long description will be here."))

(define-custom 'elatin-use-completion? #f
  '(elatin elatin-completion)
  '(boolean)
  (N_ "Use preedit completion (mainly for TeX-style input)")
  (N_ "long description will be here."))

(define-custom 'elatin-show-all-if-ambiguous? #f
  '(elatin elatin-completion)
  '(boolean)
  (N_ "Show all if ambiguous")
  (N_ "long description will be here."))


(define-custom-group 'elatin-keys
  (N_ "ELatin key bindings")
  (N_ "long description will be here."))

(define-custom 'elatin-on-key '("<Control>\\")
  '(elatin elatin-keys)
  '(key)
  (N_ "[ELatin] on")
  (N_ "long description will be here"))

(define-custom 'elatin-off-key '("<Control>\\")
  '(elatin elatin-keys)
  '(key)
  (N_ "[ELatin] off")
  (N_ "long description will be here"))

(define-custom 'elatin-backspace-key '(generic-backspace-key)
  '(elatin elatin-keys)
  '(key)
  (N_ "[ELatin] backspace")
  (N_ "long description will be here"))

(define-custom 'elatin-commit-key '(generic-commit-key)
  '(elatin elatin-keys)
  '(key)
  (N_ "[ELatin] choose candidate")
  (N_ "long description will be here"))

(define-custom 'elatin-cancel-key '(generic-cancel-key)
  '(elatin elatin-keys)
  '(key)
  (N_ "[ELatin] close candidate window")
  (N_ "long description will be here"))

(define-custom 'elatin-next-candidate-key '("down" "<IgnoreCase><Control>n")
  '(elatin elatin-keys)
  '(key)
  (N_ "[ELatin] next candidate")
  (N_ "long description will be here"))

(define-custom 'elatin-prev-candidate-key '("up" "<IgnoreCase><Control>p")
  '(elatin elatin-keys)
  '(key)
  (N_ "[ELatin] previous candidate")
  (N_ "long description will be here"))

(define-custom 'elatin-next-page-key '(generic-next-page-key)
  '(elatin elatin-keys)
  '(key)
  (N_ "[ELatin] next page of candidate window")
  (N_ "long description will be here"))

(define-custom 'elatin-prev-page-key '(generic-prev-page-key)
  '(elatin elatin-keys)
  '(key)
  (N_ "[ELatin] previous page of candidate window")
  (N_ "long description will be here"))

(define-custom 'elatin-completion-key '("tab")
  '(elatin elatin-keys)
  '(key)
  (N_ "[ELatin] start completion")
  (N_ "long description will be here"))

(for-each (lambda (hook-sym)
	    (custom-add-hook hook-sym
			     'custom-activity-hooks
			     (lambda () elatin-use-completion?)))
	  '(elatin-show-all-if-ambiguous?
	    elatin-completion-key))

;; Local Variables:
;; mode: scheme
;; coding: utf-8
;; End:
