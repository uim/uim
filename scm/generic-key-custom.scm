;;; generic-key-custom.scm: Customization variables for generic key bindings
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


(define-custom-group 'global-keys1
		     (N_ "Global key bindings 1")
		     (N_ "long description will be here."))

(define-custom-group 'global-keys2
		     (N_ "Global key bindings 2")
		     (N_ "long description will be here."))


(define-custom 'generic-on-key '("zenkaku-hankaku" "<Shift> ")
               '(global-keys1)
	       '(key)
	       (N_ "[Global] on")
	       (N_ "long description will be here"))

(define-custom 'generic-off-key '("zenkaku-hankaku" "<Shift> ")
               '(global-keys1)
	       '(key)
	       (N_ "[Global] off")
	       (N_ "long description will be here"))

(define-custom 'generic-begin-conv-key '(" ")
               '(global-keys1)
	       '(key)
	       (N_ "[Global] begin conversion")
	       (N_ "long description will be here"))

(define-custom 'generic-commit-key '("<IgnoreCase><Control>j" generic-return-key)
               '(global-keys1)
	       '(key)
	       (N_ "[Global] commit")
	       (N_ "long description will be here"))

(define-custom 'generic-cancel-key '("escape" "<Control>[" "<IgnoreCase><Control>g")
               '(global-keys1)
	       '(key)
	       (N_ "[Global] cancel")
	       (N_ "long description will be here"))

(define-custom 'generic-next-candidate-key '(" " "down" "<IgnoreCase><Control>n")
               '(global-keys1)
	       '(key)
	       (N_ "[Global] next candidate")
	       (N_ "long description will be here"))

(define-custom 'generic-prev-candidate-key '("up" "<IgnoreCase><Control>p")
               '(global-keys1)
	       '(key)
	       (N_ "[Global] previous candidate")
	       (N_ "long description will be here"))

(define-custom 'generic-next-page-key '("next")
               '(global-keys1)
	       '(key)
	       (N_ "[Global] next page of candidate window")
	       (N_ "long description will be here"))

(define-custom 'generic-prev-page-key '("prior")
               '(global-keys1)
	       '(key)
	       (N_ "[Global] previous page of candidate window")
	       (N_ "long description will be here"))

;;
;; advanced
;;

(define-custom 'generic-beginning-of-preedit-key '("home" "<IgnoreCase><Control>a")
               '(global-keys2)
	       '(key)
	       (N_ "[Global] beginning of preedit")
	       (N_ "long description will be here"))

(define-custom 'generic-end-of-preedit-key '("end" "<IgnoreCase><Control>e")
               '(global-keys2)
	       '(key)
	       (N_ "[Global] end of preedit")
	       (N_ "long description will be here"))

(define-custom 'generic-kill-key '("<IgnoreCase><Control>k")
               '(global-keys2)
	       '(key)
	       (N_ "[Global] erase after cursor")
	       (N_ "long description will be here"))

(define-custom 'generic-kill-backward-key '("<IgnoreCase><Control>u")
               '(global-keys2)
	       '(key)
	       (N_ "[Global] erase before cursor")
	       (N_ "long description will be here"))

(define-custom 'generic-backspace-key '("backspace" "<IgnoreCase><Control>h")
               '(global-keys2)
	       '(key)
	       (N_ "[Global] backspace")
	       (N_ "long description will be here"))

(define-custom 'generic-delete-key '("delete" "<IgnoreCase><Control>d")
               '(global-keys2)
	       '(key)
	       (N_ "[Global] delete")
	       (N_ "long description will be here"))

(define-custom 'generic-go-left-key '("left" "<IgnoreCase><Control>b")
               '(global-keys2)
	       '(key)
	       (N_ "[Global] go left")
	       (N_ "long description will be here"))

(define-custom 'generic-go-right-key '("right" "<IgnoreCase><Control>f")
               '(global-keys2)
	       '(key)
	       (N_ "[Global] go right")
	       (N_ "long description will be here"))

(define-custom 'generic-return-key '("return" "<IgnoreCase><Control>m")
               '(global-keys2)
	       '(key)
	       (N_ "[Global] return")
	       (N_ "long description will be here"))
