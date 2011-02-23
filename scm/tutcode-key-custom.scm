;;; tutcode-key-custom.scm: Customization variables for tutcode key bindings
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

(require "i18n.scm")

;; key defs

(define-custom-group 'tutcode-keys1
		     (N_ "TUT-Code key bindings 1")
		     (N_ "long description will be here."))

(define-custom-group 'tutcode-keys2
		     (N_ "TUT-Code key bindings 2")
		     (N_ "long description will be here."))

(define-custom 'tutcode-on-key '("<Control>\\" generic-on-key)
               '(tutcode-keys1 mode-transition)
	       '(key)
	       (N_ "[TUT-Code] on")
	       (N_ "long description will be here"))

(define-custom 'tutcode-off-key '("<Control>\\" generic-off-key)
               '(tutcode-keys1 mode-transition)
	       '(key)
	       (N_ "[TUT-Code] off")
	       (N_ "long description will be here"))

(define-custom 'tutcode-kana-toggle-key '("<IgnoreShift>'")
               '(tutcode-keys1 mode-transition)
	       '(key)
	       (N_ "[TUT-Code] toggle hiragana/katakana mode")
	       (N_ "long description will be here"))

(define-custom 'tutcode-kigou-toggle-key '("<IgnoreShift><Control>_")
               '(tutcode-keys1 mode-transition)
               '(key)
               (N_ "[TUT-Code] toggle kigou mode")
               (N_ "long description will be here"))

(define-custom 'tutcode-kigou2-toggle-key '()
               '(tutcode-keys1 mode-transition)
               '(key)
               (N_ "[TUT-Code] toggle two stroke kigou mode")
               (N_ "long description will be here"))

(define-custom 'tutcode-mazegaki-start-sequence "alj"
               '(tutcode-keys1 mode-transition)
	       '(string ".*")
	       (N_ "[TUT-Code] mazegaki conversion mode")
	       (N_ "long description will be here"))

(define-custom 'tutcode-bushu-start-sequence "ala"
               '(tutcode-keys1 mode-transition)
	       '(string ".*")
	       (N_ "[TUT-Code] bushu conversion mode")
	       (N_ "long description will be here"))

(define-custom 'tutcode-interactive-bushu-start-sequence ""
               '(tutcode-keys1 mode-transition)
	       '(string ".*")
	       (N_ "[TUT-Code] interactive bushu conversion mode")
	       (N_ "long description will be here"))

(define-custom 'tutcode-postfix-bushu-start-sequence ""
               '(tutcode-keys1)
	       '(string ".*")
	       (N_ "[TUT-Code] postfix bushu conversion")
	       (N_ "long description will be here"))

(define-custom 'tutcode-postfix-mazegaki-start-sequence ""
               '(tutcode-keys1)
	       '(string ".*")
	       (N_ "[TUT-Code] postfix mazegaki conversion")
	       (N_ "long description will be here"))

(define-custom 'tutcode-postfix-mazegaki-1-start-sequence ""
               '(tutcode-keys1)
	       '(string ".*")
	       (N_ "[TUT-Code] postfix mazegaki conversion of 1 character")
	       (N_ "long description will be here"))

(define-custom 'tutcode-postfix-mazegaki-2-start-sequence ""
               '(tutcode-keys1)
	       '(string ".*")
	       (N_ "[TUT-Code] postfix mazegaki conversion of 2 characters")
	       (N_ "long description will be here"))

(define-custom 'tutcode-postfix-mazegaki-3-start-sequence ""
               '(tutcode-keys1)
	       '(string ".*")
	       (N_ "[TUT-Code] postfix mazegaki conversion of 3 characters")
	       (N_ "long description will be here"))

(define-custom 'tutcode-postfix-mazegaki-4-start-sequence ""
               '(tutcode-keys1)
	       '(string ".*")
	       (N_ "[TUT-Code] postfix mazegaki conversion of 4 characters")
	       (N_ "long description will be here"))

(define-custom 'tutcode-postfix-mazegaki-5-start-sequence ""
               '(tutcode-keys1)
	       '(string ".*")
	       (N_ "[TUT-Code] postfix mazegaki conversion of 5 characters")
	       (N_ "long description will be here"))

(define-custom 'tutcode-postfix-mazegaki-6-start-sequence ""
               '(tutcode-keys1)
	       '(string ".*")
	       (N_ "[TUT-Code] postfix mazegaki conversion of 6 characters")
	       (N_ "long description will be here"))

(define-custom 'tutcode-postfix-mazegaki-7-start-sequence ""
               '(tutcode-keys1)
	       '(string ".*")
	       (N_ "[TUT-Code] postfix mazegaki conversion of 7 characters")
	       (N_ "long description will be here"))

(define-custom 'tutcode-postfix-mazegaki-8-start-sequence ""
               '(tutcode-keys1)
	       '(string ".*")
	       (N_ "[TUT-Code] postfix mazegaki conversion of 8 characters")
	       (N_ "long description will be here"))

(define-custom 'tutcode-postfix-mazegaki-9-start-sequence ""
               '(tutcode-keys1)
	       '(string ".*")
	       (N_ "[TUT-Code] postfix mazegaki conversion of 9 characters")
	       (N_ "long description will be here"))

(define-custom 'tutcode-postfix-mazegaki-inflection-start-sequence ""
               '(tutcode-keys1)
	       '(string ".*")
	       (N_ "[TUT-Code] postfix mazegaki conversion with inflection")
	       (N_ "long description will be here"))

(define-custom 'tutcode-postfix-mazegaki-inflection-1-start-sequence ""
               '(tutcode-keys1)
	       '(string ".*")
	       (N_ "[TUT-Code] postfix mazegaki conversion with inflection of 1 character")
	       (N_ "long description will be here"))

(define-custom 'tutcode-postfix-mazegaki-inflection-2-start-sequence ""
               '(tutcode-keys1)
	       '(string ".*")
	       (N_ "[TUT-Code] postfix mazegaki conversion with inflection of 2 characters")
	       (N_ "long description will be here"))

(define-custom 'tutcode-postfix-mazegaki-inflection-3-start-sequence ""
               '(tutcode-keys1)
	       '(string ".*")
	       (N_ "[TUT-Code] postfix mazegaki conversion with inflection of 3 characters")
	       (N_ "long description will be here"))

(define-custom 'tutcode-postfix-mazegaki-inflection-4-start-sequence ""
               '(tutcode-keys1)
	       '(string ".*")
	       (N_ "[TUT-Code] postfix mazegaki conversion with inflection of 4 characters")
	       (N_ "long description will be here"))

(define-custom 'tutcode-postfix-mazegaki-inflection-5-start-sequence ""
               '(tutcode-keys1)
	       '(string ".*")
	       (N_ "[TUT-Code] postfix mazegaki conversion with inflection of 5 characters")
	       (N_ "long description will be here"))

(define-custom 'tutcode-postfix-mazegaki-inflection-6-start-sequence ""
               '(tutcode-keys1)
	       '(string ".*")
	       (N_ "[TUT-Code] postfix mazegaki conversion with inflection of 6 characters")
	       (N_ "long description will be here"))

(define-custom 'tutcode-postfix-mazegaki-inflection-7-start-sequence ""
               '(tutcode-keys1)
	       '(string ".*")
	       (N_ "[TUT-Code] postfix mazegaki conversion with inflection of 7 characters")
	       (N_ "long description will be here"))

(define-custom 'tutcode-postfix-mazegaki-inflection-8-start-sequence ""
               '(tutcode-keys1)
	       '(string ".*")
	       (N_ "[TUT-Code] postfix mazegaki conversion with inflection of 8 characters")
	       (N_ "long description will be here"))

(define-custom 'tutcode-postfix-mazegaki-inflection-9-start-sequence ""
               '(tutcode-keys1)
	       '(string ".*")
	       (N_ "[TUT-Code] postfix mazegaki conversion with inflection of 9 characters")
	       (N_ "long description will be here"))

(define-custom 'tutcode-latin-conv-start-sequence "al/"
               '(tutcode-keys1 mode-transition)
	       '(string ".*")
	       (N_ "[TUT-Code] latin conversion mode")
	       (N_ "long description will be here"))

(define-custom 'tutcode-auto-help-redisplay-sequence ""
               '(tutcode-keys1)
	       '(string ".*")
	       (N_ "[TUT-Code] display last auto help")
	       (N_ "long description will be here"))

(define-custom 'tutcode-katakana-commit-key '()
               '(tutcode-keys1)
	       '(key)
	       (N_ "[TUT-Code] commit as katakana in yomi input of mazegaki")
	       (N_ "long description will be here"))

(define-custom 'tutcode-stroke-help-toggle-key '("<Control>/")
               '(tutcode-keys1)
               '(key)
               (N_ "[TUT-Code] toggle use of stroke help window")
               (N_ "long description will be here"))

(define-custom 'tutcode-begin-completion-key '("<Control>.")
               '(tutcode-keys1)
               '(key)
               (N_ "[TUT-Code] begin completion")
               (N_ "long description will be here"))

(define-custom 'tutcode-begin-conv-key '(generic-begin-conv-key)
               '(tutcode-keys1)
	       '(key)
	       (N_ "[TUT-Code] begin conversion")
	       (N_ "long description will be here"))

(define-custom 'tutcode-commit-key '(generic-commit-key)
               '(tutcode-keys1)
	       '(key)
	       (N_ "[TUT-Code] commit")
	       (N_ "long description will be here"))

(define-custom 'tutcode-cancel-key '("<IgnoreCase><Control>u" generic-cancel-key)
               '(tutcode-keys1)
	       '(key)
	       (N_ "[TUT-Code] cancel")
	       (N_ "long description will be here"))

(define-custom 'tutcode-next-candidate-key '(generic-next-candidate-key)
               '(tutcode-keys1)
	       '(key)
	       (N_ "[TUT-Code] next candidate")
	       (N_ "long description will be here"))

(define-custom 'tutcode-prev-candidate-key '("delete" generic-prev-candidate-key)
               '(tutcode-keys1)
	       '(key)
	       (N_ "[TUT-Code] previous candidate")
	       (N_ "long description will be here"))

(define-custom 'tutcode-next-page-key '(generic-next-page-key)
               '(tutcode-keys2)
	       '(key)
	       (N_ "[TUT-Code] next page of candidate window")
	       (N_ "long description will be here"))

(define-custom 'tutcode-prev-page-key '(generic-prev-page-key)
               '(tutcode-keys2)
	       '(key)
	       (N_ "[TUT-Code] previous page of candidate window")
	       (N_ "long description will be here"))

(define-custom 'tutcode-backspace-key '(generic-backspace-key)
               '(tutcode-keys2)
	       '(key)
	       (N_ "[TUT-Code] backspace")
	       (N_ "long description will be here"))

(define-custom 'tutcode-return-key '(generic-return-key)
               '(tutcode-keys2)
	       '(key)
	       (N_ "[TUT-Code] return")
	       (N_ "long description will be here"))

(define-custom 'tutcode-vi-escape-key '("escape" "<Control>[")
               '(tutcode-keys2)
               '(key)
               (N_ "[TUT-Code] ESC keys on vi-cooperative mode")
               (N_ "long description will be here"))

(define-custom 'tutcode-register-candidate-key '("<IgnoreShift>|")
	       '(tutcode-keys2)
	       '(key)
	       (N_ "[TUT-Code] register new entry to dictionary")
	       (N_ "long description will be here"))

(define-custom 'tutcode-purge-candidate-key '("<IgnoreShift>!")
	       '(tutcode-keys2)
	       '(key)
	       (N_ "[TUT-Code] purge the entry from dictionary")
	       (N_ "long description will be here"))

(define-custom 'tutcode-mazegaki-relimit-left-key '("<IgnoreShift><")
	       '(tutcode-keys2)
	       '(key)
	       (N_ "[TUT-Code] relimit yomi to left in mazegaki")
	       (N_ "long description will be here"))

(define-custom 'tutcode-mazegaki-relimit-right-key '("<IgnoreShift>>")
	       '(tutcode-keys2)
	       '(key)
	       (N_ "[TUT-Code] relimit yomi to right in mazegaki")
	       (N_ "long description will be here"))
