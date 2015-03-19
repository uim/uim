;;; byeoru-custom.scm: Customization variables for byeoru.scm
;;;
;;; Copyright (c) 2003-2013 uim Project http://code.google.com/p/uim/
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


(define byeoru-im-name-label (N_ "Byeoru"))
(define byeoru-im-short-desc (N_ "The Byeoru Hangul input suite"))
;; for future use
(define byeoru-im-long-desc (N_ "A comprehensive input method suite for Hangul. This covers most of the major input methods such as 2-beol and 3-beol variants, and provides features such as Hangul-Chinese conversion. The name 'byeoru' means inkstone in Korean."))

(define byeoru-layout-alist
  (list
   (list 'byeoru-layout-hangul2
	 #f				; 2-beol can not be orderless.
	 (N_ "Hangul 2-beol Windows")
	 (N_ "Hangul 2-beol keyboard that maps an undefined shifted key to a jamo"))
   (list 'byeoru-layout-hangul2hanterm
	 #f				; 2-beol can not be orderless.
	 (N_ "Hangul 2-beol Hanterm")
	 (N_ "Hangul 2-beol keyboard that maps an undefined shifted key to an alphabet"))
   (list 'byeoru-layout-strict3final
	 #f				; neither can strict 3-beol.
	 (N_ "Hangul 3-beol final strict")
	 (N_ "Hangul 3-beol that forbids composition of precomposed jamos"))
   (list 'byeoru-layout-generous3final
	 #t				; generous 3-beol can be orderless.
	 (N_ "Hangul 3-beol final generous")
	 (N_ "Hangul 3-beol that allows composition of precomposed jamos"))
   (list 'byeoru-layout-strict390
	 #f
	 (N_ "Hangul 390 strict")
	 (N_ "Hangul 390 that forbids composition of precomposed jamos"))
   (list 'byeoru-layout-generous390
	 #t
	 (N_ "Hangul 390 generous")
	 (N_ "Hangul 390 that allows composition of precomposed jamos"))
   (list 'byeoru-layout-no-shift
	 #t
	 (N_ "Hangul 3-beol no-shift")
	 (N_ "Hangul 3-beol that does not require any shift key press"))
   (list 'byeoru-layout-romaja
	 #f				; romaja can not be orderless.
	 (N_ "Hangul romaja")
	 (N_ "Hangul romaja in the style of HWP 2004"))))

;; Check hooks.

(define-custom-group 'byeoru
  byeoru-im-name-label
  byeoru-im-short-desc)

(define-custom-group 'byeoru-keyboard
  (N_ "Keyboard")
  (N_ "long description will be here."))

(define-custom-group 'byeoru-properties
  (N_ "Properties")
  (N_ "long description will be here."))

(define-custom-group 'byeoru-dict
  (N_ "Byeoru dictionaries")
  (N_ "long description will be here."))

(define-custom-group 'byeoru-workarounds
  (N_ "Workarounds")
  (N_ "long description will be here."))

;; Changing keyboard layout should flush the automata.
(define-custom 'byeoru-layout 'byeoru-layout-hangul2
  '(byeoru byeoru-keyboard)
  (cons 'choice
	(map (lambda (entry)
	       (let ((id (car entry))
		     (label (list-ref entry 2))
		     (short-desc (list-ref entry 3)))
		 (list id
		       label
		       short-desc)))
	     byeoru-layout-alist))
  (N_ "Hangul keyboard layout")
  (N_ "long description will be here."))

(define-custom 'byeoru-jamo-orderedness 'ordered
  '(byeoru byeoru-keyboard)
  (list 'choice
	 (list 'ordered
	       (N_ "ordered")
	       (N_ "long description will be here."))
	 (list 'orderless
	       (N_ "orderless")
	       (N_ "long description will be here."))
	 (list 'more-orderless
	       (N_ "more orderless")
	       (N_ "long description will be here.")))
  (N_ "Jamo input is")
  (N_ "long description will be here."))

(custom-add-hook 'byeoru-jamo-orderedness
		 'custom-activity-hooks
		 (lambda ()
		   (cadr (assoc byeoru-layout byeoru-layout-alist))))

;; For VI users.
(define-custom 'byeoru-esc-turns-off? #t
  '(byeoru byeoru-properties)
  '(boolean)
  (N_ "ESC turns off Hangul mode (for vi users)")
  (N_ "long description will be here."))

(define-custom 'byeoru-commit-by-word? #f
  '(byeoru byeoru-properties)
  '(boolean)
  (N_ "Default unit of commitment is word")
  (N_ "long description will be here."))

(define-custom 'byeoru-shifted-romaja-isolates-vowel? #f
  '(byeoru byeoru-properties)
  '(boolean)
  (N_ "Shifted roman vowel inputs an isolated vowel")
  (N_ "long description will be here."))

(custom-add-hook 'byeoru-shifted-romaja-isolates-vowel?
		 'custom-activity-hooks
		 (lambda ()
		   (eq? byeoru-layout 'byeoru-layout-romaja)))

(define-custom 'byeoru-nr-candidate-max 10
  '(byeoru candwin)
  '(integer 1 20)
  (N_ "Candidate window size")
  (N_ "long description will be here."))

(define-custom 'byeoru-symbol-cache-size 5
  '(byeoru candwin)
  '(integer 0 20)
  (N_ "Symbol cache size")
  (N_ "long description will be here."))

(define-custom 'byeoru-conversion-history-size 1000
  '(byeoru byeoru-dict)
  '(integer 0 99999)
  (N_ "Length of conversion history to keep")
  (N_ "long description will be here."))

(define-custom 'byeoru-conversion-history-path
  (string-append (or (get-config-path #t) "") "/byeoru/byeoru-history")
  '(byeoru byeoru-dict)
  '(pathname regular-file)
  (N_ "Conversion history file")
  (N_ "long description will be here."))

(define-custom 'byeoru-personal-dict-path
  (string-append (or (get-config-path #t) "") "/byeoru/byeoru-dict")
  '(byeoru byeoru-dict)
  '(pathname regular-file)
  (N_ "Personal dictionary file")
  (N_ "long description will be here."))

(define-custom 'byeoru-sys-dict-path
  (string-append (sys-pkgdatadir) "/byeoru-data/byeoru-dict")
  '(byeoru byeoru-dict)
  '(pathname regular-file)
  (N_ "System dictionary file")
  (N_ "long description will be here."))

;; Encoding of the composing character should be changed accordingly.
(define-custom 'byeoru-compatibility-jamos-for-incomplete-syllables? #t
  '(byeoru byeoru-workarounds)
  '(boolean)
  (N_ "Represent incomplete syllables using compatibility jamos")
  (N_ "long description will be here."))

;; Local Variables:
;; mode: scheme
;; coding: utf-8
;; End:
