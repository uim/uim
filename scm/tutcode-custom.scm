;;; tutcode-custom.scm: Customization variables for tutcode.scm
;;;
;;; Copyright (c) 2003-2009 uim Project http://code.google.com/p/uim/
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


(define tutcode-im-name-label (N_ "TUT-Code"))
(define tutcode-im-short-desc (N_ "uim version of TUT-Code input method"))

(define-custom-group 'tutcode
                     tutcode-im-name-label
                     tutcode-im-short-desc)

(define-custom-group 'tutcode-dict
                     (N_ "TUT-Code dictionaries")
                     (N_ "Dictionary settings for TUT-Code"))

;;
;; dictionary
;;

(define-custom 'tutcode-dic-filename (string-append (sys-datadir)
						 "/tc/mazegaki.dic")
  '(tutcode tutcode-dict)
  '(pathname regular-file)
  (N_ "Mazegaki dictionary file")
  (N_ "long description will be here."))

(define-custom 'tutcode-personal-dic-filename
  (string-append (or (home-directory (user-name)) "") "/.mazegaki.dic")
  '(tutcode tutcode-dict)
  '(pathname regular-file)
  (N_ "Personal mazegaki dictionary file")
  (N_ "long description will be here."))

(define-custom 'tutcode-rule-filename
  (string-append (sys-pkgdatadir) "/tutcode-rule.scm")
  '(tutcode)
  '(pathname regular-file)
  (N_ "Code table file")
  (N_ "Code table name is 'filename-rule' when code table file name is 'filename.scm'."))

(define-custom 'tutcode-enable-mazegaki-learning? #t
  '(tutcode)
  '(boolean)
  (N_ "Enable learning in mazegaki conversion")
  (N_ "long description will be here."))

(define-custom 'tutcode-use-with-vi? #f
  '(tutcode)
  '(boolean)
  (N_ "Enable vi-cooperative mode")
  (N_ "long description will be here."))

(define-custom 'tutcode-use-dvorak? #f
  '(tutcode)
  '(boolean)
  (N_ "Use Dvorak keyboard")
  (N_ "long description will be here."))

;;
;; candidate window
;;

(define-custom 'tutcode-use-candidate-window? #t
  '(tutcode candwin)
  '(boolean)
  (N_ "Use candidate window")
  (N_ "long description will be here."))

(define-custom 'tutcode-commit-candidate-by-label-key? #t
  '(tutcode candwin)
  '(boolean)
  (N_ "Commit candidate by heading label keys")
  (N_ "long description will be here."))

(define-custom 'tutcode-candidate-op-count 5
  '(tutcode candwin)
  '(integer 0 99)
  (N_ "Conversion key press count to show candidate window")
  (N_ "long description will be here."))

(define-custom 'tutcode-nr-candidate-max 10
  '(tutcode candwin)
  '(integer 1 99)
  (N_ "Number of candidates in candidate window at a time")
  (N_ "long description will be here."))

;; activity dependency
(custom-add-hook 'tutcode-candidate-op-count
		 'custom-activity-hooks
		 (lambda ()
		   tutcode-use-candidate-window?))

(custom-add-hook 'tutcode-nr-candidate-max
		 'custom-activity-hooks
		 (lambda ()
		   tutcode-use-candidate-window?))
