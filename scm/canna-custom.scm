;;; canna-custom.scm: Customization variables for canna.scm
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


(define canna-im-name-label (N_ "Canna"))
(define canna-im-short-desc (N_ "Japanese Kana Kanji Conversion Engine, Canna"))

(define-custom-group 'canna
                     (ugettext canna-im-name-label)
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

(custom-add-hook 'canna-segment-separator
		 'custom-activity-hooks
		 (lambda ()
		   canna-show-segment-separator?))

;;
;; canna-server-name
;;

; TODO: support cannaserver on other host
(define canna-server-name #f)
;(define canna-server-name "localhost")
;(define canna-server-name "127.0.0.1")

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
