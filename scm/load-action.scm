;;; load-action.scm: loads action.scm in accordance with platform configuration
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

(require "util.scm")
(require "i18n.scm")

(define indication-rec-spec
  '((id           #f) ;; must be first member
    (iconic-label "")
    (label        "")
    (short-desc   "")))
(define-record 'indication indication-rec-spec)

(define indication-alist-entry-extract-choice
  (lambda (entry)
    (let ((act-id (car entry))
	  (indication (cdr entry)))
      (list act-id
	    (indication-label indication)
	    (indication-short-desc indication)))))

(define action-id-list->choice
  (lambda (act-ids indication-alist)
    (map (lambda (act)
	   (indication-alist-entry-extract-choice
	    (assq act indication-alist)))
	 act-ids)))

;; Unifying custom variable definitions and actions of toolbar button
;; widgets is impossible at current libuim implementation. It requires
;; library-wide default encoding configurability rather than per
;; context encoding.  -- YamaKen 2005-01-31
(define indication-alist-indicator
  (lambda (act-id alist)
    (let* ((indication (cdr (assq act-id alist)))
	   (orig-codeset (bind-textdomain-codeset (gettext-package) #f))
	   (cur-codeset (bind-textdomain-codeset (gettext-package) "EUC-JP"))
	   (translated (list (indication-id indication)
			     (indication-iconic-label indication)
			     (indication-label indication)
			     (indication-short-desc indication))))
      (bind-textdomain-codeset (gettext-package) (or orig-codeset
						     "UTF-8"))
      (lambda (owner)
	translated))))

(define do-nothing
  (lambda args
    #f))
(define register-widget do-nothing)
(define register-action do-nothing)
(define indicator-new do-nothing)
(define activity-indicator-new do-nothing)
(define actions-new do-nothing)
(define context-init-widgets do-nothing)
(define context-list-replace-widgets! do-nothing)
(define context-update-widgets do-nothing)
(define context-prop-activate-handler do-nothing)
(define context-mode-handler do-nothing)
;; override above procedures
(if enable-action?
    (require "action.scm"))
