;;; custom-rt.scm: Partial customization support for runtime input
;;;                processes
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

;; This file provides partial custom definition support for runtime
;; input processes. The processes that wants full-featured custom API
;; such as uim-pref must overrides these definitions by loading
;; custom.scm.
;;
;; The name 'custom-rt' is not the best to represent this partial
;; functionality. Give me better name.  -- YamaKen 2005-01-14

;; TODO: write test-custom-rt.scm

(require "util.scm")
(require "key.scm")

(define-record 'custom-choice-rec
  '((sym   #f)
    (label "")
    (desc  "")))

(define custom-required-custom-files ())
(define custom-rt-primary-groups ())

;; full implementation
(define custom-load-group-conf
  (lambda (gsym)
    (let* ((group-name (symbol->string gsym))
	   (path (string-append (getenv "HOME")
				"/.uim.d/customs/custom-"
				group-name
				".scm")))
      (try-load path))))

;; full implementation
(define require-custom
  (lambda (filename)
    (let ((pre-groups (custom-list-primary-groups)))
      (require filename)
      (if (not (member filename custom-required-custom-files))
	  (set! custom-required-custom-files
		(cons filename custom-required-custom-files)))
      (let* ((post-groups (custom-list-primary-groups))
	     (new-groups (list-tail post-groups (length pre-groups))))
	(if (not (getenv "LIBUIM_VANILLA"))
	    (for-each custom-load-group-conf
		      (reverse new-groups)))))))

;; full implementation
(define custom-modify-key-predicate-names
  (lambda (keys)
    (map (lambda (key)
	   (if (symbol? key)
	       (symbolconc key '?)
	       key))
	 keys)))

;; full implementation
(define custom-rt-add-primary-groups
  (lambda (gsym)
    (if (not (member gsym custom-rt-primary-groups))
	(set! custom-rt-primary-groups
	      (cons gsym custom-rt-primary-groups)))))

;; lightweight implementation
(define custom-list-primary-groups
  (lambda ()
    (reverse custom-rt-primary-groups)))

;; lightweight implementation
(define custom-add-hook
  (lambda (custom-sym hook-sym proc)
    #f))

;; lightweight implementation
(define define-custom-group
  (lambda (gsym label desc)
    #f))

;; lightweight implementation
(define custom-exist?
  (lambda (sym type)
    (symbol-bound? sym)))

;; lightweight implementation
(define custom-value
  (lambda (sym)
    (symbol-value sym)))

;; lightweight implementation
(define define-custom
  (lambda (sym default groups type label desc)
    (custom-rt-add-primary-groups (car groups))
    (if (not (custom-exist? sym type))
	(if (eq? (car type)
		 'key)
	    (define-key-internal (symbolconc sym '?)
	                         (custom-modify-key-predicate-names default))
	    (let ((quoted-default (if (or (symbol? default)
					  (list? default))
				      (list 'quote default)
				      default)))
	      (eval (list 'define sym quoted-default)
		    toplevel-env))))))

;; lightweight implementation
;; TODO: implement
(define custom-prop-update-custom-handler
  (lambda (context custom-sym val)
    #f))
