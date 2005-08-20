;;; uim-module-manager.scm: Part of uim-module-manager, it's not a part of libuim.
;;;
;;; Copyright (c) 2005 uim Project http://uim.freedesktop.org/
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

(require "im.scm")
(require "lazy-load.scm")


;; FIXME: This function works fine, but too hard to read.
(define (get-new-registered-module-list modules old-module-list)
  (filter
   (lambda (x) ;; Test for valid module
      (if (require-module (symbol->string x))
	  #t
	  (begin (print (string-append "Error: Module " x " is not a correct module.\n"))
		 #f)))
   (remove (lambda (x) ;; Test 
	     (if (memq x old-module-list)
		 (begin (print (string-append "Error : Module " x " already registered\n"))
			#t)
		 (begin ;(print (string-append "Module " x " not registered\n"))
		   #f)))
	   modules)))

(define (remove-unregistered-modules modules old-module-list)
  (remove (lambda (x)
	    (if (memq x modules)
		(begin ;(print (string-append "Error : Module " x " already registered\n"))
		       #t)
		(begin ;(print (string-append "Module " x " not registered\n"))
		       #f)))
	    old-module-list))

;; This function will call when $ uim-module-manager --register
(define (register-modules)
  (let* ((old-module-list (read-module-list))
	 (new-module-list (get-new-registered-module-list (get-arguments) old-module-list)))
    (update-modules-installed-modules.scm-loader.scm (append new-module-list old-module-list))))

;; This function will call when $ uim-module-manager --unregister
(define (unregister-modules)
  (let* ((old-module-list (read-module-list))
	 (new-module-list (remove-unregistered-modules (get-arguments) old-module-list)))
    (update-modules-installed-modules.scm-loader.scm new-module-list)))

(define (update-modules-installed-modules.scm-loader.scm module-list)
  (update-modules module-list)
  (update-installed-modules-scm module-list)
  (update-loader-scm module-list))

(define (update-modules module-list)
  (write-module-list #f
		     (map symbol->string
			  (reverse module-list))))

;; FIXME: Current implementation is heavy.
(define (update-loader-scm module-list)
  (set! installed-im-module-list (map symbol->string module-list))
  (write-loader.scm (string-join "\n" (stub-im-generate-all-stub-im-list))))

(define (update-installed-modules-scm module-list)
  (set! installed-im-module-list (map symbol->string module-list))
  (try-require "custom.scm")
  (set! enabled-im-list
	(map custom-choice-rec-sym (custom-installed-im-list)))
  (write-installed-modules.scm
   (string-append
    "(define installed-im-module-list "
    (custom-list-as-literal installed-im-module-list)
    ")\n"
    (custom-definition-as-literal 'enabled-im-list)
    "\n")))

;(generate-installed-modules-scm))

(prealloc-heaps-for-heavy-job)
