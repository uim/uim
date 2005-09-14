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

(define add-modules-to-module-list
  (lambda (modules current-module-list)
    (append
     (filter
      (lambda (module)
	;; Test if the module is valid
	(if (require-module (symbol->string module))
	    #t
	    (begin (puts (string-append "Warning: Module "
					(symbol->string module)
					" is not a correct module.\n"))
		   #f)))
      (remove
       (lambda (module)
	 (if (memq module current-module-list)
	     (begin (puts (string-append "Warning: Module "
					 (symbol->string module)
					 " is already registered\n"))
		    #t)
	     #f))
       modules))
     current-module-list)))

(define remove-modules-from-module-list
  (lambda (removing-modules current-module-list)
    (remove
     (lambda (module)
       (if (memq module removing-modules)
	   #t
	   #f))
     current-module-list)))

;; This function is called with 'uim-module-manager --register'
(define register-modules
  (lambda (module-names)
    (let* ((modules (map string->symbol (string-split module-names " ")))
	   (current-module-list (map string->symbol installed-im-module-list))
	   (revised-module-list (add-modules-to-module-list modules
				 current-module-list)))
      (update-all-files revised-module-list))))

;; This function is called with 'uim-module-manager --unregister'
(define unregister-modules
  (lambda (module-names)
    (let* ((modules (map string->symbol (string-split module-names " ")))
	   (current-module-list (map string->symbol installed-im-module-list))
	   (revised-module-list (remove-modules-from-module-list 
				 modules
				 current-module-list)))
      (update-all-files revised-module-list))))

(define unregister-all-modules
  (lambda (dummy)
    (update-all-files '())))

(define update-all-files
  (lambda (module-list)
    (update-installed-modules-scm module-list)
    (update-loader-scm module-list)))

(define update-loader-scm
  (lambda (module-list)
    (set! installed-im-module-list (map symbol->string module-list))
    (write-loader.scm (string-join "\n" (stub-im-generate-all-stub-im-list)))))

(define update-installed-modules-scm
  (lambda (module-list)
    (set! installed-im-module-list (map symbol->string module-list))
    (try-require "custom.scm")
    (set! enabled-im-list
	  (map custom-choice-rec-sym (custom-installed-im-list)))
    (write-installed-modules.scm
     (string-append
      ";; The described order of input methods affects which IM is preferred\n"
      ";; at the default IM selection process for each locale. i.e.  list\n"
      ";; preferable IM first for each language\n"
      "(define installed-im-module-list "
      (custom-list-as-literal installed-im-module-list)
      ")\n"
      (custom-definition-as-literal 'enabled-im-list)
      "\n"
      "(define system-available-im-list enabled-im-list)\n"))))


(prealloc-heaps-for-heavy-job)
