;;; uim-module-manager.scm: Part of uim-module-manager, it's not a part of libuim.
;;;
;;; Copyright (c) 2005-2013 uim Project https://github.com/uim/uim
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
(require "im.scm")
(require "lazy-load.scm")

(define stub-im-list '())  ;; dummy

(define prepare-installed-im-list
  (lambda ()
    (let ((orig-enabled-im-list enabled-im-list)
	  (orig-require require))
      (set! enabled-im-list ())  ;; enable all IMs
      (set! im-list ()) ;; reset im-list
      ;; XXX temporary solution to register all IM in a file
      (set! require
	    (lambda (file)
	      (let* ((loaded-sym (string->symbol
				  (string-append "*" file "-loaded*")))
		     (reloaded-sym (string->symbol
				    (string-append "*" file "-reloaded*"))))
		(cond
		 ((symbol-bound? reloaded-sym)
		  loaded-sym)
		 ((try-load file)
		  (eval (list 'define loaded-sym #t)
			(interaction-environment))
		  (eval (list 'define reloaded-sym #t)
			(interaction-environment))
		  loaded-sym)
		 (else
		  #f)))))
      (for-each require-module installed-im-module-list)
      (set! require orig-require)
      (set! enabled-im-list orig-enabled-im-list)
      (reverse (delete 'direct (map im-name im-list))))))

(define add-modules-to-module-list
  (lambda (modules current-module-list)
    (append
     (filter
      (lambda (module)
	;; Test if the module is valid
	(if (require-module (symbol->string module))
	    #t
	    (begin (display (string-append "Warning: Module "
					   (symbol->string module)
					   " is not a correct module.\n"))
		   #f)))
      (remove
       (lambda (module)
	 (if (memq module current-module-list)
	     (begin (display (string-append "Warning: Module "
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
    (let* ((modules (map string->symbol module-names))
	   (current-module-list (map string->symbol installed-im-module-list))
	   (revised-module-list (add-modules-to-module-list modules
				 current-module-list)))
      (update-all-files revised-module-list))))

;; This function is called with 'uim-module-manager --unregister'
(define unregister-modules
  (lambda (module-names)
    (let* ((modules (map string->symbol module-names))
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
    (write-loader.scm
     (string-append
      "(define stub-im-rec-spec\n"
      "  '((name        #f)\n"
      "    (lang        \"\")\n"
      "    (encoding    \"\")\n"
      "    (name-label  \"\")\n"
      "    (short-desc  \"\")\n"
      "    (module-name \"\")))\n"
      "(define-record 'stub-im stub-im-rec-spec)\n\n"
      "(define stub-im-list\n"
      "  '(\n"
      (string-join (stub-im-generate-all-stub-im-list) "\n")
      "    ))\n\n"
      "(for-each (lambda (stub)\n"
      "            (if (memq (stub-im-name stub) enabled-im-list)\n"
      "                (if enable-lazy-loading?\n"
      "                    (apply register-stub-im stub)\n"
      "                    (require-module (stub-im-module-name stub)))))\n"
      "          stub-im-list)\n"
      ))))

(define update-installed-modules-scm
  (lambda (module-list)
    (set! installed-im-module-list (map symbol->string module-list))
    (try-require "custom.scm")
    (set! installed-im-list (prepare-installed-im-list))
    (write-installed-modules.scm
     (string-append
      ";; The described order of input methods affects which IM is preferred\n"
      ";; at the default IM selection process for each locale. i.e.  list\n"
      ";; preferable IM first for each language\n"
      "(define installed-im-module-list "
      (custom-list-as-literal installed-im-module-list)
      ")\n"
      "(define installed-im-list "
      (custom-list-as-literal installed-im-list)
      ")\n"
      "(define enabled-im-list installed-im-list)\n"))))


(prealloc-heaps-for-heavy-job)
(verbose 1)
