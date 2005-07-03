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

;; This function will call when $ uim-module-manager --register
(define (register-module module-name)
  (let ((module-list (read-module-list)))
    (if (memq module-name module-list)
	(puts (string-append "Error : Module " module-name " already registered\n"))
	(if (register-module-to-file module-name module-list)
	    (begin
	      (puts (string-append "Module " module-name " registered.\n"))
	      (update-loader-scm (cons module-name module-list)))
	    (puts (string-append "Error: Module " module-name " does not registered.\n"))))))

;; This function will call when $ uim-module-manager --unregister
(define (unregister-module module-name)
  (let ((module-list (read-module-list)))
    (if (memq module-name module-list)
	(begin
	  (write-module-list #f
			     (map symbol->string
				  (remove (lambda (x) (eq? module-name x))
					  (reverse module-list))))
	  (update-loader-scm (remove (lambda (x) (eq? module-name x))
				     (reverse module-list)))
	  (puts (string-append "Module " module-name " unregistered.\n")))
	(puts (string-append "Error to remove " module-name ". No such module.\n")))))


(define (register-module-to-file new-module module-list)
  (if (require-module (symbol->string new-module))
      (begin
	(if (list? module-list)
	    (write-module-list (symbol->string new-module)
			       (map symbol->string
				    (reverse module-list)))
	    (write-module-list (symbol->string new-module)
			       #f)))
      (puts (string-append "Error: Module " new-module " is not a correct module.\n"))))


;; FIXME: Current implementation is heavy.
(define (update-loader-scm module-list)
  (set! installed-im-module-list (map symbol->string module-list))
  (write-loader.scm (string-join "\n" (stub-im-generate-all-stub-im-list))))

(prealloc-heaps-for-heavy-job)
