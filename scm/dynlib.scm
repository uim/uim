;;; dynlib.scm: dynlib for uim.
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

(define uim-dynlib-load-path
  (if (setugid?)
      (list (string-append (sys-pkglibdir) "/plugin"))
      (let* ((ld-library-path (getenv "LD_LIBRARY_PATH"))
             (config-path (get-config-path #f))
             (user-plugin-path (if config-path
                                 (string-append config-path "/plugin")
                                 '())))
	(filter string?
		(append (list (getenv "LIBUIM_PLUGIN_LIB_DIR")
                              user-plugin-path
                              (string-append (sys-pkglibdir) "/plugin"))
			;; XXX
			(if ld-library-path
			    (string-split ld-library-path ":")
			    '()))))))

(define dynlib-alist ())

(define-record 'dynlib-entry
  '((name      "")
    ;;(desc      "")
    ;;(author    "")
    ;;(version   #f)
    (library   #f)
    (init-proc #f)
    (quit-proc #f)))

(define dynlib-list-append
  (lambda (dynlib-name library init quit)
   (let ((entry (dynlib-entry-new dynlib-name library init quit)))
     (set! dynlib-alist
	   (append dynlib-alist (list entry))))))

(define dynlib-list-delete
  (lambda (dynlib-name)
    (set! dynlib-alist
	  (alist-delete dynlib-name dynlib-alist string=?))))

(define dynlib-list-query
  (lambda (dynlib-name)
    (assoc dynlib-name dynlib-alist)))

(define dynlib-list-query-library
  (lambda (dynlib-name)
    (let ((entry (dynlib-list-query dynlib-name)))
      (and entry
	   (dynlib-entry-library entry)))))

(define dynlib-list-query-instance-init
  (lambda (dynlib-name)
    (let ((entry (dynlib-list-query dynlib-name)))
      (and entry
	   (dynlib-entry-init-proc entry)))))

(define dynlib-list-query-instance-quit
  (lambda (dynlib-name)
    (let ((entry (dynlib-list-query dynlib-name)))
      (and entry
	   (dynlib-entry-quit-proc entry)))))


(define find-dynlib-path
  (lambda (paths dynlib-name)
    (let ((path ()))
      (cond ((null? paths) #f)
	    ((not (string? (car paths))) #f)
	    ((file-readable? (string-append (car paths)
					     "/libuim-"
					     dynlib-name
					     ".so"))
	     (string-append (car paths) "/libuim-" dynlib-name ".so"))
	    (else
	     (find-dynlib-path (cdr paths) dynlib-name))))))

(define require-dynlib
  (lambda (dynlib-name)
    (let ((dynlib-exists? (dynlib-list-query dynlib-name)))
      (if dynlib-exists?
        #t
        (and-let* ((lib-path (find-dynlib-path uim-dynlib-load-path
                                               dynlib-name))
                   (proc-ptrs (%%dynlib-bind lib-path))
                   (library-ptr (car proc-ptrs))
                   (init-proc (car (cdr proc-ptrs)))
                   (quit-proc (car (cdr (cdr proc-ptrs)))))
                  (if (not (and (null? proc-ptrs)
                                (null? init-proc)
                                (null? quit-proc)))
                    (begin
                      (dynlib-list-append dynlib-name
                                          library-ptr
                                          init-proc
                                          quit-proc)
                      (provide dynlib-name)
                      #t)
                    #f))))))

(define dynlib-unload
  (lambda (dynlib-name)
    (and-let* ((dynlib-exists? (dynlib-list-query dynlib-name))
	       (library-ptr (dynlib-list-query-library dynlib-name))
	       (init-proc (dynlib-list-query-instance-init dynlib-name))
	       (quit-proc (dynlib-list-query-instance-quit dynlib-name)))
	      (%%dynlib-unbind library-ptr init-proc quit-proc)
	      (dynlib-list-delete dynlib-name)
              ;; (unprovide dynlib-name) ;; FIXME
              #t)))

(define dynlib-unload-all
  (lambda ()
     (%%dynlib-unbind-all dynlib-alist)
     (set! dynlib-alist '())
     #t))
