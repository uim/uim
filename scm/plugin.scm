;;;
;;; $Id:$
;;;
;;; plugin.scm: Plugin for uim.
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

(require-extension (srfi 1))
(require "util.scm")
(require "dynlib.scm")

(define uim-plugin-scm-load-path
  (if (setugid?)
      (list (sys-pkgdatadir))
      (let ((config-path (get-config-path #f))
            (scm-paths (string-split (load-path) ":")))
	(filter string?
		(append scm-paths
		      (if config-path
			  (list (string-append config-path "/plugin"))
			  '())
		      (list (sys-pkgdatadir)))))))

;; The name 'module' is adopted from a post from Hiroyuki. If you
;; feel bad about the meaning of 'module', post your opinion to
;; uim@fdo.

(define installed-im-module-list ())
(define currently-loading-module-name #f)

;;
;; TODO: write test for load-plugin
;; returns whether initialization is succeeded
(define require-module
  (lambda (module-name)
    (set! currently-loading-module-name module-name)
    (let ((succeeded (or (module-load module-name)
                         (try-require
                           (find-module-scm-path
                             uim-plugin-scm-load-path
                             module-name)))))
      (set! currently-loading-module-name #f)
      succeeded)))

;; TODO: write test
(define load-module-conf
  (lambda ()
    (let* ((config-path (get-config-path #f))
	   (user-module-dir (if config-path
				(string-append config-path "/plugin/")
				#f))
	   (conf-file "installed-modules.scm")
	   (user-conf-file (if user-module-dir
			       (string-append user-module-dir conf-file)
			       #f)))
      (try-load conf-file)
      (if (or
	   (setugid?)
	   (not user-conf-file))
	  #f
	  (if (not (getenv "LIBUIM_VANILLA"))
	      (let ((orig-module-list installed-im-module-list)
		    (orig-enabled-list enabled-im-list))
		(if (try-load user-conf-file)
		    (begin
		      (set! installed-im-module-list
                        (delete-duplicates
                          (append orig-module-list installed-im-module-list)))
		      (set! enabled-im-list
                        (delete-duplicates
                          (append orig-enabled-list enabled-im-list)))))))))))


;; TODO: write test
(define load-enabled-modules
  (lambda ()
    (let* ((config-path (get-config-path #f))
	   (user-module-dir (if config-path
				(string-append config-path "/plugin/")
				#f))
	   (file "loader.scm")
	   (user-file (if user-module-dir
			  (string-append user-module-dir file)
			  #f)))
      (and (try-load file)
	   (or (and (not (setugid?))
		    user-file
		    (try-load user-file))
	       #t)))))

(define find-module-scm-path
  (lambda (paths module-name)
    (let ((path ()))
      (cond ((null? paths) #f)
	    ((not (string? (car paths))) #f)
	    ((file-readable? (string-append (car paths)
					     "/"
					     module-name
					     ".scm"))
	     (string-append (car paths) "/" module-name ".scm"))
	    (else
	     (find-module-scm-path (cdr paths) module-name))))))

(define module-load
  (lambda (module-name)
    (if (require-dynlib module-name)
      (let ((scm-path (find-module-scm-path
				    uim-plugin-scm-load-path module-name)))
        (if (string? scm-path)
          (try-require scm-path)
          #t))
      #f)))
