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

;; This file initializes platform dependent execution
;; environment. Following codes are written for ordinary UNIX desktop
;; system. Modify this file with careful investigation to change uim
;; configuration for special platforms such as embedded environments
;;   -- YamaKen 2005-01-29

(require-extension (srfi 23 34 60) (siod))

;; FIXME: Temporary workaround to resolve circular dependency of
;; util.scm, deprecated-util.scm and light-record.scm. Record users
;; should explicitly require light-record.scm.  -- YamaKen 2008-04-29
(require "light-record.scm")

;; Disable SIOD compatibilities.
(undefine the-environment)
(undefine bit-and)
(undefine bit-or)
(undefine bit-xor)
(undefine bit-not)

(let ((vlevel (getenv "LIBUIM_VERBOSE")))
  (if (and vlevel
	   (not (setugid?)))
      (guard (err (else #f))
	(verbose (string->number vlevel)))))

(define enable-action? #t)

;; SIOD compatibility for SigScheme
(if (not (symbol-bound? 'allocate-heap))
    (eval '(define allocate-heap
	     (lambda ()
	       (%%prealloc-heaps 0)))
	  (interaction-environment)))

;; Performance tuning for heavy job such as custom.scm. The value 64
;; allocates approximately 8MB of heaps. Reduce it for less-memory
;; environment (by redefining the proc in ~/.uim or default.scm).
;;   -- YamaKen 2005-02-01, 2007-01-08
(define prealloc-heaps-for-heavy-job
  (lambda ()
    (%%prealloc-heaps 64)))

(define load-user-conf
  (lambda ()
    (let ((home-dir (or (home-directory (user-name)) "")))
      (if (or
	   (setugid?)
	   (not home-dir))
	  #f
	  (let ((orig-verbose (verbose))
	        (file (or (getenv "LIBUIM_USER_SCM_FILE")
			  (string-append home-dir "/.uim"))))
	    (if (>= (verbose)
		    1)
		(verbose 1))
	    (let ((succeeded (try-load file)))
	      (verbose orig-verbose)
	      succeeded))))))

(define load-modules
  (lambda ()
    (if (not (memq 'direct enabled-im-list))
	(set! enabled-im-list (append enabled-im-list '(direct))))

    (let ((vanilla (getenv "LIBUIM_VANILLA")))
      (cond
       ;; vanilla + toppings:
       ;; disable ~/.uim, user customs and lazy loading, but enable loading
       ;; modules
       ((equal? vanilla "2")
	(set! enable-lazy-loading? #f)
	(load-enabled-modules))

       ;; pure vanilla:
       ;; disable ~/.uim, user customs, lazy loading, loading modules
       (vanilla	;; "1", legacy "0", and so on
	(set! enable-lazy-loading? #f))

       ;; fully flavored:
       ;; enable ~/.uim, user customs, lazy loading if required, and loading
       ;; modules
       (else
	(if enable-lazy-loading?
	    (require "lazy-load.scm"))
	(load-enabled-modules))))

    ;; must be loaded at last of IMs
    (if (not (retrieve-im 'direct))
	(require-module "direct"))))

(require "plugin.scm")
(require "custom-rt.scm")
(require "key.scm")
(require "im.scm")

(load-module-conf)
(require-custom "im-custom.scm")
(load-modules)

(if (symbol-bound? 'uim-notify-load)
    (uim-notify-load (symbol->string notify-agent)))

;; redefine annotation-related procedures to use an annotation agent
(require-custom "annotation-custom.scm")
(and enable-annotation?
  (annotation-load (symbol->string annotation-agent)))

(or (getenv "LIBUIM_VANILLA")
    (load-user-conf)
    (load "default.scm"))
