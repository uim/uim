;;; lazy-load.scm: Lazy IM loading support
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

(define try-require-with-force-reload
  (lambda (file)
    (let ((loaded-str (string-append "*" file "-loaded*")))
      (if (provided? loaded-str)
        (try-load file)
        (try-require file)))))

;; see also pluin.scm
(define require-module-with-force-reload
  (lambda (module-name)
    (set! currently-loading-module-name module-name)
    ;; use try-require-with-force-reload because init-handler of the
    ;; IM may be overwritten by stub-im handler
    (let ((succeeded (or (module-load-with-force-reload module-name)
                         (try-require-with-force-reload
                           (find-module-scm-path
                             uim-plugin-scm-load-path
                             module-name)))))
      (set! currently-loading-module-name #f)
      succeeded)))

;; see also pluin.scm
(define module-load-with-force-reload
  (lambda (module-name)
    (if (require-dynlib module-name)
      (let ((scm-path (find-module-scm-path
                                    uim-plugin-scm-load-path module-name)))
        (if (string? scm-path)
          ;; use try-require-with-force-reload because init-handler of the
          ;; im may be overwritten by stub-im handler
          (try-require-with-force-reload scm-path)
          #t))
      #f)))

(define stub-im-generate-init-handler
  (lambda (name module-name)
    (lambda (id fake-im fake-arg)
      (let* ((stub-im (retrieve-im name))
	     (stub-im-init-handler (and stub-im
					(im-init-handler stub-im))))
	(and (require-module-with-force-reload module-name)
	     (let* ((im (retrieve-im name))
		    (init-handler (im-init-handler im))
		    (arg (im-init-arg im))
		    (context (if (not (eq? init-handler
					   stub-im-init-handler))
				 (init-handler id im arg)
				 (begin
				   (error "stub IM actualization failed")
				   #f))))
	       context))))))

(define register-stub-im
  (lambda (name lang encoding name-label short-desc module-name)
    (if (or (not (retrieve-im name))
	    (not (im-key-press-handler (retrieve-im name))))
	(let ((init-handler (stub-im-generate-init-handler name module-name)))
	  (register-im
	   name
	   lang
	   encoding
	   name-label
	   short-desc
	   #f ;; arg
	   init-handler
	   #f ;; release-handler
	   #f ;; mode-handler
	   #f ;; press-key-handler
	   #f ;; release-key-handler
	   #f ;; reset-handler
	   #f ;; get-candidate-handler
	   #f ;; set-candidate-index-handler
	   #f ;; prop-activate-handler
	   #f ;; input-string-handler
	   #f ;; focus-in-handler
	   #f ;; focus-out-handler
	   #f ;; place-handler
	   #f ;; displace-handler
	   )
	  (im-set-module-name! (retrieve-im name) module-name)))))

;; side effect: invoke require-module for all installed IM modules
(define stub-im-generate-stub-im-list
  (lambda (im-names)
    (let ((orig-enabled-im-list enabled-im-list))
      (set! enabled-im-list ())  ;; enable all IMs
      (for-each require-module installed-im-module-list)
      (set! enabled-im-list orig-enabled-im-list))
    (map (lambda (name)
	   (let ((im (retrieve-im name)))
	     (string-append
	      "    (" (symbol->string name) "\n"
	      "     \"" (im-lang im) "\"\n"
	      "     \"" (im-encoding im) "\"\n"
	      "     " (string-escape (im-name-label im)) "\n"
	      "     " (string-escape (im-short-desc im)) "\n"
	      "     \"" (im-module-name im) "\")\n"
	      )))
	 im-names)))

;; side effect: invoke require-module for all IM listed in
;; installed-im-module-list
(define stub-im-generate-all-stub-im-list
  (lambda ()
    (for-each require-module installed-im-module-list)
    (stub-im-generate-stub-im-list (map im-name
					(reverse im-list)))))
