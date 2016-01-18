;;; Copyright (c) 2004-2013 uim Project https://github.com/uim/uim
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
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS
;;; IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
;;; THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;;; PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR
;;; CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;;; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
;;; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
;;; OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
;;; ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;;

(use gauche.process)
(use gauche.selector)
(use gauche.version)
(use srfi-1)
(use srfi-13)
(use test.unit)

;; Must be #t when LIBUIM_VERBOSE is set to 2. This enables receiving
;; backtrace following an error.
(define UIM-SH-MULTILINE-ERROR #t)

(if (version<? *gaunit-version* "0.1.6")
    (error "GaUnit 0.1.6 is required"))

(sys-putenv "LIBUIM_SYSTEM_SCM_FILES" (uim-test-source-path "sigscheme" "lib"))
(sys-putenv "LIBUIM_SCM_FILES" (string-append (uim-test-source-path "scm") ":" (uim-test-build-path "scm")))
;; FIXME: '.libs' is hardcoded
(sys-putenv "LIBUIM_PLUGIN_LIB_DIR" (uim-test-build-path "uim" ".libs"))
(sys-putenv "LIBUIM_VERBOSE" "2")  ;; must be 1 or 2 (2 enables backtrace)
(sys-putenv "LIBUIM_VANILLA" "1")

(set! (port-buffering (current-output-port)) :none)

(define *uim-sh-process* #f)
(define *uim-sh-selector* (make <selector>))

(define (uim-sh-select port . timeout)
  (selector-add! *uim-sh-selector*
                 port
                 (lambda (port flag)
                   (selector-delete! *uim-sh-selector* port #f #f))
                 '(r))
  (not (zero? (apply selector-select *uim-sh-selector* timeout))))

(define (uim-sh-write sexp out)
  (set! (port-buffering out) :none)
  (with-output-to-port out
    (lambda ()
      (write sexp)
      (newline)
      (flush))))

(define (uim-sh-read in)
  (set! (port-buffering in) :none)
  (uim-sh-select in)
  (let ((uim-sh-output (with-error-handler
                         (lambda (err)
                           ;; (report-error err)
                           (read-line in) ;; ignore read error
                           #f)
                         (lambda ()
                           (read in)))))
    (if (eq? 'Error: uim-sh-output)
	(error (uim-sh-read-error in))
	uim-sh-output)))

(define (uim-sh-read-error in)
  (let* ((blocks (if UIM-SH-MULTILINE-ERROR
		     (unfold (lambda (in)
			       (not (or (char-ready? in)
					(begin
					  (sys-nanosleep 100000000) ;; 0.1s
					  (char-ready? in)))))
			     (lambda (in)
			       (read-block 4096 in))
			     values
			     in)
		     (list (read-line in))))
	 (msg (string-trim-both (string-concatenate blocks))))
    msg))
 
(define (uim sexp)
  (uim-sh-write sexp (process-input *uim-sh-process*))
  (uim-sh-read (process-output *uim-sh-process*)))

(define (uim-bool sexp)
  (not (not (uim sexp))))

;; only the tricky tests require this 'require' emulation.
(define (uim-define-siod-compatible-require)
  (uim
   '(begin
      (define require
        (lambda (filename)
          (let* ((provided-str (string-append "*" filename "-loaded*"))
                 (provided-sym (string->symbol provided-str)))
            (if (not (symbol-bound? provided-sym))
                (begin
                  (load filename)
                  (eval (list 'define provided-sym #t)
                        (interaction-environment))))
            provided-sym)))
      #t)))

(eval
   `(begin
      (define (*uim-sh-setup-proc*)
        (set! *uim-sh-process* (run-process ,(uim-test-build-path "uim" "uim-sh")
                                            "-b"
                                            :input :pipe
                                            :output :pipe))
	(uim '(%%set-current-error-port! (current-output-port))))
      (define (*uim-sh-teardown-proc*)
        (close-input-port (process-input *uim-sh-process*))
        (process-wait *uim-sh-process*)
        (set! *uim-sh-process* #f))

      (define-syntax define-uim-test-case
        (syntax-rules ()
          ((_ arg ...)
           (begin
             (gaunit-add-default-setup-proc! *uim-sh-setup-proc*)
             (gaunit-add-default-teardown-proc! *uim-sh-teardown-proc*)
             (define-test-case arg ...)
             (gaunit-delete-default-setup-proc! *uim-sh-setup-proc*)
             (gaunit-delete-default-teardown-proc! *uim-sh-teardown-proc*))))))
 (current-module))

(provide "test/uim-test-utils")
