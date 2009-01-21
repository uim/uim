;;; Copyright (c) 2004-2009 uim Project http://code.google.com/p/uim/
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
;;;

(define-module test.uim-test-utils-new
  (use gauche.process)
  (use gauche.selector)
  (use gauche.version)
  (use srfi-1)
  (use srfi-13)
  (use test.unit)
  (export-all))
(select-module test.uim-test-utils-new)

(define uim-test-build-path (with-module user uim-test-build-path))

;; Must be #t when LIBUIM_VERBOSE is set to 2. This enables receiving
;; backtrace following an error.
(define *uim-sh-multiline-error* #t)

(if (version<? *gaunit-version* "0.1.6")
    (error "GaUnit 0.1.6 is required"))

(sys-putenv "LIBUIM_SYSTEM_SCM_FILES" (uim-test-build-path "sigscheme" "lib"))
(sys-putenv "LIBUIM_SCM_FILES" (uim-test-build-path "scm"))
;; FIXME: '.libs' is hardcoded
(sys-putenv "LIBUIM_PLUGIN_LIB_DIR" (uim-test-build-path "uim" ".libs"))
(sys-putenv "LIBUIM_VERBOSE" "2")  ;; must be 1 or 2 (2 enables backtrace)
(sys-putenv "LIBUIM_VANILLA" "1")

(define *uim-sh-process* #f)
(define *uim-sh-selector* (make <selector>))

(define (uim-sh-select port . timeout)
  (selector-add! *uim-sh-selector*
                 port
                 (lambda (port flag)
                   (selector-delete! *uim-sh-selector* port #f #f))
                 '(r))
  (not (zero? (apply selector-select *uim-sh-selector* timeout))))

(define (uim-sh-output out thunk)
  (set! (port-buffering out) :none)
  (with-output-to-port out
    (lambda ()
      (thunk)
      (newline)
      (flush))))

(define (uim-sh-write sexp out)
  (uim-sh-output out (lambda () (write sexp))))

(define (uim-sh-display string out)
  (uim-sh-output out (lambda () (display string))))

(define (uim-sh-read-block in)
  (set! (port-buffering in) :none)
  (let ((result (call-with-output-string
                  (lambda (out)
                    (let loop ((ready (uim-sh-select in '(1 0))))
                      (when ready
                        (display (read-block 4096 in) out)
                        (loop (uim-sh-select in 1))))))))
    (if (string-prefix? "Error:" result)
      (error (string-trim-both result))
      result)))

(define (uim-read-from-string string)
  (read-from-string string))

(define (uim-read in)
  (uim-read-from-string (uim-sh-read-block in)))

(define (uim-eval sexp)
  (uim-sh-write sexp (process-input *uim-sh-process*))
  (uim-sh-read-block (process-output *uim-sh-process*)))

(define (uim sexp)
  (uim-read-from-string (uim-eval sexp)))

(define (uim-eval-raw string)
  (uim-sh-display string (process-input *uim-sh-process*))
  (uim-sh-read-block (process-output *uim-sh-process*)))

(define (uim-raw string)
  (uim-read-from-string (uim-eval-raw string)))

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

(define (uim-sh-setup)
  (set! *uim-sh-process* (run-process (uim-test-build-path "uim" "uim-sh")
                                      "-b"
                                      :input :pipe
                                      :output :pipe))
  (uim '(%%set-current-error-port! (current-output-port))))

(define (uim-sh-teardown)
  (close-input-port (process-input *uim-sh-process*))
  (process-wait *uim-sh-process*)
  (set! *uim-sh-process* #f))

(define (uim-test-setup)
  (uim-sh-setup))

(define (uim-test-teardown)
  (uim-sh-teardown))

(provide "test/uim-test-utils-new")
