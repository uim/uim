;;; annotation.scm: annotation functions for uim
;;;
;;; Copyright (c) 2010-2013 uim Project https://github.com/uim/uim
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
;; The three procedures below are used when (enable-annotation?) is #f.
;; When you add a new annotation agent named "foo", you need to create a file
;; named "annotation-foo.scm", and define foo-init, foo-get-text
;; and foo-release in annotation-foo.scm.
;; See also: init.scm

;; Initializes an annotation agent.
(define annotation-init
  (lambda ()
    #f))

;; Returns an annotation string from the given candidate.
(define annotation-get-text
  (lambda (text encoding)
    ""))

;; Releases the annotation agent.
(define annotation-release
  (lambda ()
    #f))

(define annotation-load
  (lambda (name)
    (or (and name
             (try-require (string-append "annotation-" name ".scm"))
             (let ((env (interaction-environment)))
               (set! annotation-init
                 (eval (string->symbol (string-append "annotation-" name "-init")) env))
               (set! annotation-get-text
                 (eval (string->symbol (string-append "annotation-" name "-get-text")) env))
               (set! annotation-release
                 (eval (string->symbol (string-append "annotation-" name "-release")) env))
               #t)
             (begin
               (annotation-init)
               #t))
        (and
          (begin
            (annotation-agent-reset)
            (if (and name
                     (not (string=? name "im")))
              (uim-notify-info (N_ "invalid annotation agent name"))))))))

(define annotation-unload
  (lambda ()
    (annotation-release)
    (annotation-agent-reset)))

(define annotation-agent-reset
  (lambda ()
    (set! annotation-init (lambda () #f))
    (set! annotation-get-text (lambda (text encoding) ""))
    (set! annotation-release (lambda () #f))))
