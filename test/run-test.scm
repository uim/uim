#!/usr/bin/env gosh

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

(use gauche.version)
(use gauche.interactive)
(use file.util)
(use test.unit)

(define (uim-test-build-path . components)
  (let* ((test-dir (sys-dirname *program-name*))
         (cur-dir (current-directory))
         (top-dir (sys-normalize-pathname (build-path cur-dir "..")
                                          :absolute #t
                                          :expand #t
                                          :canonicalize #t)))
    (apply build-path top-dir components)))

(define (uim-test-source-path . components)
  (let* ((test-dir (sys-dirname *program-name*))
         (top-dir (sys-normalize-pathname (build-path test-dir "..")
                                          :absolute #t
                                          :expand #t
                                          :canonicalize #t)))
    (apply build-path top-dir components)))

(define (uim-test-test-source-path . components)
  (let* ((test-dir (sys-dirname *program-name*))
         (abs-test-dir (sys-normalize-pathname test-dir
                                          :absolute #t
                                          :expand #t
                                          :canonicalize #t)))
    (apply build-path abs-test-dir components)))


(define-macro (%add-top-path-to-load-path)
  `(add-load-path ,(uim-test-source-path)))
(define-macro (%add-test-source-path-to-load-path)
  `(add-load-path ,(uim-test-test-source-path)))

(%add-top-path-to-load-path)
(%add-test-source-path-to-load-path)

(define gaunit-main main)
(define (main args)
  (let ((args
         (if (null? (cdr args))
           (append args
                   (if (version<? (gauche-version) "0.8.13")
                     (append
                      (sys-glob (uim-test-source-path "test" "test-*.scm"))
                      (sys-glob (uim-test-source-path "test" "*" "test-*.scm")))
                     (glob (uim-test-source-path "test" "**" "test-*.scm"))))
           args)))
    (gaunit-main args)))
