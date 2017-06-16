;;; openssl.scm: low-level OpenSSL functions for uim.
;;;
;;; Copyright (c) 2009-2013 uim Project https://github.com/uim/uim
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

(require-extension (srfi 9))
(require "fileio.scm")

(guard (err (else #f))
       (require-dynlib "openssl"))

(define-record-type openssl-file-internal
  (make-openssl-file-internal-port ssl-ctx ssl) openssl-file-internal?
  (ssl-ctx  ssl-ctx?  ssl-ctx!)
  (ssl      ssl?      ssl!))

(define (ssl-read-internal ssl-port bytes)
  (SSL-read (ssl? ssl-port) bytes))
(define (ssl-write-internal ssl-port bytes)
  (SSL-write (ssl? ssl-port) bytes))

(define (call-with-open-openssl-file-port fd method thunk)
  (and (not (null? fd))
       (< 0 fd)
       (let* ((port (open-openssl-file-port fd method))
              (ctx (context? port))
              (ret (thunk port)))
         (SSL-shutdown (ssl? ctx))
         (SSL-free (ssl? ctx))
         (SSL-CTX-free (ssl-ctx? ctx))
         (file-close fd)
         ret)))

(define (open-openssl-file-port fd method)
  (call/cc
   (lambda (block)
     (let ((ssl-ctx (SSL-CTX-new method)))
       (if (not ssl-ctx)
           (begin (uim-notify-fatal (format "SSL-CTX-new: ~a" (ERR-error-string (ERR-get-error))))
                  (block #f)))
       (let ((ssl (SSL-new ssl-ctx)))
         (if (not ssl)
             (begin (uim-notify-fatal (format "SSL-new: ~a" (ERR-error-string (ERR-get-error))))
                    (SSL-CTX-free ctx)
                    (block #f)))
         (if (< (SSL-set-fd ssl fd) 0)
             (begin (uim-notify-fatal (format "SSL-set-fd: ~a" (ERR-error-string (ERR-get-error))))
                    (SSL-CTX-free ctx)
                    (SSL-free ctx)
                    (block #f)))
         (if (< (SSL-connect ssl) 0)
             (begin (uim-notify-fatal (format "SSL-connect: ~a" (ERR-error-string (ERR-get-error))))
                    (SSL-CTX-free ctx)
                    (SSL-free ctx)
                    (block #f)))
         (make-file-port (make-openssl-file-internal-port ssl-ctx ssl)
                         fd
                         file-bufsiz '() ssl-read-internal ssl-write-internal))))))

