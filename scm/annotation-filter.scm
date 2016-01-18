;;; annotation-filter.scm: generic filter for uim
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

(require-extension (srfi 1 2))
(require "socket.scm")
(require "fileio.scm")
(require "process.scm")

;;
;; annotation-filter format
;;
;; query_message = "GET\t" query "\t" charset "\n"
;; result_messages = ( result_messages result_message | result_message ) ".\n"
;; result_message = <any characters> "\n"
;; quit_message = "QUIT\n"
;;

(define annotation-filter-socket-pair #f)

(define (annotation-filter-open-with-unix-domain-socket)
  (and-let* ((fd (unix-domain-socket-connect annotation-filter-unix-domain-socket-path)))
    (cons fd fd)))

(define (annotation-filter-open-with-tcp-socket)
  (and-let* ((fd (tcp-connect annotation-filter-tcpserver-name
                              annotation-filter-tcpserver-port)))
     (cons fd fd)))

(define (annotation-filter-open-with-pipe)
  (process-io annotation-filter-command))

(define (annotation-filter-init)
  (and (not (string=? "" annotation-filter-command))
       (let ((fds (cond ((eq? annotation-filter-server-setting? 'unixdomain)
                         (annotation-filter-open-with-unix-domain-socket))
                        ((eq? annotation-filter-server-setting? 'tcpserver)
                         (annotation-filter-open-with-tcp-socket))
                        ((eq? annotation-filter-server-setting? 'pipe)
                         (annotation-filter-open-with-pipe))
                        (else
                         (uim-notify-fatal (N_ "Custom filter connection is not defined"))
                         #f))))
         (if fds
           (set! annotation-filter-socket-pair (cons
                                                 (open-file-port (car fds))
                                                 (open-file-port (cdr fds))))
           (set! annotation-filter-socket-pair #f)))))

(define (annotation-filter-read-message iport)
  (let loop ((line (file-read-line iport))
             (rest ""))
    (if (or (not line)
            (eof-object? line)
            (string=? "." line))
        rest
        (loop (file-read-line iport) (string-append rest line)))))

(define (annotation-filter-get-text text enc)
  (or (and annotation-filter-socket-pair
           (and-let* ((iport (car annotation-filter-socket-pair))
                      (oport (cdr annotation-filter-socket-pair)))
             (file-display (format "GET\t~a\t~a\n" text enc) oport)
             (annotation-filter-read-message iport)))
      ""))

(define (annotation-filter-release)
  (and annotation-filter-socket-pair
       (and-let* ((iport (car annotation-filter-socket-pair))
                  (oport (cdr annotation-filter-socket-pair)))
         (file-display "QUIT\n" oport)
         (if (not (equal? iport oport))
             (close-file-port oport))
         (close-file-port iport)))
  #t)
