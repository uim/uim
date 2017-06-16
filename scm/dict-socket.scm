;;; dict.scm: rfc2229 (a dictionary server protocol) for uim.
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

(require-extension (srfi 1 8))

(require "util.scm")
(require "i18n.scm")
(require "socket.scm")
(require "fileio.scm")
(require "lolevel.scm")
(require "input-parse.scm")

(define $DICT-DEFAULT-PORT 2628)

(define (dict-server-error-responce? responce)
  (define dict-server-typical-errors '(500 501 502 503 420 421))
  (let ((errno (string->number responce)))
    (and (not errno)
         (find (lambda (n) (= errno n)) dict-server-typical-errors))))

(define (dict-server-build-message command . messages)
  (string-append
   (string-join (append (list command) messages) " ")
   "\r\n"))

(define (dict-server-parse-responce line)
  (define numbers '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
  (call-with-input-string
   line
   (lambda (port)
     (let* ((responce (next-token-of numbers port))
            (skip (skip-while '(#\space) port))
            (message (next-token '(#\space) '(#\return *eof*) (N_ "dict: Invalid message") port)))
       (values responce message)))))

(define (dict-server-get-1yz port)
  (let loop ((line (file-read-line port))
             (rest ""))
    (if (string=? line ".\r")
        (begin
          (append (list rest) (dict-server-get-message port)))
        (loop (file-read-line port) (string-append rest line)))))

(define (dict-server-get-message port)
  (let* ((line (file-read-line port)))
    (receive (responce message)
        (dict-server-parse-responce line)
      (cond ((dict-server-error-responce? responce)
             (uim-notify-fatal (format "dict (~a): ~a" (_ "Error Response") message)))
            ((string=? "151" responce)
             (dict-server-get-1yz port))
            ((string=? "150" responce)
               (let* ((responce-line (file-read-line port)))
                 (receive (responce message)
                     (dict-server-parse-responce responce-line)
                   (if (string=? "151" (substring responce-line 0 3))
                       (dict-server-get-1yz port)
                       (uim-notify-fatal (format "dict (~a): ~a" (_ "Error Response") message))))))
            ((string=? "2" (substring responce 0 1))
             '())
            ((string=? "4" (substring responce 0 1))
             '())
            ((string=? "5" (substring responce 0 1))
             '())
            (else
             (uim-notify-fatal (format "~a ~a" (_ "dict: Protocol error") message)))))))


(define (dict-server-parse-banner port)
  (dict-server-get-message port)) ;; get 1yz type message, maybe

(define (dict-server-open hostname . args)
  (let-optionals* args ((servname $DICT-DEFAULT-PORT))
    (let ((fd (tcp-connect hostname servname)))
      (if (not fd)
          (uim-notify-fatal (N_ "dict: cannot connect server")))
      (let ((port (open-file-port fd)))
        (dict-server-parse-banner port)
        port))))

(define (dict-server-get-dictionary-list port)
  (file-display (dict-server-build-message "SHOW" "DB") port)
  (dict-server-get-message port))

(define (dict-server-get-define port database word)
  (file-display (dict-server-build-message "DEFINE" database word) port)
  (dict-server-get-message port))

(define (dict-server-close port)
  (file-display (dict-server-build-message "Q") port)
  (dict-server-get-message port) ;; bye
  (close-file-port port))
