;;; socket.scm: socket library for uim.
;;;
;;; Copyright (c) 2009-2009 uim Project http://code.google.com/p/uim/
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

(require-extension (srfi 1 2 9))
(use util)
(module-load "socket")

(define addrinfo-ai-flags-alist (addrinfo-ai-flags-alist?))
(define addrinfo-ai-family-alist (addrinfo-ai-family-alist?))
(define addrinfo-ai-socktype-alist (addrinfo-ai-socktype-alist?))
(define addrinfo-ai-protocol-alist (addrinfo-ai-protocol-alist?))

(define (addrinfo-ai-flags-number l)
  (map (lambda (s)
         (assq-cdr s addrinfo-ai-flags-alist))
       l))
(define (addrinfo-ai-family-number s)
  (assq-cdr s addrinfo-ai-family-alist))
(define (addrinfo-ai-socktype-number s)
  (assq-cdr s addrinfo-ai-socktype-alist))
(define (addrinfo-ai-protocol-number s)
  (assq-cdr s addrinfo-ai-protocol-alist))

(define (string->socket-buf str)
  (map char->integer (string->list str)))
(define (socket-buf->string buf)
  (list->string (map integer->char buf)))
(define (file-read-string s len)
    (socket-buf->string (file-read s len)))
(define (file-write-string s str)
  (file-write s (string->socket-buf str)))

(define (call-with-getaddrinfo-hints flags family socktype protocol thunk)
  (let* ((hints (make-addrinfo)))
    (and flags    (addrinfo-set-ai-flags!    hints
                                             (apply logior (addrinfo-ai-flags-number flags))))
    (and family   (addrinfo-set-ai-family!   hints (addrinfo-ai-family-number   family)))
    (and socktype (addrinfo-set-ai-socktype! hints (addrinfo-ai-socktype-number socktype)))
    (and protocol (addrinfo-set-ai-protocol! hints (addrinfo-ai-protocol-number protocol)))
    (let ((ret (thunk hints)))
      (delete-addrinfo hints)
      ret)))

(define (call-with-getaddrinfo hostname servname hints thunk)
  (let* ((res (getaddrinfo hostname servname hints))
         (ret (thunk res)))
    (freeaddrinfo (car res))
    ret))

(define (call-with-sockaddr-un family path thunk)
  (let* ((sun (make-sockaddr-un)))
    (sockaddr-set-un-sun-family! sun family)
    (sockaddr-set-un-sun-path! sun path)
    (let ((ret (thunk sun)))
      (delete-sockaddr-un sun))))

(define socket-bufsiz 16384)

(define-record-type socket-port
  (make-socket-port fd inbufsiz inbuf) socket-port?
  (fd       fd?       fd!)
  (inbufsiz inbufsiz? inbufsiz!)
  (inbuf    inbuf?    inbuf!))

(define (open-socket-port fd)
  (make-socket-port fd socket-bufsiz '()))

(define (socket-read-char port)
  (if (null? (inbuf? port))
      (inbuf! port (file-read (fd? port) (inbufsiz? port))))
  (let ((c (car (inbuf? port))))
    (inbuf! port (cdr (inbuf? port)))
    (integer->char c)))

(define (socket-peek-char port)
  (if (null? (inbuf? port))
      (inbuf! port (file-read (fd? port) (inbufsiz? port))))
  (let ((c (car (inbuf? port))))
    (integer->char c)))

(define (socket-display str port)
  (file-write (fd? port) (string->socket-buf str)))

(define (socket-read-line port)
  (let loop ((c (socket-read-char port))
             (rest '()))
    (if (eq? #\newline c)
        (list->string (reverse rest))
        (loop (socket-read-char port) (cons c rest)))))

(define (socket-read-buffer port len)
  (list->string (map (lambda (i) (socket-read-char port)) (iota len))))

(define (socket-get-buffer port)
  (socket-buf->string (inbuf? port)))
