;;; socket.scm: socket library for uim.
;;;
;;; Copyright (c) 2003-2008 uim Project http://code.google.com/p/uim/
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

(define (call-with-getaddrinfo hostname servname hint thunk)
  (let* ((res (getaddrinfo hostname servname hint))
         (ret (thunk res)))
    (freeaddrinfo (car res))
    ret))
