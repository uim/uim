;;; http-clietn.scm: http client library for uim.
;;;
;;; Copyright (c) 2009 uim Project http://code.google.com/p/uim/
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

(define (http:open hostname servname)
  (call-with-getaddrinfo-hints
   '($AI_PASSIVE) '$PF_UNSPEC '$SOCK_STREAM #f
   (lambda (hints)
     (call-with-getaddrinfo
      hostname servname hints
      (lambda (res)
        (let* ((res0 (car res))
               (s (socket (addrinfo-ai-family? res0)
                          (addrinfo-ai-socktype? res0)
                          (addrinfo-ai-protocol? res0))))
          (connect s
                   (addrinfo-ai-addr? res0)
                   (addrinfo-ai-addrlen? res0))
          s))))))

(define (http:encode-uri-string str)
  (define hex '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F"))
  (define (hex-format2 x)
    (string-append "%"
                   (list-ref hex (modulo (/ x 16) 256))
                   (list-ref hex (modulo x 16))))
  (apply
   string-append
   (map (lambda (c)
          (hex-format2 (char->integer c)))
        (string->list str))))

(define (http:read-header port)
  (let loop ((str (socket-read-line port))
             (rest '()))
    (if (or (null? str)
            (string=? "\r" str))
        (reverse rest)
        (loop (socket-read-line port) (cons str rest)))))

(define (http:find-body-length l)
  (and-let* ((req (map (lambda (q)
                         (string-split
                          (car (string-split q "\r"))
                          ": "))
                       l))
             (cl (find (lambda (q)
                         (and (= 2 (length q))
                              (string-ci=? "content-length" (car q))))
                       req)))
    (guard (err
            (else #f))
           (string->number (cadr cl)))))

(define (http:make-request-string request-alist)
  (string-append
   (apply
    string-append
    (map (lambda (ent)
           (string-append (car ent) ": " (cdr ent) "\n"))
         (append request-alist)))
   "\n"))

(define (http:make-get-request-string hostname path request-alist)
  (string-append
   (format "GET ~a HTTP/1.0\n" path)
   (format "Host: ~a\n" hostname)
   (format "User-Agent: uim-~a\n" (uim-version))
   (http:make-request-string request-alist)))

(define (http:get hostname path servname request-alist)
    (and-let* ((socket (http:open hostname servname))
               (port (open-socket-port socket))
               (request (http:make-get-request-string hostname path request-alist))
               (nr (socket-display request port))
               (header (http:read-header port)))
      (let* ((body-length (http:find-body-length header))
             (body (if body-length
                       (socket-read-buffer port body-length)
                       (socket-get-buffer port))))
        (file-close socket)
        body)))

