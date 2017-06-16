;;; socket.scm: socket library for uim.
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

(require-extension (srfi 1 2 9))
(use util)
(require "fileio.scm")
(require-dynlib "socket")

(define addrinfo-ai-flags-alist (addrinfo-ai-flags-alist?))
(define addrinfo-ai-family-alist (addrinfo-ai-family-alist?))
(define addrinfo-ai-socktype-alist (addrinfo-ai-socktype-alist?))
(define addrinfo-ai-protocol-alist (addrinfo-ai-protocol-alist?))

(define (addrinfo-ai-flags-number l)
  (apply logior
         (map (lambda (s)
                (assq-cdr s addrinfo-ai-flags-alist))
              l)))
(define (addrinfo-ai-family-number s)
  (assq-cdr s addrinfo-ai-family-alist))
(define (addrinfo-ai-socktype-number s)
  (assq-cdr s addrinfo-ai-socktype-alist))
(define (addrinfo-ai-protocol-number s)
  (assq-cdr s addrinfo-ai-protocol-alist))

(define (call-with-getaddrinfo-hints flags family socktype protocol thunk)
  (let* ((hints (make-addrinfo)))
    (and flags    (addrinfo-set-ai-flags!    hints (addrinfo-ai-flags-number flags)))
    (and family   (addrinfo-set-ai-family!   hints (addrinfo-ai-family-number   family)))
    (and socktype (addrinfo-set-ai-socktype! hints (addrinfo-ai-socktype-number socktype)))
    (and protocol (addrinfo-set-ai-protocol! hints (addrinfo-ai-protocol-number protocol)))
    (let ((ret (thunk hints)))
      (delete-addrinfo hints)
      ret)))

(define (call-with-getaddrinfo hostname servname hints thunk)
  (let* ((res (getaddrinfo hostname servname hints))
         (ret (if res (thunk res) '())))
    (if res
        (freeaddrinfo (car res)))
    ret))

(define (call-with-sockaddr-un family path thunk)
  (let* ((sun (make-sockaddr-un)))
    (sockaddr-set-un-sun-family! sun family)
    (sockaddr-set-un-sun-path! sun path)
    (let ((ret (thunk sun)))
      (delete-sockaddr-un sun)
      ret)))

(define (call-with-sockaddr-storage thunk)
  (let* ((ss (make-sockaddr-storage))
         (ret (thunk ss)))
    (delete-sockaddr-storage ss)
    ret))

(define shutdown-how-alist (shutdown-how-alist?))

(define (tcp-connect hostname servname)
  (call-with-getaddrinfo-hints
   '($AI_PASSIVE) '$PF_UNSPEC '$SOCK_STREAM #f
   (lambda (hints)
     (call-with-getaddrinfo
      hostname servname hints
      (lambda (res)
        (call/cc
         (lambda (fd)
           (not (map (lambda (res0)
                       (let ((s (socket (addrinfo-ai-family? res0)
                                        (addrinfo-ai-socktype? res0)
                                        (addrinfo-ai-protocol? res0))))
                         (if (< s 0)
                             #f
                             (if (< (connect s
                                             (addrinfo-ai-addr? res0)
                                             (addrinfo-ai-addrlen? res0))
                                    0)
                                 (begin
                                   (file-close s)
                                   #f)
                                 (fd s)))))
                     res)))))))))

(define (unix-domain-socket-connect socket-path)
  (let ((s (socket (addrinfo-ai-family-number '$PF_LOCAL)
                   (addrinfo-ai-socktype-number '$SOCK_STREAM)
                   0)))
    (if (< s 0)
        #f
        (call-with-sockaddr-un
         (addrinfo-ai-family-number '$PF_LOCAL)
         socket-path
         (lambda (sun)
           (if (< (connect s sun (sun-len sun))
                  0)
               (begin
                 (file-close s)
                 #f)
               s))))))

(define *tcp-listen:backlog-length* 5)

(define (tcp-listen hostname servname)
  (filter
   integer?
   (call-with-getaddrinfo-hints
    '($AI_PASSIVE) '$PF_UNSPEC '$SOCK_STREAM #f
    (lambda (hints)
      (call-with-getaddrinfo
       hostname servname hints
       (lambda (res)
         (map (lambda (res0)
                (let ((s (socket (addrinfo-ai-family? res0)
                                 (addrinfo-ai-socktype? res0)
                                 (addrinfo-ai-protocol? res0))))
                  (if (< s 0)
                      #f
                      (if (< (bind s
                                   (addrinfo-ai-addr? res0)
                                   (addrinfo-ai-addrlen? res0))
                             0)
                          (begin
                            (file-close s)
                            #f)
                          (begin
                            (listen s *tcp-listen:backlog-length*)
                            s)))))
              res)))))))

(define (unix-domain-listen path)
  (let ((s (socket (addrinfo-ai-family-number '$PF_LOCAL)
                   (addrinfo-ai-socktype-number '$SOCK_STREAM)
                   0)))
    (if (< s 0)
        #f
        (call-with-sockaddr-un (addrinfo-ai-family-number '$PF_LOCAL)
                               path
                               (lambda (un)
                                 (if (< (bind s un (sun-len un))
                                        0)
                                     (begin
                                       (file-close s)
                                       #f)
                                     (begin
                                       (listen s *tcp-listen:backlog-length*)
                                       ;; same as tcp socket
                                       (list s))))))))

(define (make-socket-server thunk accept-pred)
  (lambda (socks)
    (let loop ((cs '()))
      (let* ((fds (file-ready? (append socks cs) -1))
             (results (map (lambda (pfd)
                             (cond ((or (not pfd)
                                        (null? pfd))
                                    #f) ;; what to do?
                                   ;; closed by peer
                                   ((not (= (cdr pfd)
                                            (assq-cdr '$POLLIN file-poll-flags-alist)))
                                    (file-close (car pfd))
                                    (cons 'delete (car pfd)))
                                   ;; start new session
                                   ((find (lambda (s)
                                            (= s (car pfd)))
                                          socks)
                                    (let ((cs (accept-pred (car pfd))))
                                      (if (= -1 cs)
                                          #f ;; XXX
                                          (cons 'new cs))))
                                   ;; in session
                                   (else
                                    (if (not (thunk (car pfd)))
                                        (begin
                                          (shutdown (car pfd)
                                                    (assq-cdr '$SHUT_RDWR shutdown-how-alist))
                                          (file-close (car pfd))
                                          (cons 'delete (car pfd)))
                                        ;; keep state
                                        (car pfd)))))
                           fds))
             (new-cs    (map cdr (filter (lambda (x) (and (pair? x) (eq? (car x) 'new)))    results)))
             (delete-cs (map cdr (filter (lambda (x) (and (pair? x) (eq? (car x) 'delete))) results))))
        (loop (remove (lambda (x)
                        (find (lambda (d) (= x d))
                              delete-cs))
                      (append cs new-cs)))))))

(define (make-tcp-server thunk)
  (make-socket-server thunk
                      (lambda (s)
                        (call-with-sockaddr-storage
                         (lambda (ss)
                           (accept s ss))))))


(define (make-unix-domain-server thunk)
  (make-socket-server thunk
                      (lambda (s)
                        (call-with-sockaddr-un
                         (addrinfo-ai-family-number '$PF_LOCAL)
                         ""
                         (lambda (ss)
                           (accept s ss))))))
