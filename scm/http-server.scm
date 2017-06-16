;;; http-server.scm: http server library for uim.
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

(require-extension (srfi 1 2 9 48))

(require "socket.scm")
(require "input-parse.scm")
(require "i18n.scm")
(require "util.scm")
(require "wlos.scm")

(set! parser-error
      (lambda (port message . specialising-msg)
        (print
         (format "~a: ~a"
                 message
                 (apply string-append
                        (map (lambda (s)
                               (write-to-string s display))
                             specialising-msg))))))

(define alist-cdr
  (lambda (key alist . args)
    (let-optionals* args ((comp eq?))
      (and-let* ((ret
                  (cond ((eq? comp eq?)
                         (assq key alist))
                        ((eq? comp eqv?)
                         (assv key alist))
                        ((eq? comp assoc)
                         (assq key alist))
                        (else
                         (find (lambda (kons)
                                 (comp (car kons) key))
                               alist)))))
        (cdr ret)))))

(define (http-server:decode-query-uri s)
  (define hex-alist '((#\0 . #x0) (#\1 . #x1) (#\2 . #x2) (#\3 . #x3)
                      (#\4 . #x4) (#\5 . #x5) (#\6 . #x6) (#\7 . #x7)
                      (#\8 . #x8) (#\9 . #x9)
                      (#\a . #xa) (#\b . #xb) (#\c . #xc) (#\d . #xd)
                      (#\e . #xe) (#\f . #xf)
                      (#\A . #xa) (#\B . #xb) (#\C . #xc) (#\D . #xd)
                      (#\E . #xe) (#\F . #xf)))
  (let loop ((l (string->list s))
             (rest '()))
    (cond ((null? l)
           (list->string (reverse rest)))
          ((eq? (car l) #\+)
           (loop (cdr l) (cons #\space rest)))
          ((and (<= 3 (length l))
                (eq? (car l) #\%)
                (assq (cadr l) hex-alist)
                (assq (caddr l) hex-alist))
           (loop (drop l 3)
                 (cons (integer->char (+ (* 16 (cdr (assq (cadr l) hex-alist)))
                                         (cdr (assq (caddr l) hex-alist))))
                       rest)))
          (else
           (loop (cdr l) (cons (car l) rest))))))

(define (http-server:parse-post s)
  (if s
      (let* ((qs (string-split s "&"))
             (pqs (map (lambda (x)
                         (let ((s (string-split x "=")))
                           (cond ((= 1 (length s))
                                  (cons (car s) ""))
                                 ((= 2 (length s))
                                  (cons (car s) (cadr s)))
                                 (else
                                  '()))))
                       qs)))
        (map (lambda (x)
               (if (pair? x)
                   (cons (http-server:decode-query-uri (car x))
                         (http-server:decode-query-uri (cdr x)))
                   '()))
             pqs))
      '()))

(define (http-server:header-field-search l h)
  (alist-cdr h l (lambda (x y)
                   (and (string? x)
                        (string? y)
                        (string-ci=? x y)))))

(define (http-server:read-header port)
  (let loop ((str (file-read-line port))
             (rest '()))
    (if (or (eof-object? str)
            (not str)
            (not (string? str))
            (string=? "\r" str))
        (reverse rest)
        (loop (file-read-line port) (cons str rest)))))

(define (http-server:parse-header lines)
  (let loop ((lines lines)
             (state '(status header))
             (rest '()))
    (if (null? lines)
        (reverse rest)
        (call-with-input-string
         (car lines)
         (lambda (port)
           (cond ((eq? 'status (car state))
                  (let ((method
                         (next-token '() '(#\space)
                                     (format (N_ "Invalid header: ~a") (car lines))
                                     port))
                        (resource
                         (next-token '(#\space) '(#\space)
                                     (format (N_ "Invalid header: ~a") (car lines))
                                     port))
                        (http
                         (find-string-from-port?
                          "HTTP/"
                          port))
                        (version-number
                         (next-token '(#\space #\.) '(#\return)
                                     (format (N_ "Invalid header: ~a") (car lines))
                                     port)))
                    (loop (cdr lines)
                          (cdr state)
                          (append rest (list
                                        (cons 'method method)
                                        (cons 'resource resource)
                                        (cons 'version-number version-number))))))
                 ((eq? 'header (car state))
                  (let ((field-name
                         (next-token '(#\space #\tab) '(#\:)
                                     (format (N_ "Invalid header: ~a") (car lines))
                                     port))
                        (field-value
                         (next-token '(#\: #\space #\tab) '(#\return *eof*)
                                     (format (N_ "Invalid header: ~a") (car lines))
                                     port)))
                    (loop (cdr lines)
                          state
                          (cons (cons field-name field-value) rest))))))))))


(define http-server-not-found-response
  (string-append "HTTP/1.0 404 Not Found\r\n"
                 "Content-Type: text/plain\r\n"
                 "\n"
                 "File not Found\n"))
(define http-server-internal-error
  (string-append "HTTP/1.0 501 Internal Error\r\n"
                 "Content-Type: text/plain\r\n"
                 "\n"
                 "File not Found\n"))

(define-class http-server object
  '((sockets #f)
    (resource ())
    (server #f))
  '(start
    stop
    regist-resource!
    ))

(class-set-method! http-server start
  (lambda (self hostname servname)
    (http-server-set-sockets!
     self
     (tcp-listen hostname servname))
    (http-server-set-server!
     self
     (make-tcp-server
      (lambda (s)
        (call-with-open-file-port
         s
         (lambda (port)
           (or
            (and-let* ((header (http-server:read-header port))
                       (parsed-header (http-server:parse-header header))
                       (resource (assq-cdr 'resource parsed-header)))
                      (let* ((service (alist-cdr resource (http-server-resource self) string=?))
                             (content-length (http-server:header-field-search parsed-header "Content-Length")))
                        (if (not service)
                            (file-display http-server-not-found-response port)
                            (let* ((body (if content-length
                                             (file-read-buffer port (string->number content-length))
                                             #f))
                                   (message (service resource parsed-header (http-server:parse-post body))))
                              (if message
                                  (file-display
                                   (string-append "HTTP/1.0 302 Found\r\n"
                                                  "Content-Type: text/html\r\n"
                                                  "Content-Length: " (number->string (string-length message)) "\r\n"
                                                  "\n"
                                                  message)
                                   port)
                                  (file-display http-server-internal-error port))))))
            ;; unknown request
            (file-display http-server-internal-error port)))))))
    ((http-server-server self)
     (http-server-sockets self))))

(class-set-method! http-server stop
  (lambda (self)
    #t))

(class-set-method! http-server regist-resource!
  (lambda (self resource thunk)
    (http-server-set-resource!
     self
     (alist-replace (cons resource thunk)
                    (http-server-resource self)))))
