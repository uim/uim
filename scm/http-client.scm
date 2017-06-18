;;; http-client.scm: http client library for uim.
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
(require "i18n.scm")
(require "socket.scm")
(require "input-parse.scm")
(require "openssl.scm")

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

(define (http:read-chunk port)
  (define (hex-decode str)
    (define hex-alist '((#\0 . 0)  (#\1 . 1)  (#\2 . 2)  (#\3 . 3) (#\4 . 4)
                        (#\5 . 5)  (#\6 . 6)  (#\7 . 7)  (#\8 . 8) (#\9 . 9)
                        (#\a . 10) (#\A . 10) (#\b . 11) (#\B . 11)
                        (#\c . 12) (#\C . 12) (#\d . 13) (#\D . 13)
                        (#\e . 14) (#\E . 14) (#\f . 15) (#\F . 15)))
    (let ((n (reverse
              (map (lambda (c)
                     (assq-cdr c hex-alist))
                   (string->list str)))))
      (let loop ((l n)
                 (sum 0))
        (if (null? l)
            sum
            (loop (map (lambda (x) (* 16 x)) (cdr l))
                  (+ sum (car l)))))))
  (define (http:drop-cr line)
    (apply string-append (string-split line "\r")))
  (define (http:drop-space line)
    (apply string-append (string-split line " ")))

  (let loop ((len-str (http:drop-space (http:drop-cr (file-read-line port))))
             (rest '()))
    (let ((len (guard (err
                       (else #f))
                      (hex-decode len-str))))
      (if (or (not len) (= len 0))
          (apply string-append (reverse rest))
          (let ((buf (file-read-buffer port len)))
            (file-read-line port) ;; blank
            (loop (http:drop-cr (file-read-line port)) (cons buf rest)))))))

(define (http:header-field-search l h)
  (find (lambda (x)
          (and (string? (car x))
               (string-ci=? (car x) h))) l))

(define (http:chunked? l)
  (and-let* ((f (http:header-field-search l "transfer-encoding"))
             (l (string-split (cdr f) ";"))
             (ent (find (lambda (ent)
                          (string=? "chunked" ent))
                        l)))
            #t))
(define (http:content-length? l)
  (and-let* ((ret (http:header-field-search l "content-length")))
            (guard (err
                    (else #f))
                   (string->number (cdr ret)))))

(define (http:parse-header lines)
  (let loop ((lines lines)
             (state '(status header))
             (rest '()))
    (if (null? lines)
        (reverse rest)
        (call-with-input-string
         (car lines)
         (lambda (port)
           (cond ((eq? 'status (car state))
                  (let ((version
                         (find-string-from-port?
                          "HTTP/"
                          port))
                        (version-number
                         (next-token '(#\space #\.) '(#\space) (N_ "Invalid header")
                                     port))
                        (status-code
                         (next-token '(#\space) '(#\space) (N_ "Invalid header") port))
                        (reason-phrase
                         (next-token '(#\space) '(#\return *eof*) (N_ "Invalid header") port)))
                    (loop (cdr lines)
                          (cdr state)
                          (cons (cons 'header
                                      (list (cons 'version-number version-number)
                                            (cons 'status-code status-code)
                                            (cons 'reason-phrase reason-phrase)))
                                rest))))
                 ((eq? 'header (car state))
                  (let ((field-name
                         (next-token '(#\space #\tab) '(#\:) (N_ "Invalid header") port))
                        (field-value
                         (next-token '(#\: #\space #\tab) '(#\return *eof*) (N_ "Invalid header") port)))
                    (loop (cdr lines)
                          state
                          (cons (cons field-name field-value) rest))))))))))

(define (http:read-header port)
  (let loop ((str (file-read-line port))
             (rest '()))
    (if (or (eof-object? str)
            (null? str)
            (string=? "\r" str))
        (reverse rest)
        (loop (file-read-line port) (cons str rest)))))

(define (http:make-request-string request-alist)
  (string-append
   (apply
    string-append
    (map (lambda (ent)
           (string-append (car ent) ": " (cdr ent) "\n"))
         (append request-alist)))
   "\n"))

(define-record-type http-proxy
  (make-http-proxy hostname port) http-proxy?
  (hostname hostname? hostname!)
  (port     port?     port!))

(define (make-http-proxy-from-custom)
  (and (eq? http-proxy-setting 'user)
       (make-http-proxy http-proxy-hostname http-proxy-port)))

(define-record-type http-ssl
  (make-http-ssl method port) http-ssl?
  (method method? method!)
  (port   port?   port!))

(define (http:make-proxy-request-string hostname port)
  (string-append
   (format "CONNECT ~a:~d HTTP/1.1\n\n" hostname port)))

(define (http:make-get-request-string hostname path servname proxy request-alist)
  (string-append
   (if proxy
       (http:make-proxy-request-string hostname servname)
       "")
   (format "GET ~a HTTP/1.1\n" path)
   (format "Host: ~a\n" hostname)
   (format "User-Agent: uim/~a\n" (uim-version))
   (http:make-request-string request-alist)))

(define (http:get hostname path . args)
  (let-optionals* args ((servname 80)
                        (proxy #f)
                        (ssl #f)
                        (request-alist '()))
    (let* ((with-ssl? (and (provided? "openssl")
                           (http-ssl? ssl)
                           (method? ssl)))
           (call-with-open-file-port-function
            (if with-ssl?
                ;; cut
                (lambda (file thunk)
                  (call-with-open-openssl-file-port file (method? ssl) thunk))
                call-with-open-file-port))
           (file (if (http-proxy? proxy)
                     (tcp-connect (hostname? proxy) (port? proxy))
                     (if with-ssl?
                         (tcp-connect hostname (port? ssl))
                         (tcp-connect hostname servname)))))
      (if (not file)
          (uim-notify-fatal (N_ "cannot connect server")))
      (call-with-open-file-port-function
       file
       (lambda (port)
         (and-let* ((request (http:make-get-request-string hostname path servname proxy request-alist))
                    (nr (file-display request port))
                    (ready? (file-ready? (list (fd? port)) http-timeout))
                    (proxy-header (if proxy
                                      (http:read-header port)
                                      '()))
                    (header (http:read-header port))
                    (parsed-header (http:parse-header header)))
             (let ((content-length (http:content-length? parsed-header)))
               (cond (content-length
                      (file-read-buffer port content-length))
                     ((http:chunked? parsed-header)
                      (http:read-chunk port))
                     (else
                      (file-get-buffer port))))))))))

