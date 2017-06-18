;;; fileio.scm: low-level file IO functions for uim.
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

(require-extension (srfi 9 48))

(require-dynlib "fileio")

(define file-bufsiz 16384)

(define file-open-flags-alist (file-open-flags?))
(define file-open-mode-alist (file-open-mode?))
(define file-position-whence-alist (file-position-whence?))
(define file-poll-flags-alist (file-poll-flags?))

(define (file-set-flag l alist)
  (apply logior
         (map (lambda (s)
                (assq-cdr s alist))
              l)))
(define (file-open-flags-number l)
  (file-set-flag l file-open-flags-alist))
(define (file-open-mode-number l)
  (file-set-flag l file-open-mode-alist))
(define (file-poll-flags-number l)
  (file-set-flag l file-poll-flags-alist))

(define (file-position field)
  (file-position-set! field 0 (assq-cdr '$SEEK_CUR file-position-whence-alist)))

(define (string->file-buf str)
  (string->list str))
(define (file-buf->string buf)
  (list->string buf))
(define (file-read-string s len)
  (let ((ret (file-read s len)))
    (if (eof-object? ret)
        ret
        (file-buf->string ret))))
(define (file-write-string s str)
  (file-write s (string->file-buf str)))

(define (file-read-string-with-terminate-char socket term-char)
  (let loop ((c (file-read socket 1))
             (rest '()))
    (cond ((eof-object? c)
           (uim-notify-fatal (N_ "unexpected terminate string."))
           "")
          ((eq? (car c) term-char)
           (file-buf->string (reverse rest)))
          (else
           (loop (file-read socket 1) (cons (car c) rest))))))

(define (file-read-string-with-terminate-chars socket term-chars)
  (let ((buf (file-read socket (length term-chars))))
    (cond ((eof-object? buf)
           (raise (N_ "unexpected terminate string.")))
          ((equal? term-chars buf)
           "")
          (else
           (let loop ((c (file-read socket 1))
                      (buf buf)
                      (rest '()))
             (cond ((eof-object? c)
                    (raise (N_ "unexpected terminate string.")))
                   ((equal? term-chars (append (cdr buf) c))
                    (file-buf->string (append rest (list (car buf)))))
                   (else
                    ;; enqueue
                    (loop (file-read socket 1)
                          (append (cdr buf) c)
                          (append rest (list (car buf)))))))))))

(define (file-read-string-with-terminate socket term-char)
  (if (char? term-char)
      (file-read-string-with-terminate-char socket term-char)
      (file-read-string-with-terminate-chars socket term-char)))

(define-record-type file-port
  (make-file-port context fd inbufsiz inbuf read write) file-port?
  (context  context?  context!)
  (fd       fd?       fd!)
  (inbufsiz inbufsiz? inbufsiz!)
  (inbuf    inbuf?    inbuf!)
  (read     read?     read!)
  (write    write?    write!))

(define (open-file-port fd)
  (make-file-port fd fd file-bufsiz '() file-read file-write))

(define (close-file-port port)
  (inbuf! port '())
  (file-close (context? port))
  (context! port #f)
  (fd! port #f))

(define (call-with-open-file-port fd thunk)
  (and (not (null? fd))
       (< 0 fd)
       (let ((ret (thunk (open-file-port fd))))
         (file-close fd)
         ret)))

(define (file-read-char port)
  (if (null? (inbuf? port))
      (begin
        ;; XXX: block
        (file-ready? (list (fd? port)) -1)
        (inbuf! port ((read? port) (context? port) (inbufsiz? port)))))
  (let ((buf (inbuf? port)))
    (if (or (eof-object? buf) ;; disconnect?
            (not buf))
        buf
        (let ((c (car buf)))
          (inbuf! port (cdr buf))
          c))))

(define (file-peek-char port)
  (if (null? (inbuf? port))
      (inbuf! port ((read? port) (context? port) (inbufsiz? port))))
  (let ((buf (inbuf? port)))
    (if (or (eof-object? buf) ;; disconnect?
            (not buf))
        buf
        (let ((c (car buf)))
          c))))

(define (file-display str port)
  ((write? port) (context? port) (string->file-buf str)))

(define (file-newline port)
  ((write? port) (context? port) (string->file-buf (list->string '(#\newline)))))

(define (file-read-line port)
  (let loop ((c (file-read-char port))
             (rest '()))
    (cond ((eq? #\newline c)
           (list->string (reverse rest)))
          ((or (eof-object? c) ;; disconnect?
               (not c))
           (if (null? rest)
             c
             (list->string (reverse rest))))
          (else
           (loop (file-read-char port) (cons c rest))))))

(define (file-read-buffer port len)
  (list->string (map (lambda (i) (file-read-char port)) (iota len))))

(define (file-get-buffer port)
  (file-buf->string (inbuf? port)))

(define (file-write-sexp l port)
  ((write? port) (context? port) (string->file-buf (write-to-string l))))

;; XXX: multi ports are not considered
(define %*file-reading* #f)

(cond-expand
 (sigscheme
  (define %file-eof-error?
    (lambda (err)
      (and (%%error-object? err)
           (string-prefix? "in read: EOF " (cadr err)))))) ;; XXX
 (else
  (error "cannot detect EOF error")))

(define (%file-partial-read . args)
  (guard (err
          ((%file-eof-error? err) err))
         (apply read args)))

(define file-read-sexp
  (let ((p (open-input-string ""))
        (buf ""))
    (lambda (port)
      (let ((expr (%file-partial-read p)))

        (if (or (eof-object? expr)
                (%file-eof-error? expr))
            (let ((line (file-read-line port)))
              (if (null? line) ;; disconnect?
                  (begin
                    (set! buf "")
                    (set! %*file-reading* #f)
                    expr)
                  (if (eof-object? line)
                      (if (%file-eof-error? expr)
                          (raise expr)
                          line)
                      (begin
                        (set! buf (if (%file-eof-error? expr)
                                      (string-append buf line)
                                      line))
                        (set! p (open-input-string buf))
                        (set! %*file-reading* #t)
                        (file-read-sexp port)))))
            (begin
              (set! buf "")
              (set! %*file-reading* #f)
              expr))))))

(define (duplicate-fileno oldd . args)
  (let-optionals* args ((newd #f))
     (duplicate2-fileno oldd newd)))

(define (file-ready? fd-list timeout)
  (let* ((fds (map (lambda (fd)
                     (cons fd (assq-cdr '$POLLIN file-poll-flags-alist)))
                   fd-list))
         (ret (file-poll fds timeout)))
    (cond ((not ret)
           (uim-notify-fatal (format "~a: '~a'" (_ "poll error") (posix-error-string)))
           #f)
          ((null? ret)
           ;;(uim-notify-info (N_ "timeout"))
           #f)
          (else
           ret))))

