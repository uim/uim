;;; fileio.scm: low-level file IO functions for uim.
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

(require-extension (srfi 9))
(and (not (provided? "fileio"))
     (module-load "fileio")
     (provide "fileio"))

(define file-bufsiz 16384)

(define file-open-flags-alist (file-open-flags?))
(define file-open-mode-alist (file-open-mode?))
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

(define (string->file-buf str)
  (map char->integer (string->list str)))
(define (file-buf->string buf)
  (list->string (map integer->char buf)))
(define (file-read-string s len)
    (file-buf->string (file-read s len)))
(define (file-write-string s str)
  (file-write s (string->file-buf str)))

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
      (inbuf! port ((read? port) (context? port) (inbufsiz? port))))
  (if (null? (inbuf? port))
      #f
      (let ((c (car (inbuf? port))))
        (inbuf! port (cdr (inbuf? port)))
        (integer->char c))))

(define (file-peek-char port)
  (if (null? (inbuf? port))
      (inbuf! port ((read? port) (context? port) (inbufsiz? port))))
  (if (null? (inbuf? port))
      #f
      (let ((c (car (inbuf? port))))
        (integer->char c))))

(define (file-display str port)
  ((write? port) (context? port) (string->file-buf str)))

(define (file-newline str port)
  ((write? port) (context? port) '(#\newline)))

(define (file-read-line port)
  (let loop ((c (file-read-char port))
             (rest '()))
    (cond ((eq? #\newline c)
           (list->string (reverse rest)))
          ((eq? #f c)
           #f)
          (else
           (loop (file-read-char port) (cons c rest))))))

(define (file-read-buffer port len)
  (list->string (map (lambda (i) (file-read-char port)) (iota len))))

(define (file-get-buffer port)
  (file-buf->string (inbuf? port)))

(define (duplicate-fileno oldd . args)
  (let-optionals* args ((newd #f))
     (duplicate2-fileno oldd newd)))
