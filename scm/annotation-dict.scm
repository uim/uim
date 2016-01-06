;;; annotation-dict.scm: dict functions for uim
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

(require-extension (srfi 1))
(require "util.scm")
(require "dict-socket.scm")


(define annotation-dict-port #f)
(define annotation-dict-cache-alist '())

(define (annotation-dict-init)
  (and (provided? "socket")
       (set! annotation-dict-port (dict-server-open annotation-dict-server annotation-dict-servname))))

(define (annotation-dict-get-text-from-server text enc)
  (apply string-append (dict-server-get-define annotation-dict-port annotation-dict-database text)))

(define (annotation-dict-get-text-with-cache text enc)
  (let ((ret (assoc text annotation-dict-cache-alist)))
    (if ret
        (cdr ret)
        (let ((new (annotation-dict-get-text-from-server text enc)))
          (if (not (string=? new ""))
              (set! annotation-dict-cache-alist (append annotation-dict-cache-alist (list (cons text new)))))
          (if (< annotation-dict-cache-words (length annotation-dict-cache-alist))
              (set! annotation-dict-cache-alist (cdr annotation-dict-cache-alist)))
          new))))


(define (annotation-dict-get-text text enc)
  (or (and annotation-dict-port
           (annotation-dict-get-text-with-cache text enc))
      ""))

(define (annotation-dict-release)
  (if annotation-dict-port
      (begin
        (dict-server-close annotation-dict-port)
        (set! annotation-dict-port #f))))
