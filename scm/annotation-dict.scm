;;; annotation-dict.scm: dict functions for uim
;;;
;;; Copyright (c) 2010 uim Project http://code.google.com/p/uim/
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


(define dict-port #f)
(define dict-cache-alist '())

(define (dict-init)
  (and (provided? "socket")
       (set! dict-port (dict-server-open dict-server dict-servname))))

(define (dict-get-text-from-server text enc)
  (apply string-append (dict-server-get-define dict-port dict-database text)))

(define (dict-get-text-with-cache text enc)
  (let ((ret (assoc text dict-cache-alist)))
    (if ret
        (cdr ret)
        (let ((new (dict-get-text-from-server text enc)))
          (if (not (string=? new ""))
              (set! dict-cache-alist (append dict-cache-alist (list (cons text new)))))
          (if (< dict-cache-words (length dict-cache-alist))
              (set! dict-cache-alist (cdr dict-cache-alist)))
          new))))


(define (dict-get-text text enc)
  (or (and dict-port
           (dict-get-text-with-cache text enc))
      ""))

(define (dict-release)
  (if dict-port
      (begin
        (dict-server-close dict-port)
        (set! dict-port #f))))
