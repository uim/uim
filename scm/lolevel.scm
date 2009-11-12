;;; lolevel.scm: low level access utility
;;;
;;; Copyright (c) 2009- uim Project http://code.google.com/p/uim/
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

(use srfi-1)
(module-load "lolevel")

(define (u8list-pack fmt . args)
  (apply append
         (map (lambda (f)
                (cond ((eq? (car f) 'u8)
                       (list (cadr f)))
                      ((eq? (car f) 'u16)
                       (u16->u8list (cadr f)))
                      ((eq? (car f) 'u32)
                       (u32->u8list (cadr f)))
                      ((eq? (car f) 's8)
                       (string->u8list (cadr f)))
                      ((eq? (car f) 's16)
                       (append (string->u8list (cadr f)) '(0))) ;; XXX
                      ((eq? (car f) 'u8list)
                       (append (cadr f)))
                      (else
                       (uim-notify-fatal (N_ "unknown byte operator")))))
              (zip fmt args))))

(define (u8list-unpack fmt arg)
  (let loop ((fmt fmt)
             (arg arg)
             (rest '()))
    (define (call-with-n-byte n thunk)
      (call-with-values (lambda () (split-at arg n))
        (lambda (h t)
          (loop (cdr fmt) t (cons (thunk h) rest)))))
    (cond ((null? fmt)
           (reverse rest))
          ((eq? 'u8 (car fmt))
           (call-with-n-byte 1 car))
          ((eq? 'u16 (car fmt))
           (call-with-n-byte 2 u8list->u16))
          ((eq? 'u32 (car fmt))
           (call-with-n-byte 4 u8list->u32))
          ((eq? 's8 (car fmt))
           (let ((ret (take-while (lambda (x) (not (= x 0))) arg)))
             (loop (cdr fmt)
                   (drop arg (+ 1 (length ret)))
                   (cons (list->string (map integer->char ret)) rest))))
          ((eq? 's16 (car fmt))
           (let ((ret (take-while (lambda (x) (not (= x 0))) arg)))
             (loop (cdr fmt)
                   (drop arg (+ 2 (length ret))) ;; XXX
                   (cons (list->string (map integer->char ret)) rest))))
          ((eq? 'u8list (car fmt))
           (loop (cdr fmt) '() (cons ret rest)))
          (else
           (uim-notify-fatal (N_ "unknown byte operator"))))))

(define (call-with-u8list-unpack fmt arg thunk)
  (apply thunk (u8list-unpack fmt arg)))

(define (u8list->string-buf l)
  (map integer->char l))
(define (string-buf->u8list l)
  (map char->integer l))

(define-macro (define-string-list->type-list type)
  `(define (,(string->symbol (format "string-list->~a-list" type)) l)
     (apply
      append
      (map (lambda (p)
             (u8list-pack '(,type) p))
           (append l (list ""))))))

(define-string-list->type-list s8)
(define-string-list->type-list s16)
