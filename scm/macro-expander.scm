;;;
;;; Copyright (c) 2011-2013 uim Project https://github.com/uim/uim
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
(use srfi-26)

(define (filter-keyword-from-list filter-pred l)
  (let loop ((l l)
             (rest '()))
    (cond ((null? l)
           rest)
          ((not (list? (car l)))
           (loop (cdr l) rest))
          ((filter-pred (car l))
           (loop (cdr l) (cons (car l) rest)))
          (else
           (loop (cdr l) (append (loop (car l) '()) (reverse rest)))))))

(define (filter-defines l)
  (define (define? l)
    (and (not (null? l))
         (list? l)
         (eq? (car l) 'define)))
  (map (lambda (l)
         ;; XXX: only support "(define name body)" type definition
         (cons (list-ref l 1)
               (list-ref l 2)))
       (filter-keyword-from-list define? l)))

(define (banner filename)
  (display (format ";; automatically generated from ~s\n" filename))
  (display ";; see copyright notice in COPYING file\n\n"))

(cond-expand
 (chicken
  (load "macro-expander-chicken.scm")))

