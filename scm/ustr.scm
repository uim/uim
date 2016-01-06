;;; ustr.scm: logical order string of abstract elements
;;;
;;; Copyright (c) 2003-2013 uim Project https://github.com/uim/uim
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

;; 'ustr' stands for 'universal (editable) string'. It represents a
;; logical order string of abstract elements for general
;; purpose. Since it does not assume any specific type of the
;; elements, we can reuse it against several generations of
;; composition table architectures such as rk or hk. It should be
;; isolated from composition table architecture to keep flexibility,
;; reusability and simplicity. ustr provides only basic string
;; operations.
;;
;; See test/test-ustr.scm to understand how it works.

(require "util.scm")

;; ustr-former, ustr-set-former!, ustr-latter and ustr-set-latter! are
;; private accessors for use in ustr.scm. Users of ustr should use
;; ustr-former-seq, ustr-set-former-seq!, ustr-latter-seq and
;; ustr-set-latter-seq! instead.

;;(define ustr-rec-spec
;;  '((former ())  ;; reversed order
;;    (latter ())))
;;(define-record 'ustr ustr-rec-spec)
;;(define ustr-new-internal ustr-new)
(define ustr-former car)
(define ustr-set-former! set-car!)
(define ustr-latter cdr)
(define ustr-set-latter! set-cdr!)

(define ustr-new
  (lambda args
    (let ((former-seq (and (not (null? args))
			   (car args)))
	  (latter-seq (and (not (null? (cdr args)))
			   (cadr args)))
	  (ustr (cons () ())))
      (and former-seq
	   (ustr-set-former-seq! ustr former-seq))
      (and latter-seq
	   (ustr-set-latter-seq! ustr latter-seq))
      ustr)))

(define ustr-whole-seq
  (lambda (ustr)
    (append-reverse (ustr-former ustr)
		    (ustr-latter ustr))))

(define ustr-former-seq
  (lambda (ustr)
    (reverse (ustr-former ustr))))

(define ustr-latter-seq ustr-latter)

(define ustr-set-whole-seq!
  (lambda (ustr seq)
    (ustr-clear-latter! ustr)
    (ustr-set-former-seq! ustr seq)))

(define ustr-set-former-seq!
  (lambda (ustr seq)
    (ustr-set-former! ustr (reverse seq))))

(define ustr-set-latter-seq! ustr-set-latter!)

(define ustr-empty?
  (lambda (ustr)
    (and (null? (ustr-former ustr))
	 (null? (ustr-latter ustr)))))

(define ustr-clear!
  (lambda (ustr)
    (ustr-clear-former! ustr)
    (ustr-clear-latter! ustr)))

(define ustr-clear-former!
  (lambda (ustr)
    (ustr-set-former! ustr ())))

(define ustr-clear-latter!
  (lambda (ustr)
    (ustr-set-latter! ustr ())))

(define ustr-copy!
  (lambda (ustr other)
    (ustr-set-former! ustr (ustr-former other))
    (ustr-set-latter! ustr (ustr-latter other))))

;; TODO: write test 
;; TODO: Rename to ustr-copy to conform to the standard naming
;; convention of light-record.scm.
(define ustr-dup
  (lambda (ustr)
    (cons (ustr-former ustr)
	  (ustr-latter ustr))))

;; ignores cursor position
(define ustr=
  (lambda (elem= ustr other)
    (and (= (ustr-length ustr)
	    (ustr-length other))
	 (every elem=
		(ustr-whole-seq ustr)
		(ustr-whole-seq other)))))

(define ustr-length
  (lambda (ustr)
    (+ (length (ustr-former ustr))
       (length (ustr-latter ustr)))))

(define ustr-nth
  (lambda (ustr n)
    (car (ustr-ref ustr n))))

(define ustr-set-nth!
  (lambda (ustr n elem)
    (set-car! (ustr-ref ustr n)
	      elem)))

;; private
(define ustr-ref
  (lambda (ustr n)
    (let* ((former (ustr-former ustr))
	   (former-len (length former))
	   (whole-len (ustr-length ustr)))
      (cond
       ((or (< n 0)
	    (<= whole-len n)
	    (<= whole-len 0))
	(error "out of range in ustr-ref"))
       ((< n former-len)
	(list-tail former
		   (- former-len n 1)))
       (else
	(list-tail (ustr-latter ustr)
		   (- n former-len)))))))

;; sequence insertion regardless of cursor position

(define ustr-append!
  (lambda (ustr seq)
    (let* ((latter (ustr-latter ustr))
	   (new-latter (append latter seq)))
      (ustr-set-latter! ustr new-latter))))

(define ustr-prepend!
  (lambda (ustr seq)
    (let* ((former (ustr-former ustr))
	   (new-former (append former (reverse seq))))
      (ustr-set-former! ustr new-former))))

;; mapping procedures

(define map-ustr-whole
  (lambda (f ustr)
    (let ((cons-map (lambda (kar kdr)
		      (cons (f kar) kdr)))
	  (former (ustr-former ustr))
	  (latter (ustr-latter ustr)))
      (fold cons-map (map f latter) former))))

(define map-ustr-former
  (lambda (f ustr)
    (let ((cons-map (lambda (kar kdr)
		      (cons (f kar) kdr)))
	  (former (ustr-former ustr)))
      (fold cons-map () former))))

(define map-ustr-latter
  (lambda (f ustr)
    (map f (ustr-latter ustr))))

(define append-map-ustr-whole
  (lambda (f ustr)
    (apply append (map-ustr-whole f ustr))))

(define append-map-ustr-former
  (lambda (f ustr)
    (apply append (map-ustr-former f ustr))))

(define append-map-ustr-latter
  (lambda (f ustr)
    (apply append (map-ustr-latter f ustr))))

;; string generators which assumes string elements for convenience

(define string-append-map-ustr-whole
  (lambda (f ustr)
    (apply string-append (map-ustr-whole f ustr))))

(define string-append-map-ustr-former
  (lambda (f ustr)
    (apply string-append (map-ustr-former f ustr))))

(define string-append-map-ustr-latter
  (lambda (f ustr)
    (apply string-append (map-ustr-latter f ustr))))

;; cursor moving

(define ustr-cursor-at-beginning?
  (lambda (ustr)
    (= (length (ustr-former ustr))
       0)))

(define ustr-cursor-at-end?
  (lambda (ustr)
    (= (length (ustr-latter ustr))
       0)))

(define ustr-cursor-pos
  (lambda (ustr)
    (length (ustr-former ustr))))

(define ustr-set-cursor-pos!
  (lambda (ustr pos)
    (if (and (>= pos 0)
	     (<= pos (ustr-length ustr)))
	(let* ((whole (ustr-whole-seq ustr))
	       (latter (list-tail whole pos))
	       (former (take whole pos)))
	  (ustr-set-former-seq! ustr former)
	  (ustr-set-latter-seq! ustr latter)
	  #t)
	#f)))

(define ustr-cursor-move!
  (lambda (ustr offset)
    (let* ((pos (ustr-cursor-pos ustr))
	   (new-pos (+ pos offset)))
      (ustr-set-cursor-pos! ustr new-pos))))

(define ustr-cursor-move-backward!
  (lambda (ustr)
    (let ((former (ustr-former ustr)))
      (if (not (null? former))
	  (let ((latter (ustr-latter ustr)))
	    (ustr-set-latter! ustr (cons (car former)
					 latter))
	    (ustr-set-former! ustr (cdr former)))))))

(define ustr-cursor-move-forward!
  (lambda (ustr)
    (let ((latter (ustr-latter ustr)))
      (if (not (null? latter))
	  (let ((former (ustr-former ustr)))
	    (ustr-set-former! ustr (cons (car latter)
					 former))
	    (ustr-set-latter! ustr (cdr latter)))))))

(define ustr-cursor-move-beginning!
  (lambda (ustr)
    (ustr-set-latter! ustr (ustr-whole-seq ustr))
    (ustr-clear-former! ustr)))

(define ustr-cursor-move-end!
  (lambda (ustr)
    (ustr-set-former! ustr (append-reverse (ustr-latter ustr)
					   (ustr-former ustr)))
    (ustr-clear-latter! ustr)))

;; retrieve, remove and insert operations

;; frontside element of cursor position
(define ustr-cursor-frontside
  (lambda (ustr)
    (let ((latter (ustr-latter ustr)))
      (if (not (null? latter))
	  (car latter)
	  (error "out of range in ustr-cursor-frontside")))))

;; backside element of cursor position
(define ustr-cursor-backside
  (lambda (ustr)
    (let ((former (ustr-former ustr)))
      (if (not (null? former))
	  (car former)
	  (error "out of range in ustr-cursor-backside")))))

(define ustr-cursor-delete-frontside!
  (lambda (ustr)
    (let ((latter (ustr-latter ustr)))
      (and (not (null? latter))
	   (ustr-set-latter! ustr (cdr latter))
	   #t))))

(define ustr-cursor-delete-backside!
  (lambda (ustr)
    (let ((former (ustr-former ustr)))
      (and (not (null? former))
	   (ustr-set-former! ustr (cdr former))
	   #t))))

(define ustr-cursor-set-frontside!
  (lambda (ustr elem)
    (let ((latter (ustr-latter ustr)))
      (and (not (null? latter))
	   (set-car! latter elem)
	   #t))))

(define ustr-cursor-set-backside!
  (lambda (ustr elem)
    (let ((former (ustr-former ustr)))
      (and (not (null? former))
	   (set-car! former elem)
	   #t))))
    
(define ustr-insert-elem!
  (lambda (ustr elem)
    (ustr-set-former! ustr (cons elem
				 (ustr-former ustr)))))

(define ustr-insert-seq!
  (lambda (ustr seq)
    (let* ((former (ustr-former ustr))
	   (new-former (append-reverse seq former)))
      (ustr-set-former! ustr new-former))))

